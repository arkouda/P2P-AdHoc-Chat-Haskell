
import Network
import System.IO
import qualified Control.Exception as E
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import System.Environment (getArgs)
import Control.Concurrent.MVar (newMVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (forever, void)

main :: IO ThreadId
main = do
  arg <- getArgs
  strM <- newEmptyMVar
  sock <- listenOn $ PortNumber (read (arg!!0) :: PortNumber)
  forkIO (do
    forkIO (forever $ do
      userInput <- getLine
      let port = PortNumber (read (fst $ break (==' ') userInput) :: PortNumber)
      handle <- connectTo "localhost" port
      hPutStr handle userInput
      hClose handle
      putMVar strM "")
    forever $ do
      (handle, host, port) <- accept sock
      handle <- waitForInput handle
      output <- hGetLine handle
      putStrLn (host ++ "@" ++ show port ++ " : " ++ output)
      putMVar strM "")
  forever $ do
    result <- takeMVar strM
    putStr result
    
waitForInput :: Handle -> IO Handle
waitForInput hdl = do
      inputAvailableOnHandle <- E.try (hReady hdl) :: IO (Either E.IOException Bool)
      case inputAvailableOnHandle of
        Right True -> return hdl
        _ -> (threadDelay 100000) >> waitForInput hdl
