{-#Language TemplateHaskell #-}

import Control.Concurrent.MVar (readMVar,newMVar,MVar(..))
import Data.List ((\\),union,intersect)
import Data.List.Split (chunksOf)
import Control.Exception (try,IOException(..))
import Text.Read (readMaybe)
import Network (HostName(..),PortNumber(..),Socket(..),PortID(..),connectTo,accept,listenOn)
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Brick.BChan (newBChan, writeBChan)
import Control.Monad.IO.Class (liftIO)
import System.IO (Handle(..),hClose,hPutStrLn,hGetLine,hReady)
import System.Process (readCreateProcess, createProcess, shell)
import Control.Concurrent (threadDelay,forkIO)
import Control.Monad (void,forever)
import qualified Graphics.Vty as V  
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Data.Vector as Vec
import Brick.Widgets.Border (borderWithLabel)
import Brick.AttrMap (AttrMap(..),attrMap)
import Brick.Widgets.Core
       ( (<+>)
       , (<=>)
       , str
       , vLimit
       , hLimit
       , hBox
       )
import System.Environment (getArgs)

data St =
  St { _focusRing :: F.FocusRing Name
     , _ipAddr :: E.Editor String Name
     , _msg :: E.Editor String Name
     , _recievedMsg :: L.List Name (HostName,String,String)
     , _friendList :: L.List Name (String)
     , _errormsg :: String
     , _pendingList :: MVar [(String,String)]
     }

data Name = IpAddr
          | MSG
          | Recieved
          | FriendList
          deriving (Ord, Show, Eq)

data NewEvent = NewEventS1 (Handle,HostName,PortNumber) | NewEventS4 [String] | NewEventS3 (String,String) deriving (Eq,Show)

type Bypass = (String, [String], [String], String)

type Register = ([String],[String])

makeLenses ''St

main :: IO()
main = do
  ip <- getIP
  neighbours <- getNeighbours
  putStrLn $ show neighbours
  mVar <- newMVar []
  sockS1 <- listenOn portS1
  sockS2 <- listenOn portS2
  sockS3 <- listenOn portS3
  sockS4 <- listenOn portS4
  chan <- newBChan 10
  let online1 = [ip]++neighbours
  activeN <- (filter id) <$> (sequence $ map (\x-> sendMessege x portS2 (show ([ip],online1))) neighbours)
  online <- gandirecursion online1 (length activeN) sockS2
  activeN1 <- (filter id) <$> (sequence $ map (\x-> sendMessege x portS4 (show (online,neighbours))) neighbours)
  forkIO $ forever $ do --Messege Reciever  S1
    hhpS1 <- accept sockS1
    waitForInput (_1Of3 hhpS1)
    writeBChan chan (NewEventS1 hhpS1)

  forkIO $ forever $ do --Node Registration  S2
    hhpS2 <- accept sockS2
    hdl <- waitForInput (_1Of3 hhpS2)
    (path,visited) <- (\x-> read x :: Register) <$> hGetLine hdl
    hClose hdl
    case (neighbours \\ visited) of
      [] -> sendMessege (last $ path) portS2 (show $ (init path,visited)) >> return ()
      e -> sequence_ $ map (\x-> sendMessege x portS2 (show (path++[ip],visited++e))) e

  forkIO $ forever $ do --Messege Bypass  S3
    hhpS3 <- accept sockS3
    handle <- waitForInput (_1Of3 hhpS3)
    bypass <- (\x-> read x :: Bypass) <$> hGetLine handle
    hClose handle
    neighbours <- getNeighbours
    if elem (getDest bypass) neighbours then sendFinalMessege bypass >> return ()
      else do
      case (neighbours \\ getVisited bypass) of
        [] -> if (getPath bypass) == [] then 
                writeBChan chan (NewEventS3 (getMsg bypass,getDest bypass))          
              else sendMessege (last $ getPath bypass) portS3 (show $ goBack ip bypass) >> return ()
        e -> sendMessege (head e) portS3 (show $ goAhead ip bypass) >> return ()

  forkIO $ forever $ do --Second Blast  S4
    hhpS4 <- accept sockS4
    handle <- waitForInput (_1Of3 hhpS4)
    (online2, visited) <- (\x-> read x :: Register) <$> hGetLine handle
    hClose handle
    case (neighbours \\ visited) of
      [] -> return ()
      e -> sequence_ $ map (\x-> sendMessege x portS4 (show $ (online2,visited++e))) e
    writeBChan chan (NewEventS4 (filter (/=ip) online2))
    
  void $ M.customMain (V.mkVty V.defaultConfig) (Just chan) theApp (initialState mVar)

gandirecursion :: [String] -> Int -> Socket -> IO [String]
gandirecursion online i sock = if i<=0 then return online else do
  hhp <- accept sock
  handle <- waitForInput (_1Of3 hhp)
  (path, visited) <- (\x-> read x :: Register) <$> hGetLine handle
  hClose handle
  gandirecursion (online `union` visited) (i-1) sock

getNeighbours :: IO [String]
getNeighbours = do
  list <- lines <$> readCreateProcess (shell neighbourCmd) ""
  ip <- getIP
  return $ filter (/=ip) list

getIP :: IO String
getIP = init <$> init <$> readCreateProcess (shell "hostname -I") ""

waitForInput :: Handle -> IO Handle
waitForInput hdl = do
  inputAvailableOnHandle <- try (hReady hdl) :: IO (Either IOException Bool)
  case inputAvailableOnHandle of
    Right True -> return hdl
    _ -> (threadDelay 10000) >> waitForInput hdl

initialState :: MVar [(String,String)] -> St
initialState mVar =
  St (F.focusRing [IpAddr,MSG,Recieved])
     (E.editor IpAddr (Just 1) "")
     (E.editor MSG (Just 1) "")
     (L.list Recieved (Vec.fromList []) 1)
     (L.list FriendList (Vec.fromList []) 1)
     ""
     mVar

----------------------------Wrapper Functions-----------------------------
getDest :: Bypass -> String
getDest (a,b,c,d) = a
getPath :: Bypass -> [String]
getPath (a,b,c,d) = b
getVisited :: Bypass -> [String]
getVisited (a,b,c,d) = c
getMsg :: Bypass -> String
getMsg (a,b,c,d) = d
goBack :: String -> Bypass -> Bypass
goBack ip (a,b,c,d) = (a,init b,c++[ip],d)
goAhead :: String -> Bypass -> Bypass
goAhead ip (a,b,c,d) = (a,b++[ip],c++[ip],d)
fun :: [String] -> T.Widget n
fun = foldl (\x y-> x <+> (str y)) (str "") 
neighbourCmd :: String
neighbourCmd = "nmap -sn 192.168.1.0-15 -n | grep \"Nmap scan\" | sed 's/Nmap scan report for //g'"
_1Of3 (a,_,_) = a
portS1 = PortNumber (3001 :: PortNumber)
portS2 = PortNumber (3002 :: PortNumber)
portS3 = PortNumber (3003 :: PortNumber)
portS4 = PortNumber (3004 :: PortNumber)

------------------------------Brick--------------------------------

theApp :: M.App St NewEvent Name
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        }

theMap :: AttrMap
theMap = attrMap V.defAttr
         []

appEvent :: St -> T.BrickEvent Name NewEvent -> T.EventM Name (T.Next St)
appEvent st (T.AppEvent (NewEventS1 hhp)) = M.continue =<< liftIO (readMsg st hhp)
appEvent st (T.AppEvent (NewEventS4 online)) = M.continue =<< liftIO (sendMsg2 online st) 
appEvent st (T.AppEvent (NewEventS3 mh)) = M.continue =<< (liftIO $ do readM <- readMVar (st^.pendingList)
                                                                       newmvar <- newMVar (readM++[mh])
                                                                       return st {_pendingList = newmvar})
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    V.EvKey V.KEnter [] -> M.continue =<< liftIO (sendMsg st)
    _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
                          Just IpAddr -> T.handleEventLensed st ipAddr E.handleEditorEvent ev
                          Just MSG -> T.handleEventLensed st msg E.handleEditorEvent ev
                          Just Recieved -> do
                            list_ <- L.handleListEvent ev (st^.recievedMsg)
                            return $ st{_recievedMsg = list_}
                          Nothing -> return st
appEvent st _ = M.continue st

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
  where
    ip = F.withFocusRing (st^.focusRing) (E.renderEditor fun)  (st^.ipAddr)
    msg1 = F.withFocusRing (st^.focusRing) (E.renderEditor fun) (st^.msg)
    rmsg = L.renderList listDrawElementR True (st^.recievedMsg)
    frndList = L.renderList listDrawElementF True (st^.friendList)
    ui = C.vCenter $ hBox [(C.hCenter $
                            (borderWithLabel (str "Friends Online") $
                             hLimit 25 $ vLimit 15 $ frndList)),
                           (C.hCenter $
                            (borderWithLabel (str "Recieved Message") $
                             hLimit 25 $ vLimit 15 $ rmsg)),
                           C.hCenter $
                           (str "Ip Address: " <+> (hLimit 30 ip)) <=>
                           str " " <=>
                           (str "Message: " <+> (hLimit 30 msg1)) <=>
                           (case st^.errormsg of
                             [] -> str " "
                             e -> (str " ") <=> (str " ") <=> (str ("error !! "++e)))]


-------------------------------------------------------------------------------------------

listDrawElementR :: Bool -> (HostName,String,String) -> T.Widget Name
listDrawElementR _ (host,portn,mssg) =  C.hCenter $ (str $ "host: " ++ host) <=> (foldl1 (<=>) (map str (chunksOf 25 ("message: "++mssg)))) <=> (str " ")

listDrawElementF :: Bool -> String -> T.Widget Name
listDrawElementF _ s =  C.hCenter $ (str s) <=> (str " ")

readMsg :: St -> (Handle,HostName,PortNumber)-> IO St
readMsg st (hnd,hsn,port) = do
  strng <- hGetLine hnd
  let pos = Vec.length $ (st^.recievedMsg)^.(L.listElementsL)
  return st{_recievedMsg = L.listInsert pos (hsn,show port,strng) (st^.recievedMsg)}

sendMsg :: St -> IO St
sendMsg st = do
  let host = head $ E.getEditContents $ (st^.ipAddr)
      mssg = head $ E.getEditContents $ (st^.msg)
  ip <- getIP
  neighbours <- getNeighbours
  if elem host neighbours then do
    programRunningStatus <- sendMessege host portS1 mssg
    if programRunningStatus then return st
      else do
      mvar <- readMVar (st^.pendingList)
      newmvar <- newMVar (mvar++[(mssg,host)])
      return st {_pendingList = newmvar}
    else (if neighbours == [] then do
             mvar <- readMVar (st^.pendingList)          
             newmvar <- newMVar (mvar++[(mssg,host)])
             return st {_pendingList = newmvar}
           else sendMessege (head neighbours) portS2 (show (host,[ip],[ip],mssg)) >> return st)

sendMsg2 :: [String] -> St -> IO St
sendMsg2 online st = do
  mvar <- readMVar $ st^.pendingList
  let newcomers = concat $ zipWith (\x y-> filter (\(m,n)-> n==x) y) online (repeat mvar)
  sequence $ map sendMsg3 newcomers
  let newmorecomers = mvar \\ newcomers
  newmvar <- newMVar newmorecomers 
  return st {_friendList = L.listReplace (Vec.fromList online) (Just 1) (st^.friendList), _pendingList = newmvar}

sendMsg3 :: (String,String) -> IO ()
sendMsg3 (msg,host) = do
  ip <- getIP
  neighbours <- getNeighbours
  if elem host neighbours then sendMessege host portS1 msg >> return ()
    else sendMessege (head neighbours) portS2 (show (host,[ip],[ip],msg)) >> return ()
    

sendMessege :: String -> PortID -> String -> IO Bool
sendMessege ip portID str = do
  handle <- try (connectTo ip portID) :: IO (Either IOException Handle)
  case handle of
    Right hdl -> do
      hPutStrLn hdl str
      hClose hdl
      return True
    Left e -> return False 
  
sendFinalMessege :: Bypass -> IO Bool
sendFinalMessege bypass = do
    finalHandle <- try (connectTo (getDest bypass) portS1) :: IO (Either IOException Handle)
    case finalHandle of
      Right handle -> do
        hPutStrLn handle (getMsg bypass)
        hClose handle
        return True
      Left e -> return False
