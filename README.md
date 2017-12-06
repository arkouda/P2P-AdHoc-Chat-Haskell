# P2P-AdHoc-Chat-Haskell
Developed By Prabhtaj Singh Bhasin and Akash Gupta 

Assumptions:
	The Program assumes that an ad-hoc network has been initiated with a static IP in the range 192.168.1.[0-255] on a 32/64-bit linux distro.

Description:

	What it does?
	The program, allows a user to send/recieve messages to/from any other user running this program with the same ad-hoc config
	but a different IP within the range 192.168.1.[0-255], connected directly or indirectly to the users ad-hoc node. It also 
	lists all the nodes within the network, running an instance of this program.

	How it does it?
	To achieve this, we did the following:
	- When an instance of a program is started, the program scansfor ad-hoc nodes (with same config) in its range (neighbours) 
	and sends each of them a copy of its list of IP's of known nodes in the network (which only consists of the its IP at the 
	start). The message also contains a list which is used to track the path. The neighbours continue this process ahead and 
	keep adding their corresponding IP's to the list(s) until no nodes exist that are not in the already known IP list. After 
	this, the path is traced back to the source and an updated list is returned. This is done for all neighbours and ensures 
	that all nodes have been visited. The source then collects this data, removes duplicates from it and finally broadcasts it
	to all nodes in a BFS like manner, each node updates its 'online nodes list'. We named this procedure as '2Blast Protocol'.
	This essentially acheives data synchronisation of the known nodes in the network, but is not limited to it.
	- To send messeges, we implemented a version of DFS to find the receiver node.

Types of messages:
      Type 1: Messages that are intended for itself (ie. if the IP of the destination of the message matches that of the current node, those messages are recieved here)
      Type 2: (First Blast) First part of the synchronisation procedure that collects list of known IP's in the network as described above.
      Type 3: (ByPass) Essentially the Routing mechanism of this program. If a destination IP is not in the direct neighbours, then the message is sent via a different socket until the destination is found. If the destination IP is in the direct neighbours then the message is sent as Type 1 message.
      Type 4: (Second Blast) second part of the synchronisation process, all nodes accept this message as their list of online nodes.
     
How to run:
    (Assumption :- 'Stack ghc' and the libraries mentioned in .cabal are set up properly)
    - Enter the following command at the linux terminal
      > make
      OR
      (To directly run the program)
      > ./Whatsapp

How to Use:
    - The UI has three separated regions. one shows all known IP's in the network. The other one shows all messages recieved. The last one is for send message.
    - To send message, user needs to enter IP of the destination node in 'IP text box' and the message in 'message text box' and then press 'Enter key'.
    - The 'recieved message box' and 'Online Friend Box' are scrollable.
      

