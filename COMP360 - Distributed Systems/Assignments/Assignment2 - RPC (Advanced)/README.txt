Author: Fabien Bessez
Course: COMP360-02 Distributed Systems
Professor: Jeff Epstein
Assignment2: Remote Procedure Calls with 
			 ViewLeader, 
			 Heartbeats, 
			 Group View
			 Deadlock Detector
			 Lock Releasing post-crash of server


Instructions:
1. Run viewleader.py by entering 'python3 viewleader.py'
	A server won't be able to operate with the viewleader!
2. Run server.py by entering 'python3 server.py'
	2a. To specify whether you want the sever to communicate 
		with the viewleader or not, use --server or --viewleader.
	2b. NEW FEATURE: Servers can use the lock_get rpc by entering, 'python3 server.py lock_get LOCKNAME'.
3. Run client.py by entering 'python3 client.py'
	3a. To specify the server address of the host, enter
		'python3 client.py --server SERVER_ADDRESS' (if you do not specify the server address, it will default to the localhost)
	3b. To call the print function, enter
		'python3 client.py --server SERVER_ADDRESS print STRING'
	3c. To call the set function, enter
		'python3 client.py --server SERVER_ADDRESS set KEY VALUE'
	3d. To call the get function, enter
		'python3 client.py --server SERVER_ADDRESS get KEY'
	3e. To call the query_all_keys function, enter
		'python3 client.py --server SERVER_ADDRESS query_all_keys'
	3f. To call the query_servers function, enter
		'python3 client.py --server SERVER_ADDRESS query_servers'
		It will communicate with the view leader and figure out which servers are available to take requests
	3g. To call the lock_get function, enter
		'python3 client.py --server SERVER_ADDRESS lock_get LOCKNAME YOURNAME'
	3h. To call the lock_release function, enter
		'python3 client.py --server SERVER_ADDRESS lock_release LOCKNAME YOURNAME'
	3f. If all else fails, run 'python3 client.py -h' for help!


I believe my work has satisfied the assignment requirements fully. 

Extra Credit: 
	I believe I have correctly implemented both a deadlock detector and the viewleader's ability to cancel locks held by a server after it has crashed. 

	In order to test the deadlock detector, run the following commands in this sequence:
	'python3 viewleader.py'
	'python3 client.py lock_get lock1 user1'
	'python3 client.py lock_get lock2 user2'
	'python3 client.py lock_get lock1 user2'
	'python3 client.py lock_get lock2 user1'
	At this point, the viewleader should print to console the presence of a deadlock. If, from a different terminal, you release any of the locks via 'python3 client.py lock_release lock1 user2', for example, then the viewleader should no longer be reporting a deadlock. NOTE: Once, you release the lock, the original (and relentless) lock_get call will re-request for the lock. Be sure to KeyboardInterrupt that call ASAP to maintain a deadlock free environment after releasing.

	In order to test the cancel locks after crash task, run the following commands:
	'python3 viewleader.py'
	'python3 server.py'
	'python3 server.py' (from different computer)
	At this point the viewleader should display the two available servers in it's terminal. 
	Now kill either of the servers...
	The viewleader should keep displaying the view as if a server has not failed! After the timeout has been met (I believe it is 30 seconds), you should see output differences in the terminal of the Viewleader. The view should no longer contain the address and port of the failed server.
	If you want to test this from the client side, 
	just call 'python3 client.py query_servers' before killing the server and then again after the timeout time has elapsed -- you should get different available servers. 

Again, I enjoyed this assignment. I decided to build on top of the code from my assignment1 and it works just fine, but now I feel like this may be a little bit inefficient if the viewleader was responsible for a large amount of locks. Some additional thoughts: I thought that we had a TON of time to complete this.In regards to support, you offered a lot of support and were willing to answer any questions about the assignment before every class, which was much appreciated. 