Fabien Bessez
Course: COMP360-02 Distributed Systems
Professor: Jeff Epstein
Assignment1: Remote Procedure Calls

Instructions:
1. Open the command line on two separate 'computers'.
2. Run server.py by entering 'python3 server.py'
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
	3f. If all else fails, run 'python3 client.py -h' for help!


I believe my work has satisfied the assignment requirements fully without any bugs. The only thing that I am unsure about is if there are other exceptions besides the port being unavailable that I should be concerned about. 



I really enjoyed this assingnment. I feel much more comfortable with client-server interaction now and generally more aware of what is going on when I access the internet. The instructions were clear and providing the argparse code was very helpful. Besides the exception handling, I believe that this assignment was very carefully worded and detailed. In regards to support, you offered a lot of support and were willing to answer any questions about the assignment before every class, which was much appreciated. 