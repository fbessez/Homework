Fabien Bessez
Assign4 : Consensus and Election
Comp360 : Distributed Systems
Jeff Epstein


Possible Bugs: 
	1 - I attempted to update the lease["timestamp"] for replicas with the time.time()
		 of the delivery of a heartbeat to the viewleader but I do not think I was
		 successful.
	2 - Sometimes, if a replica or the new viewleader is missing more than ~15 entries
		in it's log, it takes two rpc commands to fully update the log. In other words,
		if a replica is trying to sync up with the viewleader who is receiving heartbeats,
		it might take two heartbeat cycles for the new replica to have matching logs with 
		older replicas

Instructions:
	Make sure to start a viewleader with 'python viewleader.py'. 
		There is the option to enter in 'python viewleader.py --viewleader REPLICAS_GO_HERE'
		where REPLICAS_GO_HERE is a comma delimited string without spaces. for example:
			localhost:39000,localhost:39001,localhost:39002 and so on
	Then perform whichever client or server RPCs that you would like.
	The replicas will adjust their logs accordingly. However, if you kill enough of the 
		replicas that a quorom will not be met by the remaining 'alive' replicas, then none 
		of the commands will work until you reboot the downed replicas.

Additional Thoughts:
	The instructions were clear. Your support was offered plenty of times. The assignment was interesting,
	though not as interesting as the previous assignment with the distributed hash table. I couldn't
	implement the DHT, but I found it to be more challenging. This assignment was definitely satisfying though.