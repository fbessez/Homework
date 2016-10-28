
'''
Author: Fabien Bessez
Course: COMP360-02 Distributed Systems
Professor: Jeff Epstein
Assignment2: Remote Procedure Calls with 
			 ViewLeader, 
			 Heartbeats, 
			 Group View
			 Deadlock Detector
			 Lock Releasing post-crash of server
'''

import socket
import json
import struct
import time

lock_keeper = {}
id_time_server_pos = {}
active_servers = []
granted_message = json.dumps({"status" : "granted"})
retry_message = json.dumps({"status": "retry"})
ok_message = json.dumps({"status": "ok"})
heartbeat_received_message = json.dumps({"status" : "heartbeat accepted"})
success_code = 200


host = 'localhost'
port = 39000
bound_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# The following while loop will try to bind to the first available port
# between 38000 and 38010 in ascending order
while port < 39010 and port >= 39000:
	try: 
		bound_socket.bind(('', port))
		break
	except (OSError): 
		port = port
	port = port + 1
bound_socket.listen(1)

'''
PURPOSE: Simplifies a send call
BEHAVIOR: Encodes the desired message, sends the length of that message and then
			sends the rest of the message
INPUT: response (list)
OUTPUT: sock.sendall
'''
def send_to_client(response):
	msg_length_encoded = struct.pack("!i", len(response))
	sock.sendall(msg_length_encoded)
	sock.sendall(str.encode(response))


'''
PURPOSE: Manages the active servers list
BEHAVIOR: Checks to see if server had previously failed.
			If so, it does not add it to the active servers list
			Otherwise, it does.
INPUT: list
OUTPUT: 
'''
def manage_heartbeats(listdata):
	address = listdata[1]
	unique_id = listdata[2]
	time_of_receipt = listdata[3]
	if unique_id in id_time_server_pos and id_time_server_pos[unique_id]  == "failed":
		send_to_client("Not added! because server crashed previously")
		return
	elif unique_id not in id_time_server_pos:
		active_servers.append(address)
	id_time_server_pos[unique_id] = [address, time_of_receipt]
	print(id_time_server_pos)
	print("-----------------")
	print(active_servers)

'''
PURPOSE: Gets the current epoch
BEHAVIOR: Calculates the epoch based on failed servers and active servers
INPUT: dict
OUTPUT: int
'''
def get_epoch(id_time_server_pos):
	epoch = len(id_time_server_pos)
	for key in id_time_server_pos:
		if id_time_server_pos[key] == "failed":
			epoch = epoch + 1
		else:
			epoch = epoch + 0
	return epoch

'''
PURPOSE: Checks to see if servers timed out
BEHAVIOR: Checks to see if server has not sent heartbeat in 30 seconds.
			If it hasn't -> update the id_time_server_pos dict
			If it has -> do nothing
INPUT: list
OUTPUT: 
'''
def check_servers(id_time_server_pos):
	current_time = time.time()
	for key in id_time_server_pos:
		time_stamp = id_time_server_pos[key][1]
		if type(time_stamp) == str:
			None
		elif current_time - time_stamp > 30:
			active_servers.remove(id_time_server_pos[key][0])
			for lock in lock_keeper:
				if id_time_server_pos[key][0][9:] in lock_keeper[lock]:
					print("Server on " + str(id_time_server_pos[key][0][10:]) + "crashed. Released it's locks!")
					lock_keeper[lock].remove(id_time_server_pos[key][0][9:])
			id_time_server_pos[key] = "failed"
		else:
			None
	return id_time_server_pos



'''
PURPOSE: To detect deadlocks!
BEHAVIOR: It forms tuples of all requesters dependent on other
			requesters ahead of it in the lock queue
			If there exists a scenario where (a,b) and (b,a) exists
				then yes deadlock
			otherwise, no deadlock
INPUT: dict
OUTPUT: bool
'''
def detect_deadlock(lock_keeper):
	pairs = []
	for lock in lock_keeper:
		list_of_requesters = lock_keeper[lock]
		num_of_requesters = len(list_of_requesters)
		max_index = num_of_requesters - 1
		while max_index >= 0:
			index = 0
			while index != max_index:
				pairs.append((list_of_requesters[index],list_of_requesters[max_index]))
				index = index + 1
			max_index = max_index - 1
	for (a,b) in pairs:
		if (b, a) in pairs: 
			print("Deadlock has been detected...")
			print("Requester " + a + " relies on requester " + b + " and vice versa.")
			return True
	return False

'''
PURPOSE: Checks to see if the desired lock is available --> helper for lock_manager
BEHAVIOR: Checks to see if 
		a) lock exists 
		b) lock exists and has owner
	or 	c) lock exists and has no owner
INPUT: string, string, list
OUTPUT: bool
'''
def is_lock_available(lock_id, requester_id, lock_keeper):
	if lock_id not in lock_keeper:
		return True
	elif lock_id in lock_keeper and lock_keeper[lock_id] == []:
		return True
	elif lock_id in lock_keeper and lock_keeper[lock_id][0] == requester_id:
		return True
	else:
		return False

'''
PURPOSE: Responds to lock requests
BEHAVIOR: Checks to see if lock can be obtained or not and then delivers the status to requester
INPUT: string, string, list
OUTPUT: send_to_client
'''
def lock_manager(lock_id, requester_id, lock_keeper):
	if is_lock_available(lock_id, requester_id, lock_keeper):
		lock_keeper[lock_id] = [requester_id]
		send_to_client(granted_message)
	else:
		if requester_id not in lock_keeper[lock_id]:
			lock_keeper[lock_id].append(requester_id)
		send_to_client(retry_message)
	print("Current lock_system: " + str(lock_keeper))
	detect_deadlock(lock_keeper)


'''
PURPOSE: Releases locks!
BEHAVIOR: requester releases lock if it was on waiting list or owned it already
INPUT: string, string, list
OUTPUT: send_to_client
'''
def lock_release(lock_id, requester_id, lock_keeper):
	if lock_id in lock_keeper:
		for val in lock_keeper[lock_id]:
			if val == requester_id:
				lock_keeper[lock_id].remove(val)
				send_to_client("Successful Release")
				# send_to_client(ok_message)
	else:
		send_to_client("Error: The lock that you are trying to release doesn't exist")
	print("Current lock_system: " + str(lock_keeper))


'''
PURPOSE: Filters incoming messages
BEHAVIOR: checks first index of the listdata for different RPCs.
INPUT: list, list
OUTPUT: 
'''
def function_filter(listdata, lock_keeper):
	if listdata[0] == "query_servers":
		epoch = get_epoch(id_time_server_pos)
		check_servers(id_time_server_pos)
		response = json.dumps({'epoch': epoch, "servers": active_servers}) # available_servers if use that function
		send_to_client(response)
	elif listdata[0] == "heartbeat":
		listdata.append(time.time())
		print("Heartbeat Received")
		manage_heartbeats(listdata)
		check_servers(id_time_server_pos)
		print(id_time_server_pos)
		send_to_client(heartbeat_received_message)
	elif listdata[0] == "lock_get":
		lock_manager(listdata[1], listdata[2], lock_keeper)
	elif listdata[0] == "lock_release":
		lock_release(listdata[1], listdata[2], lock_keeper)

'''
PURPOSE: Receives and decodes messages
BEHAVIOR: receives message -> unpacks it -> converts to string -> convert from json to type
INPUT: sock
OUTPUT: list
'''
def receive_and_decode(sock):
	msg_length_encoded = sock.recv(4, socket.MSG_WAITALL)
	msg_length, = struct.unpack("!i", msg_length_encoded)
	data = sock.recv(msg_length, socket.MSG_WAITALL)
	if not data: return "NoData"
	strdata = data.decode("utf-8")
	listdata = json.loads(strdata)
	return listdata



while True:
	try:
		sock, (addr, accepter_port) = bound_socket.accept()
		listdata = receive_and_decode(sock)
		if listdata == "NoData":
			break
		function_filter(listdata, lock_keeper)
		sock.close()
	except struct.error:
		break

