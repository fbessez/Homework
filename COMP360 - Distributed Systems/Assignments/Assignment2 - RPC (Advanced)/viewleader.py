
'''
Author: Fabien Bessez
Course: COMP360-02 Distributed Systems
Professor: Jeff Epstein
Assignment1: Remote Procedure Calls with ViewLeader, 
			 Heartbeats, Group View and Centralized Locking
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

# if timeout:
# 	bound_socket.settimeout(timeout)

def send_to_client(response):
	msg_length_encoded = struct.pack("!i", len(response))
	sock.sendall(msg_length_encoded)
	sock.sendall(str.encode(response))

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

def get_epoch(id_time_server_pos):
	epoch = len(id_time_server_pos)
	for key in id_time_server_pos:
		if id_time_server_pos[key] == "failed":
			epoch = epoch + 1
		else:
			epoch = epoch + 0
	return epoch

def check_servers(id_time_server_pos):
	current_time = time.time()
	for key in id_time_server_pos:
		time_stamp = id_time_server_pos[key][1]
		if type(time_stamp) == str:
			None
		elif current_time - time_stamp > 5:
			active_servers.remove(id_time_server_pos[key][0])
			for lock in lock_keeper:
				if id_time_server_pos[key][0][9:] in lock_keeper[lock]:
					print("Server on " + str(id_time_server_pos[key][0][10:]) + "crashed. Released it's locks!")
					lock_keeper[lock].remove(id_time_server_pos[key][0][9:])
			id_time_server_pos[key] = "failed"
		else:
			None
	return id_time_server_pos

def is_lock_available(lock_id, requester_id, lock_keeper):
	if lock_id not in lock_keeper:
		return True
	elif lock_id in lock_keeper and lock_keeper[lock_id] == []:
		return True
	elif lock_id in lock_keeper and lock_keeper[lock_id][0] == requester_id:
		return True
	else:
		return False

def lock_manager(lock_id, requester_id, lock_keeper):
	if is_lock_available(lock_id, requester_id, lock_keeper):
		lock_keeper[lock_id] = [requester_id]
		send_to_client(granted_message)
	else:
		if requester_id not in lock_keeper[lock_id]:
			lock_keeper[lock_id].append(requester_id)
		send_to_client(retry_message)
	print("Current lock_system: " + str(lock_keeper))

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

