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
import argparse
import uuid
import time

parser = argparse.ArgumentParser()
parser.add_argument('--server', default='localhost')
parser.add_argument('--viewleader', default='localhost')
parser.add_argument('--port', default=39000)

subparsers = parser.add_subparsers(dest='cmd')

parser_lock_get = subparsers.add_parser('lock_get')
parser_lock_get.add_argument('lock_id', type=str)
parser_lock_get.add_argument('--role', default='writer')

args = parser.parse_args()

host = args.server
view_host = args.viewleader
port = int(args.port)
serverport = port
print(serverport)
socket_id = str(uuid.uuid4())


'''
PURPOSE: Simplifies the process of receiving msg_length and then the msg.
		 It reduces redundancies!
BEHAVIOR: It unpacks the length of the msg into a 32 bit binary value. then
		 it receives he rest of the incoming msg
INPUT: sock
OUTPUT: str
'''
def receive_msgs(sock):
	msg_length_encoded = sock.recv(4, socket.MSG_WAITALL)
	msg_length, = struct.unpack("!i", msg_length_encoded)
	return sock.recv(msg_length, socket.MSG_WAITALL)



'''
PURPOSE: Simplifies the msg_length_encoding and sock.send calls
BEHAVIOR: It packs the length of the msg into a 32 bit binary value
			then sends both that binary value and the rest of the msg
INPUT: msg: String
OUTPUT: sock.sendall
'''
def encode_and_send(msg, sock):
	msg_length_encoded = struct.pack("!i", len(msg))
	sock.sendall(msg_length_encoded)
	sock.sendall(str.encode(msg))


'''
PURPOSE: Connects to the viewleader to either send heartbeat or get locks
BEHAVIOR: Creates connection to viewleader -> 
			then either sends heartbeat and waits for confirmation
			OR sends a lock request and will retry until it obtains the lock
INPUT: str, int, int, str
OUTPUT: 
'''
def connect_to_viewleader(view_host, port, socket_id, command):
	while port < 39010 and port >= 39000:
		try:
			address = str(view_host) + ":" + str(port)
			print("Trying to connect to " + str(view_host) + ":" + str(port) + "...")
			sock = socket.create_connection((view_host,port))
			if sock and command == 0:
				serverport = port
				print("Connected!")
				heartbeat = json.dumps(["heartbeat", address, socket_id])
				encode_and_send(heartbeat, sock)
				print("Sending over heartbeat...")
				data = receive_msgs(sock)
				if data:
					strdata = data.decode("utf-8")
					print(json.loads(strdata))
				break
			elif sock and command == 1:
				msg = json.dumps([args.cmd, args.lock_id, (":" + str(port)), 2])
				encode_and_send(msg, sock)
				data = receive_msgs(sock)
				if data:
					strdata = data.decode("utf-8")
					if json.loads(strdata)['status'] == "retry":
						print("Retrying for " + args.lock_id + "...")
						connect_to_viewleader(view_host, port, socket_id, 0)
						time.sleep(5)
						connect_to_viewleader(view_host, port, socket_id, 1)
					else:
						print(strdata)
				break
		except (OSError):
			port = port
		else:
			break
		port = port + 1

if args.cmd == "lock_get":
	connect_to_viewleader(view_host, port, socket_id, 1)
else:
	connect_to_viewleader(view_host, port, socket_id, 0)

'''
These are the two locally stored variables
locally_stored_variable is the data structure that we will be 
	modifying with the RPC calls
'''
locally_stored_variable = {}

host = 'localhost'
port = 38000
bound_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# The following while loop will try to bind to the first available port
# between 38000 and 38010 in ascending order
while port < 38010:
	try: 
		bound_socket.bind(('', port))
		break
	except (OSError): 
		port = port
	port = port + 1
bound_socket.listen(1)


#---------------------------------------------------------

'''
PURPOSE: To handle the 'print' RPC 
BEHAVIOR: Prints the intended msg to server console and provides
			encode_and_send the appropriate response to
			send to the client.
INPUT: list, dict
OUTPUT: str
'''
def print_call(listdata, locally_stored_variable, sock):
	response = json.dumps([200, listdata[1], listdata[2]])
	encode_and_send(response, sock)
	print("Printing: " + str(listdata[1]))

'''
PURPOSE: To handle the 'set' RPC
BEHAVIOR: Assigns variable names to the intended key, val pair that
			the user inputs. Then, it updates the local dict variable.
			Lastly, it provides encode_and_send the appropriate response
			to send to the client.
INPUT: list, dict
OUTPUT: str
'''
def set_call(listdata, locally_stored_variable, sock):
	key, val = listdata[1], listdata[2]
	locally_stored_variable[key] = val
	jsonified_var = json.dumps(locally_stored_variable)
	response = json.dumps([200, locally_stored_variable, listdata[3]])
	encode_and_send(response, sock)
	print("Current dict: " + jsonified_var)

'''
PURPOSE: To handle the 'get' RPC
BEHAVIOR: Checks to see if the user-input key exists in the local dict.
			If that is the case, then it will prepare a response to the client
			and provide that response to encode_and_send. If the key does
			not exist, then it will send a 'sorry' message to the client.
INPUT: list, dict
OUTPUT: encode_and_send
'''
def get_call(listdata, locally_stored_variable, sock):
	key = listdata[1]
	if key in locally_stored_variable:
		val = locally_stored_variable[key]
		response = json.dumps([200, val, listdata[2]])
		encode_and_send(response, sock)
	else:
		encode_and_send("Sorry, but your key does not exist...yet!" + 
			" Try using the \'set\' function.", sock)
'''
PURPOSE: To handle the 'query_all_keys' RPC
BEHAVIOR: Creates a list of the local dict's keys and then prepares a
			a proper response for encode_and_send to send to the client.
			In the case where there are no keys in the dict, it will tell
			encode_and_send to inform the client. 
INPUT: list, dict
OUTPUT: str
'''
def query_call(listdata, locally_stored_variable, sock):
	list_of_keys = list(locally_stored_variable.keys())
	if len(list_of_keys) >= 1:
		response = json.dumps([200, list_of_keys, listdata[1]])
		encode_and_send(response, sock)
		print("Sent over these keys: " + str(list_of_keys))
	else:
		encode_and_send("Sorry, but there are no keys in the dict...yet!", sock)

'''
PURPOSE: To filter the RPCs in the appropriate way. So that a 'print'
			call, actually does what the client expects.
BEHAVIOR: Checks the first index of the given list and checks to see
			if it matches one of the 'agreed upon' function names. If it
			matches, then it will be sent to the appropriate function for
			further action.
INPUT: list, dict
OUTPUT: 
'''
def function_filter(listdata, locally_stored_variable, sock):
	if listdata[0] == "print":
		print_call(listdata, locally_stored_variable, sock)
	elif listdata[0] == "set":
		set_call(listdata, locally_stored_variable, sock)
	elif listdata[0] == "get":
		get_call(listdata, locally_stored_variable, sock)
	elif listdata[0] == "query_all_keys":
		query_call(listdata, locally_stored_variable, sock)
	else:
		encode_and_send("Rejecting RPC request because function is unknown")

'''
This while loop allows the server to listen and accept different clients
without closing after every client connection closes. It accepts connections,
receives msg_lengths in binary, unpacks that binary, receives the remainder
of the msgs, converts the messages from binary string to utf-8 strings, and
calls the function_filter function. Furthermore, this loop prints the string
form of what the server received, as well as the total amount of messages that
the server has received since being established.
'''
while True:
	try:
		bound_socket.settimeout(5.0)
		sock, (addr, accepter_port) = bound_socket.accept()
		data = receive_msgs(sock)
		if not data: break
		strdata = data.decode("utf-8")
		jsondata = json.loads(strdata)
		print("Received: " + str(jsondata))
		function_filter(jsondata, locally_stored_variable, sock)
	except struct.error:
		print("The client that you were connected to has failed to provide proper arguments.")
	except socket.timeout:
		connect_to_viewleader(view_host, serverport, socket_id, 0)

