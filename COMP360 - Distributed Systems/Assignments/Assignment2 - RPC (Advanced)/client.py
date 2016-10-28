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
import argparse
import struct
import time


'''
The following code allows the client to take arguments from the command line.
It assigns values to these arguments and lets the user know which functions
are acceptable (set, get, print, query_all_keys). 
'''
parser = argparse.ArgumentParser()
parser.add_argument('--server', default='localhost')
parser.add_argument('--viewleader', default='localhost')
parser.add_argument('--port', default=38000)

subparsers = parser.add_subparsers(dest='cmd')

parser_set = subparsers.add_parser('set')
parser_set.add_argument('key', type=str)
parser_set.add_argument('val', type=str)

parser_get = subparsers.add_parser('get')
parser_get.add_argument('key', type=str)

parser_print = subparsers.add_parser('print')
parser_print.add_argument('text', nargs="*")

parser_query = subparsers.add_parser('query_all_keys')

parser_query_servers = subparsers.add_parser('query_servers')

parser_lock_get = subparsers.add_parser('lock_get')
parser_lock_get.add_argument('lock_id', type=str)
parser_lock_get.add_argument('requester_id', type=str)

parser_lock_release = subparsers.add_parser('lock_release')
parser_lock_release.add_argument('lock_id', type=str)
parser_lock_release.add_argument('requester_id', type=str)

args = parser.parse_args()
host = args.server
view_host = args.viewleader # this would be the server IP address
timeout = 10
port = int(args.port)

# Determines whether to connect to server or viewleader upon startup
if args.cmd == "query_servers" or args.cmd == "lock_get" or args.cmd == "lock_release":
	port = 39000
else:
	port = 38000

while port < 39010 and port >= 39000:
	try:
		print("Trying to connect to " + str(view_host) + ":" + str(port) + "...")
		sock = socket.create_connection((view_host, port), timeout)
		if sock:
			print("Connected!")
		if sock is None:
			print("Can't connect")
		break
	except (OSError):
		port = port
	else:
		break
	port = port + 1


'''
PURPOSE: Connects to the viewleader!
BEHAVIOR: Creates connection with viewleader and then receives data! ...hopefully
INPUT: str, str, str, int, str, int
OUTPUT: str
'''
def connect_to_vl(cmd, lock_id, requester_id, msg_count, host, port):
	timeout = 5
	try:
		sock = socket.create_connection((host, port), timeout)
		if sock:
			encode_and_send_with_sock(json.dumps([cmd, lock_id, requester_id, msg_count]), sock)
			msg_length_encoded = sock.recv(4, socket.MSG_WAITALL)
			msg_length, = struct.unpack("!i", msg_length_encoded)
			response = sock.recv(msg_length, socket.MSG_WAITALL).decode("utf-8")
			return response
		else:
			print("can't connect")
	except (OSError):
		print("wowwo")

# The following while loop tries to create a connection with the specified
# host and port. If the port is unavailable it will try the following port
# until port == 38011, in which it will stop trying to connect.
while port < 38010:
	try:
		print("Trying to connect to " + str(host) + ":" + str(port) + "...") 
		sock = socket.create_connection((host, port), timeout)
		if sock: 
			print("Connected!")
		if sock is None:
			print("Can't connect")
		break
	except (OSError): 
		port = port
	else:
		break
	port = port + 1

'''
PURPOSE: Simplifies the process of sending msg_length and then the msg.
		 It reduces redundancies!
BEHAVIOR: It packs the length of the msg into a 32 bit binary value. then
		 it sends both that binary value and the rest of the msg to the client
INPUT: str, sock
OUTPUT: sock.sendall
'''
def encode_and_send_with_sock(msg, sock):
	msg_length_encoded = struct.pack("!i", len(msg))
	try: 
		sock.sendall(msg_length_encoded)
		sock.sendall(str.encode(msg))
	except (NameError):
		print("something")

'''
PURPOSE: Simplifies the process of sending msg_length and then the msg.
		 It reduces redundancies!
BEHAVIOR: It packs the length of the msg into a 32 bit binary value. then
		 it sends both that binary value and the rest of the msg to the client
INPUT: str
OUTPUT: sock.sendall
'''
def encode_and_send(msg):
	msg_length_encoded = struct.pack("!i", len(msg))
	try: 
		sock.sendall(msg_length_encoded)
		sock.sendall(str.encode(msg))
	except (NameError): 
		print("Something went wrong with the connection so your message could not be sent. Please check the server!")

'''
PURPOSE: Forms the msg to be sent to server or viewleader
BEHAVIOR: For the X amount of arguments given, it just puts those arguments into a list,
			then Jsonifys that list to be sent over the network.
INPUT: *args
OUTPUT: encode_and_send
'''
def client_msg_maker(*arg):
	msg = []
	for i in range(0, len(arg)):
		msg.append(arg[i])
	msg = json.dumps(msg)	
	encode_and_send(msg)


'''
PURPOSE: To filter the command from terminal input and direct the program
			to the appropriate function that will handle the given inputs
BEHAVIOR: Checks to see if the given command matches one of the 'known'
			functions. If not, then return an error message to user.
INPUT: list, int
OUTPUT: 
'''
def function_filter(args, msg_count):
	if args.cmd == "print":
		client_msg_maker(args.cmd, args.text, msg_count)
	elif args.cmd == "set":
		client_msg_maker(args.cmd, args.key, args.val, msg_count)
	elif args.cmd == "get":
		client_msg_maker(args.cmd, args.key, msg_count)
	elif args.cmd == "query_all_keys":
		client_msg_maker(args.cmd, msg_count)
	elif args.cmd == "query_servers":
		client_msg_maker(args.cmd, msg_count)
	elif args.cmd == "lock_get" or args.cmd == "lock_release":
		client_msg_maker(args.cmd, args.lock_id, args.requester_id, msg_count)
	else:
		print("Sorry, but that request could not be made")


'''
The following code keeps track of the msg_count, receives and unpacks
the length of incoming messages, as well as the actual incoming messages.
Lastly, it prints out the received messages in string form.
'''
msg_count = 0
function_filter(args, msg_count)
msg_count = msg_count + 1
try: 
	msg_length_encoded = sock.recv(4, socket.MSG_WAITALL)
	msg_length, = struct.unpack("!i", msg_length_encoded)
	response = sock.recv(msg_length, socket.MSG_WAITALL).decode("utf-8")
	print("Response: " + response)
	# The following causes the client to keep trying to obtain the lock every 5 seconds
	# until the client obtainas the lock!
	while json.loads(response) == {"status": "retry"}:
		print("Retrying for " + str(args.lock_id) + "...")
		time.sleep(5)
		response = connect_to_vl(args.cmd, args.lock_id, args.requester_id, msg_count, view_host, port)
		print(response)
		if response == {"status": "granted"}:
			print("Response: " + response)
			json.loads(response) == None

		else:
			response = response
	sock.close()
except (ValueError):
	sock.close()
except (struct.error):
	print("You can't do that!")


