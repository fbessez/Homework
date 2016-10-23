'''
Author: Fabien Bessez
Course: COMP360-02 Distributed Systems
Professor: Jeff Epstein
Assignment1: Remote Procedure Calls
'''

import socket
import json
import argparse
import struct


'''
The following code allows the client to take arguments from the command line.
It assigns values to these arguments and lets the user know which functions
are acceptable (set, get, print, query_all_keys). 
'''
parser = argparse.ArgumentParser()
parser.add_argument('--server', default='localhost')

subparsers = parser.add_subparsers(dest='cmd')

parser_set = subparsers.add_parser('set')
parser_set.add_argument('key', type=str)
parser_set.add_argument('val', type=str)

parser_get = subparsers.add_parser('get')
parser_get.add_argument('key', type=str)

parser_print = subparsers.add_parser('print')
parser_print.add_argument('text', nargs="*")

parser_query = subparsers.add_parser('query_all_keys')

args = parser.parse_args()

host = args.server # this would be the server IP address
port = 38000
timeout = 10

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
PURPOSE: To handle the 'print' command from terminal input
BEHAVIOR: Forms and marshals the message that the server can handle for
			a 'print' request
INPUT: str, str, int
OUTPUT: 
'''
def client_print(cmd, text, msg_count):
	msg = ""
	for val in text:
		msg = msg + " " + val
	msg = msg[1:]
	msg = json.dumps([cmd, msg, msg_count])
	encode_and_send(msg)

'''
PURPOSE: To handle the 'set' command from terminal input
BEHAVIOR: Forms and marshals the message that the server can handle for
			a 'set' request
INPUT: str, str, any, int
OUTPUT: 
'''
def client_set(cmd, key, val, msg_count):
	msg = json.dumps([cmd, key, val, msg_count])
	encode_and_send(msg)

'''
PURPOSE: To handle the 'get' command from terminal input
BEHAVIOR: Forms and marshals the message that the server can handle for
			a 'get' request
INPUT: str, str, int
OUTPUT:
'''
def client_get(cmd, key, msg_count):
	msg = json.dumps([cmd, key, msg_count])
	encode_and_send(msg)

'''
PURPOSE: To handle the 'get' command from terminal input
BEHAVIOR: Forms and marshals the message that the server can handle for
			a 'query_all_keys' request
INPUT: str, int
OUTPUT:
'''
def client_query(cmd, msg_count):
	msg = json.dumps([cmd, msg_count])
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
		client_print(args.cmd, args.text, msg_count)
	elif args.cmd == "set":
		client_set(args.cmd, args.key, args.val, msg_count)
	elif args.cmd == "get":
		client_get(args.cmd, args.key, msg_count)
	elif args.cmd == "query_all_keys":
		client_query(args.cmd, msg_count)
	else:
		print("Sorry, but that request could not be made")
		sock.close()
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
	print((sock.recv(msg_length, socket.MSG_WAITALL)).decode("utf-8"))
except (OSError): 
	print("OSError: Please call client.py with appropriate arguments.")
except (NameError):
	print("The connection was not established properly! The value, 'sock', was problematic!")




