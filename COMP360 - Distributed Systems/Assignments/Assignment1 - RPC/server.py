'''
Author: Fabien Bessez
Course: COMP360-02 Distributed Systems
Professor: Jeff Epstein
Assignment1: Remote Procedure Calls
'''

import socket
import json
import struct

'''
These are the two locally stored variables
locally_stored_variable is the data structure that we will be 
	modifying with the RPC calls
msgs_received is simply a counter that keeps track of how many
	requests the server has received
'''
locally_stored_variable = {}
msgs_received = 0

host = 'localhost'
port = 38000
timeout = 10
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
if timeout:
	bound_socket.settimeout(timeout)


#---------------------------------------------------------
'''
PURPOSE: Simplifies the msg_length_encoding and sock.send calls
BEHAVIOR: It packs the length of the msg into a 32 bit binary value
			then sends both that binary value and the rest of the msg
INPUT: msg: String
OUTPUT: sock.sendall
'''
def encode_and_send(msg):
	msg_length_encoded = struct.pack("!i", len(msg))
	sock.sendall(msg_length_encoded)
	sock.sendall(str.encode(msg))
'''
PURPOSE: To handle the 'print' RPC 
BEHAVIOR: Prints the intended msg to server console and provides
			encode_and_send the appropriate response to
			send to the client.
INPUT: list, dict
OUTPUT: str
'''
def print_call(listdata, locally_stored_variable):
	response = json.dumps([200, listdata[1], listdata[2]])
	encode_and_send(response)
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
def set_call(listdata, locally_stored_variable):
	key, val = listdata[1], listdata[2]
	locally_stored_variable[key] = val
	jsonified_var = json.dumps(locally_stored_variable)
	response = json.dumps([200, locally_stored_variable, listdata[3]])
	encode_and_send(response)
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
def get_call(listdata, locally_stored_variable):
	key = listdata[1]
	if key in locally_stored_variable:
		val = locally_stored_variable[key]
		response = json.dumps([200, val, listdata[2]])
		encode_and_send(response)
	else:
		encode_and_send("Sorry, but your key does not exist...yet!" + 
			" Try using the \'set\' function.")
'''
PURPOSE: To handle the 'query_all_keys' RPC
BEHAVIOR: Creates a list of the local dict's keys and then prepares a
			a proper response for encode_and_send to send to the client.
			In the case where there are no keys in the dict, it will tell
			encode_and_send to inform the client. 
INPUT: list, dict
OUTPUT: str
'''
def query_call(listdata, locally_stored_variable):
	list_of_keys = list(locally_stored_variable.keys())
	if len(list_of_keys) >= 1:
		response = json.dumps([200, list_of_keys, listdata[1]])
		encode_and_send(response)
		print("Sent over these keys: " + str(list_of_keys))
	else:
		encode_and_send("Sorry, but there are no keys in the dict...yet!")

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
def function_filter(listdata, locally_stored_variable):
	if listdata[0] == "print":
		print_call(listdata, locally_stored_variable)
	elif listdata[0] == "set":
		set_call(listdata, locally_stored_variable)
	elif listdata[0] == "get":
		get_call(listdata, locally_stored_variable)
	elif listdata[0] == "query_all_keys":
		query_call(listdata, locally_stored_variable)
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
		sock, (addr, accepter_port) = bound_socket.accept()
		print("Accepting connection from host", addr, ":" , port)
		msg_length_encoded = sock.recv(4, socket.MSG_WAITALL)
		msg_length, = struct.unpack("!i", msg_length_encoded)
		data = sock.recv(msg_length, socket.MSG_WAITALL)
		msgs_received = msgs_received + 1
		if not data: break
		strdata = data.decode("utf-8")
		listdata = json.loads(strdata)
		print("Received: " + str(listdata))
		print("Total Messages Received: " + str(msgs_received))
		function_filter(listdata, locally_stored_variable)
	except struct.error:
		print("The client that you were connected to has failed to provide proper arguments.")
	except socket.timeout:
		print("The server timed out!")
		break





















