''' 
Homework 4: Linked Lists
Editor Edition
Fabien Bessez
'''

import typing
from typing import List

class node:
	def __init__(self, c, pred, succ):
		self.c = c
		self.pred = pred
		self.succ = succ
		return

class buffer:
	def __init__(self):
		self.head = node(None,None,None)
		self.tail = node(None, None, None)
		self.head.succ = self.tail
		self.tail.pred = self.head
		self.cursor = self.head
		return

def insert(b: buffer,c: str) -> None:
	new_node = node(c, b.cursor, b.cursor.succ) # name new node
	b.cursor.succ.pred = new_node   # reassign the succ's pred
	b.cursor.succ = new_node		# reassign the succ
	b.cursor = new_node				# cursor is now the new node
	return None

def get_contents(b: buffer) -> List[str]:
	before = ''
	n = b.head.succ          # this loops through from head to cursor
	while n.pred != b.cursor:
		before = before + n.c
		n = n.succ

	after = ''
	while n.c != None:        # this loops through from cursor to end
		after = after + n.c
		n = n.succ	
	return [before, after] # List[str]

def delete_right(b: buffer) -> None:
	if b.cursor.succ.c != None:     # if we are not at the end
		b.cursor.succ = b.cursor.succ.succ
		b.cursor.succ.pred = b.cursor
	return None

def delete_left(b: buffer) -> None:
	if b.cursor.pred != None:		# if we are not at the beginning
		b.cursor.succ.pred = b.cursor.pred
		b.cursor.pred.succ = b.cursor.succ
		b.cursor = b.cursor.pred
	return None

def move_left(b: buffer) -> None:
	if b.cursor.pred != None: 		# if we are not at the beginning
		b.cursor = b.cursor.pred
	return None

def move_right(b: buffer) -> None:
	if b.cursor.succ.c != None:		# if we are not at the end
		b.cursor = b.cursor.succ
	return None

def set_pos(b: buffer, index: int) -> None:
	b.cursor = b.head				# reassign cursor to the beginning
	i = 0
	while i != index:				# increment through the nodes
		b.cursor = b.cursor.succ	# stop when you're at the i pos
		i = i + 1
	return None




