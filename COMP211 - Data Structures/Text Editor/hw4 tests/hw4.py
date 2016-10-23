''' 
Homework 4: Linked Lists
Editor Edition
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
		self.cursor = self.head
		self.tail = node(None, self.head, None)
		self.head.succ = self.tail
		return



def insert(buffer: buffer, c: str) -> None:
	return None

def delete_right(buffer: buffer) -> None:
	return None

def delete_left(buffer: buffer) -> None:
	return None

def move_left(buffer: buffer) -> None:
	return None

def move_right(buffer: buffer) -> None:
	return None

def set_pos(buffer: buffer, index: int) -> None:
	return None
	
def get_contents(buffer: buffer) -> List[str]:
	return ['','']














