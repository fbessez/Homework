'''
Homework 6: Hash Tables
Fabien Bessez

Generic Collection Types
Hash Tables
Linked Lists
Type Annotations

'''

import typing
from typing import List, Any, TypeVar, Generic, Callable


T = TypeVar('T')


class node(Generic[T]):

	def __init__(self, value, succ): # c = str, succ = node
		self.val = value
		self.succ = succ
		return

class set211(Generic[T]):

	def __init__(self, capacity, hashfn):
		self.cap = capacity
		self.hash = hashfn 
		self.vals = [] #type: List[node[T]]
		self.size = 0
		return


def contains(s : set211[T], a : T) -> bool:
	# s = set211[T], the set from which to check for membership
	# a = [T] a value for which to check membership

	x = s.vals[s.hash(a) % s.cap]
	while x != None:
		if x.val == a:
			return True
		x = x.succ
	return False


def add(s: set211, a: Any) -> None:
	# s = set211[T], the set to which to add a value
	# a = [T] a value for which to add to the set

	if contains(s, a) == True:
		return None

	# if s.size >= 3 * s.cap: # if it's equal, then once we add it will be more than it.
	# 	s.cap = s.cap * 2
	# 	s = set211(s.cap, hashfn)

	if s.size >= 3 * s.cap:
		s.cap = s.cap * 2

	x = s.vals[s.hash(a) % s.cap]
	new = node(a, None) #type: node

	while x != None: # traverse the list so you can add to the end
		x = x.succ
	x.succ = new

	s.size = s.size + 1

	return None


def add_all(s: set211, xs: List[Any]) -> None:
	for x in xs:
		add(s, x)
	return None


def size(s: set211) -> int:
	# size_of_s = 0
	# for i in range(s.capacity):
	# 	x = s.vals[i]
	# 	while x.succ != None:
	# 		x = x.succ
	# 		size_of_s = size_of_s + 1

	return s.size


def delete(s: set211, a: Any) -> None:
	# s = set211[T], the set to which to delete a value
	# a = [T] a value for which to delete from the set
	if contains(s, a) == False: # if it contains it, then delete it...
		return None

	x = s.vals[s.hash(a) % s.cap] # find the bin it's in
	while x.val != a: 
		x = x.succ
	x = x.succ # skip over the one you're deleting

	s.size = s.size - 1
	return None


def to_list(s: set211) -> List[Any]:
	# s = set211[T], the set to represent as a list
	listy = [] #type: List[Any]
	for i in range(s.cap):
		x = s.vals[i]
		while x.succ != None:
			x = x.succ
			listy.append(x.val)

	return listy


















