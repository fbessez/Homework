'''
Homework 7: Graphs
Fabien Bessez
Breadth First Search
Undirected Graphs and Maze-Solving
'''

import typing
from typing import List, Callable, Any


class graph:

	def __init__(self, nv):
		self.nv = nv
		self.verts = []	#type: List[List[int]]
		for i in range(nv):
			self.verts.append([])
		return


def nvertices(g: graph) -> int:
	return g.nv 

def neighbors(g: graph,  v: int) -> List[int]:
	adjacents = [] #type: List[int]
	for i in g.verts[v]:
		adjacents.append(i)
	return adjacents

def has_edge(g: graph, v0: int, v1: int) -> bool:
	x = nvertices(g)
	if 0 <= v0 < x and 0 <= v1 < x:
		if v1 in neighbors(g, v0):
			return True
		else:
			return False


def add_edge(g: graph, v0: int, v1: int) -> None:
	x = nvertices(g)
	if 0 <= v0 < x and 0 <= v1 < x:
		if has_edge(g, v0, v1) == False:
			g.verts[v0].append(v1)
			g.verts[v1].append(v0)
		return None


def remove_edge(g: graph, v0: int, v1: int) -> None:
	x = nvertices(g)
	if 0 <= v0 < x and 0 <= v1 < x:
		if has_edge(g, v0, v1) == True:
			g.verts[v0].remove(v1)
			g.verts[v1].remove(v0)
		return None



def bfs(g: graph, v_st: int, v_end: int, previsit: Callable[[int], None], postvisit: Callable[[int], None]) -> List[int]:
	to_visit = [v_st] #type: List[int]
	visited = [False for i in range(nvertices(g))] #type: List[bool]
	path = [None for i in range(nvertices(g))] #type: List[Any]
	current = to_visit[0]

	'''
	This next chunk of text will describe the following chunk of code:
	First, I make sure that my current vertex is not v_end and that there
	are still vertices that I have to visit.
	Second, I change current index value to True in my visited list.
	Third, I add all edges of my current vertex to my to_visit if they are not
	already in there and they are also not visited already.
	Fourth, I also keep track of how I got to that particular edge vertex
	in my path list
	Fifth, I pop the first value from to_visit because I just visited it
	Sixth, I make sure that there are still vertices to visit and I 
	change my current value to the next in the to_visit list
	'''

	while current != v_end and len(to_visit) > 0:
		visited[current] = True
		for val in neighbors(g, current):
			if val not in to_visit and visited[val] == False:
				to_visit.append(val)
				path[val] = current
				previsit(val)
		j = to_visit.pop(0)
		postvisit(j)

		if len(to_visit) > 0:
			current = to_visit[0]

	'''
	This next chunk of text will describe the following chunk of code:
	First, I know that my while loop has stopped running...
	So that means that my current vertex is my v_end or that there was
	nothing left to_visit. 
	If my current is my v_end:
		I change that value in my visited to True
		I pop it from the to_visit
		And from the path list, I construct a path back to v_st from v_end
		I then reverse that list to give path in v_st to v_end order.

	If my current is not my v_end:
		Then I know it is not possible to get to my v_end from my v_st
		So, I return the empty list.
	'''

	if current == v_end:
		visited[current] = True

		to_visit.pop(0)
		postvisit(current)

		v = current
		result = [] #type: List[int]
		while v != None:
			result.append(v)
			v = path[v]

		return list(reversed(result))

	else:
		postvisit(current)
		return []










