'''Fabien Bessez
HW 2B: Searching and Sorting
'''

import typing 
from typing import List

def merge(xs: List[int], k: int, size: int) -> None:
	xs_beg = xs[0:k] # first section of xs
	xs_first = xs[k: k + size] #first section of interest
	xs_second = xs[k + size: k + 2 * size]  #second section of interest
	xs_end = xs[k + 2 * size:] # remaining indeces of xs
	merged = [] # an empty list to merge onto
	while len(xs_first) != 0 or len(xs_second) != 0: 
		if len(xs_first) == 0: 
			merged = merged + xs_second
			xs_second = []
		elif len(xs_second) == 0:
			merged = merged + xs_first
			xs_first = []
		elif xs_first[0] <= xs_second[0]: 
# if the first value of the first section is <= first value of second
			merged.append(xs_first[0])
			xs_first.pop(0) # pop the value off the list so that
			                # there will be a new first value in the list
		else: # xs_first[0] > xs_second[0]:
			merged.append(xs_second[0])
			xs_second.pop(0)
	xs = xs_beg + merged + xs_end
	return None



def merge_sort(xs: List[int]) -> None:
	k = 0 #first k is 0 
	size = 1 #first size is 1
	i = 0 #first i is 0
	# k should be incremented to k = k + 2 * size
	# size should remain the same for len(xs) / 2 times
	# once size = len(xs) // 2: run merge one final time
	if len(xs) == 1: 
		return None
	else:
		while size < len(xs) //2: # unless its the final merge:
			while k < len(xs): 
				size = 2**i       # size goes up by powers of 2
				xs = merge(xs, k, size) # redefine xs
				k = k + (2 * size) # 
			i = i + 1 # 
			k = 0 
		if size == len(xs) //2: 
			xs = merge(xs, k, size) 
			return None


def radix_sort(xs:List[List[int]]) -> None:
	bin0 = []
	bin1 = []
	i = 1
	j = 2
	n = len(xs[0])
	# while i < len(xs[0]):
	for x in xs: # for each list in xs
		if x[0] == 0: # seperate the list by its most sig bit
			bin0.append(x)
		else:
			bin1.append(x)
	#  sorts bin0 by increasing the length of sig bits that its sorted by
	while i <= len(xs[0]):
		for y in bin0:
			# checks on a slicing of the bin0
			if y[:i] != [0] * i:
				bin0.remove(y)
				bin0.append(y)
		i = i + 1
	# sorts bin1 by increasing length of sig bits that match [1,1,1...]
	while j <= len(xs[0]) + 1:
		# this is crucial 
		for y in bin1:
			# checks on a slicing of the bin1
			if y[1:j] == [1] * (j - 1):
				bin1.remove(y)
				bin1.append(y)
		j = j + 1
	# bin0 = perfectly sorted starting with 0
	# bin1 = perfectly sorted starting with 1
	xs = bin0 + bin1 # merge these
	return None


# print(merge_sort([]))
# print(merge_sort([3,2,1]))
# print(merge_sort([2,1]))
# print(merge_sort([6]))
# edge = [[1,1], [1,0]]
# cookies = [[1,0,1], [1,0,0], [0,0,1], [0,1,0], [0,0,0], [1,1,0], [1,0,1]]
# candy = [[1,1,0,1], [1,0,0,1], [1,1,1,1], [0,0,0,1]]
# print(radix_sort(edge))
# print(radix_sort(candy))
# print(radix_sort(cookies))











