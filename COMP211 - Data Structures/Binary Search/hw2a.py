'''Fabien Bessez
HW 2a: Searching and Sorting
'''

import typing
from typing import List


def binary_search_min(xs: List[int], x: int) -> int:
	floor = 0           # first of range of indeces
	ceiling = len(xs) - 1 # gets highest index 
	# |           | --> 
	# |         |   --> 
	#     |     |   --> 
	#     |   |     --> 
	#       | |     -->
	#        ||     Boom
	# i = (ceiling + floor) // 2
	while True:
		if ceiling < floor: 
			return -1  # then we checked all indexes and it's not there
		i = (floor + ceiling) // 2
		if xs[i] < x: # 
			floor = i + 1 # cut again by increasing min
		elif xs[i] > x:
			ceiling = i - 1 # cut  again by decreasing max
		elif floor != ceiling: # you find a match but could be duplicates
			ceiling = i # do again with current index value
		else:
			return i    # you have a match -- 1st occurence

## The following function acts very similarly to the above function
## but it finds the maximum index that the particular x input occurs at
## This will make the binary_search_int function run much more smoothly
def binary_search_max(xs: List[int],x: int) -> int:
	floor = 0 
	ceiling = len(xs) - 1
	last_index = -1
	while floor <= ceiling:
		i = (floor + ceiling) //2
		if xs[i] == x:
			last_index = i
			floor = i + 1
		elif xs[i] < x:
			floor = i + 1
		else:
			ceiling = i - 1
	return last_index

def binary_search_int(xs: List[int], x: int) -> List[int]:
	i = binary_search_min(xs, x) 
	j = binary_search_max(xs, x)
	answer_array = [i, j]
	return answer_array


def sel_sort(xs: List[int]) -> None:
    for index in range(0, len(xs)): # for all the indexes available in array
    	minimum = index   # set the minimum index to be the index you're at
    	# NOW, check through indexes that are one greaater than minimum
    	for check_index in range(index + 1, len(xs)): 
    		# if the check index is smaller than the current minimum
    		if xs[check_index] < xs[minimum]:
    			# if xs[minimum] > than the check index one more than it 
    			# ie. if xs[0] > than xs[1] then there is a new minimum in town
    			minimum = check_index
    	# now do the swapping once you get the final minimum for that index
    	temp = xs[minimum] # stores that value into temp
    	xs[minimum] = xs[index] # swaps one way
    	xs[index] = temp # completes the swap
    return None



