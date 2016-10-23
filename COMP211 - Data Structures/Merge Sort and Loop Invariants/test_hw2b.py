from hw2b import *

'''
HW 2b Tests
'''

def test_merge1():
	xs = [0, 1, 2, 6]
	k = 0
	size = 1 
	answer = merge(xs,k,size)
	assert answer == [0,1,2,6]

def test_merge2():
	xs = []
	k = 0
	size = 0
	answer = merge(xs,k,size)
	assert answer == []

	# Should test_merge2 return error since size is not >0?

def test_merge3():
	xs = [1,2,2,9,10,10,3,10,11,69]
	k = 3
	size = 3
	answer = merge(xs,k,size)
	assert answer == [1,2,2,3,9,10,10,10,11,69]

def test_merge4():
	xs = [0,7,8,2,7,8,9,3,4,5,6]
	k = 1
	size = 2
	answer = merge(xs,k,size)
	assert answer == [0,2,7,7,8,8,9,3,4,5,6]

def test_merge_sort1():
	xs = [2]
	assert merge_sort(xs) == [2]

def test_merge_sort2():
	xs = [9,2]
	assert merge_sort(xs) == [2,9]

def test_merge_sort3():
	xs = [6,8,9,0]
	assert merge_sort(xs) == [0,6,8,9]

def test_merge_sort4():
	xs = [9,7,6,5,4,4,3,2,2,1,2,3,4,5,6,3]
	assert merge_sort(xs) == [1,2,2,2,3,3,3,4,4,4,5,5,6,6,7,9]

# This should fail
# def test_merge_sort5():
# 	xs = []
# 	assert merge_sort(xs) 

def test_radix_sort1():
	xs = [[1],[0]]
	assert radix_sort(xs) == [0,1]

def test_radix_sort2():
	xs = [[1111,1011],[1100,1011]]
	assert radix_sort(xs) == [1011,1011,1100,1111]

def test_radix_sort3():
	xs = [[111,111],[110,101]]
	assert radix_sort(xs) == [101,110,111,111]

def test_radix_sort4():
	xs = [[1111,1011,1100],[1110,1011,1111]]
	assert radix_sort(xs) == [1011,1011,1100,1110,1111,1111]

























