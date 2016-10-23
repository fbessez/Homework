'''
Homework 6: Hash Tables 
Tests
Fabien Bessez

Worked with: Shota Nakamura, Chad Malinowski. 
'''

from hypothesis import assume,given,example,note
import hypothesis.strategies as st

from hw6 import *


@example(['a'], 'a') 
@example(['a', 'b'], 'c') 
@example(['a', 'a'], 'a')
@given(st.lists(elements = st.characters(), max_size= 30), st.characters())
def test_contains(s, a):
	cs = set211(60, hash)
	exp = []
	result = True
	for c in s: 
		add(cs, c)
		if c not in exp:
			exp.append(c)
	if a not in exp:
		result = False
	assert result == contains(cs, a)
	assert size(cs) == len(exp)

@example(['a'], 'a') 
@example(['a', 'b'], 'c') 
@example(['a', 'a'], 'a')
@given(st.lists(elements = st.characters(), max_size = 30), st.characters())
def test_add(s, a):
	cs = set211(60, hash)
	exp = []

	for c in s:
		add(cs, a)
		if c not in exp:
			exp.append(c)

	if a not in exp:
		exp.append(a)

	assert contains(cs, a) == True
	assert size(cs) == len(exp)

@example(['a'], ['a']) 
@example(['a', 'b'], ['c']) 
@example(['a', 'a'], ['a'])
@given(st.lists(elements = st.characters(), max_size = 30), st.lists(elements = st.characters(), max_size = 30))
def test_add_all(s, xs):
	cs = set211(60, hash)
	exp = []
	cap_start = 60
	for c in xs:
		add(cs, c)
		if c not in exp:
			exp.append(c)

	for x in exp:
		assert contains(cs, x) == True
		
	cap_counter = 0
	for each_bin in s:
		cap_counter += 1
		assert cap_counter == cap_start *2

	assert size(s) == len(xs)


@example(['a']) 
@example(['a', 'b']) 
@example(['a', 'a']) 
@given(st.lists(elements=st.characters(), max_size=30)) 
def test_add_chars_no_doubling(cs):
	s = set211(60, hash)	# Create an empty set211 

	exp = [] # This will be the list of characters 
# in cs w/out duplicates 

	for c in cs: 
		add(s, c) 
		if c not in exp: 
			list.append(exp, c) 

# Check that s "looks right" in terms of having the right 
# size and also containing the elements of cs. 
	assert size(s) == len(exp) 

	for c in cs: 
		assert contains(s, c) 

@example(['a'], 'a') 
@example(['a', 'b'], 'c') 
@example(['a', 'a'], 'a')
@given(st.lists(elements = st.characters(), max_size = 30), st.characters())
def test_delete(s, a):
	cs = set211(60, hash)
	exp = []

	for c in s: 
		add(cs, c) 
		if c not in exp: 
			exp.append(c) 
	delete(cs, a)
	assert contains(cs, a) == False
	assert size(cs) == len(exp) - 1

@example(['a']) 
@example(['a', 'b']) 
@example(['a', 'a'])
@given(st.lists(elements = st.characters(), max_size = 30))
def test_size(s):
	cs = set211(60, hash)
	exp = []

	for c in s: 
		add(cs, c) 
		if c not in exp: 
			exp.append(c) 

	assert len(exp) == size(cs)

@example(['a']) 
@example(['a', 'b']) 
@example(['a', 'a'])
@given(st.lists(elements = st.characters(), max_size = 30))
def test_to_list(s):
	cs = set211(60, hash)
	exp = []

	for c in s: 
		add(cs, c) 
		if c not in exp: 
			exp.append(c) 

	assert to_list(cs) == exp

























