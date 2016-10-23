"""
COMP 211 HW 4:  Tests

"""

from hypothesis import assume,given,example,note
import hypothesis.strategies as st

from hw4 import *

def _from_string(s):
    """
    Construct a buffer from s by starting with an empty buffer and
    repeatedly inserting the characters of s.
    """
    b = buffer()
    for c in s:
        insert(b, c)
    return b

###
### Insertion only.
###

@example('')
@example('A')
@example('AB')
@given(st.text())
def test_insert(s):
    b = _from_string(s)
    assert get_contents(b) == [s, '']


@example('',0)
@example('A', 0)
@example('A', 1)
@example('AB', 1)
@given(st.text(), st.integers())
def test_set_pos(s,i):
    assume(0 <= i and i <= len(s))
    b = _from_string(s)
    set_pos(b, i)
    assert get_contents(b) == [s[:i], s[i:]]

@example('', 0)
@example('A', 0)
@example('A', 1)
@example('AB', 1)
@given(st.text(), st.integers())
def test_delete_left(s, i):
    assume(0 <= i and i <= len(s))
    b = _from_string(s)
    set_pos(b,i)
    delete_left(b)
    assert get_contents(b) == [s[:i], s[i+1:]]









