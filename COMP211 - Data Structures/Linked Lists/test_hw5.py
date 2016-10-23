"""
COMP 211 HW 5:  Tests
Fabien Bessez

"""

from hypothesis import assume,given,example,note
import hypothesis.strategies as st

from hw5 import *


@example('computer')
@example('hannah')
@example('a')
@example('')
@example('anna')
@given(st.text())
def test_palindrome(s):
   hd = from_string(s)
   answer = True
   for i in range(len(s) // 2): # for half of the chars
   # this works even for odd because the true middle will be the same
   # for both halves
      if s[i] != s[len(s) - 1 - i]:  # len(s)-1 is biggest index
         answer = False
   cookies = is_palindrome(hd) 
   assert answer == cookies
