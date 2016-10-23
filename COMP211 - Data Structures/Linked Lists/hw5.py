"""
Fabien Bessez
Homework 5: More Linked Lists
Q1: is_linear
Q2: is_palindrome
"""

class node:
    """
    A singly-linked list node.
    """

    def __init__(self, c, succ):
        self.c = c          # type: str
        self.succ = succ    # type: node
        #self.head = node(None, None)
        return None

def to_string(head):
    """
    Convert a character list to the corresponding string.

    @tparam{head, node} a dummy node that anchors a list.

    @treturn{str} the string consisting of the characters in the list 
    anchored by \p head.
    """
    ret = ""
    n = head.succ
    while n != None:
        ret += n.c
        n = n.succ

    return ret

def from_string(s):
    """
    Construct a character list from a string.

    @tparam{s, str} the string to convert into a character list.

    @treturn{node} a dummy node that anchors a list that represents \p s.
    """
    n = None # type: node
    for c in reversed(s):
        n = node(c, n)

    return node(None, n)



def is_linear(hd):
    """
    Identifies whether the given singly linked list is linear or not.

    @tparam{hd, node} a dummy node that anchors a list.

    @treturn {bool} True if it is linear, False otherwise.
    """

    slow = hd # slower pointer
    fast = hd # faster pointer
    while fast != None and fast.succ != None: # since it jumps twice
        slow = slow.succ # jump to next
        fast = fast.succ.succ # jump to next next
        if slow == fast: # if they meet, it's not linear
            return False

    return True         # they must be linear since they did not meet



# if given F A B 
# it returns F A B _
def add_tail(hd):
    """
    Adds a dummy tail to the given linked list.

    @tparam{hd, node} a dummy node that anchors a list.

    @treturn{hd, node} the given linked list with an additional dummy tail.
    """
    # traverses the whole list and inserts a tail at end
    # linear in length of the list
    n = hd.succ
    while n.succ != None:
        n = n.succ 
    tail = node(None, None)
    n.succ = tail
    return None

# if given _ f a b
# reverse will return b a f _
# i want it to take a linked list with two dummies
# then reverse it...
def reverse(hd):
    
    """
    Reverses the directionality of the given linked list. 
    A -> B -> C becomes C -> B -> A

    @tparam{hd, node} a dummy node that anchors a list.

    @treturn{hd, node} the reversal of the given linked list.
    """

    # reverses the given list
    # including the dummies
    # linear in length of the list
    new = None
    while hd != None:
        new = node(hd.c, new) 
        # this keeps adding the next node in original sequence
        # to the beginning of the new sequence
        # by the end, it is in the opposite order
        # or it could be seen that the direction has changed
        hd = hd.succ
    return new



# the comments below follow two cases...
# top case is not a palindrome
# bottom case is a palindrome
# linear, linear, linear in the length of the list
# storage: twice the size of the list
def is_palindrome(hd):
    """
    Determines whether the given linked list is a palindrome.

    @tparam{hd, node} a dummy node that anchors a list.

    @treturn{bool} True if hd is a palindrome, False otherwise.
    """
    if hd.succ == None:
        return True
    # given _ a b c
    # given _ a b b a
    add_tail(hd)
    # now  _ a b c _
    # now _ a b b a _
    dh = reverse(hd)
    # dh = _ c b a _ 
    # dh = _ a b b a _
    #hd = hd.succ
    ###### hd = a b c _ 
    ###### hd = a b b a _ 
    #dh = dh.succ
    ###### dh = c b a _ 
    ###### dh = a b b a _ 
    while dh.succ.c != None and hd.succ.c != None:
        # while c is not none and a is not none
        # while a is not none and a is not none
        if dh.c != hd.c:
            # if c is not a
            # if a is not a 
            return False
        # if a IS a, then check for the next val
        dh = dh.succ
        hd = hd.succ

    return True




# Original Method:

# First calculate the length of the node
# if the node length is even, then do the following:

# What I really want to do though is calculate the midpoint
# Then I want to reverse all nodes from the head to that midpoint
# and then say that while the succ of the second section != None
# if the succ of the first section != the succ of the second section, then False

# if the node length is odd, then do the following:
# What I really want to do though is calculate the midpoint
# Then I want to reverse all nodes from the head to that midpoint
# instead of starting at the midpoints successor, i want to start at its succ succ
# because its original succ will be the odd value in the middle
# and then say that while the succ of the second sections successor != None
# if the succ of the first section != the succ of the second section, then False

# else return True


# def is_palindrome(hd):
#     n = hd.succ
#     node_length = 0
#     while n != None:
#         n = n.succ
#         node_length = node_length + 1

#     new = node(None, None)
#     i = 0
#     n = hd.succ
#     while i != node_length:
#         new.succ = n
#         n = n.succ
#         i = i + 1
#         # now, new should be the first half of the linked list
#     reverse(new)

#     if node_length % 2 == 0:
#         while n != None and n.succ != None: # while the second half hasn't reached the end
#             if n.succ.c != new.succ.c:
#                 return False
#             n = n.succ
#             new = new.succ

#     else:
#         while n!= None and n.succ != None and n.succ.succ != None:
#             if n.succ.succ.c != new.succ.c:
#                 return False
#             n = n.succ
#             new = new.succ

#     return True










