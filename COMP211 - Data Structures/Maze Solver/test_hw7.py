"""
COMP 211 Homework 7 tests.
"""

from hypothesis import given, example
import hypothesis.strategies as st

import hw7

def assert_is_short_path(g, v0, v1, vs, n):
    """
    Return True if vs is a path from v0 to v1 in g of length n, False otherwise.
    """
    
    assert vs[0] == v0
    assert vs[-1] == v1
    assert len(vs) == n

    for i in range(len(vs)-1):
        assert hw7.has_edge(g, vs[i], vs[i+1])

def _noop(v):
    return None

@example(3, [(0, 1)], 0, 2)
@example(3, [(0, 1)], 2, 0)
@example(4, [(0, 1), (2, 3)], 0, 2)
@example(4, [(0, 1), (2, 3)], 2, 0)
@example(4, [(0, 1), (2, 3)], 0, 3)
@example(4, [(0, 1), (2, 3)], 3, 0)
@example(4, [(0, 1), (2, 3)], 1, 2)
@example(4, [(0, 1), (2, 3)], 2, 1)
@example(4, [(0, 1), (2, 3)], 1, 3)
@example(4, [(0, 1), (2, 3)], 3, 1)
@given(st.just(2), st.just([]), st.just(0), st.just(1))
def test_nopath(n, es, v0, v1):
    """
    Create a graph g of size n with edges in es, verify there is no path
    between v0 and v1.
    """

    g = hw7.graph(n)
    for e in es:
        hw7.add_edge(g, e[0], e[1])

    assert hw7.bfs(g, v0, v1, _noop, _noop) == []

@example(3, [(0, 1)], 0, 1, 2)
@example(3, [(0, 1), (1, 2)], 0, 2, 3)
@given(st.just(2), st.just([(0, 1)]), st.just(0), st.just(1), st.just(2))
def test_path(n, es, v0, v1, l):
    """
    Create a graph g of size n with edges in es, verify that there is path
    between v0 and v1, and if a sequence of vertices is returned, verify
    that it is a path from v0 to v1 of length l.
    """

    g = hw7.graph(n)
    for e in es:
        hw7.add_edge(g, e[0], e[1])

    vs = hw7.bfs(g, v0, v1, _noop, _noop)

    assert len(vs) > 0
    assert vs[-1] == v1

    if len(vs) > 1:
        assert_is_short_path(g, v0, v1, vs, l)

