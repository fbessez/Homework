"""
@package hw7_maze

Module for use in COMP 211 Homework 7.

@author F. Bessez
"""


import getopt
import random
import sys
import time
import tkinter

from typing import Tuple

import hw7

# Module constants and functions for directions.
#: Represents the direction north in a maze.
NORTH = 0
#: Represents the direction east in a maze.
EAST = 1
#: Represents the direction south in a maze.
SOUTH = 2
#: Represents the direction west in a maze.
WEST = 3

def opposite(direction):
    """
    Get the opposite direction to the one specified.

    @type direction: direction
    @param direction: a direction.

    @rtype: direction
    @returns:
        The direction opposite from C{d}.  That is, C{SOUTH} if
        C{d == NORTH}; C{WEST} if C{d == EAST}; etc.
    """
    return (direction+2)%4

cell = Tuple[int, int]

class maze:
    """
    A maze is a rectangular grid of cells, where adjacent cells
    may be separated by walls.  A maze has a start and end cell,
    but there need not be a path from one to the other.  A cell is
    identified by a pair (row, col), where row is a row number and 
    col is a column number (both starting at 0).  If c = (row, col) 
    and c' = (row', col') are adjacent cells, then we say that c' 
    is to the north of c if row' = row + 1; c' is to the east of c
    if col' = col + 1; c' is to the south of c if row' = row - 1;
    and c' is to the west of c if col' = col - 1.  So visually,
    if we think of north as up, then a maze is displayed with cell
    (0, 0) at the lower-left corner.

    In the following documentation, a cell is a pair of integers 
    (row, col) representing a cell in the maze.  The phrase
    "a cell in this maze" refers to a cell C{c} such that
    C{is_cell(c) == True}.

    In the following documentation, a direction is one of the following
    constants:  L{hw3util.NORTH}, L{hw3util.EAST}, L{hw3util.SOUTH}, 
    L{hw3util.WEST}.  Clients
    must use these named constants for directions, not the actual values.

    C{maze} objects may only be created by using one of the factory methods
    L{hw3util.get_maze_instance} or L{hw3util.get_prebuilt_maze_instance}.
    """

    _allow_creation = False

    # A maze is really an undirected graph.  Each vertex (cell) is labeled by
    # a pair (row, col), and we essentially use a vertex-list representation
    # with self.cells being the list of vertices, except that self.cells[r, c]
    # is the list of directions for which there is an edge from cell
    # (r, c) (row, column).  So self.cells[r, c] can also be viewed as the
    # list of *non*-walls around cell (r, c).

    # Because I think it is cute (but not good data typing), self.cells[r, c]
    # is an integer, where we only care about the four least significant
    # bits.  A bit value of 0 indicates there is no edge (there is a wall),
    # a bit value of 1 indicates there is an edge (there is no wall).
    # Bit 0 is north, bit 1 is east, bit 2 is south, bit 3 is west.
    def __init__(self, nrows, ncols, st_row, st_col, end_row, end_col):
        """
        Create a new maze.  Client may not instantiate mazes directly;
        instead, they must use either L{get_maze_instance} or
        L{get_prebuilt_maze_instance}.

        @raises NotImplementedError:
            if invoked directly by the client.
        """

        if not maze._allow_creation: raise NotImplementedError

        self.nrows = nrows
        self.ncols = ncols

        # Initialize the maze so that all walls are present.
        self.cells = {}
        for i in range(0, nrows):
            for j in range(0, ncols):
                self.cells[i, j] = 0x0

        self.st_row, self.st_col = (st_row, st_col)
        self.end_row, self.end_col = (end_row, end_col)

    @staticmethod
    def _build_prim(self):
        """
        Build a spanning tree in the maze using Prim's algorithm.  All
        walls must be present in the maze when this method is invoked.
        After invoking this method, there will be exactly one path between 
        any two cells in the maze.

        @rtype:  NoneType
        @returns: None.
        """

        # Convenience shadowing
        nrows = self.nrows
        ncols = self.ncols
        # NORTH, EAST, SOUTH, WEST = \
                # (hw3.NORTH, maze.EAST, maze.SOUTH, maze.WEST)

        # MST edges and cells.  Elements of mst_edges are of the
        # form (c1, c2), where c1 and c2 are cells of the form (r, c).
        # If (c1, c2) is in mst_edges, then (c2, c1) is not in mst_edges.
        # mst_cells consists of cells  of the form (r, c).  (c1, c2) is
        # in mst_edges iff c1 and c2 are in mst_cells.
        mst_edges = set()
        mst_cells = set()

        # The frontier.  This is the set of edges between cells in the
        # MST and cells not in the MST.  (c1, c2) is in frontier iff
        # c1 is in mst_cells and c2 is not in mst_cells.
        frontier = set()

        # Choose two adjacent cells at random to put into the MST,
        # and then populate the frontier accordingly.  For simplicity,
        # choose a cell in the interior of the maze, then randomly
        # choose a direction for the other cell.
        start1 = (random.randrange(1, nrows-1), random.randrange(1, ncols-1))
        direction = random.randrange(0, 4)
        start2 = self.get_neighbor(start1, direction)
        self.remove_wall(start1, direction)
        mst_edges.add((start1, start2))
        mst_cells.add(start1)
        mst_cells.add(start2)


        to_frontier = [(start1, self.get_neighbor(start1, d))
                for d in (0, 1, 2, 3) if d != direction and 
                                            self.is_cell(start1, d)]
        for c in to_frontier: frontier.add(c)

        to_frontier = [(start2, self.get_neighbor(start2, d))
                for d in (0, 1, 2, 3) if d != opposite(direction) and
                                            self.is_cell(start2, d)]
        for c in to_frontier: frontier.add(c)

        # As long as we don't have all the cells in the MST, choose
        # an edge in the frontier at random.  Put the edge in the
        # MST and compute the new edges to add to the frontier.
        while len(mst_cells) < nrows*ncols:
            new_edge = frontier.pop()
            old_cell = new_edge[0]
            new_cell = new_edge[1]
            self.remove_wall(old_cell, self.get_direction(old_cell, new_cell))

            mst_edges.add(new_edge)
            mst_cells.add(new_cell)

            for d in (NORTH, EAST, SOUTH, WEST):
                if self.is_cell(new_cell, d):
                    adj_cell = self.get_neighbor(new_cell, d)
                    if adj_cell not in mst_cells:
                        frontier.add((new_cell, adj_cell))
                    else:
                        if adj_cell != old_cell:
                            frontier.remove((adj_cell, new_cell))


    def is_cell(self, c, d):
        """
        Check whether there is a cell in a given direction from a
        given cell.

        @type c: cell
        @param c: a cell in this maze.
        @type d: direction
        @param d: a direction.

        @rtype: boolean
        @returns:
            False if C{get_neighbor(c, d)} would return a "cell"
            that lies outside the maze.
        """
        if (c[0] == self.nrows-1 and d == NORTH) or \
                (c[1] == self.ncols-1 and d == EAST) or \
                (c[0] == 0 and d == SOUTH) or \
                (c[1] == 0 and d == WEST): return False
        else: return True

    def get_neighbor(self, c, d):
        """
        Get the cell adjacent to a given cell in a given direction.

        @type c: cell
        @param c:  a cell in this maze.
        @type d: direction
        @param d: a direction.

        @rtype: cell
        @returns:
            The cell c' that is adjacent to C{c} in the direction C{d}.
        """
        
        if d == NORTH:  return (c[0]+1, c[1])
        elif d == EAST:  return (c[0], c[1]+1)
        elif d == SOUTH:  return (c[0]-1, c[1])
        elif d == WEST:  return (c[0], c[1]-1)

        raise ValueError

    def get_direction(self, c1, c2):
        """
        Get the direction to go from one cell to an adjacent cell.

        @type c1: cell
        @type c2: cell
        @param c1, c2:  adjacent cells, both of which are in this maze.

        @rtype: direction
        @returns:
            C{NORTH} if C{c2} is to the north of C{c1}; C{EAST} if
            C{c2} is to the east of C{c1}; etc.
        """
        
        if c2[0] == c1[0]+1:  return NORTH
        elif c2[1] == c1[1]+1:  return EAST
        elif c2[0] == c1[0]-1:  return SOUTH
        elif c2[1] == c1[1]-1:  return WEST

        raise ValueError

    def remove_wall(self, c, d):
        """
        Remove a wall from a cell.
        The wall in direction C{d} from C{c} is removed
        from the maze.  If the wall is not present in the maze,
        this method has no effect.

        @type c: cell
        @param c: a cell in this maze.
        @type d: direction
        @param d: a direction

        @rtype:  NoneType
        @return:  None.
        """

        # Update the adjacency list for cell.
        self._remove_wall_from_adj_list(c, d)

        # Update the adjacency list for the cell on the other side
        # of the wall.
        self._remove_wall_from_adj_list(self.get_neighbor(c, d),
                opposite(d))

    def _remove_wall_from_adj_list(self, c, d):
        """
        Update the adjacency list for cell c to remove the wall
        in direction d.
        The adjacency list for cell c is updated so that there
        is no wall in direction d.  If the wall is not present,
        this method has no effect.

        @type c: cell
        @param c:  a cell in this maze.
        @type d: direction
        @param d:  a direction.

        """
        self.cells[c] |= (1 << d)

    def has_wall(self, c, d):
        """
        Determine if there is a given wall in the maze.

        @type c: cell
        @param c: a cell in this maze.
        @type d: direction
        @param d: a direction.

        @rtype: boolean
        @returns:
            C{True} if there is a wall in direction d from cell C{c},
            C{False} otherwise.
        """

        return not self.has_passage(c, d)

    def has_passage(self, c, d):
        """
        Determine if there is a passage from a given cell
        in a given direction.

        @type c: cell
        @param c: a cell in this maze.
        @type d: direction
        @param d: a direction.

        @rtype: boolean
        @returns:
            C{True} if it is possible to move in direction C{d} when
            in cell C{c} (i.e., there is no wall in that direction),
            C{False} otherwise.
        """

        return (self.cells[c] & (1 << d)) > 0

    def get_nrows(self):
        """
        Get the number of rows in this maze.

        @rtype: int
        @returns:
            The number of rows in this maze.
        """
        return self.nrows

    def get_ncols(self):
        """
        Get the number of columns in this maze.

        @rtype: int
        @returns:
            The number of columns in this maze.
        """
        return self.ncols

    def get_start_cell(self):
        """
        Get the start cell of this maze.

        @rtype: cell
        @returns:
            The start cell of this maze.
        """
        return (self.st_row, self.st_col)

    def get_end_cell(self):
        """
        Get the end cell of this maze.

        @rtype: cell
        @returns:
            The end cell of this maze.
        """
        return (self.end_row, self.end_col)

    def __str__(self):
        """
        Get an ASCII (text) representation of this maze.  The start
        cell is represented by a '*' and the end cell is represented
        by a '+'.  The ASCII representation consists of lines
        separated by the newline character.  The last line corresponds
        to row 0, and the first characters of each line correspond
        to column 0.  Thus, printing this representation to the
        console gives a picture of the maze with cell (0, 0) at the
        bottom left, north going up the screen, and east going to
        the right.

        @rtype: string
        @returns: An ASCII representation of this maze.
        """
        asc =  self.ncols*"___" + "\n"
        for r in range(self.nrows-1, -1, -1):
            for s in (0, 1, 2):
                for c in range(0, self.ncols):
                    if not (self.cells[r, c] & (1 << 3)): asc = asc + "|"
                    else: asc = asc + " "
                    if s == 0: asc = asc + "  "
                    elif s == 1:
                        if (r, c) == self.get_start_cell():
                            asc = asc + "* "
                        elif (r, c) == self.get_end_cell():
                            asc = asc + "+ "
                        else: asc = asc + "  "
                    else:
                        if not (self.cells[r, c] & (1 << 2)): asc = asc + "__"
                        else: asc = asc + "  "
                asc = asc + "|\n"

        return asc


class mazeview:
    """
    A mazeview object provides a graphical view of a maze.  When
    created, it draws the maze with each cell colored white and the
    walls colored black.  It also provides methods for changing the
    color of any cell.  Cell (0, 0) is at the lower left, with
    north being upward and east being rightward on the screen.
    """

    def __init__(self):
        """
        Create a mazeview object.

        @type maze: maze
        @param maze: a maze object.
        """

        # Width and height of the window, in pixels.
        self.width = 800
        self.height = 600
        width = self.width
        height = self.height

        # Create the root window.
        self.root = tkinter.Tk()
        root = self.root

        #
        # Buttons etc.
        #
        controls = tkinter.Frame(root)
        controls.pack(side=tkinter.TOP, fill='x')

        build = tkinter.Button(controls, text='Build new maze')
        build.pack(side=tkinter.LEFT)

        reset = tkinter.Button(controls, text='Reset maze')
        reset.pack(side=tkinter.LEFT)

        solve = tkinter.Button(controls, text='Solve maze')
        solve.pack(side=tkinter.LEFT)

        # maze_type:  0 = prim, 1 = random.
        maze_type = tkinter.IntVar()
        prim = tkinter.Radiobutton(controls, text='Prim', variable=maze_type, 
                value=0)
        prim.pack(side=tkinter.LEFT)
        rand = tkinter.Radiobutton(controls, text='Random', variable=maze_type,
                value=1)
        rand.pack(side=tkinter.LEFT)
        prim.select()

        def lbl_entry(lbl, v):
            l = tkinter.Label(controls, text="{}: ".format(lbl))
            l.pack(side=tkinter.LEFT)
            e = tkinter.Entry(controls, textvariable=v, width=5)
            e.pack(side=tkinter.LEFT)

        # Maze size
        nrows_var = tkinter.StringVar()
        lbl_entry('Rows', nrows_var)
        ncols_var = tkinter.StringVar()
        lbl_entry('Columns', ncols_var)
        nrows_var.set('30')
        ncols_var.set('50')

        # Sparseness
        sparse = tkinter.StringVar()
        lbl_entry('Sparseness', sparse)
        sparse.set('.05')

        # Delay
        delay = tkinter.StringVar()
        lbl_entry('Draw delay (s)', delay)
        delay.set('0.0')

        #
        # Canvas in which to display the maze.
        #
        self.cvs = tkinter.Canvas(width=width, height=height)
        cvs = self.cvs
        cvs.pack(side=tkinter.TOP, expand=True, fill='both')

        # Build callback
        def build_act():
            nrows = int(nrows_var.get())
            ncols = int(ncols_var.get())
            sparseness = float(sparse.get())
            self.maze = self.build_fn(nrows, ncols, sparseness)
            self.display_maze()
        build.configure(command=build_act)

        # Reset callback
        def reset_act():
            self.display_maze()
        reset.configure(command=reset_act)


        # Solve callback
        def solve_act():
            self.solve_maze(float(delay.get()))

        solve.configure(command=solve_act)

        # Prim callback
        def prim_act():
            self.build_fn = get_prebuilt_maze_instance
            """
            nrows = int(nrows_var.get())
            ncols = int(ncols_var.get())
            sparseness = float(sparse.get())
            self.maze = get_prebuilt_maze_instance(nrows, ncols, sparseness)
            self.display_maze()
            """
        prim.configure(command=prim_act)

        # Random callback
        def random_act():
            self.build_fn = get_random_maze_instance
            """
            nrows = int(nrows_var.get())
            ncols = int(ncols_var.get())
            sparseness = float(sparse.get())
            self.maze = get_random_maze_instance(nrows, ncols, sparseness)
            self.display_maze()
            """
        rand.configure(command=random_act)

        prim.invoke()

        root.mainloop()

        return

    def display_maze(self):

        # Convenience shadowing
        nrows = self.maze.get_nrows()
        ncols = self.maze.get_ncols()
        width = self.width
        height = self.height
        cvs = self.cvs

        # The Tk canvas.  Each cell will be represented as a rectangle
        # in the canvas.

        # Width and height of each cell.
        self.cell_width = width/ncols
        self.cell_height = height/nrows
        cell_width = self.cell_width
        cell_height = self.cell_height

        # Map from cells (row, column) to rectangle tags in the canvas.
        self.cvs_cells = {}
        cvs_cells = self.cvs_cells

        # Map from cell (rectangle tags) to the color of that cell.
        self.cvs_cell_color = {}

        # Create the rectangles and lines for the cells and walls.
        for r in range(0, self.maze.get_nrows()):
            for c in range(0, self.maze.get_ncols()):
                cvs_cells[r, c] = cvs.create_rectangle(c*cell_width,
                        height - r*cell_height, (c+1)*cell_width,
                        height - (r+1)*cell_height, outline='white')
                if self.maze.has_wall((r, c), NORTH):
                    cvs.create_line(c*cell_width, height-(r+1)*cell_height,
                            (c+1)*cell_width, height-(r+1)*cell_height,
                            width=4)
                if self.maze.has_wall((r, c), EAST):
                    cvs.create_line((c+1)*cell_width, height-r*cell_height,
                            (c+1)*cell_width, height-(r+1)*cell_height,
                            width=4)

        # Set the initial (white) color for all the cells and draw
        # the maze.
        self.clear_colors()
        self.draw()

        # Display the maze.

    def clear_colors(self):
        """
        Set all the cells to their initial (white) color.
        All cells except the start and end cell of the maze
        will be colored white.  The start cell will be colored green
        and the end cell red.

        @rtype:  NoneType
        @returns:  None.
        """
        for r in range(0, self.maze.get_nrows()):
            for c in range(0, self.maze.get_ncols()):
                self.set_color((r, c), 'white', draw=False)

        self.cvs.itemconfig(self.cvs_cells[self.maze.get_start_cell()],
                fill='green')
        self.cvs.itemconfig(self.cvs_cells[self.maze.get_end_cell()],
                fill='red')

        self.draw()

    def draw(self):
        """
        Draw the maze, taking into account any color changes since
        the last draw.

        @rtype:  NoneType
        @returns:  None.
        """

        # Use update instead of update_idletasks because it works better
        # on some Windows machines.
        self.root.update()

    def set_color(self, c, color, draw=True):
        """
        Set the color of a cell.  If the cell is either the start
        or end cell, this method has no effect.

        @type c: cell
        @param c:  a cell in the maze being viewed.
        @type color: string
        @param color:
            a color name.  This can be the name of
            any standard color such as 'red' or 'blue' or can
            be a red-green-blue specification in the form
            '#RRGGBB', where each RR, GG, and BB are hexidecimal
            numbers between 00 and FF.
        @type draw: boolean
        @param draw: whether to immediately redraw the maze
                after changing the color of C{c}.  Optional, defaults
                to C{True}.

        @rtype:  NoneType
        @returns:  None.
        """
        
        if c == self.maze.get_start_cell() or c == self.maze.get_end_cell():
            return
        self.cvs.itemconfig(self.cvs_cells[c], fill=color)

        if draw:  self.draw()

    def draw_path(self, path, color):
        """
        Draw a path in the maze.  This method
        draws a path through the sequence of cells specified by
        path.  The path is drawn as a line joining the centers
        of the cells in the order they appear in path; the line
        is given the color C{color}.

        @type path: sequence of cell
        @param path:
            a sequence of cells such that
            each cell in the sequence is a neighbor of the
            previous cell in the sequence.
        @type color: string
        @param color: a color name as for L{set_color}.

        @rtype:  NoneType
        @returns:  None.
        """

        half_width = self.cell_width/2
        half_height = self.cell_height/2

        # List of coordinates corresponding to the center of each
        # cell in the path, in a form acceptable to cvs.create_line.
        coord_list = []
        for cell in path:
            coord_list.append((self.cell_width*cell[1] + half_width,
                self.height - (self.cell_height*cell[0] + half_height)))
                
        self.cvs.create_line(coord_list, width=4, fill=color)
        self.draw()

    def solve_maze(self, delay):
        nrows = self.maze.get_nrows()
        ncols = self.maze.get_ncols()
        m = self.maze

        # Create the graph corresponding to the maze.
        nv = nrows*ncols
        g = hw7.graph(nv)

        def cell2vert(c : cell) -> int:
            row, col = c
            return row*ncols + col

        def vert2cell(v : int) -> cell:
            return (v//ncols, v%ncols)

        for r in range(nrows):
            for c in range(ncols):
                for d in [NORTH, EAST]:
                    if m.has_passage((r, c), d):
                        hw7.add_edge(g, cell2vert((r, c)), 
                            cell2vert(m.get_neighbor((r, c), d)))

        def enter(v : int) -> None:
            c = vert2cell(v)
            self.set_color(c, '#888888')
            time.sleep(delay)
            return None

        def exit(v : int) -> None:
            c = vert2cell(v)
            self.set_color(c, '#444444')
            time.sleep(delay)
            return None

        # Solve the maze, animating the solution as we go.
        path = hw7.bfs(g, cell2vert(m.get_start_cell()), cell2vert(m.get_end_cell()),
                enter, exit)

        # Rest, then draw the path from start cell to end cell.
        time.sleep(1)
        # print(path)
        if len(path) == 0:
            print("No path!")
        elif len(path) == 1:
            print("There is a path!")
        else:
            self.draw_path(map(vert2cell, path), '#FFFF00')


def get_maze_instance(nrows, ncols, st_row, st_col, end_row, end_col):
    """
    Create a maze of the given size with the given start and end cell.
    All walls will be present in the maze.

    @type nrows: int
    @type ncols: int
    @param nrows, ncols: the number of rows and columns in the maze.

    @type st_row: int
    @type st_col: int
    @param st_row, st_col: the row and column of the start cell of the
        maze; C{0 <= st_row < nrows} and C{0 <= st_col < ncols}.

    @type end_row: int
    @type end_col: int
    @param end_row, end_col:  the row and column of the end cell of the
        maze; C{0 <= end_row < nrows} and C{0 <= end_col < ncols}.

    @rtype: maze
    @returns:  a maze as described above.
    """
    maze._allow_creation = True
    m = maze(nrows, ncols, st_row, st_col, end_row, end_col)
    maze._allow_creation = False
    return m

def get_random_maze_instance(nrows, ncols, sparseness):
    """
    Create a maze of the given size with each wall removed with probability
    \p sparseness.  The start and end cells will be chosen at random.
    """

    st_row = random.randrange(0, nrows)
    st_col = random.randrange(0, ncols)
    end_row = random.randrange(0, nrows)
    end_col = random.randrange(0, ncols)

    maze._allow_creation = True
    m = maze(nrows, ncols, st_row, st_col, end_row, end_col)
    maze._allow_creation = False

    # Remove walls
    for i in range(0, nrows):
        for j in range(0, ncols):
            for d in (EAST, NORTH):
                if m.is_cell((i, j), d) and \
                        m.has_wall((i, j), d) and \
                        random.random() < sparseness:
                    m.remove_wall((i, j), d)

    return m


def get_prebuilt_maze_instance(nrows, ncols, sparseness):
    """
    Create a maze with C{nrows} rows and C{ncols} columns.  The maze
    will be created by (1) using Prim's algorithm to remove walls so
    that there is exactly one path between any two cells, then (2) removing
    each remaining wall with probability C{sparseness}.  So if
    C{sparseness == 0.0}, there will be exactly one path between any
    two cells of the maze, and if C{sparseness == 1.0}, there will
    be no walls in the maze at all.

    @type nrows: int
    @param nrows: the number of rows in this maze.
    @type ncols: int
    @param ncols: the number of columns in this maze.
    @type sparseness: float
    @param sparseness: how "sparse" walls are in the maze.
        C{0.0 <= sparseness <= 1.0}.

    @rtype: maze
    @returns: a maze as described above.
    """

    st_row = random.randrange(0, nrows)
    st_col = random.randrange(0, ncols)
    end_row = random.randrange(0, nrows)
    end_col = random.randrange(0, ncols)

    maze._allow_creation = True
    m = maze(nrows, ncols, st_row, st_col, end_row, end_col)
    maze._allow_creation = False

    maze._build_prim(m)

    # Remove walls
    for i in range(0, nrows):
        for j in range(0, ncols):
            for d in (EAST, NORTH):
                if m.is_cell((i, j), d) and \
                        m.has_wall((i, j), d) and \
                        random.random() < sparseness:
                    m.remove_wall((i, j), d)

    return m

if __name__ == '__main__':
    mazeview()


