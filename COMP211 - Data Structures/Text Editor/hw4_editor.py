"""
@package hw4_editor

COMP 211 Homework 4:  An editor that uses the buffer defined in hw4.

To run this program, execute the command

    python3 hw4_editor

This will cause an editor window to appear.  If you have not implemented
the hw4 module with at least the buffer type and the 
\link hw4::get_contents() hw4.get_contents\endlink
function, this running this program will immediately crash, probably
leaving the window in an unusable state.  
In all cases, using the usual close-window button
will close the window and terminate the program.
"""

import sys
import traceback

import tkinter
import tkinter.constants as tkc

import hw4

class _editor(tkinter.Text):
    """
    @internal
    """

    def __init__(self, master, **kwargs):
        tkinter.Text.__init__(self, master, kwargs)

        self.bind('<Key>', lambda e: "break")

class _editor_frame(tkinter.Frame):
    """
    @internal

    A frame that displays a text area.  We disable the default keybindings
    so that all text entry and movement is controlled by functions in hw4.
    """

    def __init__(self, master):
        tkinter.Frame.__init__(self, master)

        self.editor = tkinter.Text(self, height=24, width=80)
        # self.editor = _editor(self, height=24, width=80)
        self.editor.pack(side=tkc.TOP)

        self.editor.mark_gravity(tkc.INSERT, tkc.LEFT)
        self.editor.bind('<Key>', self.key_handler)
        self.editor.bind('<Button>', lambda ev: "break")
        self.editor.bind('<Motion>', lambda ev: "break")
        self.editor.bind('<ButtonRelease>', self.mouse_handler)

        self.contents = hw4.buffer()

        self.pack()

        self.display()
        self.editor.focus()

        return

    # Warning:  Tk's way of computing the character near the click is a
    # bit dicey.  If the click is *on* the character, then you get what
    # you expect (index = character number, starting at char 0).  If the
    # click is between characters n and n+1, you get n.  That's not really
    # what we expect, but as long as we know the behavior, we know how to
    # position the cursor.
    def mouse_handler(self, ev):
        # print(self.editor.index("@%d,%d" % (ev.x, ev.y)))
        linecol = self.editor.index("@%d,%d" % (ev.x, ev.y))
        index = int(linecol.split('.')[1])
        hw4.set_pos(self.contents, index)
        self.display()

        return "break"

    def key_handler(self, ev):
        # Watch out!  If an exception is raised in the following block, then
        # it seems that it is caught and reported by the event loop, but then,
        # because "break" is not returned, event handling continues on with
        # the class event handler.  There seems to be nothing to do here
        # but catch it ourselves, print it out, and exit.  Otherwise the
        # default handler will kick in, and that could make it look like the
        # desired behavior was implemented in hw4, when in fact it is 
        # implemented in tkinter.
        try:
            if ev.keysym == 'Left':
                hw4.move_left(self.contents)
            elif ev.keysym == 'Right':
                hw4.move_right(self.contents)
            elif ev.keysym == 'BackSpace':
                hw4.delete_left(self.contents)
            elif ev.keysym == 'Delete':
                hw4.delete_right(self.contents)
            else:
                # print("key_handler: char = %s; keysym = %s" % (ev.char, ev.keysym))
                hw4.insert(self.contents, ev.char)
        except Exception:
            print("hw4 function raised exception; exiting!")
            traceback.print_exc()
            sys.exit(1)

        self.display()

        # By returning "break," we prevent the event from being propagated to
        # the class event handler, and that way we avoid triggering the default
        # binding.
        return "break"

    def display(self):
        self.editor.delete(1.0, tkc.END)
        
        [before, after] = hw4.get_contents(self.contents)
        self.editor.insert(tkc.END, before+after)
        self.editor.mark_set(tkc.INSERT, "1.%d" % (len(before)))


def _main():
    """
    @internal

    Display the editor.
    """

    root = tkinter.Tk()
    editor = _editor_frame(root)
    root.mainloop()

if __name__ == '__main__':
    _main()
