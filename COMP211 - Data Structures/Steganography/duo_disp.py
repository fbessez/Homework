"""
@package duo_disp

A program for displaying two images side-by-side.

To run this program execute the following command:

    python duo_disp.py

You can then select an image to display on either side by pressing the
"Choose" button.  The image must be 8-bit greyscale.
Once an image is loaded, you can view a representation
of the least-significant bits of the image by selecting the "Low bits" button.
When you do so, each pixel of the displayed image
will be colored black if the lsb of the corresponding pixel of the loaded
image is 0 and white if it is 1.

"""

import tkinter.filedialog
import tkinter

from PIL import Image
from PIL import ImageTk

_IMG_WIDTH = 600
_IMG_HEIGHT = 450

def _do_resize(img, width, height):
    """
    Resize an image to the given width and height, maintaining
    the aspect ratio of the original image.

    @type img: Image.Image
    @param img: the image to resize

    @type width: int
    @param width: the (maximum) width of the resized image.

    @type height: int
    @param height: the (maximum) height of the resized image.

    @rtype: Image.Image
    @returns: a new C{Image.Image} obtained by resizing C{img}.  The aspect
        ratio of C{img} is preserved, and the returned image has width
        C{width} and/or height C{height}, depending on how the aspect
        ratios of the original image and the new size compare.
    """
    img_width, img_height = img.size
    img_aspect_ratio = float(img_width)/img_height
    aspect_ratio = float(width)/height

    """
    if img_aspect_ratio >= aspect_ratio:
        return img.resize((width, int(height*(aspect_ratio/img_aspect_ratio))))
    else:
        return img.resize((int(width*(aspect_ratio/img_aspect_ratio)), height))
    """
    return img.resize((width, height))

class _image_frame(tkinter.Frame):
    """
    @internal

    A widget that displays an image in a label, along with buttons for
    selecting the image and toggling whether the image or just the
    least-significant bits are displayed.  This widget can only display
    8-bit greyscale images (PIL type 'L').
    """

    def __init__(self, master):
        tkinter.Frame.__init__(self, master)
        self.configure(relief='raised', borderwidth=1)

        empty_image = Image.new("1", (_IMG_WIDTH, _IMG_HEIGHT))
        img_tk = ImageTk.BitmapImage(empty_image)

        buttons = tkinter.Frame(self)

        choose_button = tkinter.Button(buttons, text='Choose', 
                command=self._load_image)
        self.toggle_v = tkinter.IntVar()
        self.toggle_cb = tkinter.Checkbutton(buttons, text='Low bits', 
                variable=self.toggle_v, 
                command=self._toggle_low_bits)
        self.img_lbl = tkinter.Label(self, image=img_tk, bg="black")

        choose_button.pack(side='left')
        self.toggle_cb.pack(side='left')
        buttons.pack(side='top', anchor='w')
        self.img_lbl.pack(side='top')

        self.pack()

    def _toggle_low_bits(self):
        """
        Switch between the image and low-order bits for the image in the given
        label, as per the value of C{self.toggle_v}; 
        if C{self.toogle_v.get() == 0} display the
        image itself, otherwise display just the least-significant bits.
        """
        img = self.img_tk if self.toggle_v.get() == 0 else self.img_tk_low_bits
        self.img_lbl.configure(image=img)

    def _load_image(self):
        img_filename = \
                tkinter.filedialog.askopenfilename(title='Select an image file')
        if img_filename != '':
            # We'll save a reference to the resized image as part of the label
            # so that we have easy access to it if the user just wants to see
            # the low-order bits.
            img = _do_resize(Image.open(img_filename), _IMG_WIDTH, _IMG_HEIGHT)
            self.img_tk = ImageTk.PhotoImage(img)

            # Now compute the least-significant bits.
            low_img = Image.new('1', (_IMG_WIDTH, _IMG_HEIGHT))
            pixels = img.getdata()
            low_bits = tuple(p%2 for p in pixels)
            low_img.putdata(low_bits)
            self.img_tk_low_bits = ImageTk.BitmapImage(low_img, 
                    foreground='white')

            self.toggle_v.set(0)
            self._toggle_low_bits()

        return


def _main():
    root = tkinter.Tk()
    left_f = _image_frame(root)
    right_f = _image_frame(root)
    left_f.pack(side='left')
    right_f.pack(side='right')
    root.mainloop()

if __name__ == '__main__':
    _main()
