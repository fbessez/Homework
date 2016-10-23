"""
@package util

Image utility functions.  

This module provides two programs.  One
converts an input image to greyscale and the other resizes an input image.
In all cases, input image file types are detected automatically,
and the extension of the output image file determines its type (e.g.,
`.jpg` for JPEG, `.png` for PNG, etc.).

### Convert a (color) image to greyscale

To convert any image to an 8-bit greyscale image, use the following command:

    $ python util.py grey in_img out_img

where 

  - `in_img` is the name of the original image file, and 
  - `out_img` is the name of the image file to create.  

`out_img` will be a greyscale version of `in_img`.

### Resize an image

To resize an image execute the following command:

    $ python util.py resize in_img out_img width height

where 

  - `in_img` is the name of the original image file;
  - `out_img` is the name of the image file to create with the resized image;
  - `width` and `height` are the new width and height.

The aspect ratio of `in_img` will *not* be preserved---i.e., the
image will be stretched horizontally and vertically to the new dimensions,
and this may "warp" the image if the ratio of the new width to height
does not match the original image.

@author N. Danner

"""

import sys

from PIL import Image

def _main(argv):
    # Get the command and then "shift" the argument list to start
    # with the arguments for the command.
    command = argv[0]
    argv = argv[1:]

    if command == 'grey':
        # python util grey in_img_file out_img_file
        in_img_filename = argv[0]
        out_img_filename = argv[1]
        img = Image.open(in_img_filename)
        grey_img = img.convert('L')
        grey_img.save(out_img_filename)

    elif command == 'resize':
        # python util resize in_img_file out_img_file width height
        in_img_filename = argv[0]
        out_img_filename = argv[1]
        width = int(argv[2])
        height = int(argv[3])
        in_img = Image.open(in_img_filename)
        out_img = in_img.resize((width, height), Image.BILINEAR)
        out_img.save(out_img_filename)

    else:
        print('Invalid command: %s' % command)
        sys.exit(1)

if __name__ == '__main__':
    _main(sys.argv[1:])

