"""
@package steg

A program for embedding character data into a greyscale image and extracting
data so embedded.  

### Embedding text into an image

To embed a text file into the least-significant bits
of an 8-bit greyscale image execute the command

    $ python steg.py embed in_img_file out_img_file data_file

where
  * \c in_img_file  is the name of some image file; \c data_file will be
    embedded in the least significant bits of the pixels of the image
    in \c in_img_file.
  * \c out_img_file  is the name of the image file to create with the embedded
    text.
  * \c data_file  is the name of the text file to embed.

\c in_img_file  must be an 8-bit greyscale image.
\c in_img_file  and \c out_img_file  can be of any image format (the format
will be detected by the filename extension), but \c out_img_file  must be
a lossless format such as PNG or TIFF.  Furthermore, it must be the case
that \f$32 + 8n  \leq w\cdot h\f$, where \f$n\f$ is the number of characters in
\c data_file , \f$w\f$ is the width of \c in_img_file , and \f$h\f$ is the 
height of \c in_img_file .

### Extracting text from an image

To extract a text file that has been embedded into an image with the
above command, execute the command

    $ python steg.py extract img_file

where \c img_file  is the file created by an invocation as above.
The extracted text will be printed to the terminal.

@author N. Danner

"""

# Python standard library modules.
import sys

# Python Image Library modules.
from PIL import Image

# Local modules
import hw1_steg

def _main(argv):
    command = argv[0]

    if command == 'embed':
        # python steg.py embed in_img out_img data
        in_img_filename = argv[1]
        out_img_filename = argv[2]
        data_filename = argv[3]

        # Read in the input image, quit if it is not 8-bit greyscale.
        in_img = Image.open(in_img_filename)
        if in_img.mode != 'L':
            print("Input image must be greyscale.")
            sys.exit(1)
        width, height = in_img.size
        pixels = list(in_img.getdata())

        # Read in the data file.
        data_file = open(data_filename, 'r')
        data = data_file.read()
        data_file.close()

        # Embed the data into the image, getting the resulting image
        # returned to us.
        hw1_steg.str_into_pixels(data, pixels)

        # Save the resulting image (the Python Image Library looks at
        # the filename extension and automatically writes the appropriate
        # image format).
        out_img = Image.new("L", (width, height))
        out_img.putdata(pixels)
        out_img.save(out_img_filename)

    elif command == 'extract':
        # python steg.py extract img
        in_img_filename = argv[1]

        # Read in the input image, quit if it is not 8-bit greyscale.
        in_img = Image.open(in_img_filename)
        if in_img.mode != 'L':
            print("Input image must be greyscale.")
            sys.exit(1)
        in_pixels = list(in_img.getdata())

        # Get the data and print it out.
        data = hw1_steg.str_from_pixels(in_pixels)
        print(data)

    elif command == 'cr':
        # python steg.py cr img
        in_img_filename = argv[1]

        # Open the image and make sure it is type 'L'.
        img = Image.open(in_img_filename)
        if img.mode != 'L':
            print("Input image must be greyscale.")
            sys.exit(1)

        pixels = img.getdata()
        cr = hw1_steg.cr(pixels)

        # print("Compression ratio for " + in_img_filename + ": " + str(cr))
        print("Compression ratio for %s: %1.4f" % (in_img_filename, cr))

    else:
        print('Illegal command: %s' % command)
        sys.exit(2)

if __name__ == '__main__':
    _main(sys.argv[1:])

