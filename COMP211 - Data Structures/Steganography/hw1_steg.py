"""
HW 1b: Contains the 5 required codes for the homework assignment

char_into_pixels takes a string, c, an array of numbers, pixels, and the number of pixels,pixel index. It embeds a character into the pixels and returns none.

str_into_pixels inputs a string, data, and an array of numbers, pixels. It embeds a string into pixels and returns none.

char_from_pixels inputs an array of numbers, pixels, and the total amount of numbers, pixel index, and returns a string, the corresponding letter to the found ASCII codes from the pixels.

str_from_pixels inputs only the total amount of pixels, pixel index, and returns the string which had been previously embedded into the function.

cr is a compression algorithm which inputs an array of numbers, pixels and returns the floating point number corresponding to the compression ratio.

Fabien Bessez
"""

import zlib
from typing import List

def char_into_pixels(c: (str), pixels: (List[int]), pixel_index: (int))-> None:
    character = ord(c)
    for i in range(0,8):
        last_bit = character % 2
        pixels[pixel_index] =(pixels[pixel_index] // 2) * 2 + last_bit
        character = character // 2
        pixel_index = pixel_index + 1
    return None

def str_into_pixels(data: (str), pixels: (List[int]))-> None:
    data_size = len(data)
    if 32 + 8 * data_size > len(pixels):
        return None
    for i in range(0,32):
        last_bit = data_size % 2
        pixels[i] = (pixels[i] // 2) * 2 + last_bit
        data_size = data_size // 2
    pixel_index = 32
    for character in data:
        char_into_pixels(character, pixels, pixel_index)
        pixel_index = pixel_index + 8
    return None

def char_from_pixels(pixels: (List[int]), pixel_index: (int)) -> str:
    c = 0
    for i in range (0,8):
        c = c + ((pixels[pixel_index + i] % 2) << i)
    return chr(c)

def str_from_pixels(pixels: (List[int])) -> str:
    data_size = 0
    for i in range(0,32):
        data_size = data_size + ((pixels[i] % 2) << i)
    pixel_index = 0
    c = ''
    for i in range(data_size):
        pixel_index = 32 + 8*i
        c = c + char_from_pixels(pixels, pixel_index)
    return c 

def cr(pixels: (List[int])) -> float:
   s = ''
   i = 0
   while i < len(pixels):
       ASCII= 0  
       for j in range(0,8):
            ASCII = ASCII + ((pixels[j+i] % 2) << j)
            s = s + chr(ASCII)
       i = i + 8 
   t1 = bytes(s, 'utf8')
   t2 = zlib.compress(t1)
   cr = len(t2) / len(t1)
   return cr
