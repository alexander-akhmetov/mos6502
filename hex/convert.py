#!/usr/bin/python3
import sys


"""
Probably, some files can be converted with xxd tool
"""


def convert(in_filename, out_filename):
    with open(in_filename, 'r') as in_f:
        content = in_f.readlines()

    content = [l.lstrip(': ').strip().split() for l in content]

    byte_array = []
    for line in content:
        for byte in line:
            byte_array.append(bytearray.fromhex(byte))

    byte_array = b''.join(byte_array)

    with open(out_filename, 'wb') as out_f:
        out_f.write(byte_array)


if __name__ == '__main__':
    convert(sys.argv[1], sys.argv[2])
