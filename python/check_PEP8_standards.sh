#!/bin/bash

# Scripts to check if Python files allign with the Pep8 Coding standard
# All files in the "python" and "python/lib" are checked

# Not all coding rules are required du to simple practical reasons
# or (E722) because no other way was possible in the code.

# The following picky coding rules are ignored:

############# ERROR CODES ####################

# E265: block comment should start with '#'
# E201: whitespace after '('
# E221: multiple spaces before operator
# E231: missing whitespace after ',' ';' or ':'
# E241: multiples spaces after ','
# E722: do not use bare except, specify exception instead

############# WARNINGS #######################

# W291: trailing whitespace
# W504: line break after binary operator

# python directory
./lib/pycodestyle.py --ignore=E265,W291,E201,E221,E231,E241,E722,W504 *.py

# python/lib directory
./lib/pycodestyle.py --exclude=namelist* --ignore=E265,W291,E201,E221,E231,E241,E722,W504 lib/*.py
