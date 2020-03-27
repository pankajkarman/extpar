import logging
import sys
import os
from pathlib import Path


def check_existence (dir, file):
    dir_and_file= os.path.join(dir, file)
    try:
        f= open(dir_and_file)

    except FileNotFoundError :
            logging.error('File not found', exc_info=True)
            sys.exit(1)

    f.close()
    return


