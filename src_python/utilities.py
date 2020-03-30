import logging
import sys
import os
from pathlib import Path


def remove(file):
    if os.path.exists(file):
        os.remove(file)
    else:
        logging.warning(f'Could not remove {file}')

    return

def clean_path(dir, file):
    clean_path= os.path.join(dir, file)
    try:
        f= open(clean_path)

    except FileNotFoundError :
            logging.error('File not found', exc_info=True)
            sys.exit(1)

    f.close()

    return clean_path


