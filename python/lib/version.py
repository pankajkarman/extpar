import logging
import sys

'''
module to check the Python version that runs Exptar,
it contains:

    -version_check: check if Python 3.6 or higher, if not exit
'''


def version_check(file_calling_function):
    '''
    Check if Python 3.6 or higher is used

    If wrong Python version is used, open the right logfile
    and write error message and the corresponding Python version
    '''

    # version check -> Python 3.6 or higher required
    if not (sys.version_info.major == 3 and sys.version_info.minor >= 6):

        # remove .py suffix
        filename = file_calling_function.rsplit('.',1)[0]

        # add .log suffix
        logfile = filename.__add__('.log')

        # initialize logger only for error message
        logging.basicConfig(filename=logfile,
                            level=logging.INFO,
                            format='%(message)s',
                            filemode='w')

        logging.warning('You are using Python {}.{}.'
                        .format(sys.version_info.major, 
                                sys.version_info.minor))

        logging.error('This script requires Python 3.6 or higher!')

        sys.exit(1)
