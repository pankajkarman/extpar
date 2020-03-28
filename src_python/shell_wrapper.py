import subprocess
import sys
import logging

def launch_shell(bin,*args):
    ''' wrapper to launch an external programme on the system

        bin is the executable to run
        *args are the arguments for bin, need to be convertable to string
        output of bin is written to logfile
    '''

    #convert *args to string
    arg_list=[]
    arg_list.insert(0,str(bin))
    for arg in args:
        arg_list.append(str(arg))

    logging.info(f'Launch shell command: {arg_list}')
    try:
        output=subprocess.check_output(arg_list, universal_newlines=True)
    except subprocess.CalledProcessError:
        logging.error('Shell command failed', exc_info=True)
        sys.exit(1)

    logging.info('Succesful! Output:')
    logging.info(output)

    return
