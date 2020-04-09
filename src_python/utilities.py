import logging
import sys
import os
import subprocess

'''
Module utilities provides a bunch of helpful functions for Extpar,
it contains:

-launch_shell: wrapper to lauch a programm in the shell

-remove: remove a file from the system

-clean_path: make a clean path from a dir and file

-check_gridtype : check wether itype_grid from namelist is correct

-get_omp_num_threads: get value of environment variable for OpenMP
'''


def launch_shell(bin,*args):
    '''
    wrapper to launch an external programme on the system

    bin is the executable to run
    *args are the arguments for bin, need to be convertable to string
    stdout/stderr of bin is written to logfile
    '''

    #convert *args to string
    arg_list = []
    arg_list.insert(0,str(bin))
    for arg in args:
        arg_list.append(str(arg))

    logging.info(f'Launch shell command: {arg_list}')
    logging.info('')
    try:
        output = subprocess.check_output(arg_list,stderr=subprocess.STDOUT,
                                         universal_newlines=True)
    except subprocess.CalledProcessError:
        logging.error('Shell command failed', exc_info=True)
        sys.exit(1)

    logging.info('Output:')
    logging.info(f'{output}')

    return


def remove(file):
    ''' 
    remove file from system if exists
    '''

    if os.path.exists(file):
        os.remove(file)
    else:
        logging.warning(f'Could not remove {file}')

    return


def clean_path(dir, file):
    '''
    returns a clean path from a dir and file

    used to check if all files from namelist
    exist, if not exit programme
    '''

    clean_path = os.path.join(dir, file)
    try:
        f = open(clean_path)

    except FileNotFoundError:
        logging.error('File not found', exc_info=True)
        sys.exit(1)

    f.close()

    return clean_path


def check_gridtype(grid_type):
    '''
    check gridtype for correctnes and return value, 
    if not exit programme
    '''

    if (grid_type > 2):
        logging.error(f'grid_type {grid_type} does not exist. ' 
                      f'Use 1 (Icon) or 2 (Cosmo) instead!')
        exit(1)

    return grid_type

def get_omp_num_threads():
    '''
    get environment variables for OMP,
    if not set, assume 1 as default
    '''

    try:
        omp = os.environ['OMP_NUM_THREADS']
    except KeyError:
        omp = 1
        logging.warning('OMP_NUM_THREADS not set ->'
                        'use OMP_NUM_THREADS = 1 instead')
    return omp
