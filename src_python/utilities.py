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

-check_itype_cru : check wether itype_cru from namelist is correct

-get_omp_num_threads: get value of environment variable for OpenMP

-check_albtype: check wether ialb_type from namelist is correct

-determine_albedo_varnames: assign correct varnames for different ialb_type
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


def check_albtype(alb_type):
    '''
    check alb_type for correctnes and return value, 
    if not exit programme
    '''

    if (alb_type > 3):
        logging.error(f'ialb_type {alb_type} does not exist.')
        exit(1)

    if (alb_type == 1):
        logging.info('process albedo data  for VIS, NIR and UV spectra')

    if (alb_type == 2):
        logging.info('process albedo data  for dry and saturated soil')

    if (alb_type == 3):
        logging.info('process albedo data  for VIS only')

    return alb_type


def check_emisstype(emiss_type):
    '''
    check emiss_type for correctnes and return value, 
    if not exit programme
    '''
    if (emiss_type < 1 or emiss_type > 2):
        logging.error(f'iemiss_type {emiss_type} does not exist. '
                      'Use 1 (full-range) or 2 (longwave) instead!')

    if (emiss_type == 1):
        logging.info('process full-range emissivity data')

    if (emiss_type == 2):
        logging.info('process long-wave emissivity data only')

    return emiss_type


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


def check_itype_cru(itype_cru):
    '''
    check itype_cru for correctness and return value,
    if not exit programme
    '''

    if (itype_cru > 2 or itype_cru < 1):
        logging.error(f'itype_cru {itype_cru} does not exist. ' 
                      f'Use 1 (fine) or 2 (coarse and fine) instead!')

        exit(1)

    if (itype_cru == 1):
        logging.info('Process fine resolution for land')

    if (itype_cru == 2):
        logging.info('Process fine resolution for land, '
                     'coarse resolution for sea')

        return itype_cru


def get_omp_num_threads():
    '''
    get environment variables for OMP,
    if not set, assume 1 as default
    '''

    try:
        omp = os.environ['OMP_NUM_THREADS']
    except KeyError:
        omp = 1
        logging.warning('OMP_NUM_THREADS not set -> '
                        'use OMP_NUM_THREADS = 1 instead')
    return omp


def determine_emiss_varnames(iemiss_type):
    '''
    assign the correct variable names for different iemiss_type
    '''

    if (iemiss_type == 1):
        var = 'bbemis_full'

    if (iemiss_type == 2):
        var = 'bbemis_longwave'

    return var


def determine_albedo_varnames(ialb_type):
    '''
    assign the correct variable names for different ialb_type
    '''

    if (ialb_type == 1):
        var_1 = 'al'
        var_2 = 'alnid'
        var_3 = 'aluvd'

    elif (ialb_type == 2):
        var_1 = 'ALB_DRY'
        var_2 = 'ALB_SAT'
        var_3 = ''

    elif (ialb_type == 3):
        var_1 = 'al'
        var_2 = ''
        var_3 = ''

    return var_1, var_2, var_3
