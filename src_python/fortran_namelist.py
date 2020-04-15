import logging
import sys

'''
Module providing function and classes needed for writing
Fortran-namelists with the python version of Extpar,
it contains:

    -read_variable_from_namelist: read a variable from given Fortran namelist

    -write_fortran_namelist: write a Fortran namelist using corr. class

    -classes defining the groups and its variables of a Fortran namelist
        -InputTclim
        -InputAlb
        -InputEmiss
        -InputNdvi
'''


def read_variable_from_namelist(namelist, variable):
    '''
    read variable from existing Fortran namelist

    namelist is read line by line, the first occurence of variable
    is taken as the value and returned,
    only tested with strings and integers
    '''

    with open(namelist, 'r') as f:

        # read line by line
        for line in f:
            line = line.rstrip()
            if variable in line:
                split = line.split('=')

                # return last element of split and remove "," and "'"
                return split[-1].strip().strip("',")

    # variable not found in namelist
    logging.error(f'Could not find {variable} in {namelist}')
    sys.exit(1)


def write_fortran_namelist(name, namelist, nl_class):
    '''
    write Fortran namelist with name for extpar_consistency_check

    the groups of the namelist and its variables are defined in nl_class,
    the values of the variables are stored in namelist
    '''

    with open(name, 'w') as f:
        for group in nl_class.variables.keys():
            f.write(f'{group}\n')
            for var in nl_class.variables[group]:

                if isinstance(namelist[var], str):
                    if (str(namelist[var]) and str(namelist[var]).strip()):
                        f.write(f"  {var} = '{namelist[var]}',\n")
                    else:
                        f.write(f"  {var} = '""'\n")
                else:
                    f.write(f'  {var} = {namelist[var]},\n')

            f.write('/\n')


class InputTclim:
    '''
    define structure of former namelist "INPUT_TCLIM"
    '''

    def __init__(self):

        self.variables = {'&t_clim_raw_data':{'it_cl_type'}}

        self.variables.update({'&t_clim_io_extpar':{'t_clim_buffer_file'}})


class InputAlb:
    '''
    define structure of former namelist "INPUT_ALB"
    '''

    def __init__(self):

        self.variables = {'&alb_raw_data':{'raw_data_alb_path',
                                           'raw_data_alb_filename',
                                           'ialb_type'}}

        self.variables.update({'&alnid_raw_data':{'raw_data_alb_path',
                                                  'raw_data_alnid_filename',
                                                  }})

        self.variables.update({'&aluvd_raw_data':{'raw_data_alb_path',
                                                  'raw_data_aluvd_filename',
                                                  }})

        self.variables.update({'&alb_io_extpar':{'alb_buffer_file',
                                                 'alb_output_file'}})

        self.variables.update({'&alb_source_file':{'alb_source',
                                                   'alnid_source',
                                                   'aluvd_source'}})


class InputNdvi:
    ''' 
    define structure of former namelist "INPUT_NDVI"
    '''

    def __init__(self):

        self.variables = {'&ndvi_raw_data':{'raw_data_ndvi_path',
                                            'raw_data_ndvi_filename'}}

        self.variables.update({'&ndvi_io_extpar':{'ndvi_buffer_file',
                                                  'ndvi_output_file'}})


class InputEmiss:
    '''
    define structure of former namelist "INPUT_EMISS"
    '''

    def __init__(self):

        self.variables = {'&emiss_raw_data':{'raw_data_emiss_path',
                                             'raw_data_emiss_filename'}}

        self.variables.update({'&emiss_io_extpar': {'emiss_buffer_file',
                                                    'emiss_output_file'}})
