import logging
import sys

def read_variable_from_namelist(namelist, variable):
    with open(namelist, 'r') as f:
        for line in f:
            line =line.rstrip()
            if variable in line:
                split = line.split('=')
                return split[-1].strip("',")

    logging.error(f'Could not find {variable} in {namelist}')
    sys.exit(1)

def write_fortran_namelist(name, namelist, nl_class):

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
    def __init__(self):

        self.variables = {'&t_clim_raw_data':{'raw_data_path',
                                              'raw_data_tclim_coarse',
                                              'raw_data_tclim_fine',
                                              'it_cl_type'}}

        self.variables.update({'&t_clim_io_extpar':{'raw_data_path',
                                                 'buffer_tclim'}})

class InputAlb:
    def __init__(self):

        self.variables = {'&alb_raw_data':{'raw_data_alb_path',
                                           'raw_data_alb_filename'}}

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
    def __init__(self):

        self.variables = {'&ndvi_raw_data':{'raw_data_ndvi_path',
                                            'raw_data_ndvi_filename'}}

        self.variables.update({'&ndvi_io_extpar':{'ndvi_buffer_file',
                                                  'ndvi_output_file'}})
