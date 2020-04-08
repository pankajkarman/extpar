import logging
import sys

class FortranNamelist:
    def __init__(self):
        self.groups = []
        self.variables = []

    def write_fortran_namelist(self, name, *args):

        args_list = [*args]

        vars_to_write = 0
        for i in range(len(self.groups)):
            vars_to_write += len(self.variables[i])
        
        if (vars_to_write != len(args_list)):
            logging.error('Wrong number of variables for Fortran namelist '
                          f'passed {len(args_list)}, required {vars_to_write}')
            sys.exit(1)
        import IPython; IPython.embed
        print( len(args_list))
        print(vars_to_write)
        with open(name, 'w') as f:
            idx = 0
            for i in range(len(self.groups)):
                f.write(f'{self.groups[i]}\n')
                for j in range(len(self.variables[i])):
                    f.write(f'{self.variables[i][j]} = {args_list[idx]}\n')
                    idx += 1

                f.write('/\n')

                
                


class InputTclim(FortranNamelist):
    def __init__(self):
        super().__init__()
        self.groups = ['&t_clim_raw_data', 
                       '&t_clim_io_extpar']

        self.variables = []

        # variables of &t_clim_raw_data
        self.variables.append({ 0: 'raw_data_t_clim_path',
                                1: 'raw_data_t_clim_filename',
                                2: 'it_cl_type'})

        # variables of &t_clim_io_extpar
        self.variables.append({ 0: 'raw_data_t_clim_path',
                                1: 'raw_data_t_clim_filename',
                                2: 't_clim_buffer_file',
                                3: 't_clim_output_file'})
