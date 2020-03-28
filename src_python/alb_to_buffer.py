#!/usr/bin/env python3 
import logging
import os
import sys
import subprocess

import INPUT_ALB as nl
import io_utilities as io_utils
import shell_wrapper as sw


# initialize logger
logging.basicConfig(filename='extpar_alb_to_buffer.log',\
                    level=logging.INFO, \
                    format='%(levelname)s:%(message)s', \
                    filemode='w')
                    

logging.info( '============= start alb_to_buffer ==============')
logging.info( '')
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= read namelist ====================')
logging.info( '')


io_utils.check_existence(nl.raw_data_path, nl.raw_data_alb)
io_utils.check_existence(nl.raw_data_path, nl.raw_data_aluvd)
io_utils.check_existence(nl.raw_data_path, nl.raw_data_alnid)

omp=8

if os.path.exists("weights.nc"):
    os.remove("weights.nc")

# calculate weights
sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'gendis,{nl.grid_spec}',nl.raw_data_alb, 'weights.nc')

# regrid 1
sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                f'-remap,{nl.grid_spec},weights.nc',nl.raw_data_alb, 'alb-dis.nc')
# regrid 2
sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                f'-remap,{nl.grid_spec},weights.nc',nl.raw_data_alnid, 'alnid-dis.nc')
# regrid 3
sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                f'-remap,{nl.grid_spec},weights.nc',nl.raw_data_aluvd, 'aluvd-dis.nc')


