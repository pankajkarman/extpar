#!/usr/bin/env python3 
import logging
import os
import sys
import subprocess
import numpy as np
import xarray as xr

import INPUT_ALB as IA
import INPUT_GRID as IG
import io_utilities as io_utils
import shell_wrapper as sw
#import grid_def


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
logging.info( '============= setup grid =======================')
logging.info( '')

#if (IG.itype_grid == 1):
#    #icon grid to do
#elif(IG.itype_grid == 2):
#    cosmo_grid= grid_def.construct_cosmo_grid()


io_utils.check_existence(IA.raw_data_path, IA.raw_data_alb)
io_utils.check_existence(IA.raw_data_path, IA.raw_data_aluvd)
io_utils.check_existence(IA.raw_data_path, IA.raw_data_alnid)

omp=1

if os.path.exists("weights.nc"):
    os.remove("weights.nc")

# calculate weights
sw.launch_shell('cdo','-f', 'nc2','-P', omp,f'gendis,{IA.grid_spec}',IA.raw_data_alb, 'weights.nc')

# regrid 1
sw.launch_shell('cdo','-f', 'nc2','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                f'-remap,{IA.grid_spec},weights.nc',IA.raw_data_alb, 'alb-dis.nc')
# regrid 2
sw.launch_shell('cdo','-f', 'nc2','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                f'-remap,{IA.grid_spec},weights.nc',IA.raw_data_alnid, 'alnid-dis.nc')
# regrid 3
sw.launch_shell('cdo','-f', 'nc2','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                f'-remap,{IA.grid_spec},weights.nc',IA.raw_data_aluvd, 'aluvd-dis.nc')


# read the cdo processed data and retrieve all attributes needed

alb=xr.open_dataset('alb-dis.nc')
albni=xr.open_dataset('alnid-dis.nc')
albuv=xr.open_dataset('aluv-dis.nc')
