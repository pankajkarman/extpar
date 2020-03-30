#!/usr/bin/env python3 
import logging
import os
import sys
import subprocess
import xarray as xr

import INPUT_ALB as ia
import INPUT_GRID as ig
import utilities as utils
import shell_wrapper as sw
import grid_def
import buffer as buffer
import metadata as metadata

# OpenMP threads for CDO
omp  = 8                  # OpenMP threads for CDO
grid = 'grid_description' # name for grid description file

# initialize logger
logging.basicConfig(filename='extpar_alb_to_buffer.log',\
                    level=logging.INFO, \
                    format='%(levelname)s:%(message)s', \
                    filemode='w')
                    

logging.info( '============= start extpar_alb_to_buffer =======')
logging.info( '')
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= init grid ========================')
logging.info( '')

if (ig.igrid_type == 1):
    grid=ig.icon_grid
elif(ig.igrid_type == 2):
    tg= grid_def.CosmoGrid()
    tg.create_grid_description(grid)

else:
    logging.error(f'igrid_type {igrid_type} does not exist. Use 1 (Icon) or 2 (Cosmo) instead!')
    exit(1)

raw_data_alb   = utils.clean_path(ia.raw_data_path, ia.raw_data_alb)
raw_data_aluvd = utils.clean_path(ia.raw_data_path, ia.raw_data_aluvd)
raw_data_alnid = utils.clean_path(ia.raw_data_path, ia.raw_data_alnid)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= remap to target grid =============')
logging.info( '')


utils.remove("weights.nc")

# calculate weights
#sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'gendis,{grid}',ia.raw_data_alb, 'weights.nc')
#
## regrid 1
#sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
#                f'-remap,{grid},weights.nc',raw_data_alb, 'alb-dis.nc')
## regrid 2
#sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
#                f'-remap,{grid},weights.nc',raw_data_alnid, 'alnid-dis.nc')
## regrid 3
#sw.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
#                f'-remap,{grid},weights.nc',raw_data_aluvd, 'aluvd-dis.nc')

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= initialize MetaData classes ======')
logging.info( '')

lat_meta=metadata.LatMeta()
lon_meta=metadata.LonMeta()

albedo_meta=metadata.AlbMeta()

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= write to buffer file =============')
logging.info( '')
#
if (ig.igrid_type == 1):
    buffer.write_icon(ia.buffer_alb)
elif(ig.igrid_type == 2):
    buffer.write_cosmo(ia.buffer_alb, tg)
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= extpar_alb_to_buffer done ========')
logging.info( '')
