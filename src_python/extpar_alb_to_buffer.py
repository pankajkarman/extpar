#!/usr/bin/env python3.6 
import logging
import os
import sys
import subprocess
import netCDF4 as nc
import numpy as np

import utilities as utils
import grid_def
import buffer
import metadata
import fortran_namelist
from namelist import input_alb as ia
from namelist import input_grid as ig

# initialize logger
logging.basicConfig(filename='extpar_alb_to_buffer.log',
                    level=logging.INFO,
                    format='%(message)s',
                    filemode='w')


logging.info('============= start extpar_alb_to_buffer =======')
logging.info('')

# get number of OpenMP threads for CDO
omp = utils.get_omp_num_threads()

# unique names for files written to system to allow parallel execution
grid = 'grid_description_albedo'  # name for grid description file
weights = 'weights_albedo'        # name for weights of spatial interpolation

# names for output of CDO
alb_cdo_1 = 'alb_1.nc'
alb_cdo_2 = 'alb_2.nc'
alb_cdo_3 = 'alb_3.nc'

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= delete files from old runs =======')
logging.info('')

utils.remove(grid)
utils.remove(weights)
utils.remove(alb_cdo_1)
utils.remove(alb_cdo_2)
utils.remove(alb_cdo_3)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= init variables from namelist =====')
logging.info('')

ialb_type = utils.check_albtype(ia['ialb_typ'])

igrid_type = utils.check_gridtype(ig['igrid_type'])

if (igrid_type == 1):
    grid = utils.clean_path('', ig['icon_grid'])
elif(igrid_type == 2):
    tg = grid_def.CosmoGrid()
    tg.create_grid_description(grid)


if (ialb_type != 2):
    raw_data_alb_1   = utils.clean_path(ia['raw_data_alb_path'], ia['raw_data_alb_filename'])

    if (ialb_type == 1):
        raw_data_alb_2 = utils.clean_path(ia['raw_data_alb_path'], ia['raw_data_aluvd_filename'])
        raw_data_alb_3 = utils.clean_path(ia['raw_data_alb_path'], ia['raw_data_alnid_filename'])

else:
    raw_data_alb_1   = utils.clean_path(ia['raw_data_alb_path'], ia['raw_data_alb_filename'])

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= initialize metadata ==============')
logging.info('')

lat_meta   = metadata.Lat()
lon_meta   = metadata.Lon()

if (ialb_type == 2):
    alb_meta_1 = metadata.AlbSat()
    alb_meta_2 = metadata.AlbDry()
else:
    alb_meta_1 = metadata.AL()
    alb_meta_3 = metadata.UV()
    alb_meta_2 = metadata.NI()

var_1, var_2, var_3 = utils.determine_albedo_varnames(ialb_type)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= write FORTRAN namelist ===========')
logging.info( '')

input_alb = fortran_namelist.InputAlb()
fortran_namelist.write_fortran_namelist('INPUT_ALB', ia, input_alb)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= CDO: remap to target grid ========')
logging.info('')

# calculate weights
utils.launch_shell('cdo', '-f', 'nc4', '-P', omp, f'gendis,{grid}',
                   raw_data_alb_1, weights)

# regrid 1
utils.launch_shell('cdo', '-f', 'nc4', '-P', omp, 
                   f'setrtoc,-1000000,0.02,0.02',
                   f'-remap,{grid},{weights}', raw_data_alb_1, alb_cdo_1)

if (ialb_type == 1):
    # regrid 2
    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                   f'setrtoc,-1000000,0.02,0.02',
                   f'-remap,{grid},{weights}', raw_data_alb_2, alb_cdo_2)

    # regrid 3
    utils.launch_shell('cdo','-f', 'nc4', '-P', omp,
                   f'setrtoc,-1000000,0.02,0.02',
                   f'-remap,{grid},{weights}', raw_data_alb_3, alb_cdo_3)


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= reshape CDO output ===============')
logging.info('')

alb_nc_1 = nc.Dataset(alb_cdo_1, "r")

if (ialb_type == 1):
    alb_nc_2 = nc.Dataset(alb_cdo_2, "r")
    alb_nc_3 = nc.Dataset(alb_cdo_3, "r")

if (igrid_type == 1):

    # infer coordinates/dimensions form CDO file
    ie_tot = len(alb_nc.dimensions['cell'])
    lon   = np.rad2deg(np.reshape(alb_nc_1.variables['clon'][:], (1, 1, cells)))
    lat   = np.rad2deg(np.reshape(alb_nc_1.variables['clat'][:], (1, 1, cells)))
    je_tot = 1
    ke_tot = 1


else:

    # infer coordinates/dimensions from tg
    lat, lon = tg.latlon_cosmo_to_latlon_regular()
    ie_tot   = tg.ie_tot
    je_tot   = tg.je_tot
    ke_tot   = tg.ke_tot

    if (ialb_type != 2):
        alb_1 = np.reshape(alb_nc_1.variables[var_1][:,:], (12, ke_tot, je_tot, ie_tot))
        if (ialb_type == 1):
            alb_2 = np.reshape(alb_nc_2.variables[var_2][:,:,:],
                               (12,1,tg.je_tot,tg.ie_tot))
            alb_3 = np.reshape(alb_nc_2.variables[var_3][:,:,:],
                               (12,1,tg.je_tot,tg.ie_tot))

    else:
        alb_1    = np.reshape(alb_nc_1.variables[var_1][:,:], (ke_tot, je_tot, ie_tot))
        alb_2    = np.reshape(alb_nc_1.variables[var_2][:,:], (ke_tot, je_tot, ie_tot))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= write to buffer file =============')
logging.info( '')

# init buffer file
if (igrid_type == 1):
    buffer_file = buffer.init_netcdf(ia['alb_buffer_file'], 1, cells)
elif(igrid_type == 2):
    buffer_file = buffer.init_netcdf(ia['alb_buffer_file'], tg.je_tot, tg.ie_tot)

# add 12 months as additional dimension
if (ialb_type != 2):
    buffer_file = buffer.add_dimension_month(buffer_file)

# write lat/lon
buffer.write_3d_field(buffer_file, lon, lon_meta)
buffer.write_3d_field(buffer_file, lat, lat_meta)

# write albedo fields
if (ialb_type != 2):
    buffer.write_4d_field(buffer_file, alb_1, alb_meta_1)

    if (ialb_type == 1):
        buffer.write_4d_field(buffer_file, alb_2, alb_meta_2)
        buffer.write_4d_field(buffer_file, alb_3, alb_meta_3)

else:
    buffer.write_3d_field(buffer_file, alb_1, alb_meta_1)
    buffer.write_3d_field(buffer_file, alb_2, alb_meta_2)

buffer.close_netcdf(buffer_file)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= extpar_alb_to_buffer done ========')
logging.info( '')
