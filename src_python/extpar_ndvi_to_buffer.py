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
from namelist import input_ndvi as indvi
from namelist import input_grid as ig

# initialize logger
logging.basicConfig(filename='extpar_ndvi_to_buffer.log',
                    level=logging.DEBUG,
                    format='%(levelname)s:%(message)s',
                    filemode='w')


logging.info('============= start extpar_ndvi_to_buffer ======')
logging.info('')

# get number of OpenMP threads for CDO
try:
    omp = os.environ['OMP_NUM_THREADS']
except KeyError:
    omp = 1
    logging.warning('OMP_NUM_THREADS not set ->'
                    'use OMP_NUM_THREADS = 1 instead')

# unique names for files written to system to allow parallel execution
grid = 'grid_description_ndvi'  # name for grid description file
weights = 'weights_ndvi'        # name for weights of spatial interpolation

# names for output of CDO
ndvi_cdo = 'ndvi_ycon.nc'

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= delete files from old runs =======')
logging.info('')

utils.remove(grid)
utils.remove(weights)
utils.remove(ndvi_cdo)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= init variables from namelist =====')
logging.info('')


igrid_type = ig['igrid_type']
if (igrid_type > 2):
    logging.error(f'igrid_type {igrid_type} does not exist. ' 
                  f'Use 1 (Icon) or 2 (Cosmo) instead!')
    exit(1)

if (igrid_type == 1):
    grid = utils.clean_path('', ig['icon_grid'])
elif(igrid_type == 2):
    tg = grid_def.CosmoGrid()
    tg.create_grid_description(grid)

import IPython; IPython.embed()
raw_data_ndvi  = utils.clean_path(indvi['raw_data_ndvi_path'], indvi['raw_data_ndvi_filename'])

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= initialize metadata ==============')
logging.info('')

lat_meta   = metadata.Lat()
lon_meta   = metadata.Lon()

ndvi_meta  = metadata.NDVI()
max_meta   = metadata.NdviMax()
mrat_meta  = metadata.NdviMrat()

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= write FORTRAN namelist ===========')
logging.info( '')

input_ndvi = fortran_namelist.InputNdvi()
fortran_namelist.write_fortran_namelist('INPUT_NDVI', indvi, input_ndvi)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= CDO: remap to target grid ========')
logging.info('')

# calculate weights
utils.launch_shell('cdo', '-f', 'nc4', '-P', omp, f'genycon,{grid}',
                   raw_data_ndvi, weights)

# regrid 1
utils.launch_shell('cdo', '-f', 'nc4', '-P', omp, 
                   f'settaxis,1111-01-01,0,1mo',
                   f'-remap,{grid},{weights}', raw_data_ndvi, ndvi_cdo)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= reshape CDO output ===============')
logging.info('')

ndvi_nc = nc.Dataset(ndvi_cdo, "r")

if (igrid_type == 1):

    # infer coordinates/dimensions form CDO file
    ie_tot = len(ndvi_nc.dimensions['cell'])
    je_tot = 1
    ke_tot = 1
    lon    = np.rad2deg(np.reshape(ndvi_nc.variables['clon'][:], 
                                   (ke_tot, je_tot, ie_tot)))
    lat    = np.rad2deg(np.reshape(ndvi_nc.variables['clat'][:],
                                   (ke_tot, je_tot, ie_tot)))

else:

    # infer coordinates/dimensions from tg
    lat, lon = tg.latlon_cosmo_to_latlon_regular()
    ie_tot   = tg.ie_tot
    je_tot   = tg.je_tot
    ke_tot   = tg.ke_tot

ndvi  = np.reshape(ndvi_nc.variables['ndvi'][:,:], 
                   (12, ke_tot, je_tot, ie_tot))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= compute NDVI_MAX and NDVI_MRAT ===')
logging.info('')

# calculate maxval over 12 month
ndvi_max = np.amax(np.reshape(ndvi_nc.variables['ndvi'][:,:], 
                              (12, ke_tot, je_tot, ie_tot)), axis=0)

# calculate ratio of ndvi/ndvi_max per month and set 'missing value' to -1
ndvi_mrat = np.empty((12, ke_tot, je_tot, ie_tot), dtype=mrat_meta.type)

for t in np.arange(12):
    ndvi_mrat[t,:,:,:] = np.divide(ndvi[t,:,:,:], ndvi_max[:,:,:],
                                   where=ndvi_max[:,:,:] != 0.0)
    ndvi_mrat[t,:,:,:] = np.where(ndvi_max[:,:,:] <= 0.0, -1.0, 
                                  ndvi_mrat[t,:,:,:])

# debug -> print these statistics setting level=level logging.DEBUG
logging.debug('Diagnostics:')

logging.debug('   NDVI max')
logging.debug('   min: {0:8.5f} mean: {1:8.5f} max: {2:8.5f}'
              .format(np.min(ndvi_max),
                      np.mean(ndvi_max),
                      np.max(ndvi_max)))

logging.debug('   NDVI')
for t in np.arange(12):
    logging.debug('   min: {0:8.5f} mean: {1:8.5f} max: {2:8.5f}'
                  .format(np.min(ndvi[t,:,:,:]),
                          np.mean(ndvi[t,:,:,:]),
                          np.max(ndvi[t,:,:,:])))

logging.debug('   NDVI mrat')
for t in np.arange(12):
    logging.debug('   min: {0:8.5f} mean: {1:8.5f} max: {2:8.5f}'
                  .format(np.min(ndvi_mrat[t,:,:,:]),
                          np.mean(ndvi_mrat[t,:,:,:]),
                          np.max(ndvi_mrat[t,:,:,:])))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= write to buffer file =============')
logging.info( '')

# init buffer file
buffer_file = buffer.init_netcdf(indvi['ndvi_buffer_file'], je_tot, ie_tot)

# write lat/lon
buffer.write_3d_field(buffer_file, lon, lon_meta)
buffer.write_3d_field(buffer_file, lat, lat_meta)

# write NDVI fields
buffer.write_3d_field(buffer_file, ndvi_max, max_meta)
buffer.write_4d_field(buffer_file, ndvi, ndvi_meta)
buffer.write_4d_field(buffer_file, ndvi_mrat, mrat_meta)

buffer.close_netcdf(buffer_file)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= extpar_ndvi_to_buffer done =======')
logging.info( '')
