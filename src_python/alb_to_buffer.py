#!/usr/bin/env python3 
import logging
import os
import sys
import subprocess
import netCDF4 as nc
import numpy as np

import INPUT_ALB as ia
import INPUT_GRID as ig
import utilities as utils
import grid_def
import buffer 
import metadata

# initialize logger
logging.basicConfig(filename='extpar_alb_to_buffer.log',\
                    level=logging.INFO, \
                    format='%(levelname)s:%(message)s', \
                    filemode='w')
                    

logging.info( '============= start extpar_alb_to_buffer =======')
logging.info( '')

# get number of OpenMP threads for CDO
try:  
    omp = os.environ['OMP_NUM_THREADS']
except KeyError: 
    omp = 1 
    logging.warning('OMP_NUM_THREADS not set -> use OMP_NUM_THREADS = 1 instead')

# unique names for files written to system to allow parallel execution
grid = 'grid_description_albedo' # name for grid description file
weights = 'weights_albedo'       # name for weights of spatial interpolation

# names for output of CDO
alb_cdo = 'alb-dis.nc'
alnid_cdo = 'alnid-dis.nc'
aluvd_cdo = 'aluvd-dis.nc'

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= delete files from old runs =======')
logging.info( '')

utils.remove(grid)
utils.remove(weights)
utils.remove(alb_cdo)
utils.remove(alnid_cdo)
utils.remove(aluvd_cdo)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= init variables from namelist =====')
logging.info( '')


igrid_type = ig.igrid_type
if (igrid_type > 2):
    logging.error(f'igrid_type {igrid_type} does not exist. Use 1 (Icon) or 2 (Cosmo) instead!')
    exit(1)

if (igrid_type == 1):
    grid= utils.clean_path('',ig.icon_grid)
elif(igrid_type == 2):
    tg= grid_def.CosmoGrid()
    tg.create_grid_description(grid)

raw_data_alb   = utils.clean_path(ia.raw_data_path, ia.raw_data_alb)
raw_data_aluvd = utils.clean_path(ia.raw_data_path, ia.raw_data_aluvd)
raw_data_alnid = utils.clean_path(ia.raw_data_path, ia.raw_data_alnid)


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= initialize metadata ==============')
logging.info( '')

lat_meta=metadata.Lat()
lon_meta=metadata.Lon()

al_meta=metadata.AL()
alnid_meta=metadata.NI()
aluvd_meta=metadata.UV()

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= CDO: remap to target grid ========')
logging.info( '')

# calculate weights
utils.launch_shell('cdo','-f', 'nc4','-P', omp,f'gendis,{grid}',raw_data_alb, weights)

# regrid 1
utils.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                   f'-remap,{grid},{weights}',raw_data_alb, alb_cdo)
# regrid 2
utils.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                   f'-remap,{grid},{weights}',raw_data_alnid, alnid_cdo)
# regrid 3
utils.launch_shell('cdo','-f', 'nc4','-P', omp,f'setrtoc,-1000000,0.02,0.02', \
                   f'-remap,{grid},{weights}',raw_data_aluvd, aluvd_cdo)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= reshape CDO output ===============')
logging.info( '')

alb_nc = nc.Dataset(alb_cdo, "r")
albni_nc = nc.Dataset(alnid_cdo, "r")
albuv_nc = nc.Dataset(aluvd_cdo, "r")

if (igrid_type == 1):

    # infer coordinates/dimensions form CDO file
    cells = len(alb_nc.dimensions['cell'])
    lon   = np.rad2deg(np.reshape(alb_nc.variables['clon'][:], (1,1,cells)))
    lat   = np.rad2deg(np.reshape(alb_nc.variables['clat'][:], (1,1,cells)))

    al    = np.reshape(alb_nc.variables['al'][:,:],(12,1,1,cells))
    alnid = np.reshape(albni_nc.variables['alnid'][:,:], (12,1,1,cells))
    aluvd = np.reshape(albuv_nc.variables['aluvd'][:,:], (12,1,1,cells))

else:

    # infer coordinates/dimensions from tg
    lat,lon = tg.latlon_cosmo_to_latlon_regular()

    al    = np.reshape(alb_nc.variables['al'][:,:,:],(12,1,tg.je_tot,tg.ie_tot))
    alnid = np.reshape(albni_nc.variables['alnid'][:,:,:], (12,1,tg.je_tot,tg.ie_tot))
    aluvd = np.reshape(albuv_nc.variables['aluvd'][:,:,:], (12,1,tg.je_tot,tg.ie_tot))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= write to buffer file =============')
logging.info( '')

# init buffer file
if (igrid_type == 1):
    buffer_file = buffer.init_netcdf(ia.buffer_alb,1,cells)
elif(igrid_type == 2):
    buffer_file = buffer.init_netcdf(ia.buffer_alb,tg.je_tot,tg.ie_tot)

# write lat/lon
buffer.write_3d_field(buffer_file,lat, lat_meta)
buffer.write_3d_field(buffer_file,lon, lon_meta)

# write albedo fields
buffer.write_4d_field(buffer_file,al, al_meta)
buffer.write_4d_field(buffer_file,alnid, alnid_meta)
buffer.write_4d_field(buffer_file,aluvd, aluvd_meta)

buffer.close_netcdf(buffer_file)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= extpar_alb_to_buffer done ========')
logging.info( '')
