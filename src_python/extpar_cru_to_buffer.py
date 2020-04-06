#!/usr/bin/env python3 
import logging
import os
import sys
import subprocess
import netCDF4 as nc
import numpy as np

import INPUT_TCLIM as itcl
import INPUT_GRID as ig
import utilities as utils
import grid_def
import buffer
import metadata

# initialize logger
logging.basicConfig(filename='extpar_cru_to_buffer.log',
                    level=logging.INFO,
                    format='%(levelname)s:%(message)s',
                    filemode='w')


logging.info('============= start extpar_cru_to_buffer ========')
logging.info('')

# get number of OpenMP threads for CDO
try:
    omp = os.environ['OMP_NUM_THREADS']
except KeyError:
    omp = 1
    logging.warning('OMP_NUM_THREADS not set ->'
                    'use OMP_NUM_THREADS = 1 instead')

# unique names for files written to system to allow parallel execution
grid = 'grid_description_tclim'  # name for grid description file
weights = 'weights_tclim'        # name for weights of spatial interpolation

# names for output of CDO
coarse_regrid_to_fine = 'coarse_regrid.nc'
fine_contains_coarse = 'fine_with_sea.nc'
tclim_remap = 'tclim_remap_grid.nc'
tclim_cdo = 'tclim_to_write.nc'
topo_icon = 'topography_ICON.nc'
hh_topo_only = 'hh_topo_only.nc'
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= delete files from old runs =======')
logging.info('')

utils.remove(grid)
utils.remove(weights)
utils.remove(coarse_regrid_to_fine)
utils.remove(fine_contains_coarse)
utils.remove(tclim_cdo)
utils.remove(hh_topo_only)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= init variables from namelist =====')
logging.info('')

try:
    merge_fine_and_coarse = itcl.ltcl_merge
except AttributeError:
    logging.warning('parameter ltcl_merge not set in namelist INPUT_TCLIM'
                    ' -> set to default False instead')
    merge_fine_and_coarse = False

igrid_type = ig.igrid_type
if (igrid_type > 2):
    logging.error(f'igrid_type {igrid_type} does not exist. ' 
                  f'Use 1 (Icon) or 2 (Cosmo) instead!')
    exit(1)

if (igrid_type == 1):
    grid = utils.clean_path('', ig.icon_grid)
    topo_icon = utils.clean_path('', topo_icon)
elif(igrid_type == 2):
    tg = grid_def.CosmoGrid()
    tg.create_grid_description(grid)

raw_data_tclim_coarse  = utils.clean_path(itcl.raw_data_path,
                                          itcl.raw_data_tclim_coarse)
raw_data_tclim_fine  = utils.clean_path(itcl.raw_data_path,
                                        itcl.raw_data_tclim_fine)


# set names of variables in order to skip some intermediate processing steps
# with CDO
if not merge_fine_and_coarse:
    fine_contains_coarse = raw_data_tclim_coarse  # 2 intermediate steps skipped
    tclim_remap = tclim_cdo  # last step skipped

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= initialize metadata ==============')
logging.info('')

lat_meta   = metadata.Lat()
lon_meta   = metadata.Lon()

hsurf_meta  = metadata.HsurfClim()
temp_meta   = metadata.TempClim()

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= CDO: remap to target grid ========')
logging.info('')

#if not merge_fine_and_coarse:
    # calculate yearly mean of T_CL
    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                       '-selname,T_CL',
                       '-addc,273.15', '-yearmonmean', 
                       raw_data_tclim_coarse, coarse_regrid_to_fine)

   # utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
   #                    '-merge', coarse_regrid_to_fine, raw_data_tclim_coarse,
   #                    fine_contains_coarse)

if merge_fine_and_coarse:

    # remap t_clim coarse to t_clim fine and calculate yearly mean
    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,'addc,273.15', '-yearmonmean', 
                       f'-remapdis,{raw_data_tclim_fine}',
                       raw_data_tclim_coarse, coarse_regrid_to_fine)


    # take land point from fine, rest from coarse
    utils.launch_shell('cdo', 
                       'expr, T_CL = ((FR_LAND != 0.0)) ? T_CL : ' 
                       'tem; HSURF; FR_LAND;',
                       '-merge', raw_data_tclim_fine, coarse_regrid_to_fine,
                       fine_contains_coarse)

    # take output from topo_to_buffer
    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                       '-chname,topography_c,HH_TOPO',
                       '-selname,topography_c', topo_icon,
                       hh_topo_only)

# remap to target grid
utils.launch_shell('cdo', '-f', 'nc4', '-P', omp, 
                   'smooth,maxpoints=16',
                   '-setmisstonn', f'-remapdis,{grid}',
                   fine_contains_coarse, tclim_remap)

if merge_fine_and_coarse:

    # topographic correction of temperature
    utils.launch_shell('cdo',
                       'expr, T_CL = ((FR_LAND != 0.0)) ? '
                       'T_CL+0.0065*(HSURF-HH_TOPO) : T_CL; HSURF;',
                       '-merge', tclim_remap, hh_topo_only,
                       tclim_cdo)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= reshape CDO output ===============')
logging.info('')

tclim_nc = nc.Dataset(tclim_cdo, "r")
import IPython; IPython.embed()
if (igrid_type == 1):

    # infer coordinates/dimensions form CDO file
    ie_tot = len(tclim_nc.dimensions['cell'])
    je_tot = 1
    ke_tot = 1
    lon    = np.rad2deg(np.reshape(tclim_nc.variables['clon'][:], 
                                   (ke_tot, je_tot, ie_tot)))
    lat    = np.rad2deg(np.reshape(tclim_nc.variables['clat'][:],
                                   (ke_tot, je_tot, ie_tot)))

    temp  = np.reshape(tclim_nc.variables['T_CL'][:], 
                       (ke_tot, je_tot, ie_tot))

    hsurf  = np.reshape(tclim_nc.variables['HSURF'][:], 
                       (ke_tot, je_tot, ie_tot))

else:

    # infer coordinates/dimensions from tg
    lat, lon = tg.latlon_cosmo_to_latlon_regular()
    ie_tot   = tg.ie_tot
    je_tot   = tg.je_tot
    ke_tot   = tg.ke_tot

    temp  = np.reshape(tclim_nc.variables['T_CL'][:,:], 
                       (ke_tot, je_tot, ie_tot))

    hsurf  = np.reshape(tclim_nc.variables['HSURF'][:,:], 
                       (ke_tot, je_tot, ie_tot))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= write to buffer file =============')
logging.info( '')

# init buffer file
buffer_file = buffer.init_netcdf(itcl.buffer_tclim, je_tot, ie_tot)

# write lat/lon
buffer.write_3d_field(buffer_file, lon, lon_meta)
buffer.write_3d_field(buffer_file, lat, lat_meta)

# write CRU fields
buffer.write_3d_field(buffer_file, hsurf, hsurf_meta)
buffer.write_3d_field(buffer_file, temp, temp_meta)

buffer.close_netcdf(buffer_file)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info( '')
logging.info( '============= extpar_cru_to_buffer done ========')
logging.info( '')
