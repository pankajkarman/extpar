#!/usr/bin/env python3 
import logging
import os
import sys
import subprocess
import netCDF4 as nc
import numpy as np

from INPUT_TCLIM import input_tclim as itcl
import INPUT_GRID as ig
import utilities as utils
import grid_def
import buffer
import metadata
import fortran_namelist

# initialize logger
logging.basicConfig(filename='extpar_cru_to_buffer.log',
                    level=logging.INFO,
                    format='%(message)s',
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

# processing steps using CDO
step1_cdo = 'step_1.nc'
step2_cdo = 'step_2.nc'
step3_cdo = 'step_3.nc'
step4_cdo = 'step_4.nc'
step5_cdo = 'step_5.nc'

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= delete files from old runs =======')
logging.info('')

utils.remove(grid)
utils.remove(step1_cdo)
utils.remove(step2_cdo)
utils.remove(step3_cdo)
utils.remove(step4_cdo)
utils.remove(step5_cdo)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= init variables from namelist =====')
logging.info('')

itype_cru = itcl['it_cl_type']
if (itype_cru > 2):
    logging.error(f'itype_cru {itype_cru} does not exist. ' 
                  f'Use 1 (fine) or 2 (coarse) instead!')
    exit(1)

igrid_type = ig.igrid_type
if (igrid_type > 2):
    logging.error(f'igrid_type {igrid_type} does not exist. ' 
                  f'Use 1 (Icon) or 2 (Cosmo) instead!')
    exit(1)

if (igrid_type == 1):
    grid = utils.clean_path('', ig.icon_grid)
elif(igrid_type == 2):
    tg = grid_def.CosmoGrid()
    tg.create_grid_description(grid)

raw_data_tclim_fine  = utils.clean_path(itcl['raw_data_path'],
                                        itcl['raw_data_tclim_fine'])
        
if (itype_cru == 2):
    buffer_topo = fortran_namelist.read_variable_from_namelist('INPUT_ORO', 'orography_output_file')
    buffer_topo = utils.clean_path('', buffer_topo)
    raw_data_tclim_coarse  = utils.clean_path(itcl['raw_data_path'],
                                              itcl['raw_data_tclim_coarse'])

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
logging.info( '')
logging.info( '============= write FORTRAN namelist ===========')
logging.info( '')

input_tclim = fortran_namelist.InputTclim()
fortran_namelist.write_fortran_namelist('input_tclim', itcl,input_tclim)
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= CDO: remap to target grid ========')
logging.info('')


if (itype_cru == 2):

    logging.info('STEP 1: ' 
                 f'convert {raw_data_tclim_coarse} to Kelvin, '
                 f'calculate yearly mean and remap {raw_data_tclim_coarse} '
                 f'to grid of {raw_data_tclim_fine} '
                 f'--> {step1_cdo}')
    logging.info('')

    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,'addc,273.15', '-yearmonmean', 
                       f'-remapdis,{raw_data_tclim_fine}',
                       raw_data_tclim_coarse, step1_cdo)

    logging.info(f'STEP 2: ' 
                 f'take landpoints from {raw_data_tclim_fine}, '
                 f'sea-point from {step1_cdo} --> {step2_cdo}')
    logging.info('')

    utils.launch_shell('cdo', 
                       'expr, T_CL = ((FR_LAND != 0.0)) ? T_CL : ' 
                       'tem; HSURF; FR_LAND;',
                       '-merge', raw_data_tclim_fine, step1_cdo,
                       step2_cdo)

    logging.info(f'STEP 3: ' 
                 f'extract HH_TOPO from {buffer_topo} --> {step3_cdo}')
    logging.info('')

    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                       '-chname,topography_c,HH_TOPO',
                       '-selname,topography_c', buffer_topo,
                       step3_cdo)

    logging.info(f'STEP 4: '
                 f'smooth {step2_cdo} with maxpoint=16, '
                 f'remap {step2_cdo} to target grid --> {step4_cdo}')
    logging.info('')

    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp, 
                       'smooth,maxpoints=16',
                       '-setmisstonn', f'-remapdis,{grid}',
                       step2_cdo, step4_cdo)

    logging.info(f'STEP 5: ' 
                 f'correct T_CL from {step4_cdo} with HH_TOPO from {buffer_topo} '
                 f'--> {step5_cdo}')
    logging.info('')

    # topographic correction of temperature
    utils.launch_shell('cdo',
                       'expr, T_CL = ((FR_LAND != 0.0)) ? '
                       'T_CL+0.0065*(HSURF-HH_TOPO) : T_CL; HSURF;',
                       '-merge', step4_cdo, step3_cdo,
                       step5_cdo)
else:

    logging.info('STEP 1: ' 
                 f'set sea points to missing value in {raw_data_tclim_fine} '
                 f'--> {step1_cdo}')

    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                       '-selname,T_CL', '-setrtomiss,-1,10',
                       raw_data_tclim_fine, step1_cdo)

    logging.info('STEP 2: ' 
                 f'extract HSURF from {raw_data_tclim_fine} '
                 f'--> {step2_cdo}')

    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                       '-selname,HSURF,', raw_data_tclim_fine,
                       step2_cdo)

    logging.info('STEP 3: ' 
                 f'merge {step1_cdo} and {step2_cdo} '
                 f'--> {step3_cdo}')

    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                       '-merge', step1_cdo, step2_cdo,
                       step3_cdo)

    logging.info('STEP 4: ' 
                 f'remap {step3_cdo} to target grid '
                 f'--> {step4_cdo}')

    utils.launch_shell('cdo', '-f', 'nc4', '-P', omp,
                       f'-remapbil,{grid}',step3_cdo,
                       step4_cdo)

    # missing step 5, swap names instead
    step5_cdo = step4_cdo
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
logging.info('')
logging.info('============= reshape CDO output ===============')
logging.info('')

tclim_nc = nc.Dataset(step5_cdo, "r")

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
buffer_file = buffer.init_netcdf(itcl['buffer_tclim'], je_tot, ie_tot)

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
