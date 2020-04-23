#! /usr/bin/env python

import numpy as np
import netCDF4 as nc

#_________________________________________________________________________________________________
#
# read the cdo processed data and retrieve all attributes needed

ERA_I = nc.Dataset("ei_an1986-2015_0026_R03B07_G.nc", "r")
cells = len(ERA_I.dimensions['ncells'])

#_________________________________________________________________________________________________
#
# read in ndvi field as interpolated by cdo remapycon
tsea_field = np.reshape(ERA_I.variables['T_SEA'][:,:], (12, 1, 1, cells))
wsnow_field = np.reshape(ERA_I.variables['W_SNOW'][:,:], (12, 1, 1, cells))


# create extpar BUFFER
extpar_era_i = nc.Dataset("ei_an1986-2015_BUFFER.nc", "w", format='NETCDF4')

extpar_era_i.createDimension('ie', cells)
extpar_era_i.createDimension('je', 1)
extpar_era_i.createDimension('ke', 1)
extpar_era_i.createDimension('time', None)

extpar_time = extpar_era_i.createVariable('time', np.float32, ('time'))
extpar_time[:] = np.array([11110101, 11110201, 11110301, 11110401, 11110501, 11110601,
                           11110701, 11110801, 11110901, 11111001,  11111101, 11111201],
                          dtype=np.float32)

extpar_tsea_field = extpar_era_i.createVariable('T_SEA', np.float32, ('time', 'ke', 'je', 'ie'))
extpar_tsea_field.standard_name = ''
extpar_tsea_field.long_name = 'Water temperature'
extpar_tsea_field.units = 'K'
extpar_tsea_field[:,:,:,:] = tsea_field

extpar_wsnow_field = extpar_era_i.createVariable('W_SNOW', np.float32, ('time', 'ke', 'je', 'ie'))
extpar_wsnow_field.standard_name = ''
extpar_wsnow_field.long_name = 'Snow depth water equivalent'
extpar_wsnow_field.units = 'kg m-2'
extpar_wsnow_field[:,:,:,:] = wsnow_field


extpar_era_i.close()
