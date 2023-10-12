#! /usr/bin/env python

import numpy as np
import netCDF4 as nc

#_________________________________________________________________________________________________
#
# read the cdo processed data and retrieve all attributes needed

emiss = nc.Dataset("emiss-icon.nc", "r")
cells = len(emiss.dimensions['cell'])

#_________________________________________________________________________________________________
#
# calculate additional values

# TODO: check if things are fine

# read in emiss field as interpolated by cdo remapicon
emiss_field = np.reshape(emiss.variables['bbemis_longwave'][:,:], (12, 1, 1, cells))

# calculate maxval over 12 month
emiss_field_max = np.amax(np.reshape(emiss.variables['bbemis_longwave'][:,:], (12, 1, 1, cells)), axis=0)

# calculate ratio of emiss/emiss_max per month and set 'missing value' to -1
emiss_field_mrat = np.empty((12, 1, 1, cells), dtype=np.float32)
for t in np.arange(12):
    emiss_field_mrat[t,:,:,:] = np.divide(emiss_field[t,:,:,:], emiss_field_max[:,:,:], where=emiss_field_max[:,:,:] != 0.0)
    emiss_field_mrat[t,:,:,:] = np.where(emiss_field_max[:,:,:] <= 0.0, -1.0, emiss_field_mrat[t,:,:,:])

# print some statistics    
# print("Diagnostics:")

#print("   EMISS")
#for t in np.arange(12):
#    print("   min: {0:8.5f} mean: {1:8.5f} max: {2:8.5f}".format(np.min(emiss_field[t,:,:,:]),
#                                                  np.mean(emiss_field[t,:,:,:]),
#                                                  np.max(emiss_field[t,:,:,:])))
    
#_________________________________________________________________________________________________
#
# create extpar BUFFER

extpar_emiss = nc.Dataset("emiss-icon_BUFFER.nc", "w", format='NETCDF4')

extpar_emiss.createDimension('ie', cells)
extpar_emiss.createDimension('je', 1)
extpar_emiss.createDimension('ke', 1)
extpar_emiss.createDimension('time', None)
extpar_emiss.createDimension('mlev', 1)

extpar_time = extpar_emiss.createVariable('time', np.float32, ('time'))
extpar_time[:] = np.array([11110101, 11110201, 11110301, 11110401, 11110501, 11110601,
                           11110701, 11110801, 11110901, 11111001,  11111101, 11111201],
                          dtype=np.float32)

extpar_mlev = extpar_emiss.createVariable('mlev', np.float32, ('mlev'))
extpar_mlev[:] = np.array([1], dtype=np.float32)

extpar_lon = extpar_emiss.createVariable('lon', np.float32, ('ke', 'je', 'ie'))
extpar_lon.standard_name = 'longitude'
extpar_lon.long_name = 'geographical longitude'
extpar_lon.units = 'degrees_north'
extpar_lon[:,:,:] =  np.rad2deg(np.reshape(emiss.variables['clon'][:], (1,1,cells)))

extpar_lat = extpar_emiss.createVariable('lat', np.float32, ('ke', 'je', 'ie'))
extpar_lat.standard_name = 'latitude'
extpar_lat.long_name = 'geographical latitude'
extpar_lat.units = 'degrees_east'
extpar_lat[:,:,:] = np.rad2deg(np.reshape(emiss.variables['clat'][:], (1,1,cells)))

extpar_emiss_field = extpar_emiss.createVariable('EMISS', np.float32, ('time', 'ke', 'je', 'ie'))
extpar_emiss_field.standard_name = ''
extpar_emiss_field.long_name = 'monthly mean EMISS climatology 2010-2015'
extpar_emiss_field.units = '-'
extpar_emiss_field[:,:,:,:] = emiss_field

extpar_emiss_field_max = extpar_emiss.createVariable('EMISS_MAX', np.float32, ('ke', 'je', 'ie'))
extpar_emiss_field_max.standard_name = ''
extpar_emiss_field_max.long_name = 'MAX of monthly mean EMISS climatology 2010-2015'
extpar_emiss_field_max.units = '-'
extpar_emiss_field_max[:,:,:] = emiss_field_max

extpar_emiss_field_mrat = extpar_emiss.createVariable('EMISS_MRAT', np.float32, ('time', 'ke', 'je', 'ie'))
extpar_emiss_field_mrat.standard_name = ''
extpar_emiss_field_mrat.long_name = 'monthly proportion of actual value/maximum Emissivity'
extpar_emiss_field_mrat.units = '-'
extpar_emiss_field_mrat[:,:,:,:] = emiss_field_mrat


extpar_emiss.close()
