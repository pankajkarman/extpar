import netCDF4 as nc
import numpy as np
import logging

def write_coordinates(buffer,lat, lon):
    
    extpar_lon  = buffer.createVariable('lon', np.float32, ('ke', 'je', 'ie',))
    extpar_lon.standard_name = 'longitude'
    extpar_lon.long_name = 'geographical longitude'
    extpar_lon.units = 'degrees_east'
    extpar_lon[0,:,:] = lon

    extpar_lat  = buffer.createVariable('lat', np.float32, ('ke', 'je', 'ie',))
    extpar_lat.standard_name = 'latitude'
    extpar_lat.long_name = 'geographical latitude'
    extpar_lat.units = 'degrees_north'
    extpar_lat[0,:,:] = lat 
    
def prepare_netcdf(buffer_name):

    # create extpar BUFFER
    buffer = nc.Dataset(buffer_name, "w", format='NETCDF4')

    extpar_alb.createDimension('ie',tg.ie_tot )
    extpar_alb.createDimension('je', tg.je_tot)
    extpar_alb.createDimension('ke', 1)
    extpar_alb.createDimension('time', None)
    extpar_alb.createDimension('mlev', 1)

    extpar_time = extpar_alb.createVariable('time', np.float32, ('time',))
    extpar_time[:] = np.array([11110101, 11110201, 11110301, 11110401, 11110501, 11110601,
                               11110701, 11110801, 11110901, 11111001,  11111101, 11111201],
                              dtype=np.float32)

    extpar_mlev = extpar_alb.createVariable('mlev', np.float32, ('mlev',))
    extpar_mlev[:] = np.array([1], dtype=np.float32)

def write_4d_fields(buffer,4d_fields, meta):

    for idx in range(meta.number_of_fields):

        netcdf_var = buffer.createVariable(meta.fields[idx].name, \
                                     meta.type, \
                                     (meta.dim[0],meta.dim[1],meta.dim[2]))

        netcdf_var.standard_name = meta.fields[idx].long
        netcdf_var.long_name = meta.fields[idx].long
        netcdf_var.units = meta.units
        netcdf_var[:,:,:,:] = 4d_field[idx]
                                     
                                    
def write_cosmo(buffer_name, var_meta, tg):

    # read the cdo processed data and retrieve all attributes needed
    alb = nc.Dataset("alb-dis.nc", "r")
    albni = nc.Dataset("alnid-dis.nc", "r")
    albuv = nc.Dataset("aluvd-dis.nc", "r")

    
    lat,lon = tg.latlon_cosmo_to_latlon_regular()

    create_latlon(lat_meta, lon_meta)


    extpar_alb_dif12 = extpar_alb.createVariable('ALB_DIF12', np.float32, ('time', 'ke', 'je', 'ie',))
    extpar_alb_dif12.standard_name = 'Albedo'
    extpar_alb_dif12.long_name = 'Albedo'
    extpar_alb_dif12.units = '%'
    extpar_alb_dif12[:,:,:,:] = np.reshape(alb.variables['al'][:,:,:],(12,1,tg.je_tot,tg.ie_tot))

    extpar_alnid12 = extpar_alb.createVariable('ALNID12', np.float32, ('time', 'ke', 'je', 'ie',))
    extpar_alnid12.standard_name = 'NI_Albedo'
    extpar_alnid12.long_name ='NI_Albedo'
    extpar_alnid12.units = '%'
    extpar_alnid12[:,:,:,:] = np.reshape(albni.variables['alnid'][:,:,:], (12,1,tg.je_tot,tg.ie_tot))

    extpar_aluvd12 = extpar_alb.createVariable('ALUVD12', np.float32, ('time', 'ke', 'je', 'ie',))
    extpar_aluvd12.standard_name = 'UV_Albedo'
    extpar_aluvd12.long_name = 'UV_Albedo'
    extpar_aluvd12.units = '%'
    extpar_aluvd12[:,:,:,:] = np.reshape(albuv.variables['aluvd'][:,:,:], (12,1,tg.je_tot,tg.ie_tot))

    extpar_alb.close()

    logging.info(f'{buffer_name} written')

def write_icon(buffer_file):

    # read the cdo processed data and retrieve all attributes needed


    alb = nc.Dataset("alb-dis.nc", "r")
    cells = len(alb.dimensions['cell'])

    albni = nc.Dataset("alnid-dis.nc", "r")
    albuv = nc.Dataset("aluvd-dis.nc", "r")

    prepare_netcdf(buffer_file)

    lon = np.rad2deg(np.reshape(alb.variables['clon'][:], (1,1,cells)))
    lat  = np.rad2deg(np.reshape(alb.variables['clat'][:], (1,1,cells)))

    write_coordinates(lat,lon)

    extpar_alb_dif12 = extpar_alb.createVariable('ALB_DIF12', np.float32, ('time', 'ke', 'je', 'ie',))
    extpar_alb_dif12.standard_name = 'Albedo'
    extpar_alb_dif12.long_name = 'Albedo'
    extpar_alb_dif12.units = '%'
    extpar_alb_dif12[:,:,:,:] = np.reshape(alb.variables['al'][:,:], (12, 1, 1, cells))

    extpar_alnid12 = extpar_alb.createVariable('ALNID12', np.float32, ('time', 'ke', 'je', 'ie',))
    extpar_alnid12.standard_name = 'NI_Albedo'
    extpar_alnid12.long_name ='NI_Albedo'
    extpar_alnid12.units = '%'
    extpar_alnid12[:,:,:,:] = np.reshape(albni.variables['alnid'][:,:], (12, 1, 1, cells))

    extpar_aluvd12 = extpar_alb.createVariable('ALUVD12', np.float32, ('time', 'ke', 'je', 'ie',))
    extpar_aluvd12.standard_name = 'UV_Albedo'
    extpar_aluvd12.long_name = 'UV_Albedo'
    extpar_aluvd12.units = '%'
    extpar_aluvd12[:,:,:,:] = np.reshape(albuv.variables['aluvd'][:,:], (12, 1, 1, cells))

    extpar_alb.close()

