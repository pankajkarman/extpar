import numpy as np
from netCDF4 import Dataset

ncfile = Dataset('/extpar_raw_data/ASTER_orig_T001.nc','r')
lon = ncfile.variables['lon'][:]
lat = ncfile.variables['lat'][:]
nx = lon.size
ny = lat.size
dimtype = lon.dtype
nxp2 = nx + 2
nyp2 = ny + 2

for i in range(1,13):
   cstr0 = format(i,'03d')
   if i == 1:
      cstr1 = format(12,'03d')
      cstr2 = format( 2,'03d')
      cstr3 = format(24,'03d')
      cstr4 = format(13,'03d')
      cstr5 = format(14,'03d')
   elif i == 12:
      cstr1 = format(11,'03d')
      cstr2 = format( 1,'03d')
      cstr3 = format(23,'03d')
      cstr4 = format(24,'03d')
      cstr5 = format(13,'03d')
   else:
      cstr1 = format(i-1,'03d')
      cstr2 = format(i+1,'03d')
      cstr3 = format(i+11,'03d')
      cstr4 = format(i+12,'03d')
      cstr5 = format(i+13,'03d')
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr0 + '.nc'
   print 'process file: ', filename
   ncfile = Dataset(filename,'r')
   lon =  ncfile.variables['lon'][:]
   lat = ncfile.variables['lat'][:]
   dlon = (lon[nx-1]-lon[0])/nx 
   dlat = (lat[ny-1]-lat[0])/ny 
   lonnew = np.ndarray(shape=(nxp2),dtype=dimtype)
   latnew = np.ndarray(shape=(nyp2),dtype=dimtype)
   lonnew[1:nx+1] = lon[:]
   lonnew[0] = lonnew[1] - dlon
   lonnew[nx+1] = lonnew[nx] + dlon
   latnew[1:ny+1] = lat[:]
   latnew[0] = latnew[1] - dlat
   latnew[ny+1] = latnew[ny] + dlat
   Z = ncfile.variables['Z'][:,:]
   ztype = Z.dtype
   Znew = np.ndarray(shape=(nyp2,nxp2),dtype=ztype)
   Znew[1:ny+1,1:nx+1] = Z[:,:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr1 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][:,nx-1]
   Znew[1:ny+1,0] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr2 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][:,0]
   Znew[1:ny+1,nx+1] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr3 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][0,nx-1]
   Znew[ny+1,0] = Z
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr4 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][0,:]
   Znew[ny+1,1:nx+1] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr5 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][0,0]
   Znew[ny+1,nx+1] = Z
   Znew[0,:] = Znew[1,:]
   ncfile.close()

   # write newfile
   filename = '/buffer/extpar/aster/ASTER_exp_T' + cstr0 + '.nc'
   ncfile = Dataset(filename,'w')
   ncfile.createDimension('lon',nxp2)
   ncfile.createDimension('lat',nyp2)
   lon = ncfile.createVariable('lon',dimtype,('lon',))
   lat = ncfile.createVariable('lat',dimtype,('lat',)) 
   lon.long_name = 'Longitude'
   lon.units = 'degrees_east'    
   lat.long_name = 'Latitude'
   lon.units = 'degrees_north'    
   Z = ncfile.createVariable('Z',ztype,('lat','lon'))
   lon[:] = lonnew
   lat[:] = latnew
   Z[:,:] = Znew 
         


for i in range(229,241):
   cstr0 = format(i,'03d')
   if i == 229:
      cstr1 = format(240,'03d')
      cstr2 = format(230,'03d')
      cstr3 = format(228,'03d')
      cstr4 = format(217,'03d')
      cstr5 = format(218,'03d')
   elif i == 240:
      cstr1 = format(239,'03d')
      cstr2 = format(229,'03d')
      cstr3 = format(227,'03d')
      cstr4 = format(228,'03d')
      cstr5 = format(217,'03d')
   else:
      cstr1 = format(i-1,'03d')
      cstr2 = format(i+1,'03d')
      cstr3 = format(i-13,'03d')
      cstr4 = format(i-12,'03d')
      cstr5 = format(i-11,'03d')
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr0 + '.nc'
   print 'process file: ', filename
   ncfile = Dataset(filename,'r')
   lon =  ncfile.variables['lon'][:]
   lat = ncfile.variables['lat'][:]
   dlon = (lon[nx-1]-lon[0])/nx 
   dlat = (lat[ny-1]-lat[0])/ny 
   lonnew = np.ndarray(shape=(nxp2),dtype=dimtype)
   latnew = np.ndarray(shape=(nyp2),dtype=dimtype)
   lonnew[1:nx+1] = lon[:]
   lonnew[0] = lonnew[1] - dlon
   lonnew[nx+1] = lonnew[nx] + dlon
   latnew[1:ny+1] = lat[:]
   latnew[0] = latnew[1] - dlat
   latnew[ny+1] = latnew[ny] + dlat
   Z = ncfile.variables['Z'][:,:]
   ztype = Z.dtype
   Znew = np.ndarray(shape=(nyp2,nxp2),dtype=ztype)
   Znew[1:ny+1,1:nx+1] = Z[:,:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr1 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][:,nx-1]
   Znew[1:ny+1,0] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr2 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][:,0]
   Znew[1:ny+1,nx+1] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr3 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][ny-1,nx-1]
   Znew[0,0] = Z
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr4 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][ny-1,:]
   Znew[0,1:nx+1] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr5 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][ny-1,0]
   Znew[0,nx+1] = Z
   ncfile.close()
   Znew[ny+1,:] = Znew[ny,:]

   # write newfile
   filename = '/buffer/extpar/aster/ASTER_exp_T' + cstr0 + '.nc'
   ncfile = Dataset(filename,'w')
   ncfile.createDimension('lon',nxp2)
   ncfile.createDimension('lat',nyp2)
   lon = ncfile.createVariable('lon',dimtype,('lon',))
   lat = ncfile.createVariable('lat',dimtype,('lat',)) 
   lon.long_name = 'Longitude'
   lon.units = 'degrees_east'    
   lat.long_name = 'Latitude'
   lon.units = 'degrees_north'    
   Z = ncfile.createVariable('Z',ztype,('lat','lon'))
   lon[:] = lonnew
   lat[:] = latnew
   Z[:,:] = Znew 
         
for i in range(13,229):
   cstr0 = format(i,'03d')
   if i%12 == 1:
      cstr1 = format(i+12,'03d')
      cstr2 = format(i+1,'03d')
      cstr3 = format(i-1,'03d')
      cstr4 = format(i-12,'03d')
      cstr5 = format(i-11,'03d')
      cstr6 = format(i+23,'03d')
      cstr7 = format(i+12,'03d')
      cstr8 = format(i+13,'03d')
   elif i%12 == 0:
      cstr1 = format(i-1,'03d')
      cstr2 = format(i-11,'03d')
      cstr3 = format(i-13,'03d')
      cstr4 = format(i-12,'03d')
      cstr5 = format(i-23,'03d')
      cstr6 = format(i+11,'03d')
      cstr7 = format(i+12,'03d')
      cstr8 = format(i+1,'03d')
   else:
      cstr1 = format(i-1,'03d')
      cstr2 = format(i+1,'03d')
      cstr3 = format(i-13,'03d')
      cstr4 = format(i-12,'03d')
      cstr5 = format(i-11,'03d')
      cstr6 = format(i+11,'03d')
      cstr7 = format(i+12,'03d')
      cstr8 = format(i+13,'03d')
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr0 + '.nc'
   print 'process file: ', filename
   ncfile = Dataset(filename,'r')
   lon =  ncfile.variables['lon'][:]
   lat = ncfile.variables['lat'][:]
   dlon = (lon[nx-1]-lon[0])/nx 
   dlat = (lat[ny-1]-lat[0])/ny 
   lonnew = np.ndarray(shape=(nxp2),dtype=dimtype)
   latnew = np.ndarray(shape=(nyp2),dtype=dimtype)
   lonnew[1:nx+1] = lon[:]
   lonnew[0] = lonnew[1] - dlon
   lonnew[nx+1] = lonnew[nx] + dlon
   latnew[1:ny+1] = lat[:]
   latnew[0] = latnew[1] - dlat
   latnew[ny+1] = latnew[ny] + dlat
   Z = ncfile.variables['Z'][:,:]
   ztype = Z.dtype
   Znew = np.ndarray(shape=(nyp2,nxp2),dtype=ztype)
   Znew[1:ny+1,1:nx+1] = Z[:,:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr1 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][:,nx-1]
   Znew [1:ny+1,0] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr2 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][:,0]
   Znew[1:ny+1,nx+1] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr3 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][ny-1,nx-1]
   Znew[0,0] = Z
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr4 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][ny-1,:]
   Znew[0,1:nx+1] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr5 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][ny-1,0]
   Znew [0,nx+1] = Z
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr6 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][0,nx-1]
   Znew[ny+1,0] = Z
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr7 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][0,:]
   Znew[ny+1,1:nx+1] = Z[:]
   ncfile.close()
   filename = '/extpar_raw_data/ASTER_orig_T' + cstr8 + '.nc'
   ncfile = Dataset(filename,'r')
   Z = ncfile.variables['Z'][0,0]
   Znew[ny+1,nx+1] = Z

   # write newfile
   filename = '/buffer/extpar/aster/ASTER_exp_T' + cstr0 + '.nc'
   ncfile = Dataset(filename,'w')
   ncfile.createDimension('lon',nxp2)
   ncfile.createDimension('lat',nyp2)
   lon = ncfile.createVariable('lon',dimtype,('lon',))
   lat = ncfile.createVariable('lat',dimtype,('lat',)) 
   lon.long_name = 'Longitude'
   lon.units = 'degrees_east'    
   lat.long_name = 'Latitude'
   lon.units = 'degrees_north'    
   Z = ncfile.createVariable('Z',ztype,('lat','lon'))
   lon[:] = lonnew
   lat[:] = latnew
   Z[:,:] = Znew 
         



    
