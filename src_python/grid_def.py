import INPUT_GRID as ig
import numpy as np
import math

class CosmoGrid:

    def __init__(self):
        self.pollon=ig.pollon
        self.pollat=ig.pollat
        self.startlon_tot=ig.startlon_tot
        self.startlat_tot=ig.startlat_tot
        self.dlon=ig.dlon
        self.dlat=ig.dlat
        self.ie_tot=ig.ie_tot
        self.je_tot=ig.je_tot
        self.gridsize=ig.ie_tot * ig.je_tot


    def create_grid_description(self,name):
        '''
        create grid description for cdo
        '''
        with open(name,'w') as f:
        
            f.write(f'gridtype  = projection\n')
            f.write(f'gridsize  = {self.gridsize}\n')
            f.write(f'xsize     = {self.ie_tot}\n')
            f.write(f'ysize     = {self.je_tot}\n')
            f.write(f'xinc      = {self.dlon}\n')
            f.write(f'yinc      = {self.dlat}\n')
            f.write(f'xfirst    = {self.startlon_tot}\n')
            f.write(f'yfirst    = {self.startlat_tot}\n')

            f.write(f'grid_mapping_name = rotated_latitude_longitude\n')
            f.write(f'grid_north_pole_longitude = {self.pollon}\n') 
            f.write(f'grid_north_pole_latitude  = {self.pollat}\n') 
    
    def lon_rot(self):
       lon = np.empty(self.ie_tot)

       for i in range(self.ie_tot):
           lon[i] = self.startlon_tot + i*self.dlon
           print('lon')
           print(lon[i])

       return lon


    def lat_rot(self):
       lat = np.empty([self.je_tot])

       for j in range(self.je_tot):
           lat[j] = self.startlat_tot + j*self.dlat
           print('lat')
           print(lat[j])

       return lat

    def latlon_cosmo_to_latlon_regular(self):
        '''
        lon and lat geo are stored in j,i order for netCDF
        '''

        lat_cosmo=self.lat_rot()
        lon_cosmo=self.lon_rot()

        lat_reg=np.empty([self.je_tot,self.ie_tot])
        lon_reg=np.empty([self.je_tot,self.ie_tot])

        for j in range(self.je_tot):
            for i in range(self.ie_tot):

               lon_reg[j,i] = self.rlarot2rla(lat_cosmo[j], lon_cosmo[i], self.pollat, self.pollon)
               lat_reg[j,i] = self.phirot2phi(lat_cosmo[j], lon_cosmo[i], self.pollat, self.pollon)
        
        return lat_reg,lon_reg

    def rlarot2rla(self,phirot, rlarot, polphi, pollam): 


        zrpi18 = 57.29577951308232
        zpir18 = 0.017453292519943295

        zsinpol = math.sin (zpir18 * polphi)
        zcospol = math.cos (zpir18 * polphi)

        zlampol = zpir18 * pollam
        zphis   = zpir18 * phirot

        if (rlarot > 180.0): 
          zrlas = rlarot - 360.0
        else:
            zrlas = rlarot

        zrlas   = zpir18 * zrlas

        zarg1   = math.sin (zlampol) * (-zsinpol * math.cos(zrlas) * math.cos(zphis) + \
                  zcospol * math.sin(zphis)) - math.cos(zlampol) * math.sin(zrlas) * math.cos(zphis)

        zarg2   = math.cos(zlampol) * (-zsinpol * math.cos(zrlas) * math.cos(zphis) +  \
                  zcospol * math.sin(zphis)) + math.sin(zlampol) * math.sin(zrlas) * math.cos(zphis)

        if (zarg2 == 0.0): 
            zarg2 = 1.0E-20

        rla = zrpi18 * math.atan2(zarg1,zarg2)
        
        return rla


    def phirot2phi(self,phirot,rlarot,polphi,pollam): 

        zrpi18 = 57.29577951308232
        zpir18 = 0.017453292519943295

        zsinpol     = math.sin (zpir18 * polphi)
        zcospol     = math.cos (zpir18 * polphi)

        zphis       = zpir18 * phirot

        if (rlarot > 180.0):
            zrlas = rlarot - 360.0
        else:
            zrlas = rlarot

        zrlas       = zpir18 * zrlas

        zarg  = zcospol * math.cos (zphis) * math.cos (zrlas) + zsinpol * math.sin (zphis)

        phi  = zrpi18 * math.asin (zarg)

        return phi
