;+
; :Description:
;   This code read the ASTER GDEM Version 2 GeoTiff Tiles and
;   composites them for a given output region and resolution.
;
;   For more information on the ASTER GDEM2 please see:
;
;   http://srtm.csi.cgiar.org/
;
;   For downloading the ASTER GDEM2 data and bypassing their web
;   interface please go to:
;
;   ftp://srtm.csi.cgiar.org/SRTM_V41/SRTM_Data_GeoTiff/ wrong
;
;   ftp://xftp.jrc.it/pub/srtmV4/tiff/ wrong
;
; :Categories:
;   Geographic Processing
;
; :Params:
;   asterdir : in, required, type=string 
;     absolute path (directory) to the ASTER GDEM2 GeoTiff files
;   lonmin: in, required, type="float or double"
;     western edge of requested domain (degrees E)
;   latmin: in, required, type="float or double"
;      southern edge of requested domain (degrees N)
;   lonmax: in, required, type="float or double"
;      eastern edge of requested domain (degrees E)
;   latmax: in, required, type="float or double"
;      northern edge of requested domain (degrees N)
;   dlon: in, required, type="float or double"
;      requested resolution west-east (degrees)
;   dlat: in, required, type="float or double"
;      reqested resolution south-north (degrees)
;   topo: out, required, type="intarr"
;      topography on chosen grid (2D grid)
;
; :Keywords:
;   out_lonmin: out, optional, type="float or double"
;     western edge of extracted domain (degrees E)
;   out_latmin: out, optional, type="float or double"
;      southern edge of extracted domain (degrees N)
;   out_lonmax: out, optional, type="float or double"
;      eastern edge of extracted domain (degrees E)
;   out_latmax: out, optional, type="float or double"
;      northern edge of extracted domain (degrees N)
;   out_dlon: out, optional, type="float or double"
;      output resolution west-east (degrees)
;   out_dlat: out, optional, type="float or double"
;      output resolution south-north (degrees)
;      
;
; :Uses:
;   rebin_new
;
; :Requires:
;   IDL 7.1
;
; :Examples:
;
; :History:
;   2010/11/22 Reto Stockli (MeteoSwiss)
;
;   2011/03/18 : added standard idldoc-compatible rst-style header
;
;   2012/07/09 : added cgiar_lon/lat/min/max keywords which give the
;   actual extracted domain (that may differ from the requrested
;   domain by +/- grid size of the CGIAR SRTM
;
; :Author:
;   Reto Stockli (MeteoSwiss)
;    
; :Copyright:
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see http://www.gnu.org/licenses/.
;
;-
PRO read_aster, asterdir, lonmin, latmin, lonmax, latmax, dlon, dlat, topo, $
                    out_lonmin=out_lonmin,out_lonmax=out_lonmax, $
                    out_latmin=out_latmin,out_latmax=out_latmax, $
                    out_dlon=out_dlon, out_dlat=out_dlat

  ;; how do we fill the missing areas (ocean?) 
  ;; topo_fillvalue = 0
    topo_fillvalue = -9999  ;; -> Ocean bodies are already set as 0 in the
                            ;; ASTER data

  ;; output dimensions
  nlon = round((lonmax - lonmin)/dlon)
  nlat = round((latmax - latmin)/dlat)

  ;; ASTER GDEM2 files: geographical file structure
  aster_nx = 3601L             ; number of columns in GeoTIFF
  aster_ny = 3601L             ; number of lines in GeoTIFF

  aster_nlon = 1L              ; longitude spread for GeoTIFF
  aster_nlat = 1L              ; latitude spread for GeoTIFF
  
  aster_lon0 = -180.           ; starting longitude of input file x index
  aster_lat0 = 60.            ; starting latitude of input file y index
  aster_lon1 = 180.            ; ending longitude of input file x index
  aster_lat1 = -60.            ; ending latitude of input file x index

  ;; derived ASTER geographical parameters
  aster_lonlen = round(aster_nx/aster_nlon) ; number of ASTER columns per degree lon
  aster_latlen = round(aster_ny/aster_nlat) ; number of ASTER lines per degree lat
  aster_dlon = double(aster_nlon)/double(aster_nx) ; longitude spacing of ASTER dataset pixels
  aster_dlat = double(aster_nlat)/double(aster_ny) ; latitude spacing of ASTER dataset pixels

  ;; western longitudes for each input file x index
  aster_lonind0 = lindgen(round((aster_lon1-aster_lon0)/aster_nlon))*aster_nlon*aster_lonlen + $
                  round(aster_lon0*double(aster_lonlen))
  ;; eastern longitudes for each input file x index
  aster_lonind1 = lindgen(round((aster_lon1-aster_lon0)/aster_nlon))*aster_nlon*aster_lonlen + $
                  round(aster_lon0*double(aster_lonlen)) + aster_nlon*aster_lonlen
  ;; southern latitude for each input file y index  
  aster_latind0 = - lindgen(round((aster_lat0-aster_lat1)/aster_nlat))*aster_nlat*aster_latlen + $
                  round(aster_lat0*double(aster_latlen)) - aster_nlat*aster_latlen 
  ;; northern latitude for each input file y index
  aster_latind1 = - lindgen(round((aster_lat0-aster_lat1)/aster_nlat))*aster_nlat*aster_latlen + $
                  round(aster_lat0*double(aster_latlen)) 

  ;; ASTER missing data value (Version 2 of ASTER GDEM)
  aster_nodata = -9999

  lonind0 = round(lonmin*double(aster_lonlen))
  lonind1 = round(lonmax*double(aster_lonlen))
  latind0 = round(latmin*double(aster_latlen))
  latind1 = round(latmax*double(aster_latlen))

  nlon_aster = lonind1 - lonind0
  nlat_aster = latind1 - latind0
  
  print,nlon_aster,nlat_aster
  
  aster = intarr(nlon_aster,nlat_aster)

  out_lonmin=float(lonind0)*aster_dlon
  out_lonmax=float(lonind1)*aster_dlon
  out_latmin=float(latind0)*aster_dlat
  out_latmax=float(latind1)*aster_dlat

  print,'Reading ASTER GDEM2 with bounds: ',out_lonmin,'-',out_lonmax,'/', $
        out_latmin,'-',out_latmax
  print,'We expect : ',nlon_aster,'x',nlat_aster,' grid points on input'
  
  print,'searching for: ',lonind0,lonind1,latind0,latind1

  cx0 = where((lonind0 GE aster_lonind0) AND (lonind0 LT aster_lonind1),x0count)+1
  cy1 = where((latind0 GE aster_latind0) AND (latind0 LT aster_latind1),y1count)+1
  cx1 = where((lonind1 GT aster_lonind0) AND (lonind1 LE aster_lonind1),x1count)+1
  cy0 = where((latind1 GT aster_latind0) AND (latind1 LE aster_latind1),y0count)+1
   
   IF (x0count EQ 0L) OR (x1count EQ 0L) OR (y0count EQ 0L) OR (y1count EQ 0L) THEN BEGIN
     print,"ASTER GDEM2 Bounds reached. Stopping."
     stop
     ENDIF
     
     IF (latmax EQ FLOAT(FIX(latmax))) THEN BEGIN
       name_lat = latmax-1
    ENDIF ELSE BEGIN
     name_lat = latmax
  ENDELSE

     name_lon = lonmin

     FOR cy=cy0[0],cy1[0] DO BEGIN

        IF (name_lat LE -1L) THEN BEGIN              ;neu
           name_lat = ABS(name_lat)                  ;neu
           scy = string(name_lat,format='(I2.2)')    ;neu
           latitude_part = 'ASTGTM2_S'+scy           ;neu
           name_lat = -name_lat                       ;neu
        ENDIF ELSE BEGIN                             ;neu
           scy = string(name_lat,format='(I2.2)')    ;neu
           latitude_part = 'ASTGTM2_N'+scy           ;neu
        ENDELSE                                      ;neu

       FOR cx=cx0[0],cx1[0] DO BEGIN

        IF (name_lon LE -1L) THEN BEGIN
          name_lon = ABS(name_lon)
          scx = string(name_lon,format='(I3.3)')
          longitude_part = 'W'+scx
          asterfile = latitude_part+longitude_part+'/'+latitude_part+longitude_part+'_dem' ;neu
          name_lon = -name_lon
       ENDIF ELSE BEGIN
         scx = string(name_lon,format='(I3.3)')
         longitude_part = 'E'+scx
         asterfile = latitude_part+longitude_part+'/'+latitude_part+longitude_part+'_dem' ;neu      
      ENDELSE 
        infile = file_search(asterdir+asterfile+'.tif',count=count)
        IF count EQ 0L THEN BEGIN
           infile = file_search(asterdir+asterfile+'.tif.gz',count=count)
           
           IF count GT 0L THEN BEGIN
              spawn,'gunzip -f '+asterdir+asterfile+'.tif.gz'
           ENDIF
        ENDIF
        
        IF count EQ 0L THEN BEGIN
           infile = file_search(asterdir+asterfile+'.zip',count=count)
           
           IF count GT 0L THEN BEGIN
              spawn,'cd '+asterdir+'; unzip -o '+asterfile+'.zip'
              spawn,'\rm -f '+asterdir+asterfile+'.zip'
           ENDIF
        ENDIF

        infile = file_search(asterdir+asterfile+'.tif',count=count)
        IF count GT 0L THEN BEGIN
           
           print,'Reading: ',asterdir+asterfile+'.tif'
                  
           ;; cut desired sub-area
           x0 = lonind0 - aster_lonind0[cx-1]
           x1 = lonind1 - aster_lonind0[cx-1] - 1
           y0 = latind0 - aster_latind0[cy-1]
           y1 = latind1 - aster_latind0[cy-1] - 1
           
           dx0 = (x0>0) - x0
           dx1 = x1 - (x1<(aster_nx-1))
           dy0 = (y0>0) - y0
           dy1 = y1 - (y1<(aster_ny-1))
           
           x0 = x0 + dx0
           x1 = x1 - dx1
           y0 = y0 + dy0
           y1 = y1 - dy1
           
           nx = x1-x0+1
           ny = y1-y0+1
           
           ;; geotiff is saved with LL corner = 1/1 
           y0 = aster_ny - 1 - y0
           y1 = aster_ny - 1 - y1
           
;           IF (nx GT 0) AND (ny GT 0) THEN BEGIN
           aster[dx0:dx0+nx-1,dy0:dy0+ny-1] =  $
              reverse(read_tiff(asterdir+asterfile+'.tif', sub_rect =[x0,y1,x1-x0+1,y0-y1+1]),2)
;           ENDIF
        ENDIF ELSE BEGIN
           print,'Not Found: ',asterdir+asterfile+'.tif'
        ENDELSE
       
      name_lon = name_lon+1
   ENDFOR
    name_lon = lonmin  
    name_lat = name_lat-1
  ENDFOR

  ;; gaps in SRTM are extensive water bodies, fill with standard fill value
  ;; sometimes we have a 255 fill values in deep ocean
  ;; areas. Andy tells me that it happens with GeoTiff if no
  ;; values within the file are above the byte range.
  aster_nodata2 = 255          
  ystep = 1000L
  nty = ((nlat_aster-1L)/ystep)>1L
  IF max(aster) EQ aster_nodata2 THEN BEGIN
     FOR ty=0L,nty-1L DO BEGIN
        y0=nlat_aster*ty/nty
        y1=nlat_aster*(ty+1L)/nty-1
        aster[*,y0:y1]=aster[*,y0:y1]*((aster[*,y0:y1] NE aster_nodata) AND (aster[*,y0:y1] NE aster_nodata2))  + $
                      topo_fillvalue*((aster[*,y0:y1] EQ aster_nodata) OR (aster[*,y0:y1] EQ aster_nodata2))
     ENDFOR
  ENDIF ELSE BEGIN
     FOR ty=0L,nty-1L DO BEGIN
        y0=nlat_aster*ty/nty
        y1=nlat_aster*(ty+1L)/nty-1
        aster[*,y0:y1]=aster[*,y0:y1]*(aster[*,y0:y1] NE aster_nodata) + $
                      topo_fillvalue*(aster[*,y0:y1] EQ aster_nodata)
     ENDFOR
  ENDELSE

  ;; rescale ASTER if needed
  out_dlon = dlon
  out_dlat = dlat
  IF (nlon NE nlon_aster) OR (nlat NE nlat_aster) THEN BEGIN

     IF ((nlon_aster EQ (nlon_aster/nlon)*nlon) AND (nlat_aster EQ (nlat_aster/nlat)*nlat)) OR $
        ((float(nlon)/float(nlon_aster) EQ float(nlon/nlon_aster)) AND $
         (float(nlat)/float(nlat_aster) EQ float(nlat/nlat_aster))) THEN BEGIN
        ;; simple rebinning -- fast
        topo = rebin(aster,nlon,nlat) 
     ENDIF ELSE BEGIN
        ;; new dimensions are not integer factor of old dimensions
        fac1 = float(nlon)/float(nlon_aster)
        fac2 = float(nlat)/float(nlat_aster)
        IF ((fac1 GT 0.95) AND (fac1 LT 1.05)) AND ((fac2 GT 0.95) AND (fac2 LT 1.05)) THEN BEGIN
           ;; do not rebin since dimensions almost match
           out_dlon = aster_dlon
           out_dlat = aster_dlat
           topo = aster
        ENDIF ELSE BEGIN
           ;; use new rebinning for non-integer dimensional scaling
           topo = fix(rebin_new(aster,nlon,nlat))
        ENDELSE
     ENDELSE

  ENDIF ELSE BEGIN
     ;; no regridding
     topo = aster

  ENDELSE

END
