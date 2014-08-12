;+
; :Description:
;   This script creates a subgrid - topography distribution 
;   ((flag: histo=1) or a mean composite topography map (histo=0) from the 90 m
;   CGIAR SRTM v4.1 and 1 km GTOPO30 topography at a chosen output resolution.
;
;   Note: The RADARSAT-II Antarctica support was removed. We'll
;   wait for either the IceSat GLAS or the ASTER DEM for polar areas
;   and use GTOPO30 in the meantime.
;
;   Dependencies: 
;   - CGIAR SRTM Dataset in GeoTIFF format. Download from: ftp://srtm.csi.cgiar.org/SRTM_v41/SRTM_Data_GeoTIFF/
;   - GTOPO30 Dataset in Binary DEM format. Download from: ftp://edcftp.cr.usgs.gov/pub/data/gtopo30/global/
; 
; :Categories:
;   Geographical Data Processing
;
; :Params:
;   srtmdir: in, required, type=string
;      directory where CGIAR-SRTM GeoTiff files are located (include
;      trailing slash)
;   gtopodir: in, required, type=string
;      directory where GTOPO30 binary DEM files are located (include
;      trailing slash)
;   asterdir: in, required, type=string
;      directory where ASTER GDEM GeoTiff files are located (include
;      trailing slash)
;   outdir: in, required, type=string
;      directory where output NetCDF are written to(include
;      trailing slash)
;   region : in, required, type=string
;      name of the area of interest (becomes output file name)
;   lonmin0: in, required, type=float
;      western longitude of output domain (grid edge, degree east)
;   lonmax0: in, required, type=float
;      eastern longitude of output domain (grid edge, degree east)
;   latmin0: in, required, type=float
;      southern latitude of output domain (grid edge, degree north)
;   latmax0: in, required, type=float
;      northern latitude of output domain (grid edge, degree north)
;   dlon: in, required, type=float 
;      longitudinal grid spacing of output domain (degrees)
;   dlat: in, required, type=float 
;      latitudinal grid spacing of output domain (degrees)
;   nx0: in, required, type=integer
;      number of longitudinal tiles used to subdivide processing (for
;      memory considerations)
;   ny0: in, required, type=integer
;      number of latitudinal tiles used to subdivide processing (for
;      memory considerations)
;
; :Keywords:
;   histo: in, optional, type=boolean
;      produce elevation histogram (1) or mean elevation map (0)
;
; :Returns: 
; 
; :Uses:
;   read_cgiarsrtm
;   read_gtopo30
;   rebin_new
;
; :Bugs:
;
; :Todo:
;
; :Requires:
;   IDL 7.1
;
; :Examples:
;   basedir = '/Users/stockli'
;   maketopo,basedir+'/topo/SRTM_CGIAR/GeoTiff/',basedir+'/topo/GTOPO30/',basedir+'/temp/','testregion',5.0,15.0,40.0,50.0,0.01,0.01,1,1
;
; :History:
;   2007-2008: Initial development at Colorado State University
;
;   2009: Continued development at MeteoSwiss
;
;   2012/09/28 : revised code, included IDL/RST documentation
;
;   2012/10/01 : removed lonlen, latlen, dlon0, dlat0 keywords and
;   replaced with nx0, ny0, dlon, dlat keywords
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
;srtmdir und gtopodir gelöscht
PRO maketopo_aster, asterdir, outdir, region, lonmin0, lonmax0, latmin0, latmax0, dlon, dlat, nx0, ny0, histo=histo

;; flags
  verbose = 1 ;; print stuff or not
  graphics = 0 

;; elevation ranges (for distributed - histogram elevation mode)
  minhgt = 0.
  maxhgt = 4000.
  nhgt = 40

;; how do we fill the missing areas (ocean?)
;; topo_fillvalue = 0
  topo_fillvalue = -9999   ;; Ocean bodies are 0 in the ASTER data already


;; we'll work through the chosen output domain with tiles defined by nx0 and ny0
;; in order to save memory.

;; -------- START MAIN CODE ---------

;; define graphics output if needed
  IF graphics THEN BEGIN
     device,retain=2
     device,decomposed=0,set_font='HELVETICA',/tt_font
     loadct,26
  ENDIF

;; define elevation levels (histogram mode)
  hgtlevels = (findgen(nhgt)+0.5)/float(nhgt)*(maxhgt-minhgt) + minhgt
  
;; define netcdf names
  IF keyword_set(histo) THEN BEGIN
     nvar = 1
     ncvarid=lonarr(nvar)
     ncvarnames = ['hgt']
     ncvarlongnames = ['Subgrid-Scale elevation distribution']
     ncvarunits = '%'
     ncrange=[0B,100B]
     ncmissing = 129B
     outfile='hgt.'+region+'.nc'
  ENDIF ELSE BEGIN
     nvar = 1
     ncvarid=lonarr(nvar)
     ncvarnames = ['Z']
     ncvarlongnames = ['Elevation above sea level']
     ncvarunits = 'm'
     ncrange=[-10000s,10000s]
     ncmissing = -32767s
     outfile='topo.'+region+'.nc'
  ENDELSE

;; global WRAPPER
  dlon0 = (lonmax0 - lonmin0)/float(nx0)
  dlat0 = (latmax0 - latmin0)/float(ny0)
  nlon_tot = round((lonmax0 - lonmin0)/dlon)
  nlat_tot = round((latmax0 - latmin0)/dlat)

  print,'We expect : ',nx0,'x',ny0,' global tiles'
  print,'We expect : ',nlon_tot,'x',nlat_tot,' global grid points'

;; write NetCDF files

  ;; ---------- BEGIN NETCDF DEFINE ----------
  
  ;; create NetCDF file (header and dimensions etc.)
  ncid = NCDF_CREATE(outdir+outfile,/CLOBBER)
  NCDF_CONTROL,ncid,/NOFILL
  londim = NCDF_DIMDEF(ncid, 'lon',nlon_tot)
  latdim = NCDF_DIMDEF(ncid, 'lat',nlat_tot)
  IF keyword_set(histo) THEN hgtdim = NCDF_DIMDEF(ncid, 'Z',nhgt)
  
  ;; create lon/lat variables for a regular grid
  
  lonid = NCDF_VARDEF(ncid, 'lon', [londim], /DOUBLE)
  NCDF_ATTPUT,ncid,lonid,'long_name','Longitude'
  NCDF_ATTPUT,ncid,lonid,'units','degrees_east'
  
  latid = NCDF_VARDEF(ncid, 'lat', [latdim], /DOUBLE)
  NCDF_ATTPUT,ncid,latid,'long_name','Latitude'
  NCDF_ATTPUT,ncid,latid,'units','degrees_north'
  
  IF keyword_set(histo) THEN BEGIN
     hgtid = NCDF_VARDEF(ncid, 'Z', [hgtdim], /FLOAT)
     NCDF_ATTPUT,ncid,hgtid,'long_name','Elevation above sea level'
     NCDF_ATTPUT,ncid,hgtid,'positive','up'
     NCDF_ATTPUT,ncid,hgtid,'units','m'
  ENDIF

  ;; define offset/scale for compressed variables
  scale = 1.0
  offset = 0.0

  ;; create data variables
  FOR v = 0,nvar-1 DO BEGIN
     
     ;; 2D data variable
     IF keyword_set(histo) THEN BEGIN
        ncvarid[v] = NCDF_VARDEF(ncid,ncvarnames[v], [londim,latdim,hgtdim], /BYTE)
     ENDIF ELSE BEGIN
        ncvarid[v] = NCDF_VARDEF(ncid,ncvarnames[v], [londim,latdim], /SHORT)
     ENDELSE
     ;; Attributes for data variables
     NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',ncmissing
;;     NCDF_ATTPUT, ncid, ncvarid[v], 'missing_value',ncmissing
     NCDF_ATTPUT, ncid, ncvarid[v], 'valid_range',ncrange           
     NCDF_ATTPUT, ncid, ncvarid[v], 'axis','YX'                              
     NCDF_ATTPUT, ncid, ncvarid[v], 'long_name',ncvarlongnames[v]
     NCDF_ATTPUT, ncid, ncvarid[v], 'units', ncvarunits
     NCDF_ATTPUT, ncid, ncvarid[v], 'scale_factor',scale
     NCDF_ATTPUT, ncid, ncvarid[v], 'add_offset',offset
     
  ENDFOR

  ;; create global attributes
  IF keyword_set(histo) THEN BEGIN
     NCDF_ATTPUT, ncid, /GLOBAL, 'title','Fractional Elevation Distribution Map'
  ENDIF ELSE BEGIN
     NCDF_ATTPUT, ncid, /GLOBAL, 'title','Elevation Map'
  ENDELSE
  NCDF_ATTPUT, ncid, /GLOBAL, 'institution','MeteoSwiss'
  NCDF_ATTPUT, ncid, /GLOBAL, 'source','ASTER GDEM2'
;  NCDF_ATTPUT, ncid, /GLOBAL, 'source','SRTM (CGIAR) + GTOPO30 (USGS)'
  NCDF_ATTPUT, ncid, /GLOBAL, 'history','Version 1.1, June 2009 by R. Stockli'
;  NCDF_ATTPUT, ncid, /GLOBAL, 'references','Bliss, N.B., and Olsen, L.M., 1996. Development of a 30-arc-second digital elevation model of South America. In: Pecora Thirteen, Human Interactions with the Environment - Perspectives from Space, Sioux Falls, South Dakota, August 20-22, 1996. AND Jarvis A., H.I. Reuter, A.  Nelson, E. Guevara, 2008, Hole-filled  seamless SRTM data V4, International  Centre for Tropical  Agriculture (CIAT), available  from http://srtm.csi.cgiar.org.'
  NCDF_ATTPUT, ncid, /GLOBAL, 'conventions','NetCDF Climate and Forecast (CF) Metadata Conventions Version 1.1'
  
  NCDF_CONTROL, ncid, /ENDEF    ; Put file in data mode. 

  ;; ----------- END NETCDF DEFINE ---------------

  ;; write elevation levels
  IF keyword_set(histo) THEN NCDF_VARPUT, ncid, hgtid, hgtlevels

  ;; loop through global tiles here
  FOR ty=0,ny0-1 DO BEGIN
     FOR tx=0,nx0-1 DO BEGIN

        lonmin = lonmin0+float(tx)*dlon0
        lonmax = lonmin0+float(tx+1)*dlon0
        latmin = latmin0+float(ty)*dlat0
        latmax = latmin0+float(ty+1)*dlat0

        
        nlon = round((lonmax - lonmin)/dlon)
        nlat = round((latmax - latmin)/dlat)

        ;; output lon/lat arrays (centered on pixel)
        lon = ((dindgen(nlon) + 0.5d0)*dlon + double(lonmin)) # replicate(1.d0,nlat) ; W->E
        lat = replicate(1.d0,nlon) # ((dindgen(nlat) + 0.5d0)*dlat + double(latmin)) ; S->N

        print,lonmin,'-',lonmax,'/',latmin,'-',latmax
        print,'We expect : ',nlon,'x',nlat,' grid points on output for this tile'
        print, min(lon), max(lon),min(lat),max(lat)
      
           ;; ASTER DEM native resolution
           dlon_raw = double(1)/double(3600)
           dlat_raw = double(1)/double(3600)

           read_aster,asterdir,lonmin,latmin,lonmax,latmax,dlon_raw,dlat_raw,topo        
        ;; build output arrays
        IF keyword_set(histo) THEN BEGIN
           ncvar = bytarr(nlon,nlat,nhgt)
        ENDIF ELSE BEGIN
           ncvar = intarr(nlon,nlat)
        ENDELSE
        
        IF keyword_set(histo) THEN BEGIN
           ;; make a histogram of elevation bins
           scx = round(dlon/dlon_raw)
           scy = round(dlat/dlat_raw)
           FOR y=0,nlat-1 DO BEGIN
              FOR x=0,nlon-1 DO BEGIN
                 a = histogram((topo[x*scx:(x+1)*scx-1,y*scy:(y+1)*scy-1]>minhgt)<(maxhgt-1.0), $
                               min=minhgt,max=maxhgt-(hgtlevels[1]-hgtlevels[0]),nbins=nhgt)

                 sum = 0
                 pct = round(a/total(a)*100.)
                 FOR h=0,nhgt-1 DO BEGIN
                    sum = sum + pct[h]
                    ncvar[x,y,h] = pct[h] - ((sum - 100)>0)
                    sum = sum < 100
                 ENDFOR

              ENDFOR
           ENDFOR
           
           print,'check for <100% total everywhere'
           less = where(total(ncvar,3) LT 100,lesscount)
           WHILE (lesscount GT 0) DO BEGIN
              v = fix(randomu(seed)*nhgt)
              temp = fix(ncvar[*,*,v])
              temp[less] = (temp[less] + 1*(temp[less] NE 0.))<100
              ncvar[*,*,v] = byte(temp)
              less = where(total(ncvar,3) LT 100,lesscount)
           ENDWHILE

           IF verbose THEN BEGIN
              FOR h = 0,nhgt-1 DO print,'level: ',hgtlevels[h],' %: ',mean(ncvar[*,*,h])
              print,'total: ',mean(total(ncvar,3))
              print,'min: ',min(ncvar), ' max: ',max(ncvar)
           ENDIF
        ENDIF ELSE BEGIN

           dims = size(topo,/dimensions)
           nlon_raw = dims[0]
           nlat_raw = dims[1]

           IF (nlon NE nlon_raw) OR (nlat NE nlat_raw) THEN BEGIN

              IF ((nlon_raw EQ (nlon_raw/nlon)*nlon) AND (nlat_raw EQ (nlat_raw/nlat)*nlat)) OR $
                 ((float(nlon)/float(nlon_raw) EQ float(nlon/nlon_raw)) AND $
                  (float(nlat)/float(nlat_raw) EQ float(nlat/nlat_raw))) THEN BEGIN
                 ;; simple rebinning -- fast
                 ncvar[*] = rebin(topo,nlon,nlat) 
              ENDIF ELSE BEGIN
                 ;; use new rebinning for non-integer dimensional scaling
                 ncvar[*] = fix(rebin_new(topo,nlon,nlat))
              ENDELSE

           ENDIF ELSE BEGIN
              ;; no regridding
              ncvar[*] = topo
           ENDELSE           

           IF verbose THEN BEGIN
              print,'min: ',min(ncvar), ' max: ',max(ncvar)
           ENDIF
        ENDELSE

        ncvar[*] = reverse(ncvar,2)
        
        ;; --------- NETCDF WRITE DATA SECTION ---------------
        
        ;; Now write the data to the NetCDF file(s)

        ;; write longitude, latitude
        tyy = ny0-1-ty
        NCDF_VARPUT, ncid, latid, reverse(reform(lat[0,*])), offset=[tyy*nlat], count=[nlat]
        NCDF_VARPUT, ncid, lonid, reform(lon[*,0]), offset=[tx*nlon], count=[nlon]

        ;; write data
        IF keyword_set(histo) THEN BEGIN
           NCDF_VARPUT, ncid, ncvarid[0],ncvar, offset=[tx*nlon,ty*nlat,0],count=[nlon,nlat,nhgt]
        ENDIF ELSE BEGIN
           NCDF_VARPUT, ncid, ncvarid[0],ncvar, offset=[tx*nlon,tyy*nlat],count=[nlon,nlat]
        ENDELSE

     ENDFOR
  ENDFOR
  
  ;; close NetCDF file
  NCDF_CLOSE, ncid 

END
