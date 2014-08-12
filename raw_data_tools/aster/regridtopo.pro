PRO regridtopo
;; Reto Stockli, MeteoSwiss, 2009/04/30

;; This script reprojects the global topography dataset from a regular
;; cylindrical coordinate system to a rotated pole coordinate system
;; It currently uses bilinear interpolation and can only be used to
;; reproject the Elevation grid, and NOT the histogrammed elevation
;; bins.

;; THIS CODE IS NOT READY, NOT DOCUMENTED AND NOT FOR USE!

;; Arguments:

;; flags
  histo = 0 ;; do not set this to 1 at this point

;; directories
  basedir = '/project/msclim/stockli'
  indir = basedir + '/topo/'
  outdir = indir
  inregion = 'global'
  outregion = 'cosmo7'

;; output grid (rotated)
  rlonmin = -17.0
  rlonmax = 8.0
  rlatmin = -10.0
  rlatmax = 11.0

  pollon = -170.0
  pollat = 43.0
  rlonlen = 120
  rlatlen = 120

;; -------- START MAIN CODE ---------

;; define output resolution (degrees per pixel)
  drlon = 1d0/double(rlonlen)
  drlat = 1d0/double(rlatlen)

  nrlon = round((rlonmax - rlonmin)*double(rlonlen))
  nrlat = round((rlatmax - rlatmin)*double(rlatlen))

  ;; output rlon/rlat arrays (centered on pixel)
  rlon = ((dindgen(nrlon) + 0.5d0)*drlon + double(rlonmin)) # replicate(1.d0,nrlat) ; W->E
  rlat = replicate(1.d0,nrlon) # ((dindgen(nrlat) + 0.5d0)*drlat + double(rlatmin)) ; S->N
  
  ;; reproject these arrays to geographical rlon/rlat pairs
  RPOL2LL,RLON,RLAT,LON,LAT,pollon=pollon,pollat=pollat

  ;; define boundaries which we will read from input topography dataset
  lonmin = min(lon)-0.5
  lonmax = max(lon)+0.5
  latmin = min(lat)-0.5
  latmax = max(lat)+0.5

  ;; define netcdf names
  IF histo THEN BEGIN
     nvar = 1
     ncvarid=lonarr(nvar)
     ncvarnames = ['hgt']
     ncvarlongnames = ['Subgrid-Scale elevation distribution']
     ncvarunits = '%'
     ncrange=[0,100]
     ncmissing = 255B 
     infile='hgt.'+inregion+'.nc'
     outfile='hgt.'+outregion+'.nc'
  ENDIF ELSE BEGIN
     nvar = 1
     ncvarid=lonarr(nvar)
     ncvarnames = ['Z']
     ncvarlongnames = ['Elevation above sea level']
     ncvarunits = 'm'
     ncrange=[-10000,10000]
     ncmissing = -32767 
     infile='topo.'+inregion+'.nc'
     outfile='topo.'+outregion+'.nc'
  ENDELSE

  ;; Define output arrays
  IF histo THEN BEGIN
     ncvar = bytarr(nrlon,nrlat,nhgt)
  ENDIF ELSE BEGIN
     ncvar = intarr(nrlon,nrlat)
  ENDELSE

  ;; read original NetCDF file
  ncid = NCDF_OPEN(indir+infile)

  ncdf_varget,ncid,'lon',lons_topo
  ncdf_varget,ncid,'lat',lats_topo
  
  dlon_topo = abs(lons_topo[1]-lons_topo[0])
  dlat_topo = abs(lats_topo[1]-lats_topo[0])
  
  latmin_topo = min(lats_topo) - 0.5d0*dlat_topo
  lonmin_topo = min(lons_topo) - 0.5d0*dlon_topo
  latmax_topo = max(lats_topo) + 0.5d0*dlat_topo
  lonmax_topo = max(lons_topo) + 0.5d0*dlon_topo
  
  nlon_topo = n_elements(lons_topo)
  nlat_topo = n_elements(lats_topo)

  xmin_topo = -1L
  xmax_topo = -1L
  ymin_topo = -1L
  ymax_topo = -1L
  
  ;; define precision for getting rid of rounding errors below
  pr = 1.d-6
  
  FOR i=nlon_topo-1L,0L,-1L DO IF (round((lons_topo[i]-0.5d0*dlon_topo)/pr,/L64) LE round(lonmin/pr,/L64)) $
     AND (xmin_topo EQ -1L) THEN xmin_topo = i
  FOR i=nlat_topo-1L,0L,-1L DO IF (round((lats_topo[i]-0.5d0*dlat_topo)/pr,/L64) LE round(latmin/pr,/L64)) $
     AND (ymin_topo EQ -1L) THEN ymin_topo = i
  FOR i=0L,nlon_topo-1L,1L DO IF (round((lons_topo[i]+0.5d0*dlon_topo)/pr,/L64) GE round(lonmax/pr,/L64)) $
     AND (xmax_topo EQ -1L) THEN xmax_topo = i
  FOR i=0L,nlat_topo-1L,1L DO IF (round((lats_topo[i]+0.5d0*dlat_topo)/pr,/L64) GE round(latmax/pr,/L64)) $
     AND (ymax_topo EQ -1L) THEN ymax_topo = i
  

  IF ((xmin_topo EQ (-1L)) OR (ymin_topo EQ (-1L)) OR (xmax_topo EQ (-1L)) OR (ymax_topo EQ (-1L))) THEN BEGIN
     print,'topography dataset does not cover requested area. stopping.'
     print,'We need: ',latmin,latmax,lonmin,lonmax
     print,'We get:  ',latmin_topo,latmax_topo,lonmin_topo,lonmax_topo
     stop
  ENDIF

  nx_topo = xmax_topo - xmin_topo + 1L
  ny_topo = ymax_topo - ymin_topo + 1L

  ;; read topography from chosen area
  ncdf_varget,ncid, 'Z', data, offset = [xmin_topo,ymin_topo], count = [nx_topo,ny_topo]

  ncdf_close,ncid

  lons_data = lons_topo[xmin_topo:xmax_topo]
  lats_data = lats_topo[ymin_topo:ymax_topo]

  ;; Reprojection goes here
  lonidx = (lon - lons_data[0])/dlon_topo
  latidx = (lat - lats_data[0])/dlat_topo
  ncvar[*,*] = bilinear(data,lonidx,latidx)

  ;; write NetCDF file

  ;; ---------- BEGIN NETCDF DEFINE ----------
  
  ;; create output NetCDF file (header and dimensions etc.)
  ncid = NCDF_CREATE(outdir+outfile,/CLOBBER)
  NCDF_CONTROL,ncid,/NOFILL
  rlondim = NCDF_DIMDEF(ncid, 'rlon',nrlon)
  rlatdim = NCDF_DIMDEF(ncid, 'rlat',nrlat)
  IF histo THEN hgtdim = NCDF_DIMDEF(ncid, 'Z',nhgt)
  
  ;; create rlon/rlat variables for a regular grid
  
  rlonid = NCDF_VARDEF(ncid, 'rlon', [rlondim], /DOUBLE)
  NCDF_ATTPUT,ncid,rlonid,'long_name','Longitude in rotated pole grid'
  NCDF_ATTPUT,ncid,rlonid,'units','degrees'
  
  rlatid = NCDF_VARDEF(ncid, 'rlat', [rlatdim], /DOUBLE)
  NCDF_ATTPUT,ncid,rlatid,'long_name','Latitude in rotated pole grid'
  NCDF_ATTPUT,ncid,rlatid,'units','degrees'
  
  poleid = NCDF_VARDEF(ncid, 'rotated_pole', /CHAR)
  NCDF_ATTPUT,ncid,poleid,'grid_mapping_name','rotated_latitude_longitude'
  NCDF_ATTPUT,ncid,poleid,'grid_north_pole_latitude',pollat
  NCDF_ATTPUT,ncid,poleid,'grid_north_pole_longitude',pollon
  
  IF histo THEN BEGIN
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
     IF histo THEN BEGIN
        ncvarid[v] = NCDF_VARDEF(ncid,ncvarnames[v], [rlondim,rlatdim,hgtdim], /BYTE)
     ENDIF ELSE BEGIN
        ncvarid[v] = NCDF_VARDEF(ncid,ncvarnames[v], [rlondim,rlatdim], /SHORT)
     ENDELSE
     ;; Attributes for data variables
     NCDF_ATTPUT, ncid, ncvarid[v], '_FillValue',ncmissing
     NCDF_ATTPUT, ncid, ncvarid[v], 'valid_range',ncrange           
     NCDF_ATTPUT, ncid, ncvarid[v], 'axis','YX'                              
     NCDF_ATTPUT, ncid, ncvarid[v], 'long_name',ncvarlongnames[v]
     NCDF_ATTPUT, ncid, ncvarid[v], 'units', ncvarunits
     NCDF_ATTPUT, ncid, ncvarid[v], 'scale_factor',scale
     NCDF_ATTPUT, ncid, ncvarid[v], 'add_offset',offset
     
  ENDFOR

  ;; create global attributes
  IF histo THEN BEGIN
      NCDF_ATTPUT, ncid, /GLOBAL, 'title','Fractional elevation distribution map on rotated pole grid'
  ENDIF ELSE BEGIN
      NCDF_ATTPUT, ncid, /GLOBAL, 'title','Elevation map on rotated pole grid'
  ENDELSE
  NCDF_ATTPUT, ncid, /GLOBAL, 'institution','MeteoSwiss'
  NCDF_ATTPUT, ncid, /GLOBAL, 'source','SRTM (CGIAR) + GTOPO30 (USGS)'
  NCDF_ATTPUT, ncid, /GLOBAL, 'history','Version 1.1, May 2009 by R. Stockli'
  NCDF_ATTPUT, ncid, /GLOBAL, 'references','Bliss, N.B., and Olsen, L.M., 1996. Development of a 30-arc-second digital elevation model of South America. In: Pecora Thirteen, Human Interactions with the Environment - Perspectives from Space, Sioux Falls, South Dakota, August 20-22, 1996. AND Jarvis A., H.I. Reuter, A.  Nelson, E. Guevara, 2008, Hole-filled  seamless SRTM data V4, International  Centre for Tropical  Agriculture (CIAT), available  from http://srtm.csi.cgiar.org.'
  NCDF_ATTPUT, ncid, /GLOBAL, 'conventions','NetCDF Climate and Forecast (CF) Metadata Conventions Version 1.1'
  
  NCDF_CONTROL, ncid, /ENDEF    ; Put file in data mode. 

  ;; ----------- END NETCDF DEFINE ---------------


  ;; write elevation levels
  IF histo THEN NCDF_VARPUT, ncid, hgtid, hgtlevels
        
  ;; write longitude, latitude
  NCDF_VARPUT, ncid, rlatid, reform(rlat[0,*]), offset=[0], count=[nrlat]
  NCDF_VARPUT, ncid, rlonid, reform(rlon[*,0]), offset=[0], count=[nrlon]
  
  ;; write data
  IF histo THEN BEGIN
     NCDF_VARPUT, ncid, ncvarid[0],ncvar, offset=[0,0,0],count=[nrlon,nrlat,nhgt]
  ENDIF ELSE BEGIN
     NCDF_VARPUT, ncid, ncvarid[0],ncvar, offset=[0,0],count=[nrlon,nrlat]
  ENDELSE
  
  ;; close NetCDF file
  NCDF_CLOSE, ncid 

END
