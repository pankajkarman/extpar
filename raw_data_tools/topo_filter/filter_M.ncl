;*********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;*********************************************

external EX01 "./lanczos_window.so"

begin

in = addfile("/store/s83/tsm/extpar/raw_data_nc/GLOBE_M10.nc","r")
hsurf_p1 = in->altitude
lat_p1 = in->lat
lon_p1 = in->lon
dim_real = dimsizes(hsurf_p1)
delete(in)

in = addfile("/store/s83/tsm/extpar/raw_data_nc/GLOBE_P10.nc","r")
hsurf_p2 = in->altitude(:,10799-39:10799)
lon_p2 = in->lon(10799-39:10799)
delete(in)
printVarSummary(hsurf_p2)

in = addfile("/store/s83/tsm/extpar/raw_data_nc/GLOBE_N10.nc","r")
hsurf_p3 = in->altitude(:,0:39)
lon_p3 = in->lon(0:39)
delete(in)

in = addfile("/store/s83/tsm/extpar/raw_data_nc/GLOBE_L10.nc","r")
hsurf_p4 = in->altitude(5996:5999,10799-39:10799)
lat_p4 = in->lat(5996:5999)
delete(in)
printVarSummary(hsurf_p4)

in = addfile("/store/s83/tsm/extpar/raw_data_nc/GLOBE_I10.nc","r")
hsurf_p5 = in->altitude(5996:5999,:)

delete(in)

in = addfile("/store/s83/tsm/extpar/raw_data_nc/GLOBE_J10.nc","r")
hsurf_p6 = in->altitude(5996:5999,0:39)

delete(in)

hsurf_new = hsurf_p1
lat_new = lat_p1
lon_new = lon_p1
copy_VarAtts(hsurf_p1,hsurf_new)

hsurf = new((/dim_real(0)+4,dim_real(1)+80/),float)
lat = new((/dim_real(0)+4/),float)
lon = new((/dim_real(1)+80/),float)
hsurf(4:dim_real(0)+3,0:39) = hsurf_p2
hsurf(4:dim_real(0)+3,40:dim_real(1)+39) = hsurf_p1
hsurf(4:dim_real(0)+3,dim_real(1)+40:dim_real(1)+79) = hsurf_p3
hsurf(0:3,0:39) = hsurf_p4
hsurf(0:3,40:dim_real(1)+39) = hsurf_p5
hsurf(0:3,dim_real(1)+40:dim_real(1)+79) = hsurf_p6
lat(4:dim_real(0)+3) = lat_p1
lat(0:3) = lat_p4
lon(0:39) = lon_p2
lon(40:dim_real(1)+39) = lon_p1
lon(dim_real(1)+40:dim_real(1)+79) = lon_p3

dlat = 1./120.
dlon = dlat
pi = 3.14159265358979
radius_earth = 6371.0                            ;radius WGS84
dist_lat = 2*pi*radius_earth/360.0
number_lat_pix = round((1/(dist_lat/3))/dlat,3)
nboarder = 40
nboarder_lat = 4

if (round(max(lat),0) .eq. 90.) then
  h = hsurf({85:min(lat)},:)
  lat_h = lat({85:min(lat)})
  print("cutting beyond 85° N")
else if (round(min(lat),0) .eq. -90.) then
  h = hsurf({max(lat):-85},:)
  lat_h = lat({max(lat):-85})
  print("cutting beyond 85° S")
else 
  h = hsurf
  lat_h = lat
  print("no cutting needed")
end if
end if

dims = dimsizes(h)
hh = h

dim_1 = dims(0)
dim_2 = dims(1)
print(""+dim_1+"  "+dim_2)

EX01:: lanczos_window(dim_1, dim_2, lat_h, h, hh)
;h = hh
;h = where(ismissing(hh1), hh1@_FillValue, h)
;EX01:: lanczos_window(dim_1, dim_2, lat_h, h, hh)
;h = hh
;h = where(ismissing(hh1), hh1@_FillValue, h)
;EX01:: lanczos_window(dim_1, dim_2, lat_h, h, hh)
;h = hh
;h = where(ismissing(hh1), hh1@_FillValue, h)
;EX01:: lanczos_window(dim_1, dim_2, lat_h, h, hh)

hsurf_new(0:dims(0)-5,:) = floattointeger(hh(4:dims(0)-1:,40:10839))
hsurf_p1@missing_value = hsurf_p1@_FillValue
hsurf_new = where(ismissing(hsurf_p1), hsurf_p1@_FillValue, hsurf_new)
hsurf_new = where(hsurf_new .gt. -500 .and. hsurf_new .le. 0, hsurf_p1, hsurf_new)

print("writing variables to netcdf file")

delete(lon)
delete(lat)

y = systemfunc("rm /store/s83/messmerm/ncl/GLOBE_M_filt_lanczos_window.nc")
fout = addfile("/store/s83/messmerm/ncl/GLOBE_M_filt_lanczos_window.nc","c")
fout->altitude = hsurf_new
fout->lon = lon_new
fout->lat = lat_new

  
end


