;*********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;*********************************************

begin

in = addfile("/extpar_raw_data/GLOBE_O10.nc","r")
hsurf_p2 = in->altitude(:,10799)
lon_p2 = in->lon
delete(in)
printVarSummary(hsurf_p2)

in = addfile("/extpar_raw_data/GLOBE_M10.nc","r")
hsurf_p3 = in->altitude(:,0)
lon_p3 = in->lon
delete(in)

in = addfile("/extpar_raw_data/GLOBE_K10.nc","r")
hsurf_p4 = in->altitude(5999,10799)
lat_p4 = in->lat
delete(in)
printVarSummary(lat_p4)

in = addfile("/extpar_raw_data/GLOBE_L10.nc","r")
hsurf_p5 = in->altitude(5999,:)

delete(in)

in = addfile("/extpar_raw_data/GLOBE_I10.nc","r")
hsurf_p6 = in->altitude(5999,0)

delete(in)

in = addfile("/extpar_raw_data/GLOBE_P10.nc","r")
hsurf_p1 = in->altitude
lat_p1 = in->lat
lon_p1 = in->lon
dim_real = dimsizes(hsurf_p1)
delete(in)
printVarSummary(hsurf_p1)

hsurf_new = new((/dim_real(0)+2,dim_real(1)+2/),float)
;copy_VarAtts(hsurf_p1,hsurf_new)

lat_new = new((/dim_real(0)+2/),float)
lon_new = new((/dim_real(1)+2/),float)
hsurf_new(1:dim_real(0),0) = hsurf_p2
hsurf_new(1:dim_real(0),1:dim_real(1)) = hsurf_p1
hsurf_new(1:dim_real(0),dim_real(1)+1) = hsurf_p3
hsurf_new(dim_real(0)+1,0) = hsurf_p4
hsurf_new(dim_real(0)+1,1:dim_real(1)) = hsurf_p5
hsurf_new(dim_real(0)+1,dim_real(1)+1) = hsurf_p6
hsurf_new(dim_real(0)+1,:) = hsurf_new(dim_real(0),:)
lat_new(1:dim_real(0)) = lat_p1
lat_new(4801) = lat_p1(4799)
lat_new(0) = lat_p4(5999)
lon_new(0) = lon_p2(10799)
lon_new(1:dim_real(1)) = lon_p1
lon_new(10801) = lon_p3(0)

print("writing variables to netcdf file")

;;delete(lon)
;;delete(lat)
fout = addfile("/buffer/extpar/GLOBE_P_1.nc","c")
fout->altitude = hsurf_new
fout->lon = lon_new
fout->lat = lat_new

  
end


