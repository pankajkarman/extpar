;*********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;*********************************************

begin

in = addfile("/extpar_raw_data/GLOBE_H10.nc","r")
hsurf_p1 = in->altitude
lat_p1 = in->lat
lon_p1 = in->lon
dim_real = dimsizes(hsurf_p1)
delete(in)

in = addfile("/extpar_raw_data/GLOBE_G10.nc","r")
hsurf_p2 = in->altitude(:,10799)
lon_p2 = in->lon
delete(in)
printVarSummary(hsurf_p2)

in = addfile("/extpar_raw_data/GLOBE_E10.nc","r")
hsurf_p3 = in->altitude(:,0)
lon_p3 = in->lon
delete(in)

in = addfile("/extpar_raw_data/GLOBE_K10.nc","r")
hsurf_p4 = in->altitude(0,10799)
lat_p4 = in->lat
delete(in)
printVarSummary(hsurf_p4)

in = addfile("/extpar_raw_data/GLOBE_L10.nc","r")
hsurf_p5 = in->altitude(0,:)

delete(in)

in = addfile("/extpar_raw_data/GLOBE_I10.nc","r")
hsurf_p6 = in->altitude(0,0)

delete(in)

in = addfile("/extpar_raw_data/GLOBE_C10.nc","r")
hsurf_p7 = in->altitude(4799,10799)
lat_p7 = in->lat
delete(in)
printVarSummary(hsurf_p7)

in = addfile("/extpar_raw_data/GLOBE_D10.nc","r")
hsurf_p8 = in->altitude(4799,:)

delete(in)

in = addfile("/extpar_raw_data/GLOBE_A10.nc","r")
hsurf_p9 = in->altitude(4799,0)

delete(in)


hsurf_new = new((/dim_real(0)+2,dim_real(1)+2/),float)
lat_new = new((/dim_real(0)+2/),float)
lon_new = new((/dim_real(1)+2/),float)
hsurf_new(1:dim_real(0),0) = hsurf_p2
hsurf_new(1:dim_real(0),1:dim_real(1)) = hsurf_p1
hsurf_new(1:dim_real(0),dim_real(1)+1) = hsurf_p3
hsurf_new(dim_real(0)+1,0) = hsurf_p4
hsurf_new(dim_real(0)+1,1:dim_real(1)) = hsurf_p5
hsurf_new(dim_real(0)+1,dim_real(1)+1) = hsurf_p6
hsurf_new(0,0) = hsurf_p7
hsurf_new(0,1:dim_real(1)) = hsurf_p8
hsurf_new(0,dim_real(1)+1) = hsurf_p9
lat_new(1:dim_real(0)) = lat_p1
lat_new(dim_real(0)+1) = lat_p4(0)
lat_new(0) = lat_p7(4799)
lon_new(0) = lon_p2(10799)
lon_new(1:dim_real(1)) = lon_p1
lon_new(dim_real(1)+1) = lon_p3(0)
;print(""+hsurf&lat)
;print(""+hsurf&lon)


fout = addfile("/buffer/extpar/GLOBE_H_1.nc","c")
fout->altitude = hsurf_new
fout->lon = lon_new
fout->lat = lat_new

end
