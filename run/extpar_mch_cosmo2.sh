#!/usr/bin/ksh

ulimit -s unlimited
ulimit -c unlimited

# adjust the path setting!
export GRIB_DEFINITION_PATH=/oprusers/osm/lib/libgrib_api_1.9.9.1_pgi12.2.0/share/definitions:/oprusers/osm/lib/libgrib_api_1.9.9.1_pgi12.2.0/share/definitions

export GRIB_SAMPLES_PATH=/oprusers/osm/lib/libgrib_api_1.9.9.1_pgi12.2.0/share/samples

# path to working directory
workdir=./                                                   # adjust the path setting!
# path to raw data for external parameter
data_dir=/store/s83/rochesa/projects/extpar/raw_data_netcdf  # adjust the path setting!
# path to binaries
progdir=../build                                             # adjust the path setting!

binary_lu=extpar_landuse_to_buffer
binary_globe=extpar_globe_to_buffer
binary_aot=extpar_aot_to_buffer
binary_tclim=extpar_cru_to_buffer
binary_ndvi=extpar_ndvi_to_buffer
binary_soil=extpar_soil_to_buffer
binary_flake=extpar_flake_to_buffer

binary_extpar_consistency_check=extpar_consistency_check

if [[ ! -d ${workdir} ]] ; then
  mkdir -p ${workdir} 
fi
cd ${workdir}
pwd


#---

grib_output_filename='external_parameter_mch_cosmo2.g1'
stf_output_filename='external_parameter_mch_cosmo2.stf'
netcdf_output_filename='external_parameter_mch_cosmo2.nc'
grib_sample='DWD_rotated_ll_7km_G_grib1'

raw_data_aot='aerosol_optical_thickness.nc'
buffer_aot='extpar_buffer_aot.nc'
output_aot='aot_extpar_cosmo.nc'

raw_data_tclim='absolute_hadcrut3.nc'
buffer_tclim='crutemp_clim_extpar_buffer.nc'
output_tclim='crutemp_clim_extpar_cosmo.nc'

raw_data_glc2000='glc2000_byte.nc'
buffer_glc2000='extpar_landuse_buffer.nc'
output_glc2000='extpar_landuse_cosmo.nc'
raw_data_glcc='glcc_usgs_class_byte.nc'
buffer_glcc='glcc_landuse_buffer.nc'
output_glcc='glcc_landuse_cosmo.nc'

raw_data_globcover='GLOBCOVER_L4_200901_200912_V2.3_int16.nc'
raw_data_globcover_gz='GLOBCOVER_L4_200901_200912_V2.3_int16.nc.gz'
buffer_lu='extpar_landuse_buffer.nc'
output_lu='extpar_landuse_cosmo.nc'

raw_data_globe_A10='GLOBE_A10.nc'
raw_data_globe_B10='GLOBE_B10.nc'
raw_data_globe_C10='GLOBE_C10.nc'
raw_data_globe_D10='GLOBE_D10.nc'
raw_data_globe_E10='GLOBE_E10.nc'
raw_data_globe_F10='GLOBE_F10.nc'
raw_data_globe_G10='GLOBE_G10.nc'
raw_data_globe_H10='GLOBE_H10.nc'
raw_data_globe_I10='GLOBE_I10.nc'
raw_data_globe_J10='GLOBE_J10.nc'
raw_data_globe_K10='GLOBE_K10.nc'
raw_data_globe_L10='GLOBE_L10.nc'
raw_data_globe_M10='GLOBE_M10.nc'
raw_data_globe_N10='GLOBE_N10.nc'
raw_data_globe_O10='GLOBE_O10.nc'
raw_data_globe_P10='GLOBE_P10.nc'
buffer_globe='GLOBE_buffer.nc'
output_globe='GLOBE_COSMO.nc'

raw_data_ndvi='NDVI_1998_2003.nc'
buffer_ndvi='NDVI_buffer.nc'
output_ndvi='ndvi_extpar_cosmo.nc'

raw_data_soil='FAO_DSMW.nc'
buffer_soil='FAO_DSMW_buffer.nc'
output_soil='FAO_DSMW_COSMO.nc'

raw_data_flake='lakedepth.nc'
buffer_flake='flake_buffer.nc'
output_flake='ext_par_flake_cosmo.nc'

# set target grid definition 
cat > INPUT_grid_org << EOF_go
&GRID_DEF 
 igrid_type = 2,
 domain_def_namelist='INPUT_COSMO_GRID'
/ 
EOF_go

#---
cat > INPUT_COSMO_GRID << EOF_grid
&lmgrid
 pollon=-170.0, 
 pollat=43.0, 
 startlon_tot=-7.0, 
 startlat_tot=-7.0,
 dlon=0.02,
 dlat=0.02,
 ie_tot=701,
 je_tot=701,
/
EOF_grid
#---
# create input namelists 
cat > INPUT_AOT << EOF_aot
&aerosol_raw_data
  raw_data_aot_path='',
  raw_data_aot_filename='${raw_data_aot}'
/  
&aerosol_io_extpar
  aot_buffer_file='${buffer_aot}',
  aot_output_file='${output_aot}'
/
EOF_aot
#---
cat > INPUT_TCLIM << EOF_tclim
&t_clim_raw_data
  raw_data_t_clim_path='',
  raw_data_t_clim_filename='${raw_data_tclim}'
/  

&t_clim_io_extpar
  t_clim_buffer_file='${buffer_tclim}',
  t_clim_output_file='${output_tclim}'
/  
EOF_tclim
#---
cat > INPUT_LU << EOF_lu
&lu_raw_data
   raw_data_lu_path='',
   raw_data_lu_filename='${raw_data_glc2000}',
   i_landuse_data=2,
   ilookup_table_lu=2
/
&lu_io_extpar
   lu_buffer_file='${buffer_lu}',
   lu_output_file='${output_lu}'
/
&glcc_raw_data
   raw_data_glcc_path='',
   raw_data_glcc_filename='${raw_data_glcc}'
/
&glcc_io_extpar
   glcc_buffer_file='${buffer_glcc}',
   glcc_output_file='${output_glcc}'
/
EOF_lu
#---
cat > INPUT_ORO << EOF_oro
&orography_io_extpar
  orography_buffer_file='${buffer_globe}',
  orography_output_file='${output_globe}'
/
&orography_raw_data
 raw_data_orography_path='', 
 GLOBE_FILES = '${raw_data_globe_A10}' '${raw_data_globe_B10}'  '${raw_data_globe_C10}'  '${raw_data_globe_D10}'  '${raw_data_globe_E10}'  '${raw_data_globe_F10}'  '${raw_data_globe_G10}'  '${raw_data_globe_H10}'  '${raw_data_globe_I10}'  '${raw_data_globe_J10}'  '${raw_data_globe_K10}'  '${raw_data_globe_L10}'  '${raw_data_globe_M10}'  '${raw_data_globe_N10}'  '${raw_data_globe_O10}'  '${raw_data_globe_P10}'  
/
EOF_oro
#---
cat > INPUT_OROSMOOTH << EOF_orosm
&orography_smoothing
  lfilter_oro=.FALSE.,
!  ilow_pass_oro=4,
!  numfilt_oro=1,
  !ilow_pass_xso=5,
!  ilow_pass_xso=2,
!  lxso_first=.FALSE.,
!  numfilt_xso=1,
 ! rxso_mask=750.0,
!  rxso_mask=0.0,
!  eps_filter=0.1,
!  rfill_valley=0.0,
!  ifill_valley=1
/
EOF_orosm
#---
cat > INPUT_NDVI << EOF_ndvi
&ndvi_raw_data
  raw_data_ndvi_path='',
  raw_data_ndvi_filename='${raw_data_ndvi}'
/  
&ndvi_io_extpar
 ndvi_buffer_file='${buffer_ndvi}',
 ndvi_output_file='${output_ndvi}'
/
EOF_ndvi
#---
cat > INPUT_SOIL << EOF_soil
&soil_raw_data
 raw_data_soil_path='',
 raw_data_soil_filename='${raw_data_soil}'
/
&soil_io_extpar
  soil_buffer_file='${buffer_soil}',
  soil_output_file='${output_soil}'
/ 
EOF_soil
#---
cat > INPUT_FLAKE << EOF_flake
&flake_raw_data
   raw_data_flake_path='',
   raw_data_flake_filename='${raw_data_flake}'
/
&flake_io_extpar
   flake_buffer_file='${buffer_flake}'
   flake_output_file='${output_flake}'
/
EOF_flake
#---
# consistency check
cat > INPUT_CHECK << EOF_check
&extpar_consistency_check_io
  grib_output_filename="${grib_output_filename}",
  grib_sample="${grib_sample}",
  netcdf_output_filename="${netcdf_output_filename}",
  orography_buffer_file="${buffer_globe}",
  soil_buffer_file="${buffer_soil}",
  lu_buffer_file="${buffer_lu}",
  glcc_buffer_file="${buffer_glcc}",
  flake_buffer_file="${buffer_flake}",
  ndvi_buffer_file="${buffer_ndvi}",
  t_clim_buffer_file="${buffer_tclim}",
  aot_buffer_file="${buffer_aot}"
/  
EOF_check

# link raw data files to local workdir
ln -s ${data_dir}/${raw_data_aot}

ln -s ${data_dir}/${raw_data_tclim}

ln -s ${data_dir}/${raw_data_glc2000}
ln -s ${data_dir}/${raw_data_glcc}

ln -s ${data_dir}/${raw_data_globcover}

ln -s ${data_dir}/${raw_data_globe_A10} 
ln -s ${data_dir}/${raw_data_globe_B10}
ln -s ${data_dir}/${raw_data_globe_C10} 
ln -s ${data_dir}/${raw_data_globe_D10} 
ln -s ${data_dir}/${raw_data_globe_E10}
ln -s ${data_dir}/${raw_data_globe_F10} 
ln -s ${data_dir}/${raw_data_globe_G10} 
ln -s ${data_dir}/${raw_data_globe_H10} 
ln -s ${data_dir}/${raw_data_globe_I10} 
ln -s ${data_dir}/${raw_data_globe_J10} 
ln -s ${data_dir}/${raw_data_globe_K10} 
ln -s ${data_dir}/${raw_data_globe_L10} 
ln -s ${data_dir}/${raw_data_globe_M10} 
ln -s ${data_dir}/${raw_data_globe_N10} 
ln -s ${data_dir}/${raw_data_globe_O10} 
ln -s ${data_dir}/${raw_data_globe_P10} 

ln -s ${data_dir}/${raw_data_ndvi}

ln -s ${data_dir}/${raw_data_soil}

ln -s ${data_dir}/${raw_data_flake}

# run the programs
# the next seven programs can run independent of each other
time ${progdir}/${binary_aot}
time ${progdir}/${binary_tclim}
time ${progdir}/${binary_lu}
time ${progdir}/${binary_globe}
time ${progdir}/${binary_ndvi}
time ${progdir}/${binary_soil}
time ${progdir}/${binary_flake}

# the consistency check requires the output of 
# ${binary_aot}, ${binary_tclim}, ${binary_lu}, ${binary_globe}, 
# ${binary_ndvi}, ${binary_soil} and ${binary_flake}
time ${progdir}/${binary_extpar_consistency_check}

ls
echo 'External parameters for COSMO model generated'



