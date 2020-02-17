#!/bin/ksh
#________________________________________________________________________________
set -eu

ulimit -s unlimited
ulimit -c 0
#________________________________________________________________________________
error_count=0
run_command()
{
    set +e
    echo ">> Run $1 ..."    
    start=$(date +%s.%N)
    eval ${progdir}/$1 2>&1 >> ${logfile}
    rc=$?
    end=$(date +%s.%N)
    (( runtime = end - start ))
    if (( rc > 0 ))
    then
        (( error_count++ ))
    fi 
    case $rc in
        0)
            echo "   SUCCESS $1"            
            ;;
        127)
            echo "   ERROR $1: command not found"
            ;;
        130)
            echo "   ERROR $1: script terminated by Ctrl-C"
            ;;             
        *)
            echo "   ERROR $1: fatal error - return code $rc"
            ;;
    esac
    echo "   execution time: $runtime s"
    set -e
}
#________________________________________________________________________________

scriptpath=$0
scriptname=${scriptpath##*/}
logfile=${scriptname%.*}_$(date +%Y%m%d%H%M%S).log

currentdir=$(pwd)
rootdir=${currentdir}/..
progdir=${currentdir}

#________________________________________________________________________________
# NetCDF raw data for external parameter; adjust the path setting!
data_dir=/pool/data/ICON/grids/private/mpim/icon_preprocessing/source/extpar_input.2016/

# Output file format and names; adjust!
netcdf_output_filename='external_parameter.nc'
#________________________________________________________________________________
# Names of executables
binary_alb=extpar_alb_to_buffer.exe
binary_lu=extpar_landuse_to_buffer.exe
binary_topo=extpar_topo_to_buffer.exe
binary_aot=extpar_aot_to_buffer.exe
binary_tclim=extpar_cru_to_buffer.exe
binary_ndvi=extpar_ndvi_to_buffer.exe
binary_soil=extpar_soil_to_buffer.exe
binary_flake=extpar_flake_to_buffer.exe
binary_sgsl=extpar_sgsl_to_buffer.exe
binary_ahf=extpar_ahf_to_buffer.exe
binary_isa=extpar_isa_to_buffer.exe
binary_consistency_check=extpar_consistency_check.exe
#________________________________________________________________________________
if [[ -e ${logfile} ]] ; then
  rm -f ${logfile}
fi
echo ">>>> Data will be processed and produced in $(pwd) <<<<"
#________________________________________________________________________________

raw_data_alb='month_alb.nc'
raw_data_alnid='month_alnid.nc'
raw_data_aluvd='month_aluvd.nc'
sed -i 's#@raw_data_alb_filename@#'"$raw_data_alb"'#' INPUT_ALB
sed -i 's#@raw_data_alnid_filename@#'"$raw_data_alnid"'#' INPUT_ALB
sed -i 's#@raw_data_aluvd_filename@#'"$raw_data_aluvd"'#' INPUT_ALB
buffer_alb='month_alb_buffer.nc'
output_alb='month_alb_extpar_cosmo.nc'

raw_data_aot='aerosol_optical_thickness.nc'
buffer_aot='extpar_buffer_aot.nc'
output_aot='aot_extpar_cosmo.nc'

raw_data_tclim_coarse='CRU_T2M_SURF_clim_coarse.nc'
raw_data_tclim_fine='CRU_T2M_SURF_clim.nc'
sed -i 's#@raw_data_t_clim_filename@#'"$raw_data_tclim_fine"'#' INPUT_TCLIM
buffer_tclim='crutemp_clim_extpar_buffer.nc'
output_tclim='crutemp_clim_extpar_cosmo.nc'

raw_data_glc2000='glc2000_byte.nc'
sed -i 's#@raw_data_lu_filename@#'"$raw_data_glc2000"'#' INPUT_LU
buffer_glc2000='extpar_landuse_buffer.nc'
output_glc2000='extpar_landuse_cosmo.nc'
raw_data_glcc='glcc_usgs_class_byte.nc'
sed -i 's#@raw_data_glcc_filename@#'"$raw_data_glcc"'#' INPUT_LU
buffer_glcc='glcc_landuse_buffer.nc'
output_glcc='glcc_landuse_cosmo.nc'

# raw_data_globcover='GLOBCOVER_L4_200901_200912_V2.3_int16.nc'
raw_data_globcover_0='GLOBCOVER_0_16bit.nc'
raw_data_globcover_1='GLOBCOVER_1_16bit.nc'
raw_data_globcover_2='GLOBCOVER_2_16bit.nc'
raw_data_globcover_3='GLOBCOVER_3_16bit.nc'
raw_data_globcover_4='GLOBCOVER_4_16bit.nc'
raw_data_globcover_5='GLOBCOVER_5_16bit.nc'

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

# lanczos filter is recommended when activating scale separation
raw_filt_globe_A10='GLOBE_A_filt_lanczos_window.nc'
raw_filt_globe_B10='GLOBE_B_filt_lanczos_window.nc'
raw_filt_globe_C10='GLOBE_C_filt_lanczos_window.nc'
raw_filt_globe_D10='GLOBE_D_filt_lanczos_window.nc'
raw_filt_globe_E10='GLOBE_E_filt_lanczos_window.nc'
raw_filt_globe_F10='GLOBE_F_filt_lanczos_window.nc'
raw_filt_globe_G10='GLOBE_G_filt_lanczos_window.nc'
raw_filt_globe_H10='GLOBE_H_filt_lanczos_window.nc'
raw_filt_globe_I10='GLOBE_I_filt_lanczos_window.nc'
raw_filt_globe_J10='GLOBE_J_filt_lanczos_window.nc'
raw_filt_globe_K10='GLOBE_K_filt_lanczos_window.nc'
raw_filt_globe_L10='GLOBE_L_filt_lanczos_window.nc'
raw_filt_globe_M10='GLOBE_M_filt_lanczos_window.nc'
raw_filt_globe_N10='GLOBE_N_filt_lanczos_window.nc'
raw_filt_globe_O10='GLOBE_O_filt_lanczos_window.nc'
raw_filt_globe_P10='GLOBE_P_filt_lanczos_window.nc'

raw_data_aster_T01='ASTER_eu_T01.nc'
raw_data_aster_T02='ASTER_eu_T02.nc'
raw_data_aster_T03='ASTER_eu_T03.nc'
raw_data_aster_T04='ASTER_eu_T04.nc'
raw_data_aster_T05='ASTER_eu_T05.nc'
raw_data_aster_T06='ASTER_eu_T06.nc'
raw_data_aster_T07='ASTER_eu_T07.nc'
raw_data_aster_T08='ASTER_eu_T08.nc'
raw_data_aster_T09='ASTER_eu_T09.nc'
raw_data_aster_T10='ASTER_eu_T10.nc'
raw_data_aster_T11='ASTER_eu_T11.nc'
raw_data_aster_T12='ASTER_eu_T12.nc'
raw_data_aster_T13='ASTER_eu_T13.nc'
raw_data_aster_T14='ASTER_eu_T14.nc'
raw_data_aster_T15='ASTER_eu_T15.nc'
raw_data_aster_T16='ASTER_eu_T16.nc'
raw_data_aster_T17='ASTER_eu_T17.nc'
raw_data_aster_T18='ASTER_eu_T18.nc'
raw_data_aster_T19='ASTER_eu_T19.nc'
raw_data_aster_T20='ASTER_eu_T20.nc'
raw_data_aster_T21='ASTER_eu_T21.nc'
raw_data_aster_T22='ASTER_eu_T22.nc'
raw_data_aster_T23='ASTER_eu_T23.nc'
raw_data_aster_T24='ASTER_eu_T24.nc'
raw_data_aster_T25='ASTER_eu_T25.nc'
raw_data_aster_T26='ASTER_eu_T26.nc'
raw_data_aster_T27='ASTER_eu_T27.nc'
raw_data_aster_T28='ASTER_eu_T28.nc'
raw_data_aster_T29='ASTER_eu_T29.nc'
raw_data_aster_T30='ASTER_eu_T30.nc'
raw_data_aster_T31='ASTER_eu_T31.nc'
raw_data_aster_T32='ASTER_eu_T32.nc'
raw_data_aster_T33='ASTER_eu_T33.nc'
raw_data_aster_T34='ASTER_eu_T34.nc'
raw_data_aster_T35='ASTER_eu_T35.nc'
raw_data_aster_T36='ASTER_eu_T36.nc'
aster_prefix='topo.ASTER_orig'
sed -i 's#@aster_prefix@#'"$aster_prefix"'#g' INPUT_ORO

buffer_topo='topography_buffer.nc'
output_topo='topography_COSMO.nc'

raw_data_ndvi='NDVI_1998_2003.nc'
buffer_ndvi='ndvi_buffer.nc'
output_ndvi='ndvi_extpar_cosmo.nc'

raw_data_soil_FAO='FAO_DSMW_DP.nc'
raw_data_soil_HWSD='HWSD0_30_texture_2.nc'
raw_data_deep_soil='HWSD30_100_texture_2.nc'
buffer_soil='soil_buffer.nc'
output_soil='soil_COSMO.nc'

raw_lookup_table_HWSD='LU_TAB_HWSD_UF.data'
raw_HWSD_data='HWSD_DATA_COSMO.data'
raw_HWSD_data_deep='HWSD_DATA_COSMO_S.data'
raw_HWSD_data_extpar='HWSD_DATA_COSMO_EXTPAR.asc'

raw_data_flake='lakedepth.nc'
sed -i 's#@raw_data_flake_filename@#'"$raw_data_flake"'#' INPUT_FLAKE
buffer_flake='flake_buffer.nc'
output_flake='ext_par_flake_cosmo.nc'

# link raw data files to local workdir
ln -s -f ${data_dir}/*.nc .
#________________________________________________________________________________
# run the programs
# the next seven programs can run independent of each other

run_command ${binary_alb}
run_command ${binary_aot}
run_command ${binary_tclim}
run_command ${binary_lu}
run_command ${binary_topo}
run_command ${binary_ndvi}
run_command ${binary_soil}
run_command ${binary_flake}

if [ -f INPUT_AHF ] ; then
  run_command ${binary_ahf}
fi
if [ -f INPUT_ISA ] ; then
  run_command ${binary_isa}
fi
if [ -f INPUT_SGSL ] ; then
  run_command ${binary_sgsl}
fi
#________________________________________________________________________________
# the consistency check requires the output of 
# ${binary_aot}, ${binary_tclim}, ${binary_lu}, ${binary_globe}, 
# ${binary_ndvi}, ${binary_soil} and ${binary_flake}

run_command ${binary_consistency_check}
#________________________________________________________________________________
if (( error_count > 0 ))
then
    echo "CRITICAL: External parameter generation for COSMO model failed!"
else
    echo ">>>> External parameters for COSMO model generated <<<<"
fi
#________________________________________________________________________________


