#! /bin/bash
#_______________________________________________________________________________
#
# EMISS processing
#
#&emiss_raw_data
#  raw_data_emiss_path='/e/rhome/routfor/routfox/extpar/rawdata/',
#  raw_data_emiss_filename='CAM_bbe_int_2010-2015_lw.nc'
#/
#&emiss_io_extpar
# emiss_buffer_file='emiss_BUFFER.nc',
# emiss_output_file='emiss_ICON.nc'
#/
# EXAMPLE:
#
# ./extpar_emiss_to_buffer.sh -r CAM_bbe_int_2010-2015_lw.nc \
#                                -b emiss_BUFFER.nc \    
#                                -g icon_grid_0026_R03B07_G.nc \
#                                -p /e/rhome/routfor/routfox/extpar/rawdata/
#_______________________________________________________________________________
# disable core dumps
ulimit -c 0
# limit stacksize
ulimit -s unlimited

set -x

EXTPAR_HOME=~routfor/routfox/extpar

set -eu
#_______________________________________________________________________________

usage() {
    echo "Usage: $0 <options>"
    echo 
    echo "Required Options:"
    echo 
    echo "   -r <emiss raw data file>"
    echo "   -b <output buffer file>"    
    echo "   -g <ICON grid file>"
    echo 
    echo "Optional options:"
    echo 
    echo "  [-p <raw data path>]"
}

raw_data_path=""
raw_data_emiss=""
buffer_emiss=""
icon_grid_file=""

while getopts ":r:b:g:p:" opt; do
  case $opt in
    r)
      raw_data_emiss="$OPTARG"
      ;;
    b)
      buffer_emiss="$OPTARG"
      ;;
    g)
      icon_grid_file="$OPTARG"
      ;;
    
    p)
      raw_data_path="$OPTARG"
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      usage
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires arguments" >&2
      usage
      exit 1
      ;;
  esac
done

raw_data_dir="${raw_data_path%%/}"

test -f "${raw_data_dir}/${raw_data_emiss}" || echo "ERROR: emiss raw data could not be found"
test -f "${icon_grid_file}"                 || echo "ERROR: ICON grid file could not be found" 

rm -rf bbemis_0.nc
rm -rf bbemis_1.nc
rm -rf bbemis_2.nc

cp ${raw_data_dir}/${raw_data_emiss} bbemis_0.nc

# Check of artificial low values (useful range is between approx. 0.6 and 1. for earth surface)
cdo -expr,'bbemis_longwave=(bbemis_longwave<0.5)?-999:bbemis_longwave;' bbemis_0.nc bbemis_1.nc
# Ensure artificial low values are set to missing
cdo setmissval,-999 bbemis_1.nc bbemis_2.nc


export OMP_NUM_THREADS=8

# proper CF convention files for checking 
output_emiss=${buffer_emiss/_BUFFER.nc/_ICON.nc}

rm -rf weights.nc

cdo -f nc4 -P ${OMP_NUM_THREADS} \
    genycon,$icon_grid_file  bbemis_2.nc  weights.nc
cdo -f nc4 -P ${OMP_NUM_THREADS} \
    settaxis,1111-01-01,0,1mo -remap,$icon_grid_file,weights.nc bbemis_2.nc emiss-icon.nc

${EXTPAR_HOME}/job/cdo2emiss-buffer.py

mv emiss-icon.nc ${output_emiss}
mv emiss-icon_BUFFER.nc ${buffer_emiss}

exit 0
