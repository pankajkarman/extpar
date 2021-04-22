#!/bin/ksh

# import functions to launch Extpar executables
. ./runcontrol_functions.sh

ulimit -s unlimited
ulimit -c 0

# get hostname
hostname="`echo $HOSTNAME`"
logfile="extpar_runscript.log"

rm ${logfile}

#--------------------------------------------------------------------------------
# define host-dependent paths and variables

# CSCS
if [[ $hostname == daint* || $hostname == nid* || $hostname == tsa* ]]; then

    # NetCDF raw data for external parameter
    data_dir=/store/c2sm/extpar_raw_data/linked_data

# mistral
elif [[ $hostname == m* ]]; then

    export OMP_NUM_THREADS=2
    
    # directories
    data_dir=/work/pd1167/extpar-input-data/linked_data

# unkown host
else

    # exit script in case of unknown host
    echo ERROR: Unkown host: $hostname >> ${logfile}
    exit 1
fi
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# define paths and variables independent from host

# directories
currentdir=$(pwd)
rootdir=${currentdir}/../../../../..
src_python=${rootdir}/python/lib

# change dir to src_python to get absolute path
cd $src_python
export PYTHONPATH=$PYTHONPATH:$(pwd)
cd - >> ${logfile}

echo PYTHONPATH: ${PYTHONPATH} >> ${logfile}

# Names of executables

# python executables
binary_alb=extpar_alb_to_buffer.py
binary_ndvi=extpar_ndvi_to_buffer.py
binary_emiss=extpar_emiss_to_buffer.py
binary_tclim=extpar_cru_to_buffer.py
binary_era=extpar_era_to_buffer.py

# fortran executables
binary_lu=extpar_landuse_to_buffer.exe
binary_topo=extpar_topo_to_buffer.exe
binary_aot=extpar_aot_to_buffer.exe
binary_soil=extpar_soil_to_buffer.exe
binary_flake=extpar_flake_to_buffer.exe
binary_isa=extpar_isa_to_buffer.exe
binary_consistency_check=extpar_consistency_check.exe

# link raw data files to local workdir
ln -s -f ${data_dir}/*.nc .
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# define test-specific paths and variables 

type_of_test=`echo $currentdir | rev | cut -d"/" -f2 | rev`
name_of_test=`echo $currentdir | rev | cut -d"/" -f1 | rev`

# allowed tests for testsuite
if [[ $type_of_test == mpim || $type_of_test == dwd || $type_of_test == ecmwf ]]; then

    echo Current test is $type_of_test/$name_of_test  >> ${logfile}

#unknown test
else

    # exit script in case of unknown host
    echo ERROR: Unkown test: $type_of_test >> ${logfile}
    exit 1
fi

#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# launch extpar executables

echo ">>>> Data will be processed and produced in `pwd` <<<<"

# 1) topography needs to be processed first - result is input for the
#    CRU data processing

run_sequential ${binary_topo}

#________________________________________________________________________________
# 2) all other executables

run_sequential ${binary_alb}

run_sequential ${binary_ndvi}

run_sequential ${binary_tclim}

run_sequential ${binary_aot}

run_sequential ${binary_lu}

run_sequential ${binary_soil}

run_sequential ${binary_flake}

if [[ $type_of_test == mpim ]]; then
    run_sequential ${binary_emiss}
fi

if [[ $name_of_test == icon_d2 || $name_of_test == icon_d2_caching ]]; then
    run_sequential ${binary_era}
fi

run_sequential ${binary_consistency_check}
#________________________________________________________________________________

echo ">>>> External parameters for ICON model generated <<<<"
