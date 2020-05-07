#!/bin/bash

# COSMO TECHNICAL TESTSUITE
#
# This script checks whether the Extpar code output is different
# from the reference using the cdo diffv function.  
#
# Author       Katie Osterried, Jonas Jucker
# Maintainer   katherine.osterried@env.ethz.ch

log='diffv.log'

# define fields that can have round-off smaller than 0.001
# names that occur in other names (NDVI -> NDVI_MRAT) must
# have a leading and trailing whitespace
fields_with_roundoff=(" NDVI " "NDVI_MAX" "NDVI_MRAT")

pattern_to_ignore="diffn\|differ"

# clean-up old stuff
[[ ! -f $log ]] ||  rm ${log}

# check environment variables
RUNDIR=${TS_RUNDIR}
VERBOSE=${TS_VERBOSE}
REFOUTDIR=${TS_REFOUTDIR}
if [ -z "${VERBOSE}" ] ; then
  echo "Environment variable TS_VERBOSE is not set" 1>&1
  exit 20 # FAIL
fi
if [ -z "${RUNDIR}" ] ; then
  echo "Environment variable TS_RUNDIR is not set" 1>&1
  exit 20 # FAIL
fi
if [ -z "${REFOUTDIR}" ] ; then
  echo "Environment variable TS_REFOUTDIR is not set" 1>&1
  exit 20 # FAIL
fi
if [ ! -d "${RUNDIR}" ] ; then
  echo "Directory TS_RUNDIR=${RUNDIR} does not exist" 1>&1
  exit 20 # FAIL
fi

if [ ! -f "$REFOUTDIR/external_parameter"* ] ; then
  echo "No netCDF reference file found"  1>&1
  exit 20 # FAIL
fi

FILE=$(ls -1 ${RUNDIR}/external_parameter*.nc 2>/dev/null)
if [ $? -ne 0 ] ; then
  echo "No netCDF output file found"  1>&1
  exit 20 # FAIL
fi


cdo --sortname diffv $REFOUTDIR/external_parameter*.nc $FILE &> $log 

grep Abort $log > /dev/null
if [ $? -ne 1 ] ; then
   echo "CDO abort during file comparison" 1>&1
   exit 20 # FAIL
fi

grep 'differ'  $log > /dev/null
if [ $? -ne 0 ] ; then
    exit 0 # MATCH
else

    grep '0 of'  $log > /dev/null
    if [ $? -ne 0 ] ; then
       echo "Differences from reference file found" 1>&1
       exit 20 # FAIL
    fi

    counter=0
    for field in "${fields_with_roundoff[@]}"; do
        if [[ $counter == "0" ]]; then
            # look for warnings, but ignore 1 of those defined above
            grep -v "$pattern_to_ignore" $log | grep -v "$field"  > field_step_${counter}
        else
            # look for warnings, but ignore 1 of those defined above
            grep -v "$field" field_step_${last_iteration} > field_step_${counter}
        fi
        last_iteration=$counter
        (( counter++ ))
    done

    should_be_zero=`wc -l field_step_${last_iteration} | awk '{print $1}'`

    if [[ $should_be_zero -ne 1 ]];then
        echo "Differences from reference file found" 1>&1
        head field_step_${last_iteration} 1>&1
        exit 20 #FAIL
    else
        rm field_step_*
        exit 10 # OK
    fi
fi
