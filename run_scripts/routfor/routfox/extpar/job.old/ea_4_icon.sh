#!/bin/ksh -l

#PBS -w NAME=extpar_monmeanERA5
#PBS -N ${NAME}
#PBS -S /bin/ksh
#PBS -q rc_big
#PBS -m n
#PBS -r n
#PBS -l cpunum_job=1
#PBS -l memsz_prc=12gb
#PBS -l vmemsz_prc=12gb
#PBS -l memsz_job=120gb
#PBS -l vmemsz_job=120gb
#PBS -l elapstim_req=23400
#PBS -o %loggingfile%
#PBS -j o
#
#  CAUTION!!! Runs ONLY on LCE not on XCE !!!
#
#  Calculated climatological means from monthly means of the ERA5 data
#  Interpolate SST, W_SNOW, T_2M to ICON grids
#
#  The same script is at ecgate:~dw7/ICON/Invar
#  It is best if the ERA5 data is retrieved at ecgate.
#
#     Helmut Frank, 12.07.2019
#
set -x

# exit on error:
set -e

integer retrieve=0              # retrieve ERA5 data from MARS and calculate means over several years
                                # The retrieval from MARS should be done at ECMWF
integer interpolate=1           # interpolate ERA5 data to ICON grid
integer debug=0

#ulimit -s unlimited
ulimit -c 0

typeset icon_grid_file extpar_date out_griddir workdir

icon_grid_file=${1:-%igrid%}
extpar_date=${2:-%extpar_date%}
out_griddir=${3:-%igrid_dir%}
workdir=${4:-%iwork_dir%}
PROGDIR=%progdir_lc%

if [[ ${icon_grid_file} == '-h' ]]; then
    cat <<HELP

   $0 [icon_grid_file] [extpar_date] [out_griddir] [workdir]

Interpolate climatological fields of sst, sd, and 2t from ERA5 to
monthly means of T_SEA, W_SNOW, T_2M_CL on ICON grids.

The most important parameter is the icon_grid_file.

HELP
    exit
fi

# Set default values
if (( retrieve == 0 )); then
    if [[ "${icon_grid_file}" == %igrid* ]]; then
        icon_grid_file=~routfor/routfox/icon/grids/public/edzw/icon_grid_0026_R03B07_G.nc
    fi
    if [[ "${extpar_date}" == %extpar_date* ]]; then
        extpar_date=$(grep extpar_date= ~routfor/routfox/const/iglo/param_iglo)
        extpar_date=${extpar_date#extpar_date=}
    fi
fi
if [[ "${out_griddir}" == %igrid_dir* ]]; then
    out_griddir=${WORK-$SCRATCH}/era2icon
    if [[ ! -d ${out_griddir} ]]; then
        mkdir -p ${out_griddir}
    fi
elif [[ ! -d ${out_griddir} ]]; then
    print -- "Error! out_griddir=${out_griddir} does NOT exist!" >& 2
    exit 5
fi
if [[ "${workdir}" == %iwork_dir* ]]; then
    workdir=${SCRATCHDIR:-$TMPDIR}
fi

 module load cdo
 module load netcdf4/4.7.3-x86-intel

typeset icon_res
#icon_res=$(basename ${icon_grid_file})
icon_res=${icon_grid_file#*icon_grid_}
icon_res=${icon_res%.nc}

[[ -d ${workdir} ]] || mkdir -p ${workdir}
cd ${workdir}

typeset icon_grid_dir=${ICON_GRID_DIR:-~routfor/routfox/icon/grids/public/edzw}
typeset gpid old_extpar
old_extpar=$(ls -r ${icon_grid_dir}/icon_extpar_${icon_res}*.g2 | head -1)
if [[ -s ${old_extpar} ]]; then
    gpid=$(grib_get -p generatingProcessIdentifier -w count=1 ${old_extpar})
else
    gpid=1
fi

integer year1 year2   
year1=1990    # first year of ERA5 data to use
year2=2019    # last year of ERA5 data to use

typeset id   # ID of ERA5 data interpolated to ICON
id=ea2icon

if (( debug > 0 )); then set -x; fi

function rm_or_mv {
    if (( debug > 0 )); then
        set -x
        set +e
        mv $* ${TMPDIR}
        set -e
    else
        rm -f $*
    fi
}

typeset modul_iconremap=${MODUL_ICONREMAP:-iconremap}
if [[ "${modul_iconremap}" == iconremap* ]] ; then
  modul_iconremap=$(PATH=${PROGDIR} whence ${modul_iconremap})
fi

typeset valid_day=${extpar_date}
typeset validity_date=${valid_day}00         # localValidityDate*

typeset ifs_file era_oro era_clim
ifs_file="ea_an${year1}-${year2}"
era_oro="ea_oro_${year1}"

era_clim=${ifs_file}.mean     # climatological fields from ERA5 reanalysis
typeset -Z2 mo

typeset era_dir
# if [[ ${whoami} == routfor ]]; then
    era_dir=~routfor/routfox/extpar/rawdata/ERA5
# else
#     era_dir=${SCRATCH:-$TMPDIR}/ERA5
# fi

###############################
#                             #
#  Retrieve ERA5 climatology  #
#                             #
###############################
if (( retrieve == 1 )); then
#
#   Retrieve monthly means of ERA5 data from MARS
#
    [[ ! -d ${era_dir} ]] && mkdir -p ${era_dir}

    typeset req=${SCRATCHDIR:-$TMPDIR}/ea_4_icon.req
    rm -f ${req}
    typeset dates
    integer year decade month
    typeset era_in_mars era_grid
    era_in_mars='class=ea, stream=moda, type=an, expver=1'
    era_grid='resol=av, grid=F320'

#   Retrieve the orography of the ERA data
    decade=$(( (year1/10)*10))
    cat >> ${req} <<OROREQ
retrieve, ${era_in_mars}, date=${year1}0101, decade=${decade}, ${era_grid},
  levtype=sfc, param=z, target="${era_oro}.z"
OROREQ

    year=year1
    while (( year <= year2 )); do
        decade=$(( (year/10)*10))
        dates="${year}0101"
        month=2
        while (( month <= 12 )); do
            mo=${month}
            dates="${dates}/${year}${mo}01"
            month=month+1
        done
        cat >> ${req} <<MARSREQ
retrieve, ${era_in_mars}, date=${dates}, decade=${decade}, ${era_grid},
  levtype=sfc, param=skt/sd/sst/2t, target="${ifs_file}"
MARSREQ

        year=year+1
    done
    dates=${dates:1}

    cat ${req}
    mars ${req}
#
#   Calculate mean over all years
#
    typeset ifs_mean
    ifs_mean=${ifs_file}.mean_orig
    cdo ymonmean ${ifs_file} ${ifs_mean}

#   Fill missing SST on land points
#   cdo fillmiss ${ifs_mean}.mean_orig ${ifs_file}.mean

#   Extract sst
    grib_copy ${ifs_mean} ${ifs_mean}.[shortName]
    sst_mean=${ifs_mean}.sst

#   Fill sst at coasts with nearest neighbor
    cdo shiftx,1,cyclic  ${sst_mean} ${sst_mean}.x+1
    cdo shiftx,-1,cyclic ${sst_mean} ${sst_mean}.x-1
    cdo shifty,1         ${sst_mean} ${sst_mean}.y+1
    cdo shifty,-1        ${sst_mean} ${sst_mean}.y-1

    cdo -O setmisstoc,0 ${sst_mean} msk
    cdo ifthenelse msk ${sst_mean} ${sst_mean}.x+1 ${sst_mean}.xx1
    cdo -O setmisstoc,0 ${sst_mean}.xx1 msk
    cdo ifthenelse msk ${sst_mean}.xx1 ${sst_mean}.x-1 ${sst_mean}.xx2
    cdo -O setmisstoc,0 ${sst_mean}.xx2 msk
    cdo ifthenelse msk ${sst_mean}.xx2 ${sst_mean}.y+1 ${sst_mean}.xy1
    cdo -O setmisstoc,0 ${sst_mean}.xy1 msk
    cdo ifthenelse msk ${sst_mean}.xy1 ${sst_mean}.y-1 ${sst_mean}.xy2

#   Fill missing values of sst with skt
    cdo -O setmisstoc,0 ${sst_mean}.xy2 msk
    cdo ifthenelse msk ${sst_mean}.xy2 ${ifs_mean}.skt ${ifs_mean}.sst_skt

    cat ${ifs_file}.mean_orig.sst_skt \
        ${ifs_file}.mean_orig.sd      \
        ${ifs_file}.mean_orig.2t      \
        ${ifs_file}.mean_orig.skt     \
      > ${ifs_file}.mean

    rm_or_mv ${ifs_file}.mean_orig.sst_skt \
             ${ifs_file}.mean_orig.sst     \
             ${ifs_file}.mean_orig.skt     \
             ${ifs_file}.mean_orig.sd      \
             ${ifs_file}.mean_orig.2t

#   Convert from geopotential to height
#   typeset climdate="centuryOfReferenceTimeOfData=12,yearOfCentury=11,day=11,hour=0"
    float ginv=1./9.80665  # to convert from geopotential to heights
#   grib_set -s indicatorOfParameter=8,table2Version=2,scaleValuesBy=${ginv},${climdate} \
    grib_set -s indicatorOfParameter=8,table2Version=2,scaleValuesBy=${ginv} \
        ${era_oro}.z ${era_dir}/${era_oro}

    mv ${ifs_file}.mean ${era_dir}
    rm_or_mv ${era_oro}.z \
             ${ifs_file} \
             ${ifs_mean} ${ifs_mean}.sst.[xy]?[12]
fi

#######################################
#                                     #
#  Interpolate from IFS to ICON grid  #
#                                     #
#######################################
if (( interpolate == 1 )); then
    integer nthreads=10      # number of parallel threads
    typeset intp_method      #   Interpolation method
#   2: area-weighted formula, 3: RBF scalar, 4: nearest-neighbor scalar, 8: barycentric scalar
    intp_method=4            # nearest-neighbor scalar

#   time for local validity date
    typeset now
    now=$(date +%Y%m%d%H)
    if (( validity_date < now )) ; then
        validity_date=${now}
    fi
    set -A locCreateDate $(datconv -C ${now})
    set -A valdate $(datconv -C ${validity_date})

    typeset ncstorage_file icon_file remap_nml
    ncstorage_file=${id}${icon_res}_storage.nc
    icon_file=${ifs_file}_${icon_res}

#   iconremap sets one date for all GRIB records. Therefore, the interpolation must
#   be done separately for each month.
#   grib_copy -w dataDate!=10101 ${era_dir}/${era_clim} ${id}.[month]
    typeset filter=era_filter.$$
    cat > ${filter} <<FILTER_EOF
if ( dataDate == 10101 ) {
    write "${id}.0";
} else {
    if ( shortName is 'HSURF' ) {
        set dataDate = 10101;
        set stepType = 'instant';
        write "${id}.0";
    } else {
        write "${id}.[month]";
    }
}
FILTER_EOF
    grib_filter ${filter} ${era_dir}/${era_clim} ${era_dir}/${era_oro}

    integer month year day hour
    for month in $(seq 0 12)
    do
        mo=${month}
        remap_nml=ea_an2icon${icon_res}_${month}.nml$$
        if (( month == 0 )); then
            year=1
            day=1
            hour=0
        else
            year=1111
            day=11
            hour=11
        fi
            

cat > ${remap_nml} <<REMAP_NML_EOF
&remap_nml
 in_grid_filename="${id}.${month}",
 in_filename="${id}.${month}",
 in_type  = 1,
 out_grid_filename="${icon_grid_file}",
!out_filename="${icon_file}.tmp",
 out_filename="${icon_file}.${mo}",
 out_type = 2,                    ! ICON triangular grid
 out_filetype=2,                  ! output format GRIB2
!out_mask_filename="${icon_extpar_file}",
 ncstorage_file="${ncstorage_file}"
 extra_grib_keys_int = "centre",78, "subCentre",255, 
                       "year",${year}, "day",${day}, "hour",${hour}, "minute",0,
                       "grib2LocalSectionPresent",1, "localDefinitionNumber",254, 
                       "localCreationDateYear", ${locCreateDate[1]}, "localValidityDateYear", ${valdate[1]},
                       "localCreationDateMonth",${locCreateDate[2]}, "localValidityDateMonth",${valdate[2]},
                       "localCreationDateDay",  ${locCreateDate[3]}, "localValidityDateDay",  ${valdate[3]},
                       "localCreationDateHour", ${locCreateDate[4]}, "localValidityDateHour", ${valdate[4]},
                       "shapeOfTheEarth",6, "typeOfGeneratingProcess",9, "backgroundProcess",0,
                       "generatingProcessIdentifier", ${gpid},
                       "significanceOfReferenceTime",0, "typeOfProcessedData",0,
                       "bitsPerValue",16,
!                      "productDefinitionTemplateNumber",8
/
REMAP_NML_EOF
if (( mo > 0 )); then
    cat >> ${remap_nml} <<INPUT_EOF
&input_field_nml
 inputname='SST',
 outputname='T_SEA',
 code = 34,
 intp_method=4,
! var_out_mask = 'FR_LAND'
! out_mask_below = .TRUE.
! out_mask_threshold = 0.95
! missval    = 271.15
! SST has a bitmap. Use it as mask for the input field
! var_in_mask = 'SST',
! code_in_mask = 34,
! in_mask_threshold = 270.
! in_mask_below = .TRUE.
/
&input_field_nml
 inputname='SD',
 outputname='W_SNOW',
 code = 141,
 intp_method=${intp_method}
! var_out_mask = 'FR_LAND'
! out_mask_below = .FALSE.
! out_mask_threshold = 0.05
! missval    = 0
/
&input_field_nml
 inputname='2T',
 outputname='T_2M',
 code = 167,
 intp_method=${intp_method}
/
INPUT_EOF
#! &input_field_nml     ! Use sst (T_SEA) instead of skt (T_S)
#!  inputname='SKT',
#!  outputname='T_S',
#!  code = 235,
#!  intp_method=${intp_method}
#! ! missval    = 200.
#! ! var_out_mask = 'FR_LAND'
#! ! out_mask_below = .TRUE.
#! ! out_mask_threshold = 0.95
#! /

else
    cat >> ${remap_nml} <<INPUT_EOF0
&input_field_nml
 inputname='2T',
 outputname='T_2M',
 code = 167,
 intp_method=4,
/
&input_field_nml
 inputname='HSURF',
 outputname='HSURF',
 code = 8,
 intp_method=${intp_method}
/
INPUT_EOF0

fi

        cat ${remap_nml}

        OMP_NUM_THREADS=${nthreads} \
        CDI_ECCODES_GRIB1=1 \
        ${modul_iconremap} -vv --remap_nml=${remap_nml}
        if [[ -r nml.log ]]; then
            cat      nml.log
            rm_or_mv nml.log
        fi
        rm_or_mv ${remap_nml}
    done

#   Rename H_SNOW to W_SNOW and concatenate all month to one file
#   grib_set -w shortName=H_SNOW -s shortName=W_SNOW ${icon_file}.??  ${out_griddir}/${icon_file}.g2
    grib_set -w shortName=H_SNOW -s shortName=W_SNOW ${icon_file}.??  ${icon_file}.grib2
    typeset rename_rules
    rename_rules=rename_rules.$$
    cat > ${rename_rules} <<RULES_EOF
if ( shortName is 'H_SNOW' ) {
    set shortName = 'W_SNOW';
}
if ( shortName is 'T_G' ) {
    set productDefinitionTemplateNumber = 8;
    if ( dataDate == 10101 ) {
        set shortName = 'T_CL';
    } else {
        set shortName = 'T_2M_CL';
    }
}
write;
RULES_EOF
#   grib_filter -o ${out_griddir}/${icon_file}.g2 ${rename_rules} ${icon_file}.??
    grib_filter -o ${icon_file}.grib2 ${rename_rules} ${icon_file}.??
    rm_or_mv ${rename_rules}

    grib_list_dwd -v -P localValidityDateYear,localValidityDateMonth,localValidityDateDay,localValidityDateHour \
            -stat ${icon_file}.grib2

#   convert to netCDF for extpar
    cdo -a -f nc copy  ${icon_file}.grib2  ${icon_file}.nc
    ncdump -h ${icon_file}.nc

#   rm_or_mv ${split_month}
    rm_or_mv ncstorage_file
    rm_or_mv ${id}.*

fi

#
# Beautify the resulting netcdf to use it in EXTPAR consistency check which expects a special buffer
# format time,ie,je,ke
#
 module load unsupported
 module load nco/4.8.1

# remove depth, height
mv ${icon_file}.nc ${icon_file}.nc_tmp
ncwa -C -x -v depth,height -a depth,height ${icon_file}.nc_tmp ${icon_file}.nc.tmp

ncrename -O -v HSURF,TOPO_CLIM ${icon_file}.nc.tmp ${icon_file}.nc
rm ${icon_file}.nc.tmp ${icon_file}.nc_tmp

#add dimensions je,ke -> Caution! ordering of dimensions is opposite in NetCDF and Fortran
ncap2 -s 'defdim("je",1); defdim("ke",1)'           \
      -s 'T_SEA_tmp[$time,$ke,$je,$ncells]=T_SEA'   \
      -s 'W_SNOW_tmp[$time,$ke,$je,$ncells]=W_SNOW' \
      -s 'T_2M_CL_tmp[$time,$ke,$je,$ncells]=T_2M_CL' \
      -O ${icon_file}.nc  ${icon_file}_tmp.nc

# delete old variables
ncks -C -O -x -v T_SEA,W_SNOW,T_2M_CL ${icon_file}_tmp.nc ${icon_file}_Tmp.nc

# rename variables back, and dimension ncells to ie
ncrename -O -d ncells,ie -v T_SEA_tmp,T_SEA -v W_SNOW_tmp,W_SNOW \
         -v T_2M_CL_tmp,T_2M_CLIM \
         ${icon_file}_Tmp.nc  ${icon_file}_BUFFER.nc

# @ckoziar, 20200518: split up result file to fullfill consistency_check requirements:
ncks -O -v T_SEA,W_SNOW ${icon_file}_BUFFER.nc ei_an${year1}-${year2}_${icon_res}_BUFFER.nc
ncks -O -v TOPO_CLIM,T_2M_CLIM ${icon_file}_BUFFER.nc ei_2t_an${year1}-${year2}_${icon_res}_BUFFER.nc

# clean up
rm ${icon_file}_tmp.nc ${icon_file}_Tmp.nc
