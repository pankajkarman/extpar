#!/bin/ksh
#
#  Interpolate ERA interim monthly means of SST and W_SNOW to ICON grid
#  CAUTION!!! Runs ONLY on LCE not on XCE !!!
#
#cd /lustre2/uscratch/mlange/ecmwf/ERAinterim

#for icon_res in R2B06 R3B07 R2B05 R3B05 R3B06 R2B07; do

#for icon_res in 0024_R02B06_G 0026_R03B07_G 0030_R02B05_G 0033_R03B05_G 0036_R03B06_G 0039_R02B07_G; do
#gpid=1
#for icon_res in 0027_R03B08_N02 0028_R02B07_N02 0031_R02B06_N02 0034_R03B06_N02 0037_R03B07_N02 0040_R02B08_N02; do
#gpid=2

icon_res=$1 
gpid=1

module unload cdo
module load unsupported
module load netcdf
module load nco/4.6.2
module load cdo/1.9.1-prerelease

#ulimit -f 32777216

icon_grid_file=icon_grid_${icon_res}.nc
# directory containing target grid
#OUT_GRIDDIR="/e/rhome/routfor/routfox/icon/grids/public/edzw"
OUT_GRIDDIR=$2
WORKDIR=$3
[[ -d ${WORKDIR} ]] || mkdir -p $WORKDIR
cd $WORKDIR
#rm *
# data from /lustre2/uscratch/hfrank/ERAinterim
HFDATA=/lustre2/uscratch/jhelmert/ep_netcdf/ERAinterim
integer retrieve=1
integer interpolate=1
do_remap=1

#typeset modul_iconremap=iconremap_mpi
typeset modul_iconremap=/e/rhome/routfor/routfox/abs/iconremap
typeset extpar_date=20161004
typeset validi_date=${extpar_date}00         # localValidityDate*
#typeset validi_date=2016021400               # localValidityDate*

typeset ifs_file
ifs_file="ei_an1986-2015"

typeset -Z2 mo
# 1-netCDF 2-grib2
integer out_format=1 


[[ -s ${ifs_file}.mean ]] || cp ${HFDATA}/${ifs_file}.mean .
if [[ ! -s ${ifs_file}.mean ]]; then
   echo "$0 error: ERA Interim input data are missing, run $0 using retrieve=1"
   exit
fi

if (( retrieve == 1 )); then
#
# Retrieve monthly means of ERA Interim data from mars
#
    typeset dates
    integer year month

    year=1986
    while (( year <= 2015 )); do
        month=1
        while (( month <= 12 )); do
            mo=${month}
            dates="${dates}/${year}${mo}01"
            month=month+1
        done
        year=year+1
    done
    dates=${dates:1}
echo "retrieve skt/sd/sst to $ifs_file"
    cat <<EOF
retrieve, class=ei, stream=moda, type=an, expver=1,
  date=${dates},
  repress=gg, resol=av, grid=128, gaussian=regular,
  levtype=sfc, param=skt/sd/sst,
  target="${ifs_file}"
EOF

 cp ${HFDATA}/${ifs_file} .

#   Calculate mean over all years
    cdo ymonmean ${ifs_file} ${ifs_file}.mean_orig

    ifs_mean=${ifs_file}.mean_orig

#   Fill missing SST on land points
#    cdo fillmiss ${ifs_file}.mean_orig ${ifs_file}.mean

#   Fill missing SST on land points with SKT
    grib_copy ${ifs_mean} ${ifs_mean}.[shortName]



#   Fill with nearest neighbor
    sst_mean=${ifs_mean}.sst
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


#    cdo ifthenelse -setmisstoc,0 ${ifs_file}.mean_orig.sst \
#                   ${ifs_file}.mean_orig.sst    \
#                   ${ifs_file}.mean_orig.skt    \
#                   ${ifs_file}.mean_orig.sst_skt

    cdo -O setmisstoc,0 ${sst_mean}.xy2 msk
    cdo ifthenelse msk ${sst_mean}.xy2 ${ifs_mean}.skt ${ifs_mean}.sst_skt

    cat ${ifs_file}.mean_orig.sst_skt \
        ${ifs_file}.mean_orig.skt     \
        ${ifs_file}.mean_orig.sd      > ${ifs_file}.mean

    rm ${ifs_file}.mean_orig.sst_skt \
       ${ifs_file}.mean_orig.sst     \
       ${ifs_file}.mean_orig.skt     \
       ${ifs_file}.mean_orig.sd
fi

if (( interpolate == 1 )); then
#
#  Interpolate from IFS to ICON grid
#
    typeset extpar_date=20161004
#eval $(CONST=${CONST:-~routfor/routfox/const} eval_ires ${Dflag} -p || print -- exit $?)
#    typeset icon_dyngrid_fn icon_extpar_fn icon_n1_dyngrid_fn icon_n1_extpar_fn
#    eval $(icon_grid_files ${icon_res} ${extpar_date} || print -- exit $?)

    integer nt=20    # number of parallel threads
    typeset intp_method
#   intp_method=2            # area-weighted formula
#   intp_method=3            # RBF scalar
    intp_method=4            # nearest-neighbor scalar
#   intp_method=8            # barycentric scalar

    typeset remap_nml=ei_an2icon.nml$$

    integer month

    set -A locCreateDate $(datconv -C $(date +%Y%m%d%H))
    set -A valdate $(datconv -C ${validi_date})

#
#  Write rule files for grib_filter
#
    typeset split_month=split_month$$
    typeset split_var_month=split_shortName_month$$

    cat > ${split_month} <<REOFM
write "ei2icon.[month]";
REOFM

    cat > ${split_var_month} <<REOFV
write "ei2icon.[shortName]_[month]";
REOFV

    typeset set_localSection=set_localSection$$

    grib_filter ${split_month} ${ifs_file}.mean

    for ires in ${icon_res} 
    do
        ncstorage_file=ei2icon${ires}_storage.nc
        icon_file=${ifs_file}_${ires}

# set gpid=1 for global grids
icon_grid=${OUT_GRIDDIR}/${icon_grid_file}
icon_extpar=${OUT_GRIDDIR}/icon_extpar_${icon_res}_${extpar_date}_tiles.g2

#        grib_copy -w shortName=FR_LAND ${icon_extpar} fr_land.${ires}
#        cdo ltc,0.95 fr_land.${ires} water.${ires}

#set land points to missing value
#cdo setrtomiss,-1,0.05 water.r3b07 water_miss.r3b07

        cat > ${set_localSection} <<REOF
set grib2LocalSectionPresent = 0;
if ( shortName is 'H_SNOW') {
    set shortName = 'W_SNOW';
}
set centre    = 78; 
set grib2LocalSectionPresent = 1;
set subCentre = 255;
set year   = 1111;
set day    = 11;
set hour   = 11;
set minute = 0;
set localDefinitionNumber = 254; 
set localCreationDateYear =  ${locCreateDate[1]};
set localCreationDateMonth = ${locCreateDate[2]};
set localCreationDateDay =   ${locCreateDate[3]};
set localCreationDateHour =  ${locCreateDate[4]};
set localValidityDateYear =  ${valdate[1]};
set localValidityDateMonth = ${valdate[2]};
set localValidityDateDay =   ${valdate[3]};
set localValidityDateHour =  ${valdate[4]};
set shapeOfTheEarth = 6;
set typeOfGeneratingProcess = 9;
set backgroundProcess = 0;
set generatingProcessIdentifier = ${gpid};
set significanceOfReferenceTime = 0;
set typeOfProcessedData = 0;
set bitsPerValue = 16;
write;
REOF


#       iconremap sets one date for all GRIB records. Therefore, the interpolation must
#       be done separately for each month.
        for month in $(seq 1 12)
        do
            mo=${month}
if [[ $do_remap -eq 1 ]]; then
cat > ${remap_nml} <<REMAP_EOF
&remap_nml
 in_grid_filename="ei2icon.${month}",
 in_filename="ei2icon.${month}",
 in_type  = 1,
 out_grid_filename="${OUT_GRIDDIR}/${icon_grid_file}",
 out_filename="${icon_file}.tmp",
 out_type = 2,                    ! ICON triangular grid
 out_filetype=2,                  ! output format 1-netCDF 2-GRIB2
! ncstorage_file="${ncstorage_file}"
 out_mask_filename="${OUT_GRIDDIR}/icon_extpar_${icon_res}_${extpar_date}_tiles.g2",
  extra_grib_keys_int = "centre",78, "subCentre",255, 
                       "year",1111, "day",11, "hour",11, "minute",0,
                       "grib2LocalSectionPresent",1, "localDefinitionNumber",254, 
                       "localCreationDateYear", ${locCreateDate[1]}, "localValidityDateYear", ${valdate[1]},
                       "localCreationDateMonth",${locCreateDate[2]}, "localValidityDateMonth",${valdate[2]},
                       "localCreationDateDay",  ${locCreateDate[3]}, "localValidityDateDay",  ${valdate[3]},
                       "localCreationDateHour", ${locCreateDate[4]}, "localValidityDateHour", ${valdate[4]},
                       "shapeOfTheEarth",6, "typeOfGeneratingProcess",9, "backgroundProcess",0,
                       "generatingProcessIdentifier", ${gpid},
                       "significanceOfReferenceTime",0, "typeOfProcessedData",0,
                       "bitsPerValue",16
/
&input_field_nml
 inputname='SKT',
 outputname='T_S',
 code = 235,
 intp_method=${intp_method}
!missval    = 200.
!var_out_mask = 'FR_LAND'
!out_mask_below = .TRUE.
!out_mask_threshold = 0.95
/
&input_field_nml
 inputname='SST',
 outputname='T_SEA',
 code = 34,
!intp_method=${intp_method}
 intp_method=4,
!missval    = 271.15
!var_out_mask = 'FR_LAND'
!out_mask_below = .TRUE.
!out_mask_threshold = 0.95
!missval    = 271.15
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
!missval    = 0
/
REMAP_EOF

            cat ${remap_nml}

            OMP_NUM_THREADS=${nt} \
            ${modul_iconremap} -vv --remap_nml=${remap_nml}
#            cat nml.log
#           rm -f nml.log
fi #do_remap

#           split up the interpolated ICON file for different variables
            grib_filter ${split_var_month} ${icon_file}.tmp

#           Use T_SEA (sst) at water points and T_S (interpolated skin temp. skt) at land points
            #cdo ifthenelse water.${ires} ei2icon.T_SEA_${month} ei2icon.T_S_${month} ei2icon.T_SEA.${mo}
#ml         Use T_SEA (sst) at water points and missing values over "not water covered" gridpoints
#            cdo div ei2icon.T_SEA_${month} water.${ires} ei2icon.T_SEA.${mo}
	    cp ei2icon.T_SEA_${month}  ei2icon.T_SEA.${mo}

#           combine T_S and W_SNOW and set local GRIB information
#           rename T_SEA to T_S, H_SNOW to W_SNOW
            grib_filter -o ${icon_file}.${mo} ${set_localSection} ei2icon.T_SEA.${mo} ei2icon.H_SNOW_${month}

#exit
#            rm ${icon_file}.tmp
            #rm ei2icon.T_S_${month} ei2icon.T_SEA_${month} ei2icon.H_SNOW_${month} ei2icon.T_SEA.${mo}
#            mv ei2icon.T_S_${month} ../ei2icon.T_S_${mo}_${ires}
#            rm ei2icon.T_SEA_${month} ei2icon.H_SNOW_${month} ei2icon.T_SEA.${month} ei2icon.T_S_${month}
        done

        cp ${icon_file}.?? $TMP/
        cat ${icon_file}.?? > ${icon_file}.g2

    done

    #ml convert to netCDF for extpar
    if (( out_format == 1 )); then
       cdo -f nc copy  ${icon_file}.g2  ${icon_file}.nc
    fi

#Beautify the resulting netcdf to use it in EXTPAR consistency check that expects a special buffer format time,ie,je,ke
#needs module nco!!!
#rename first dimension into ie
/hpc/rhome/software/nco/4.6.2/bin/ncrename -d ncells,ie ei_an1986-2015_${ires}.nc ei_an1986-2015_${ires}_ie.nc
#add second dimension je -> Caution! ordering of dimensions in opposite in NetCDF and Fortran
/hpc/rhome/software/nco/4.6.2/bin/ncap2 -s 'defdim("je",1);T_SEA_ieje[$time,$je,$ie]=T_SEA' -s 'W_SNOW_ieje[$time,$je,$ie]=W_SNOW' -O ei_an1986-2015_${ires}_ie.nc ei_an1986-2015_${ires}_ieje.nc
#add third dimension ke
/hpc/rhome/software/nco/4.6.2/bin/ncap2 -s 'defdim("ke",1);T_SEA_iejeke[$time,$ke,$je,$ie]=T_SEA_ieje' -s 'W_SNOW_iejeke[$time,$ke,$je,$ie]=W_SNOW_ieje' -O ei_an1986-2015_${ires}_ieje.nc ei_an1986-2015_${ires}_iejeke.nc
# delete old variables
/hpc/rhome/software/nco/4.6.2/bin/ncks -C -O -x -v T_SEA,T_SEA_ieje,W_SNOW,W_SNOW_ieje  ei_an1986-2015_${ires}_iejeke.nc ei_an1986-2015_${ires}_iejeke_tmp.nc
# rename variables back
/hpc/rhome/software/nco/4.6.2/bin/ncrename -O -v T_SEA_iejeke,T_SEA -v W_SNOW_iejeke,W_SNOW ei_an1986-2015_${ires}_iejeke_tmp.nc ei_an1986-2015_${ires}_BUFFER.nc

# cp /e/rhome/routfor/routfox/icon/grids/public/edzw/icon_extpar_0024_R02B06_G_20161124.nc .
#/e/uhome/jhelmert/bin/nco-4.4.7/src/nco/ncks -C -A ei_an1986-2015_${ires}_MODIF.nc -v T_SEA icon_extpar_${ires}_20161124.nc

#    cd ../
#    rm ${remap_nml}
#    rm ${split_month} ${split_var_month} ${set_localSection}
#    rm ei2icon.*

fi
