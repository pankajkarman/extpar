#!/bin/ksh -l

#PBS -w NAME=extpar_modules
#PBS -N ${NAME}
#PBS -S /bin/ksh
#PBS -q rc_big
#PBS -m n
#PBS -r n
#PBS -l cpunum_job=1
#PBS -l memsz_prc=192gb
#PBS -l vmemsz_prc=192gb
#PBS -l memsz_job=384gb
#PBS -l vmemsz_job=384gb
#PBS -l elapstim_req=23400
#PBS -o %loggingfile%
#PBS -j o

set -x

# exit on error:
set -e

# module load unsupported
# module unload intel/14.0.0.080
module load intel
# module unload python
module load netcdf4/4.7.3-x86-intel
module load cdo
module load nco/4.8.1

module load python/2020.1

ulimit -s unlimited
ulimit -c 0

#________________________________________________________________________________
error_count=0
run_command()
{
    progdir="${PATH}" # :default
    argno=$(echo "$*" | awk '{ print NF-1 }')
    if (( argno == 0 )) ; then
      # Fortran binary (arguments overgiven implicit by namelist):
      progdir="${PROGDIR}"
    elif (( argno > 0 )) ; then
      # Shell Skript (arguments overgiven explicit by skript parameters):
      progdir="${EXTPAR_HOME}/job"
    fi
    modul=$(echo "$*" | awk '{ print $1 }')
    args=$(echo "$*" | awk '{ sep=""; for(i=2; i<=NF; i++) { printf "%s%s", sep, $i; sep=" " } }')
    set +e
    modul=$(PATH=${progdir} whence ${modul})
    rc=$?
    set -e
    if (( rc != 0 )) ; then
      modul=modul_not_set
    fi
    if [[ ! -e ${modul} || ! -x ${modul} ]] ; then
      print -- "   ERROR: command '${modul}' not found or not executable."
      exit 201
    fi
    echo ">> Run $* ..."
    start=$(date +%s.%N)
    set +e
#   eval ./$* > ${logfile} 2>&1
    eval ${modul} ${args} > ${logfile} 2>&1
    rc=$?
    set -e
    printf "   Return code: %i\n" $rc
    end=$(date +%s.%N)
    runtime=$(bc -l <<< "$end - $start")
    if (( rc > 0 ))
    then
        (( error_count++ ))
    fi
    case $rc in
        0)
            echo "   SUCCESS ${1%% *}"
            ;;
        127)
            echo "   ERROR ${1%% *}: command not found"
            ;;
        130)
            echo "   ERROR ${1%% *}: script terminated by Ctrl-C"
            ;;
        *)
            echo "   ERROR ${1%% *}: fatal error - return code $rc"
            ;;
    esac
    echo "   execution time: $runtime s"
    [[ -f ${logfile} ]] && cat ${logfile}
    [[ -f ${logfile} ]] && cat ${logfile} >> ${logfileforzip}

    # @ckoziar, 20190926:
    if (( rc != 0 )) ; then
        exit ${rc}
    fi
}

eval $(routine_config)

TMP=${TMPDIR}
cd $TMP

logfile=extpar_modules_$(date +%Y%m%d%H%M%S).log
logfileforzip=%logfile4zip%
globeflag=%globeflag%
nml_dir=%nml_dir%

ICON_GRID=%igrid%
GRIDDIR=%igrid_dir%
WORKDIR=%iwork_dir%
PROGDIR=%progdir_lc%
EXTPAR_HOME=~routfor/routfox/extpar

emissFlag=%emissFlag%

#________________________________________________________________________________
# Names of executables

# Scripts
binary_alb=extpar_alb_to_buffer.sh
#binary_ndvi=extpar_ndvi_to_buffer.sh
#binary_tclim=extpar_cru_to_buffer.sh
#
#binary_alb=extpar_alb_to_buffer
binary_ndvi=extpar_ndvi_to_buffer.new.new
#binary_emiss=extpar_emiss_to_buffer.new.new
binary_emiss=extpar_emiss_to_buffer.sh
binary_tclim=extpar_cru_to_buffer.new.new
binary_lu=extpar_landuse_to_buffer.new.new
binary_topo=extpar_topo_to_buffer.new.new
binary_aot=extpar_aot_to_buffer.new.new
binary_soil=extpar_soil_to_buffer.new.new
binary_flake=extpar_flake_to_buffer.new.new
binary_consistency_check=extpar_consistency_check.new.new

#________________________________________________________________________________

# # cp $PROGDIR/$binary_alb $WORKDIR/
# cp ${EXTPAR_HOME}/job/$binary_alb $WORKDIR/
# cp $PROGDIR/$binary_ndvi $WORKDIR/
# cp $PROGDIR/$binary_emiss $WORKDIR/
# cp $PROGDIR/$binary_tclim $WORKDIR/
# cp $PROGDIR/$binary_lu $WORKDIR/
# cp $PROGDIR/$binary_topo $WORKDIR/
# cp $PROGDIR/$binary_aot $WORKDIR/
# cp $PROGDIR/$binary_soil $WORKDIR/
# cp $PROGDIR/$binary_flake $WORKDIR/
# cp $PROGDIR/$binary_consistency_check $WORKDIR/
# for file in $PROGDIR/*.py ; do
#   if [[ -s ${file} ]] ; then
#     cp -p ${file} $WORKDIR/
#   fi
# done

export OMP_NUM_THREADS=8

export NETCDF_OUTPUT_FILETYPE=NETCDF4

cd $WORKDIR

# @ckoziar, 20210211: uncomment following 2 lines:
# ncks='/hpc/sw/nco/4.8.1/bin/ncks'
# ncwa='/hpc/sw/nco/4.8.1/bin/ncwa'

# Assume GLOBE orography as default
cp INPUT_ORO_GLOBE INPUT_ORO
# @fprill/ckoziar, 20190924:
# Use ASTER orography for resolutions higher than 3.0 km
if [[ -s ${GRIDDIR}/${ICON_GRID} ]] ; then
  meshsize_km=$(ncks -M ${GRIDDIR}/${ICON_GRID} | \
                awk '/grid_root/  { GRIDROOT=$(NF-1)  }
                     /grid_level/ { GRIDLEVEL=$(NF-1) }
                     END { print(5050./(GRIDROOT*2**GRIDLEVEL))}
                    ' -
               )

  # @fprill/ckoziar, 20191204: cdo infov doesn't work in every case --> replace by ncwa/ncks
# min_lat_vert=$(cdo infov ${GRIDDIR}/${ICON_GRID} | \
#                grep -i latitude_vertices | awk '{ print $9 * 180. / atan2(0, -1) }')
  ncwa -y min -v latitude_vertices -O ${GRIDDIR}/${ICON_GRID} foo.$$.nc && \
  min_lat_vert=$(ncks -H -s "%f" foo.$$.nc | awk '/^[-+0-9.,]+$/ { print $1 * 180. / atan2(0, -1) }')

# max_lat_vert=$(cdo infov ${GRIDDIR}/${ICON_GRID} | \
#                grep -i latitude_vertices | awk '{ print $11 * 180. / atan2(0, -1) }')
  ncwa -y max -v latitude_vertices -O ${GRIDDIR}/${ICON_GRID} foo.$$.nc && \
  max_lat_vert=$(ncks -H -s "%f" foo.$$.nc | awk '/^[-+0-9.,]+$/ { print $1 * 180. / atan2(0, -1) }')

# min_lon_vert=$(cdo infov ${GRIDDIR}/${ICON_GRID} | \
#                grep -i longitude_vertices | awk '{ print $9 * 180. / atan2(0, -1) }')
  ncwa -y min -v longitude_vertices -O ${GRIDDIR}/${ICON_GRID} foo.$$.nc && \
  min_lon_vert=$(ncks -H -s "%f" foo.$$.nc | awk '/^[-+0-9.,]+$/ { print $1 * 180. / atan2(0, -1) }')

# max_lon_vert=$(cdo infov ${GRIDDIR}/${ICON_GRID} | \
#                grep -i longitude_vertices | awk '{ print $11 * 180. / atan2(0, -1) }')
  ncwa -y max -v longitude_vertices -O ${GRIDDIR}/${ICON_GRID} foo.$$.nc && \
  max_lon_vert=$(ncks -H -s "%f" foo.$$.nc | awk '/^[-+0-9.,]+$/ { print $1 * 180. / atan2(0, -1) }')

  if [[ "${meshsize_km}"  != +([-+0-9.,]) ]] ; then
    print -- "ERROR: Cannot determine horizontal resolution of target grid file!\n" >&2
    exit 202
  fi

  if [[ "${min_lat_vert}" != +([-+0-9.,]) || \
        "${max_lat_vert}" != +([-+0-9.,]) || \
        "${min_lon_vert}" != +([-+0-9.,]) || \
        "${max_lon_vert}" != +([-+0-9.,])    \
     ]] ; then
    print -- "ERROR: Cannot determine minimum/maximum latitude/longitude of target grid file!\n" >&2
    exit 203
  fi

  rm -f foo.$$.nc

  print -- "        grid file information for '${GRIDDIR}/${ICON_GRID}': "
  print -- "        --->  mesh size: ${meshsize_km} km"
  print -- "        --->  north latitude of vertices: ${max_lat_vert} deg"
  print -- "        --->  south latitude of vertices: ${min_lat_vert} deg"
  print -- "        --->  west longitude of vertices: ${min_lon_vert} deg"
  print -- "        --->  east longitude of vertices: ${max_lon_vert} deg"

  if [[ -z "${globeflag}" ]] ; then
    if (( meshsize_km < 3.0 )) ; then
      print -- "switch to ASTER orography because target horizontal resolution is higher than 3.0 km"
      cp INPUT_ORO_ASTER INPUT_ORO

      aster_dir=$(awk '$1 == "raw_data_orography_path" { print substr($NF,2,length($NF)-2) }' INPUT_ORO)
      set -A arr_tilenames
      set -A arr_tilerows
      set -A arr_tilecols
      typeset -i row
      typeset -i col
      typeset -i xint

      for tile in $(eval_numlist -f %03d 1-240) ; do
        tile_name="ASTER_orig_T${tile}.nc"
        if [[ -s ${aster_dir}/${tile_name}.xz ]] ; then

          # prepare tile info file before by:
          # for i in ASTER_orig_T0??.nc ; do
          #   min_lon_tile=$(cdo sinfov $i 2>/dev/null | awk '$NF == "degrees_east" { print $3 }')
          #   max_lon_tile=$(cdo sinfov $i 2>/dev/null | awk '$NF == "degrees_east" { print $5 }')
          #   min_lat_tile=$(cdo sinfov $i 2>/dev/null | awk '$NF == "degrees_north" { print $5 }')
          #   max_lat_tile=$(cdo sinfov $i 2>/dev/null | awk '$NF == "degrees_north" { print $3 }')
          #   echo "${i} ${min_lon_tile} ${max_lon_tile} ${min_lat_tile} ${max_lat_tile}" >> \
          #        ${aster_dir}/ASTER_tile_domains.txt
          # done

          # Scanning mode for lon is west to east
          min_lon_tile=$(awk '$1 == "'"${tile_name}"'" { print $2 }' ${aster_dir}/ASTER_tile_domains.txt)
          max_lon_tile=$(awk '$1 == "'"${tile_name}"'" { print $3 }' ${aster_dir}/ASTER_tile_domains.txt)
          # Scanning mode for lat is north to south
          min_lat_tile=$(awk '$1 == "'"${tile_name}"'" { print $4 }' ${aster_dir}/ASTER_tile_domains.txt)
          max_lat_tile=$(awk '$1 == "'"${tile_name}"'" { print $5 }' ${aster_dir}/ASTER_tile_domains.txt)

          # echo "${tile}: longitude: ( ${min_lon_tile} ... ${max_lon_tile} )"
          # echo "${tile}: latitude: ( ${min_lat_tile} ... ${max_lat_tile} )"

          if (( ( ( max_lat_vert  > min_lat_tile  &&  max_lat_vert  < max_lat_tile ) || \
                  ( max_lat_vert  > max_lat_tile  &&  min_lat_vert  < min_lat_tile ) || \
                  ( min_lat_vert  > min_lat_tile  &&  min_lat_vert  < max_lat_tile ) ) && \
                ( ( min_lon_vert  > min_lon_tile  &&  min_lon_vert  < max_lon_tile ) || \
                  ( min_lon_vert  < min_lon_tile  &&  max_lon_vert  > max_lon_tile ) || \
                  ( max_lon_vert  > min_lon_tile  &&  max_lon_vert  < max_lon_tile ) ) )) ; then
              arr_tilenames[${#arr_tilenames[*]}]=${tile_name}

              # ASTER consist of 240 tiles, 12 per row, 20 per column;
              # one tile is 30� (latitudinal) x 6� (longitudinal):
              (( xint = ( tile - 1 ) / 12 ))
              (( col = tile - ( 12 * xint ) ))
              (( row = ( tile + 11 ) / 12 ))
              arr_tilerows[${row}]=${row}
              arr_tilecols[${col}]=${col}
          fi
        fi
      done

      echo "needed ASTER tile names ${arr_tilenames[*]}"
      echo "needed ASTER rows ${arr_tilerows[*]}"
      echo "needed number of ASTER rows ${#arr_tilerows[*]}"
      echo "needed ASTER columns ${arr_tilecols[*]}"
      echo "needed number of ASTER columns ${#arr_tilecols[*]}"

      # prepare strings for pshell/namelist:
      typeset cmdfile_pshell="cmd_xz.pshell.$$"
      rm -f ${cmdfile_pshell}
      typeset parallel_pshell=8
      str=''
      blank=''
      for i in ${arr_tilenames[*]} ; do
        # copy ASTER tiles to $WORKDIR und uncompress them (do later on by pshell):
        print -- "cp ${aster_dir}/${i}.xz ${WORKDIR}/ && xz -dv ${WORKDIR}/${i}.xz" >> ${cmdfile_pshell}

        str=$(printf -- "%s%s'%s'" "${str}" "${blank}" ${i})
        blank=' '
      done

      # modify namelist for ASTER:
      sed "s/ topo_files = .*$/ topo_files = ${str}/" INPUT_ORO > foo1.$$
      sed "s/ ntiles_row = .*$/ ntiles_row = ${#arr_tilerows[*]}/" foo1.$$ > foo2.$$
      sed "s/ ntiles_column = .*$/ ntiles_column = ${#arr_tilecols[*]}/" foo2.$$ > foo3.$$
      sed "s: raw_data_orography_path = .*$: raw_data_orography_path = '${WORKDIR}/':" foo3.$$ > INPUT_ORO

      rm -f foo?.$$
      cat INPUT_ORO

      # uncompress ASTER tiles in parallel:
      if [[ -s ${cmdfile_pshell} ]] ; then
        cat ${cmdfile_pshell}

        date
        set +e
        pshell -p ${parallel_pshell} -taback -taback_fast_sleep -L -T -f ${cmdfile_pshell}
        rc=$?
        set -e
        date

        if (( rc != 0 )) ; then
          print -- "ERROR: Cannot uncompress ASTER tiles by pshell!\n" >&2
          exit ${rc}
        else
          set -A arr_rc_parallel -- $(<rc_parallel)
          rc=$(print -- ${arr_rc_parallel[*]} | tr ' ' '+' | bc)
          if (( rc != 0 )) ; then
            print -- "ERROR: Cannot uncompress all ASTER tiles by pshell!\n" >&2
            ls -l ${WORKDIR}/ASTER_orig_T*
            exit ${rc}
          else
            ls -l ${WORKDIR}/ASTER_orig_T*
          fi
        fi
        rm -f ${cmdfile_pshell}
      fi
    fi
  else
    if (( meshsize_km < 1.5 )) ; then
      # @ckoziar, 20191108:
      # don't generate SSO parameters for GLOBE
      # in case of horizontal resolution < 1.5 km:
      sed "s/ lsso_param = .*$/ lsso_param = .FALSE./" INPUT_ORO > foo.$$ && mv foo.$$ INPUT_ORO
    fi
  fi
fi

# final namelists modifications:
if [[ -n "${nml_dir}" && -d ${nml_dir} ]] ; then
  for file in ${nml_dir}/INPUT_* ; do
    if [[ -s ${file} ]] ; then
      if [[ "${file}" == *.mod ]] ; then
        ifile=${file##*/} ; ifile=${ifile%.mod}
        if [[ -s ${ifile} ]] ; then
          print -- "modify namelist '${ifile}' by following instructions:"
          cat ${file}
          echo
          print -- "namelist before modification:"
          cat ${ifile}
          set +e
          # @ckoziar, 20191108:
          # convert AWK to Perl because named arrays are not supported by gawk v3:
          # ~fprill/software/gawk-4.1.1-build/bin/awk -f ${EXTPAR_HOME}/job/modify_nml.awk -v modfile=${file} ${ifile} > ${ifile}.$$.new
          perl > ${ifile}.$$.new <<__HERE__
\$modfile = "${file}";
\$nmlfile = "${ifile}";
\$active_nml = "";
if(open(INPUT, "<\$modfile"))
{
  printf("!  read modification table from '%s'.\n!\n", \$modfile);
  while(<INPUT>)
  {
    @a = split('\|', \$_);
    for(\$i = 0; \$i < scalar(@a); \$i++)
    {
      \$a[\$i] =~ s/^\s+//; \$a[\$i] =~ s/\s+$//; # trim
    }
    if(scalar(@a) == 3)
    {
      \$mod_dict{"&" . \$a[0]}{\$a[1]} = \$a[2];
    }
  }
  close(INPUT);
  foreach \$nml ( sort keys(%mod_dict) )
  {
    foreach \$key ( sort keys %{ \$mod_dict{\$nml} } )
    {
      printf("!  %s %s %s\n", \$nml, \$key, \$mod_dict{\$nml}{\$key});
    }
  }
  printf("!\n!  original namelist file: '%s'.\n\n", \$nmlfile);
}
else
{
  print STDERR "Error: Cannot open namelist modification file '\$modfile' for reading!\n";
  exit 204;
}

if(open(INPUT, "<\$nmlfile"))
{
  while(\$line = <INPUT>)
  {
    chomp(\$line);
    if(\$line =~ /^&[a-zA-Z_]+/)
    { # parse namelist start:
      \$active_nml = \$line;
      \$active_nml =~ s/^\s+//; \$active_nml =~ s/\s+$//; # trim
      printf("%s\n", \$line);
      %visited_arr = ();
      next;
    }
    elsif(\$line =~ /\/\s*$/)
    { # parse namelist close:
      if(defined(\$mod_dict{\$active_nml}))
      {
        foreach \$key ( sort keys %{ \$mod_dict{\$active_nml} } )
        {
          if(!defined(\$visited_arr{\$key}))
          {
            printf("%s = %s\n", \$key, \$mod_dict{\$active_nml}{\$key});
          }
        }
      }
      \$active_nml = "";
      printf("%s\n", \$line);
      next;
    }
    else
    { # search for namelist keys to replace:
      @a = split('=', \$line);
      \$a[0] =~ s/^\s+//; \$a[0] =~ s/\s+$//; # trim
      if(defined(\$mod_dict{\$active_nml}) && defined(\$mod_dict{\$active_nml}{\$a[0]}))
      {
        printf("%s = %s\n", \$a[0], \$mod_dict{\$active_nml}{\$a[0]});
        \$visited_arr{\$a[0]} = 1;
      }
      else
      {
        printf("%s\n", \$line);
      }
    }
  }
}
else
{
  print STDERR "Error: Cannot open namelist input file '\$nmlfile' for reading!\n";
  exit 205;
}

exit 0;
__HERE__
          rc=$?
          set -e
          if (( rc != 0 )) ; then
            print -- "ERROR: Cannot modify namelist '${ifile}' by '${file}'!\n" >&2
            exit ${rc}
          fi
  
          mv ${ifile}.$$.new ${ifile}
          print -- "namelist after modification:"
          cat ${ifile}
        fi
      else
        ifile=${file##*/}
        print -- "total namelist replacing by 'cp ${file} ${PWD}/${ifile}'"
        set +e
        ls -l ${file} ${PWD}/${ifile}
        print -- "diff ${file} ${PWD}/${ifile}:"
        diff ${file} ${PWD}/${ifile}
        set -e
        cp ${file} ${PWD}/${ifile}
      fi
    fi
  done
fi

cp INPUT_TCLIM_COARSE INPUT_TCLIM
run_command ${binary_tclim}
cp INPUT_TCLIM_FINE INPUT_TCLIM

run_command ${binary_tclim}

# prepare grid subset for albedo Python script:
ncks -v clat,clon,cell_area,clon_vertices,clat_vertices ${GRIDDIR}/${ICON_GRID} GRID_SUBSET.nc

if [[ -z "${emissFlag}" ]] ; then
  # skip $binary_emiss and disable corresponding consistency check:
  echo "remove following line from INPUT_CHECK now:"
  grep ' emiss_buffer_file = ' INPUT_CHECK
  echo
  sed '/ emiss_buffer_file = /d' INPUT_CHECK > foo.$$ && mv foo.$$ INPUT_CHECK
  # ...and remove INPUT file of extpar_emiss_to_buffer binary because
  # consistency_check enable check of emissivity if this file exists:
  ls -l INPUT_*
  echo "rm -f INPUT_EMISS*"
  rm -f INPUT_EMISS*
  ls -l INPUT_*
else
  sed '/ emiss_buffer_file = /d' INPUT_CHECK > foo.$$ && mv foo.$$ INPUT_CHECK
# run_command ${binary_emiss}
  run_command ${binary_emiss} -r CAM_bbe_int_2010-2015_lw.nc \
                              -b emiss_BUFFER.nc \
                              -g GRID_SUBSET.nc \
                              -p ${EXTPAR_HOME}/rawdata/
fi

run_command ${binary_topo}

# switch (slow) FORTRAN binary by (fast) Python Skript:
#run_command ${binary_alb}

run_command ${binary_alb} -r month_alb_new.nc \
                          -u month_aluvd_new.nc \
                          -i month_alnid_new.nc \
                          -b month_alb_BUFFER.nc \
                          -g GRID_SUBSET.nc \
                          -p ${EXTPAR_HOME}/rawdata/

run_command ${binary_ndvi}
run_command ${binary_lu}
run_command ${binary_aot}
run_command ${binary_soil}
run_command ${binary_flake}

# @jhelmert/ckoziar, 20191212:
# avoid 'SIGSEGV, segmentation fault' in consistency_check
# by switching to intel module version 2018.2.199:
# module_count=$(module list 2>&1 | grep -c 'intel/')
# if [[ -z "${module_count}" ]] || (( module_count == 0 )) ; then
#   module load intel/2018.2.199
# else
#   module swap intel/2018.2.199
# fi

echo
echo "checking existence of INPUT_EMISS before consistency check:"
ls -l INPUT_*
echo

run_command ${binary_consistency_check}

