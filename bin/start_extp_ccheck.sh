#! /bin/ksh
#-----------------------------------------------------------------------------
#PBS -q lc_big
#PBS -l pmem=120gb
#PBS -l pvmem=120gb
#PBS -l select=1:ncpus=1:ompthreads=1:mem=120gb:vmem=120gb
#PBS -l walltime=01:00:00
#PBS -l file=128gb
#PBS -j oe
# ----------------------------------------------------------------------
set -x
module load netcdf
ulimit -f 32777216
export OMP_NUM_THREADS=1
cd /lustre2/gtmp/jhelmert/.jtmp.lce01.20191112.074400.9057/icon_grid_0026_R03B07_G.nc_1575535976_20749
./extpar_consistency_check.new | tee log_consistency_check.txt

