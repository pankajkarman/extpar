#!/bin/bash
#SBATCH --job-name="extpar"
#SBATCH --nodes=1
#SBATCH --output="job.out"
#SBATCH --time=01:00:00
#SBATCH --partition=postproc

export MODULEPATH=$MODULEPATH:/oprusers/owm/modules/RH7.5/modulefiles
module load PE/17.06
module load gcc
module load netcdf/4.4.1.1-gmvolf-17.02
source /users/juckerj/venv_jenkins/bin/activate
module load cdo
export HDF5_DISABLE_VERSION_CHECK=1

./src/testsuite.py --exe=run_extpar_cosmo.sh -v 1 -o testsuite.out --testlist=testlist_cosmo.xml --mpicmd='srun -u -n'  

