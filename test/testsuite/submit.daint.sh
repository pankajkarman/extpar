#!/bin/bash
#SBATCH --constraint=gpu
#SBATCH --output="job.out"
#SBATCH --account=g110
#SBATCH --time=01:00:00
module load daint-gpu
module load CDO
source ../../modules.env
module load cray-python
module load netcdf-python
echo $PYTHONPATH
./src/testsuite.py --exe=run_extpar_cosmo.sh -v 3 -o testsuite.out --testlist=testlist_cosmo.xml --mpicmd='srun -u -n'  
./src/testsuite.py -a --exe=run_extpar_icon.sh -v 3 -o testsuite.out --testlist=testlist_icon.xml --mpicmd='srun -u -n'  
