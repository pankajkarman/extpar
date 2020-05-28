#!/bin/bash
#SBATCH --job-name="extpar"
#SBATCH --nodes=1
#SBATCH --output="job.out"
#SBATCH --time=01:15:00
#SBATCH --partition=compute
#SBATCH --account=mh0287

source ../../modules.env

./src/testsuite.py --exe=run_extpar_cosmo.sh -v 1 -o testsuite.out --testlist=testlist_cosmo.xml --mpicmd='srun -u -n' 
./src/testsuite.py -a --exe=run_extpar_icon.sh -v 1 -o testsuite.out --testlist=testlist_icon.xml --mpicmd='srun -u -n'  
./src/testsuite.py -a --exe=run_extpar_icon.sh -v 1 -o testsuite.out --testlist=testlist_landuse.xml --mpicmd='srun -u -n'  
