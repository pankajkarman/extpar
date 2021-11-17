#!/bin/bash
#SBATCH --job-name="extpar"
#SBATCH --mem-per-cpu=3G
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12 
#SBATCH --output="job.out"
#SBATCH --time=02:00:00
#SBATCH --partition=compute,compute2,gpu,prepost,shared
#SBATCH --account=mh0287

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

source ../../modules.env
./src/testsuite.py --only=mch,c7_globe --exe=run_extpar_cosmo.sh -v 2 -o testsuite.out --testlist=testlist_cosmo_intel.xml --mpicmd='srun -u -n' 
./src/testsuite.py -a --only=clm,12km_globe --exe=run_extpar_cosmo.sh -v 2 -o testsuite.out --testlist=testlist_cosmo_intel.xml --mpicmd='srun -u -n' 
#./src/testsuite.py -a --exe=run_extpar_icon.sh -v 1 -o testsuite.out --testlist=testlist_icon_intel.xml --mpicmd='srun -u -n'  
