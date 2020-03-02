#!/bin/bash
#SBATCH --job-name="extpar"
#SBATCH --nodes=1
#SBATCH --output="job.out"
#SBATCH --time=03:00:00
#SBATCH --partition=compute
#SBATCH --account=mh0287

./src/testsuite.py --exe=run_extpar_mch.sh -v 1 -o testsuite.out --testlist=testlist_cosmo_intel.xml --mpicmd='srun -u -n' 
./src/testsuite.py -a --exe=run_extpar_mistral_dwd.sh -v 1 -o testsuite.out --testlist=testlist_cosmo_dwd_intel.xml --mpicmd='srun -u -n'  
module load python
./src/testsuite.py -a --exe=run_extpar_mistral_mpim.sh -v 1 -o testsuite.out --testlist=testlist_icon_intel.xml --mpicmd='srun -u -n'  
