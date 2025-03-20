#!/bin/bash
set -e

#./src/testsuite.py -a --exe=run_extpar_icon.sh -v 2 -o testsuite.out --testlist=testlist_icon.xml --mpicmd='sleep 1 &&'  
./src/testsuite.py -a --exe=run_extpar_icon.sh -v 3 -o testsuite.out --testlist=testlist_landuse.xml --only=dwd,icon_ecci --mpicmd='sleep 1 &&'  
#./src/testsuite.py -a --exe=run_extpar_icon.sh -v 2 -o testsuite.out --testlist=testlist_art.xml --mpicmd='sleep 1 &&'  
