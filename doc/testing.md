# Testing
Extpar is tested with an adapted version of the [COSMO technical testsuite](https://github.com/C2SM-RCM/testsuite).

## Run Tests
First step is to compile the code following the instructions in [compile_run](compile_run.md).

#### Daint

```
cp bin/* test/testsuite/bin/.
cd test/testsuite
./data/get_data.sh
./bin/extract_inputfiles_from_namelist.py
sbatch --wait ./submit.daint.transfer.sh
sbatch submit.daint.sh
```

#### Tsa
```
cp bin/* test/testsuite/bin/.
cd test/testsuite
./data/get_data.sh
./bin/extract_inputfiles_from_namelist.py
sbatch --wait ./submit.tsa.transfer.sh
sbatch submit.tsa.sh
```

#### Mistral

```
cp bin/* test/testsuite/bin/.
cd test/testsuite
./data/get_data.sh
sbatch submit.mistral.sh # or submit.mistral.intel.sh
```

The results of the testsuite can be found in file _testsuite.out_

An example output could look as follows:

## Testlists
There are many different testlist, each containing a set of tests for different setups, compiler or models:

#### GCC
* [COSMO](../test/testsuite/testlist_cosmo.xml)
* [ICON](../test/testsuite/testlist_icon.xml)
* [Landuse](../test/testsuite/testlist_landuse.xml)

#### Intel
* [COSMO](../test/testsuite/testlist_cosmo_intel.xml)
* [ICON](../test/testsuite/testlist_icon_intel.xml)
 
 
 To add a test to the Extpar testsuite:

  1. Modify the testlist.xml file to add the new test.  Alternatively, you could also add a new 
     testlist XML file (with a new name).  The testlist which is run can be chosen from the testsuite
     command line.  

  2. Make a folder in the data folder for the new test containing the INPUT_* files and the namelist.py for the Python-CDO modules.  

  3. Send me (katherine.osterried@env.ethz.ch) any binary reference files to upload to the ftp site. 
     I will also modify the get_data.sh script accordingly.  
