# EXTPAR

# General Information
EXTPAR (External Parameters for Numerical Weather Prediction and Climate Application) is an official software of the [COSMO Consortium](www.cosmo-model.org).  It is used to prepare the external parameter data files that are used as input for the COSMO model, and additionally now the ICON model.

The code is written mostly in Fortran 90 and also includes some python and cdo scripting specifically for high resolution ICON model grids.  Currently the code is tested regularly using the gcc, NAG, and Intel Fortran compilers.  The code is also accelerated in some places with OpenMP parallelization.  

The code once compiled generates 12 executables, which can be run simultaneously except for the final extpar_consistency_check.exe, which is used to tie together all the external parameter results into one output file.  

In order to run Extpar, raw data files for the external parameter variables are needed.  These raw files are currently available on CSCS machines, DWD machines, and on mistral at DKRZ.  

A full documentation of code can be found in the [manual](doc/user_and_implementation_manual.pdf)

# Getting started
For compilation instructions see: [README.compile_run](doc/README.compile_run)

Once you have a compiled version, there are several sample run scripts available in the run_scripts folder of this repository.  You should be able to adapt one of these for your needs.  

# Testing
The extpar code comes with a technical testsuite to ensure the accuracy of the results.  More information about the testsuite can be found [here](test/testsuite/README.md)

# Information for developers
The coding rules and development workflow for Extpar can be found [here](doc/development.md)

# Support 
In the case of issues or questions, please contact the current source code administrator (Katie Osterried) at katherine.osterried@env.ethz.ch.  


