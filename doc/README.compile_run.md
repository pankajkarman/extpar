# EXTPAR: how to compile and run
## Code structure
EXTPAR contains FORTAN-Code as well as hybrid Python-CDO scripts.
Both code bases need external libraries and installations.
### Libraries
#### Fortran
EXTPAR needs the following libraries for the Fortran-Code:
- NetCDF
- JASPER
- PNG
- Z LIB
#### Python-CDO
EXTPAR needs the following Python-packages and installations:
- at least Python 3.6
- module netCDF4

The module netCDF is the Python interface to the netCDF C library. It allows the user create and manipulate netCDF files with Python.
For more detailed information please visit [netCDF Python](https://unidata.github.io/netcdf4-python/netCDF4/index.html).

Additionally an installation of CDO (Climate Data Operator) is required. All necessary information about this tool can be found at [CDO-MPI]( https://code.mpimet.mpg.de/projects/cdo/).

Be sure that these libraries are installed on your system
or install them yourself by following the installation
instructions provided with the libraries.

#### On the CSCS machines

All the required libraries are already installed on the CSCS machines. 

#### On the DKRZ machine mistral

Please use the provided script contrib/install-extpar-swstack.sh. This
script requires just an decent installed GCC with gcc, g++, and
gfortran (minimum requirement gcc-6.2).

This script can also be changed to use gcc/intel-18.0.2(Fortran only)
or gcc/NAGfor-6.2(Fortran only). But the main production environment
is supposed to be GCC.

This script fetches all required libraries and tools, compiles and
installs in $HOME/local.gcc.

### Compilation

Since Version 5.4 Extpar follows the configure/make/make install paradigm of
providing executables. Furthermore, out-of-source builds are
supported.
Because of the embedded submodule CDI in Extpar, please clone Extpar from GitHub the following:

* git clone --recursive git@github.com:C2SM-RCM/extpar.git
* git submodule update


Next step is to create a configure wrapper script to ease the
configure process. For now we provide scripts for all machines listed below.

#### On the CSCS machines

##### Piz Daint
Run configure.daint.gcc  
source modules.env  
make  
##### Kesch

Since Extpart Version 5.4, Kesch is no longer supported.

##### Tsa
Run configure.tsa.gcc  
source modules.env  
make
#### On the DKRZ machine mistral
Run configure.mistral.*your_compiler*, where *your_compiler* can be gcc, intel or nag  
source modules.env  
make

#### Restart build from scratch
make distclean 

### Run

Some runscript examples are available under /run_scripts.
Just adapt them to your needs!
### Check

1. that you haven't got any error message at runtime,
2. that you got a file in NetCDF format (.nc), and
3. in case of use with cosmo that Int2lm is able to read these 2 files 
