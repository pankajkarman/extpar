Hermann Asensio

To compile the "extpar" software the netcdf and grib_api libraries are necessary. 
Adjust the path settings with hints where to find the libs in
CMakeLists.txt
FindNETCDF.cmake
FindGRIB_API.cmake

For an "out of source build" create a working diretcory, generate a Makefile and compile with

mkdir work
cd work
cmake /path/to/extpar_src
make

For an "insource build"  
cd  /path/to/extpar_src
cmake .
make


