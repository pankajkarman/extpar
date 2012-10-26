# Find NETCDF include directories and libraries
#
# NETCDF_INCLUDE - where to find netcdf.h
# NETCDF_LIBRARY - list of libraries to link against when using NETCDF
# NETCDFF_LIBRARY - Fortran netcdf library
# NETCDF_FOUND   - Do not attempt to use NETCDF if "no", "0", or undefined.
#roa CSCS>
SET(NETCDF_PREFIX "" CACHE PATH "/opt/cray/netcdf/4.1.3/pgi/109")
#SET(NETCDF_PREFIX "" CACHE PATH "/usr/local/pkg/for0adm")
#roa CSCS<
MESSAGE(STATUS "HA debug NETCDF_PREFIX: ${NETCDF_PREFIX}")
FIND_PATH(NETCDF_INCLUDE netcdf.h
  #/usr/local/pkg/for0adm/include
#roa CSCS>
   /opt/cray/netcdf/4.1.3/pgi/109/include
	 #/opt/cray/netcdf/4.0.1.1/netcdf-pgi/include
#roa CSCS<
  ${NETCDF_PREFIX}/include
  /include
  /usr/include
  /usr/local/include
  NO_DEFAULT_PATH
)

#FIND_LIBRARY(NETCDF_LIBRARY
#  NAMES netcdf 
#  PATHS
#roa CSCS>
  #/usr/local/pkg/for0adm/lib
#	/opt/cray/netcdf/4.0.1.1/netcdf-pgi/lib
#roa CSCS<
#  ${NETCDF_PREFIX}
#  ${NETCDF_PREFIX}/lib64
#  ${NETCDF_PREFIX}/lib
#  /usr/local/lib64
#  /usr/lib64
#  /usr/lib64/netcdf-3
#  /usr/local/lib
#  /usr/lib
#  /usr/lib/netcdf-3
#  NO_DEFAULT_PATH
#)
#roa CSCS>
SET(NETCDF_LIBRARY "/opt/cray/netcdf/4.1.3/pgi/109/lib/libnetcdff.a")
#roa CSCS<
IF(NETCDF_INCLUDE AND NETCDF_LIBRARY)
  SET(NETCDF_FOUND 1)
ELSE(NETCDF_INCLUDE AND NETCDF_LIBRARY)
  SET(NETCDF_FOUND 0)
ENDIF(NETCDF_INCLUDE AND NETCDF_LIBRARY)

MARK_AS_ADVANCED(
  NETCDF_PREFIX
  NETCDF_INCLUDE
  NETCDF_LIBRARY
)

IF (NETCDF_FOUND)
  MESSAGE(STATUS "Found NETCDF headers: ${NETCDF_INCLUDE}")
  MESSAGE(STATUS "Found NETCDF library: ${NETCDF_LIBRARY}")
ELSE (NETCDF_FOUND)
  MESSAGE(STATUS "Did not find NETCDF")
ENDIF (NETCDF_FOUND)
