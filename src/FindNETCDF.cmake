# Find NETCDF include directories and libraries
#
# NETCDF_INCLUDE - where to find netcdf.h
# NETCDF_LIBRARY - list of libraries to link against when using NETCDF
# NETCDFF_LIBRARY - Fortran netcdf library
# NETCDF_FOUND   - Do not attempt to use NETCDF if "no", "0", or undefined.

SET(NETCDF_PREFIX "" CACHE PATH "/usr/local/pkg/for0adm")
MESSAGE(STATUS "HA debug NETCDF_PREFIX: ${NETCDF_PREFIX}")
FIND_PATH(NETCDF_INCLUDE netcdf.h
  /usr/local/pkg/for0adm/include
  ${NETCDF_PREFIX}/include
  /include
  /usr/include
  /usr/local/include
  NO_DEFAULT_PATH
)

FIND_LIBRARY(NETCDFF_LIBRARY
  NAMES netcdff 
  PATHS
  /usr/local/pkg/for0adm/lib
  ${NETCDF_PREFIX}
  ${NETCDF_PREFIX}/lib64
  ${NETCDF_PREFIX}/lib
  /usr/local/lib64
  /usr/lib64
  /usr/lib64/netcdf-3
  /usr/local/lib
  /usr/lib
  /usr/lib/netcdf-3
  NO_DEFAULT_PATH
)


FIND_LIBRARY(NETCDF_LIBRARY
  NAMES netcdf
  PATHS
  /usr/local/pkg/for0adm/lib
  ${NETCDF_PREFIX}
  ${NETCDF_PREFIX}/lib64
  ${NETCDF_PREFIX}/lib
  /usr/local/lib64
  /usr/lib64
  /usr/lib64/netcdf-3
  /usr/local/lib
  /usr/lib
  /usr/lib/netcdf-3
  NO_DEFAULT_PATH
)

IF(NETCDF_INCLUDE AND NETCDF_LIBRARY)
  SET(NETCDF_FOUND 1)
  MESSAGE(STATUS "Found NETCDF library: ${NETCDF_LIBRARY}")
  SET(NETCDF_LIBRARY
      ${NETCDFF_LIBRARY}
      ${NETCDF_LIBRARY}
 )
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
