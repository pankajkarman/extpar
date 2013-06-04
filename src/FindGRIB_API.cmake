# Find GRIB_API include directories and libraries
#
# GRIB_API_INCLUDE - where to find grib_api.h 
# GRIB_API_LIBRARY - list of libraries to link against when using GRIB_API
# GRIB_API_LIBRARY_F90 - list of libraries to link against when using GRIB_API with Fortran 90 code
# GRIB_API_LIBRARY_F77 - list of libraries to link against when using GRIB_API with Fortran 77 code
# GRIB_API_C_FOUND   - Do not attempt to use GRIB_API if "no", "0", or undefined.
# GRIB_API_F90_FOUND   - Do not attempt to use GRIB_API with Fortran 90 if "no", "0", or undefined.
# GRIB_API_F77_FOUND   - Do not attempt to use GRIB_API with Fortran 77 if "no", "0", or undefined.

FIND_PATH(GRIB_API_INCLUDE grib_api.h
  ${GRIB_API_PREFIX}/include
#roa CSCS>
/oprusers/osm/lib/libgrib_api_1.9.9.1_pgi12.2.0/include
   #/users/rochesa/work/oro_smooth/libs/grib_api1.9.5/include
#roa CSCS<
  #/usr/local/pkg/grib_api/prerelease-1.9.18/include          # on DWD hpc
  #/usr/local/pkg/grib_api/include          # on DWD hpc
  #/opt/grib_api/include                    # on DWD workstation
  #/usr/local/pkg/grib_api/prerelease-1.9.18/include          # on DWD hpc

  NO_DEFAULT_PATH
)

IF(GRIB_API_INCLUDE)
  MESSAGE(STATUS "Found GRIB_API header: ${GRIB_API_INCLUDE}")
ELSE(GRIB_API_INCLUDE)
  MESSAGE(STATUS "Did not find GRIB_API header")
ENDIF(GRIB_API_INCLUDE)

# First search static then shared library
SET(CMAKE_FIND_LIBRARY_SUFFIXES ".a;.so")

FIND_LIBRARY(GRIB_API_LIBRARY_C
  NAMES grib_api
  PATHS
#roa CSCS>
/oprusers/osm/lib/libgrib_api_1.9.9.1_pgi12.2.0/lib
   #/users/rochesa/work/oro_smooth/libs/grib_api1.9.5/lib
#roa CSCS<
  #/usr/local/pkg/grib_api/prerelease-1.9.18/lib
  #/opt/grib_api/lib                    # on DWD workstation
  ${GRIB_API_PREFIX}
  ${GRIB_API_PREFIX}/lib64
  ${GRIB_API_PREFIX}/lib
  /usr/local/lib64
  /usr/lib64
  /usr/local/lib
  /usr/lib
  NO_DEFAULT_PATH
)

IF(GRIB_API_LIBRARY_C)
  MESSAGE(STATUS "Found GRIB_API library: ${GRIB_API_LIBRARY_C}")
ELSE(GRIB_API_LIBRARY_C)
  MESSAGE(STATUS "Did not find GRIB_API library")
ENDIF(GRIB_API_LIBRARY_C)

FIND_LIBRARY(GRIB_API_LIBRARY_F90
  NAMES grib_api_f90
  PATHS
  ${GRIB_API_PREFIX}
  ${GRIB_API_PREFIX}/lib64
  ${GRIB_API_PREFIX}/lib
#roa CSCS>
/oprusers/osm/lib/libgrib_api_1.9.9.1_pgi12.2.0/lib
    #/users/rochesa/work/oro_smooth/libs/grib_api1.9.5/lib
#roa CSCS<
  #/usr/local/pkg/grib_api/prerelease-1.9.18/lib          # on DWD hpc
  #/opt/grib_api/lib                    # on DWD workstation
  /usr/local/lib64
  /usr/lib64
  /usr/local/lib
  /usr/lib
  NO_DEFAULT_PATH
)

IF(GRIB_API_LIBRARY_F90)
  MESSAGE(STATUS "Found GRIB_API Fortran 90 library: ${GRIB_API_LIBRARY_F90}")
ELSE(GRIB_API_LIBRARY_F90)
  MESSAGE(STATUS "Did not find GRIB_API Fortran 90 library")
ENDIF(GRIB_API_LIBRARY_F90)

FIND_LIBRARY(GRIB_API_LIBRARY_F77
  NAMES grib_api_f77
  PATHS
  ${GRIB_API_PREFIX}
  ${GRIB_API_PREFIX}/lib64
  ${GRIB_API_PREFIX}/lib
#roa CSCS>
/oprusers/osm/lib/libgrib_api_1.9.9.1_pgi12.2.0/lib
   #/users/rochesa/work/oro_smooth/libs/grib_api1.9.5/lib
#roa CSCS<
  #/usr/local/pkg/grib_api/prerelease-1.9.18/lib          # on DWD hpc
  #/opt/grib_api/lib                    # on DWD workstation

  /usr/local/lib64
  /usr/lib64
  /usr/local/lib
  /usr/lib
  NO_DEFAULT_PATH
)

IF(GRIB_API_LIBRARY_F77)
  MESSAGE(STATUS "Found GRIB_API Fortran 77 library: ${GRIB_API_LIBRARY_F77}")
ELSE(GRIB_API_LIBRARY_F77)
  MESSAGE(STATUS "Did not find GRIB_API Fortran 77 library")
ENDIF(GRIB_API_LIBRARY_F77)

IF(GRIB_API_INCLUDE AND GRIB_API_LIBRARY_C)

  FIND_LIBRARY(JASPER_LIBRARY
    NAMES jasper
    PATHS
 #/data/hasensio/sw/jasper/lib            # on DWD workstation oflws87
  )

  IF(NOT JASPER_LIBRARY)
    MESSAGE(FATAL_ERROR "JASPER library needed for linking against GRIB_API is not found.")
  ENDIF(NOT JASPER_LIBRARY)

  FIND_LIBRARY(PNG_LIBRARY
    NAMES png
  )

  IF(NOT PNG_LIBRARY)
    MESSAGE(FATAL_ERROR "PNG library needed for linking against GRIB_API is not found.")
  ENDIF(NOT PNG_LIBRARY)

  FIND_LIBRARY(ZLIB_LIBRARY
    NAMES z
  )

  IF(NOT ZLIB_LIBRARY)
    MESSAGE(FATAL_ERROR "ZLIB library needed for linking against GRIB_API is not found.")
  ENDIF(NOT ZLIB_LIBRARY)

  SET(GRIB_API_C_FOUND 1)
  SET(GRIB_API_LIBRARY_C
      ${GRIB_API_LIBRARY_C}
      ${JASPER_LIBRARY}
      ${PNG_LIBRARY}
      ${ZLIB_LIBRARY}
  )
  IF(GRIB_API_LIBRARY_F90)
    SET(GRIB_API_F90_FOUND 1)
    SET(GRIB_API_LIBRARY_F90
        ${GRIB_API_LIBRARY_F90}
        ${GRIB_API_LIBRARY_C}
    )
  ELSE(GRIB_API_LIBRARY_F90)
    SET(GRIB_API_F90_FOUND 0)
  ENDIF(GRIB_API_LIBRARY_F90)

  IF(GRIB_API_LIBRARY_F77)
    SET(GRIB_API_F77_FOUND 1)
    SET(GRIB_API_LIBRARY_F77
        ${GRIB_API_LIBRARY_F77}
        ${GRIB_API_LIBRARY_C}
    )
  ELSE(GRIB_API_LIBRARY_F77)
    SET(GRIB_API_F77_FOUND 0)
  ENDIF(GRIB_API_LIBRARY_F77)
ELSE(GRIB_API_INCLUDE AND GRIB_API_LIBRARY_C)
  SET(GRIB_API_C_FOUND 0)
  SET(GRIB_API_F90_FOUND 0)
  SET(GRIB_API_F77_FOUND 0)
ENDIF(GRIB_API_INCLUDE AND GRIB_API_LIBRARY_C)
