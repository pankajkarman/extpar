# Set project name and supported programing language i.e., Fortran
# By default all programing languages are supported
PROJECT(EXTPAR Fortran C)

# Set cmake min. version, otherwise cmake will warn
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}")

MESSAGE(STATUS "HA ${CMAKE_SOURCE_DIR}")
MESSAGE(STATUS "Compiler is ${CMAKE_Fortran_COMPILER}" )

# Default build is in debug mode
IF (NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE "Debug")
ENDIF (NOT CMAKE_BUILD_TYPE)

IF (${CMAKE_SYSTEM_NAME} MATCHES "NEC-SX9")
  # Set the root dir of the stuff to find for NEC-SX
  SET(CMAKE_FIND_ROOT_PATH
    /usr/local/pkg-sx9/for0adm/
    ${CMAKE_FIND_ROOT_PATH}
  )
  SET(CMAKE_Fortran_FLAGS_DEBUG "-DSX=1")
ELSEIF (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  # Set the root dir of the stuff to find for HPC
  SET(CMAKE_FIND_ROOT_PATH
    /usr/local/pkg/for0adm/
    ${CMAKE_FIND_ROOT_PATH})
  IF (CMAKE_Fortran_COMPILER MATCHES "f95")
      SET(CMAKE_Fortran_FLAGS_DEBUG "-xpp=cpp -fpp -C -g -ftrap=common -fnonstd")
      SET(CMAKE_Fortran_FLAGS_RELEASE "-xpp=cpp -fpp -fast ")
   #   SET(CMAKE_Fortran_FLAGS_RELEASE "-xpp=cpp -fpp -xO2 -xipo=0 -xlibmopt")

  ENDIF (CMAKE_Fortran_COMPILER MATCHES "f95")
  IF (CMAKE_Fortran_COMPILER MATCHES "gfortran")
      SET(CMAKE_Fortran_FLAGS_DEBUG "-g -fbounds-check ")
      SET(CMAKE_Fortran_FLAGS_RELEASE "-O3")
      # SET(CMAKE_Fortran_FLAGS_DEBUG "-g -fbounds-check -fbacktrace -Warray-bounds -Wsurprising -Wconversion -Wunderflow")
      SET(COMMON_COMPILE_FLAGS "-x f95-cpp-input")
      SET(COMMON_LINK_FLAGS "-g -static-libgfortran -static-libgcc")
  ENDIF (CMAKE_Fortran_COMPILER MATCHES "gfortran")
ENDIF (${CMAKE_SYSTEM_NAME} MATCHES "NEC-SX9")


MESSAGE(STATUS "Building for ${CMAKE_SYSTEM_NAME}")


FIND_PACKAGE(NETCDF)

IF (NOT NETCDF_FOUND)
  UNSET(NETCDF_INCLUDE CACHE)
  UNSET(NETCDF_LIBRARY CACHE)
  MESSAGE(SEND_ERROR "NETCDF include and library are NOT found. Building this program is NOT possible.")
ENDIF (NOT NETCDF_FOUND)

FIND_PACKAGE(GRIB_API)

IF (NOT GRIB_API_F90_FOUND)
  MESSAGE(SEND_ERROR "GRIB_API include and library are NOT found. Building this program is NOT possible.")
ENDIF (NOT GRIB_API_F90_FOUND)



# Set include dir i.e., -I/dir/where/prog/sources/are
# The variable PROJECTNAME_SOURCE_DIR is auto-created and
# points to the directory where this CMakeLists.txt is
INCLUDE_DIRECTORIES(${EXTPAR_SOURCE_DIR}
                    ${NETCDF_INCLUDE}
)


# Set include dir i.e., -I/dir/where/prog/sources/are
# The variable PROJECTNAME_SOURCE_DIR is auto-created and
# points to the directory where this CMakeLists.txt is
INCLUDE_DIRECTORIES(${EXTPAR_SOURCE_DIR}
                    ${GRIB_API_INCLUDE}
)



# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_aot_to_buffer            # binary name
               extpar_aot_to_buffer.f90       # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_agg_aot.f90
               mo_aot_data.f90
               mo_aot_target_fields.f90
               mo_aot_output_nc.f90
               mo_base_geometry.f90
               mo_bilinterpol.f90
               mo_cosmo_grid.f90
               mo_exception.f90
               mo_gme_grid.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_math_constants.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               )

TARGET_LINK_LIBRARIES(extpar_aot_to_buffer 
                      ${NETCDF_LIBRARY}
)



# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_cru_to_buffer            # binary name
               extpar_cru_to_buffer.f90       # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_agg_cru.f90
               mo_base_geometry.f90
               mo_bilinterpol.f90
               mo_cosmo_grid.f90
               mo_cru_data.f90
               mo_cru_output_nc.f90
               mo_cru_target_fields.f90
               mo_exception.f90
               mo_gme_grid.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_math_constants.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               
               )

TARGET_LINK_LIBRARIES(extpar_cru_to_buffer 
                      ${NETCDF_LIBRARY}
)

# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_flake_to_buffer            # binary name
               extpar_flake_to_buffer.f90       # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_agg_flake.f90
               mo_base_geometry.f90
               mo_cosmo_grid.f90
               mo_exception.f90
               mo_gme_grid.f90
               mo_flake_data.f90
               mo_flake_output_nc.f90
               mo_flake_routines.f90
               mo_flake_tg_fields.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_math_constants.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_search_target_grid.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               )
               

TARGET_LINK_LIBRARIES(extpar_flake_to_buffer 
                      ${NETCDF_LIBRARY}
)



# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_globe_to_buffer            # binary name
               extpar_globe_to_buffer.f90      # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_agg_globe.f90
               mo_base_geometry.f90
               mo_bilinterpol.f90
               mo_cosmo_grid.f90
               mo_exception.f90
               mo_globe_data.f90
               mo_globe_routines.f90
               mo_globe_output_nc.f90
               mo_globe_tg_fields.f90
               mo_gme_grid.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_math_constants.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_search_target_grid.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               )
               

TARGET_LINK_LIBRARIES(extpar_globe_to_buffer 
                      ${NETCDF_LIBRARY}
)


# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_landuse_to_buffer            # binary name
               extpar_landuse_to_buffer.f90       # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_agg_glc2000.f90
               mo_agg_glcc.f90
               mo_base_geometry.f90
               mo_cosmo_grid.f90
               mo_exception.f90
               mo_glc2000_data.f90
               mo_glc2000_lookup_tables.f90
               mo_glc2000_tg_fields.f90
               mo_glcc_data.f90
               mo_glcc_lookup_tables.f90
               mo_glcc_tg_fields.f90
               mo_gme_grid.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_landuse_routines.f90
               mo_landuse_output_nc.f90
               mo_math_constants.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_search_target_grid.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               )
               

TARGET_LINK_LIBRARIES(extpar_landuse_to_buffer 
                      ${NETCDF_LIBRARY}
)




# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_ndvi_to_buffer            # binary name
               extpar_ndvi_to_buffer.f90      # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_agg_ndvi.f90
               mo_base_geometry.f90
               mo_bilinterpol.f90
               mo_cosmo_grid.f90
               mo_exception.f90
               mo_gme_grid.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_math_constants.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_search_target_grid.f90
               mo_ndvi_data.f90
               mo_ndvi_output_nc.f90
               mo_ndvi_routines.f90
               mo_ndvi_tg_fields.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               )
               

TARGET_LINK_LIBRARIES(extpar_ndvi_to_buffer 
                      ${NETCDF_LIBRARY}
)




# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_soil_to_buffer            # binary name
               extpar_soil_to_buffer.f90      # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_agg_soil.f90
               mo_base_geometry.f90
               mo_cosmo_grid.f90
               mo_exception.f90
               mo_gme_grid.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_math_constants.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_search_target_grid.f90
               mo_soil_data.f90
               mo_soil_output_nc.f90
               mo_soil_routines.f90
               mo_soil_tg_fields.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               )
               

TARGET_LINK_LIBRARIES(extpar_soil_to_buffer 
                      ${NETCDF_LIBRARY}
)

# Set final binary name and list all source files needed to
# compile it
ADD_EXECUTABLE(extpar_consistency_check            # binary name
               extpar_consistency_check.f90       # source file
               info_extpar.f90
               mo_additional_geometry.f90
               mo_aot_data.f90
               mo_aot_target_fields.f90
               mo_aot_output_nc.f90
               mo_base_geometry.f90
               mo_cru_data.f90
               mo_cru_target_fields.f90
               mo_cru_output_nc.f90
               mo_cosmo_grid.f90
               mo_exception.f90
               mo_extpar_output_grib.f90
               mo_extpar_output_nc.f90
               mo_flake_data.f90
               mo_flake_output_nc.f90
               mo_flake_tg_fields.f90
               mo_glc2000_data.f90
               mo_glc2000_lookup_tables.f90
               mo_glc2000_tg_fields.f90
               mo_glcc_lookup_tables.f90
               mo_glcc_tg_fields.f90
               mo_globe_data.f90
               mo_globe_output_nc.f90
               mo_globe_tg_fields.f90
               mo_globe_routines.f90
               mo_gme_grid.f90
               mo_grid_structures.f90
               mo_icon_domain.f90
               mo_icon_grid_data.f90
               mo_icon_grid_routines.f90
               mo_io_grib_api.f90
               mo_io_units.f90
               mo_io_utilities.f90
               mo_kind.f90
               mo_landuse_output_nc.f90
               mo_landuse_routines.f90
               mo_landuse_tg_fields.f90
               mo_math_constants.f90
               mo_ndvi_data.f90
               mo_ndvi_output_nc.f90
               mo_ndvi_routines.f90
               mo_ndvi_tg_fields.f90
               mo_physical_constants.f90
               mo_read_extpar_namelists.f90
               mo_search_gmegrid.f90
               mo_search_icongrid.f90
               mo_search_ll_grid.f90
               mo_soil_data.f90
               mo_soil_output_nc.f90
               mo_soil_routines.f90
               mo_soil_tg_fields.f90
               mo_target_grid_data.f90
               mo_target_grid_routines.f90
               mo_utilities_extpar.f90
               mo_var_meta_data.f90
               )
               

TARGET_LINK_LIBRARIES(extpar_consistency_check 
                      ${NETCDF_LIBRARY}
                      ${GRIB_API_LIBRARY_F90}
)




## If Doxygen is found, add a target to make API documentation.
#INCLUDE(FindDoxygen)
#IF(DOXYGEN_FOUND)
#  SET(DOC_OUTPUT_DIR "doc-extpar")
#  SET(DOC_FILE_PATTERN "*.f90")
#  CONFIGURE_FILE(doc_extpar.dox COPYONLY)
#
#  ADD_CUSTOM_TARGET(doc-extpar
#    "${DOXYGEN_EXECUTABLE}" "${CMAKE_BINARY_DIR}/doc_extpar.dox"
#    COMMENT "Create a full API documentation."
#    )
#  
#ELSE(DOXYGEN_FOUND)
#  MESSAGE(STATUS "WARNING! If you want to have an API documentation, please install Doxygen (see www.doxygen.org).")
#ENDIF(DOXYGEN_FOUND)
#

