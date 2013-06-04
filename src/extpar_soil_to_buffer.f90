!+ Fortran main program to read in soil data and aggregate to target grid
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  small bug fixes accroding to Fortran compiler warnings
! V1_2         2011/03/25 Hermann Asensio
!  update to support ICON refinement grids
! V1_7         2013/01/25 Guenther Zaengl 
!   Parallel threads for ICON and COSMO using Open-MP, 
!   Several bug fixes and optimizations for ICON search algorithm, 
!   particularly for the special case of non-contiguous domains; 
!   simplified namelist control for ICON  
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran main program to read in soil data and aggregate to target grid
!>  
!> \author Hermann Asensio
PROGRAM extpar_soil_to_buffer

! Load the library information data:
USE info_extpar, ONLY: info_define, info_readnl, info_print


USE mo_kind, ONLY: wp
USE mo_kind, ONLY: i4
USE mo_kind, ONLY: i8

USE mo_target_grid_data, ONLY: no_raw_data_pixel
USE mo_target_grid_data, ONLY: lon_geo
USE mo_target_grid_data, ONLY: lat_geo

USE mo_target_grid_data, ONLY: allocate_com_target_fields
USE mo_target_grid_data, ONLY: tg
 
USE mo_grid_structures, ONLY: rotated_lonlat_grid
USE mo_grid_structures, ONLY: icosahedral_triangular_grid
USE mo_grid_structures, ONLY: target_grid_def

USE mo_grid_structures, ONLY: igrid_icon
USE mo_grid_structures, ONLY: igrid_cosmo
USE mo_grid_structures, ONLY: igrid_gme

USE  mo_cosmo_grid, ONLY: COSMO_grid, &
  &                         lon_rot, &
  &                         lat_rot, &
  &                         allocate_cosmo_rc, &
  &                         get_cosmo_grid_info, &
  &                         calculate_cosmo_domain_coordinates

  
USE  mo_cosmo_grid, ONLY: calculate_cosmo_target_coordinates


USE  mo_icon_grid_data, ONLY: ICON_grid !< structure which contains the definition of the ICON grid


USE mo_base_geometry,    ONLY:  geographical_coordinates, &
                                 cartesian_coordinates
  
USE mo_additional_geometry,   ONLY: cc2gc,                  &
                              gc2cc,                  &
                              arc_length,             &
                              cos_arc_length,         &
                              inter_section,          &
                              vector_product,         &
                              point_in_polygon_sp

                              

USE mo_icon_domain,          ONLY: icon_domain, &
                              grid_cells,               &
                              grid_vertices,            &
                              construct_icon_domain,    &
                              destruct_icon_domain

USE mo_io_units,          ONLY: filename_max

USE mo_exception,         ONLY: message_text, message, finish

USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_math_constants,  ONLY: pi, pi_2, dbl_eps,rad2deg

USE mo_agg_soil, ONLY: agg_soil_data_to_target_grid, &
                       nearest_soil_data_to_target_grid

USE mo_read_extpar_namelists, ONLY: read_namelists_extpar_grid_def
USE mo_soil_routines, ONLY: read_namelists_extpar_soil

                      
USE mo_soil_routines, ONLY: get_soil_data, &
                          get_dimension_soil_data

USE mo_soil_data, ONLY: allocate_raw_soil_fields, &
        dsmw_legend, &
        soil_texslo, &
        dsmw_soil_unit, &
        n_unit,         &
        dsmw_grid, &
        lon_soil, &
        lat_soil

USE   mo_soil_tg_fields, ONLY:  fr_land_soil
USE   mo_soil_tg_fields, ONLY:  soiltype_fao
USE   mo_soil_tg_fields, ONLY:  allocate_soil_target_fields


USE mo_soil_output_nc, ONLY: write_netcdf_soil_cosmo_grid
USE mo_soil_output_nc, ONLY: write_netcdf_soil_icon_grid
USE mo_soil_output_nc, ONLY: write_netcdf_soil_buffer



USE mo_target_grid_routines, ONLY: init_target_grid

  IMPLICIT NONE


      INTEGER  (KIND=i4) :: nlon_soil  !< number of grid elements in zonal direction for soil raw dataset
      INTEGER  (KIND=i4) :: nlat_soil  !< number of grid elements in meridional direction for soil raw dataset

      CHARACTER(len=filename_max) :: netcdf_filename
 
      CHARACTER (len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition
      CHARACTER (len=filename_max) :: namelist_soil_data_input !< file with input namelist with soil data information
    
      CHARACTER (len=filename_max) :: raw_data_path        !< path to raw data
      CHARACTER (len=filename_max) :: path_soil_file      !< filename with path for soil raw data
      CHARACTER (len=filename_max) :: netcdf_out_filename      !< filename for netcdf file with soil data on COSMO grid
      CHARACTER (len=filename_max) :: soil_buffer_file  !< name for soil buffer file
      CHARACTER (len=filename_max) :: soil_output_file  !< name for soil output file
      CHARACTER (len=filename_max) :: soil_buffer_file_consistent !< name for soil buffer file after consistency check
      CHARACTER (len=filename_max) :: soil_output_file_consistent !< name for soil output file after consistency check

      CHARACTER (len=filename_max) :: raw_data_soil_path        !< path to raw data
      CHARACTER (len=filename_max) :: raw_data_soil_filename !< filename soil raw data


      CHARACTER (len=filename_max) :: namelist_grid_def !< filename with namelists for grid settings for EXTPAR
      CHARACTER (len=filename_max) :: domain_def_namelist !< namelist file with domain definition

      
      REAL (KIND=wp) :: point_lon_geo !< longitude of a point in geographical system
      REAL (KIND=wp) :: point_lat_geo !< latitude of a point in geographical system


      REAL(KIND=wp) :: undefined !< value to indicate undefined grid elements in cosmo_ndvi_field
      INTEGER (KIND=i4) :: undefined_integer   !< value for undefined integer

      INTEGER (KIND=i4) :: default_value !< default value


      INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid

      INTEGER :: idom  !< ICON Domain Number

      INTEGER :: i,j,k !< counters




      ! Print the default information to stdout:
      CALL info_define ('soil_to_buffer')      ! Pre-define the program name as binary name
      CALL info_print ()                     ! Print the information to stdout
      !--------------------------------------------------------------------------------------------------------

      undefined_integer = 0 ! set undefined to zero
      undefined = -99.0 ! undef vlaue
      default_value =  3 ! default value


    !--------------------------------------------------------------------------------------------------------
    !--------------------------------------------------------------------------------------------------------
    ! get information on target grid, allocate target fields with coordinates and determin the coordinates 
    ! for th target grid

    namelist_grid_def = 'INPUT_grid_org'
    CALL  init_target_grid(namelist_grid_def)

    !--------------------------------------------------------------------------------------------------------
    !--------------------------------------------------------------------------------------------------------


    ! get information on soil raw data
    !--------------------------------------------------------------------------------------------------------
      
    ! read namelist with soil data information (path, filename)

    namelist_soil_data_input = 'INPUT_SOIL'
    CALL read_namelists_extpar_soil(namelist_soil_data_input, &
                                         raw_data_soil_path, &
                                         raw_data_soil_filename, &
                                         soil_buffer_file, &
                                         soil_output_file, &
                                         soil_buffer_file_consistent, &
                                         soil_output_file_consistent)

     




      !HA debug
      print *,'raw_data_soil_path: ', TRIM(raw_data_soil_path)
      print *,'raw_data_soil_filename: ', TRIM(raw_data_soil_filename)
      print *,'soil_output_file: ', TRIM(soil_output_file)

      path_soil_file = TRIM(raw_data_soil_path) // TRIM(raw_data_soil_filename)

      !HA debug
      print *, 'path_soil_file: ', TRIM(path_soil_file)

      ! inquire dimesnions from raw data file

      CALL  get_dimension_soil_data(path_soil_file, &
                                          nlon_soil, &
                                          nlat_soil, &
                                          n_unit)

      !HA debug
      print *, 'nlon_soil', nlon_soil
      print *, 'nlat_soil', nlat_soil
      print *, 'n_unit', n_unit



      ! get coordinates and legend and data from raw data file
      ! define value of global variables soil_raw_data_grid, lon_reg, lat_reg, soil_texslo, dsmw_soil_unit
      !--------------------------------------------------------------------------------------------------------

      CALL allocate_raw_soil_fields(nlon_soil, nlat_soil, n_unit) 
      CALL allocate_soil_target_fields(tg)
      CALL get_soil_data(path_soil_file)




      !--------------------------------------------------------------------------------------------------------



      !HA debug
      print *,'FAO DSMW read from file ', TRIM(path_soil_file)


      ! aggregate soil data to target grid

      print *,'aggregate soil data to target grid'

      undefined = 0.0

      CALL agg_soil_data_to_target_grid(tg,    &
                  &                             undefined,  &
                  &                             soil_texslo, &
                  &                             dsmw_soil_unit, &
                  &                             n_unit,    &
                  &                             dsmw_grid, &
                  &                             lon_soil, &
                  &                             lat_soil, &
                  &                             fr_land_soil, &
                  &                             soiltype_fao)

      
        print *,'MAXVAL(no_raw_data_pixel): ', MAXVAL(no_raw_data_pixel)
        print *,'MINVAL(no_raw_data_pixel): ', MINVAL(no_raw_data_pixel)

        print *,'MAXVAL(cosmo_soiltyp): ', MAXVAL(soiltype_fao)
        print *,'MINVAL(cosmo_soiltyp): ', MINVAL(soiltype_fao)

        print *,'MAXVAL(fr_land_soil): ', MAXVAL(fr_land_soil)
        print *,'MINVAL(fr_land_soil): ', MINVAL(fr_land_soil)



        print *,'Start filling of undefined target grid elements with nearest grid point raw data'

        CALL nearest_soil_data_to_target_grid(tg,    &
                  &                             undefined,  &
                  &                             soil_texslo, &
                  &                             dsmw_soil_unit, &
                  &                             n_unit,    &
                  &                             dsmw_grid, &
                  &                             lon_soil, &
                  &                             lat_soil, &
                  &                             fr_land_soil, &
                  &                             soiltype_fao)


         print *,'Filling of undefined target grid elements with nearest grid point raw data done.'


        print *,'MAXVAL(no_raw_data_pixel): ', MAXVAL(no_raw_data_pixel)
        print *,'MINVAL(no_raw_data_pixel): ', MINVAL(no_raw_data_pixel)

        print *,'MAXVAL(cosmo_soiltyp): ', MAXVAL(soiltype_fao)
        print *,'MINVAL(cosmo_soiltyp): ', MINVAL(soiltype_fao)

        print *,'MAXVAL(fr_land_soil): ', MAXVAL(fr_land_soil)
        print *,'MINVAL(fr_land_soil): ', MINVAL(fr_land_soil)

         PRINT *,'Start buffer output'

         netcdf_filename=  TRIM(soil_buffer_file)

         undefined = -999.0
         undefined_integer= 999

         CALL write_netcdf_soil_buffer(netcdf_filename,  &
   &                                     tg,         &
   &                                     undefined, &
   &                                     undefined_integer,   &
   &                                     lon_geo,     &
   &                                     lat_geo, &
   &                                     fr_land_soil,       &
   &                                     soiltype_fao)

         PRINT *,'buffer output done'
         PRINT *,'Start target grid output'


    SELECT CASE(tg%igrid_type)
       !-----------------------------------------------------------------
       CASE(igrid_icon) ! ICON GRID

         netcdf_filename= TRIM(soil_output_file)

         undefined = -999.0
         undefined_integer= -999


         CALL write_netcdf_soil_icon_grid(netcdf_filename,  &
   &                                     icon_grid,         &
   &                                     tg,         &
   &                                     undefined, &
   &                                     undefined_integer,   &
   &                                     lon_geo,     &
   &                                     lat_geo, &
   &                                     fr_land_soil,       &
   &                                     soiltype_fao)


       CASE(igrid_cosmo) ! COSMO grid

         netcdf_filename= TRIM(soil_output_file)

         undefined = -999.0
         undefined_integer= -999


          CALL write_netcdf_soil_cosmo_grid(netcdf_filename,  &
   &                                     cosmo_grid,         &
   &                                     tg,         &
   &                                     undefined, &
   &                                     undefined_integer,   &
   &                                     lon_geo,     &
   &                                     lat_geo, &
   &                                     fr_land_soil,       &
   &                                     soiltype_fao)


       CASE(igrid_gme) ! GME grid   
    END SELECT


   PRINT *,'PROGRAM soil_to_buffer done.'
   PRINT *, achar(27)//'[32m DONE'//achar(27)//'[0m'  !mes
        

END PROGRAM extpar_soil_to_buffer
