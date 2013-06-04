!+  Fortran main program to aggregate aerosol optical thickness raw data to target grid
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  small bug fixes accroding to Fortran compiler warnings
! V1_3         2011/04/19 Hermann Asensio
!  clean up of code
! V1_7         2013/01/25 Guenther Zaengl 
!   Parallel threads for ICON and COSMO using Open-MP, 
!   Several bug fixes and optimizations for ICON search algorithm, 
!   particularly for the special case of non-contiguous domains; 
!   simplified namelist control for ICON  
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!>  Fortran main program to aggregate aerosol optical thickness raw data to target grid
!>
!! @par extpar_aot_to_buffer 
!! 
!! This program interpolates monthly aerosol optical thicknesses for five different types of aerosols 
!! <ul>
!!   <li> black carbon </li>
!!   <li> dust </li>
!!   <li> organic </li>
!!   <li> SO4 </li>
!!   <li> sea salt </li>
!! </ul>
!! from a global climatology from Ina Tegen (Tegen et al. 1997) to a target grid (COSMO/ICON). 
!! The raw data and the describing paper are available at NASA/GISS at the Global Aerosol Climatology Project 
!! (GACP http://gacp.giss.nasa.gov/data_sets/transport/). 
!!
!!
!! Tegen, I., P. Hollrigl, M. Chin, I. Fung, D. Jacob, and J. Penner 1997.
!!  <a href="http://pubs.giss.nasa.gov/abstracts/1997/Tegen_etal.html">
!!  Contribution of different aerosol species to the global aerosol extinction optical thickness: 
!!  Estimates from model results</a>.
!! J. Geophys. Res., <b>102</b>, 23895-23915.
!!
PROGRAM extpar_aot_to_buffer

  ! Load the library information data:
  USE info_extpar, ONLY: info_define, info_readnl, info_print

  USE mo_kind,              ONLY: wp, &
                                  i4, &
                                  i8

  USE mo_grid_structures, ONLY: reg_lonlat_grid,     &
                                rotated_lonlat_grid, &
                                target_grid_def
  
  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme

  USE mo_target_grid_data, ONLY: lon_geo, &
    &                            lat_geo, &
    &                            no_raw_data_pixel

  USE mo_target_grid_data, ONLY: tg
  USE mo_target_grid_routines, ONLY: init_target_grid
 
 
  USE  mo_icon_grid_data, ONLY: ICON_grid !< structure which contains the definition of the ICON grid
   
  USE  mo_cosmo_grid, ONLY: cosmo_grid, &
     &                       lon_rot, &
     &                       lat_rot, &
     &                       allocate_cosmo_rc, &
     &                       get_cosmo_grid_info, &
     &                       calculate_cosmo_domain_coordinates

  USE  mo_cosmo_grid, ONLY: allocate_cosmo_rc
  USE  mo_cosmo_grid, ONLY: calculate_cosmo_target_coordinates


  USE mo_base_geometry,    ONLY:  geographical_coordinates, &
     &                               cartesian_coordinates

  
  USE mo_icon_domain,          ONLY: icon_domain, &
    &                            grid_cells,               &
    &                            grid_vertices,            &
    &                            construct_icon_domain,    &
    &                            destruct_icon_domain

  USE mo_icon_domain, ONLY: max_dom


  USE mo_io_units,          ONLY: filename_max

  USE mo_exception,         ONLY: message_text, message, finish

  USE mo_utilities_extpar, ONLY: abort_extpar
  
  USE mo_additional_geometry,   ONLY: cc2gc,                  &
    &                            gc2cc,                  &
    &                            arc_length,             &
    &                            cos_arc_length,         &
    &                            inter_section,          &
    &                            vector_product,         &
    &                            point_in_polygon_sp


  USE mo_math_constants,  ONLY: pi, pi_2, dbl_eps,rad2deg

  USE mo_aot_data, ONLY:  read_namelists_extpar_aerosol

  USE mo_aot_data, ONLY : allocate_aot_data, &
    &                      deallocate_aot_data, &
    &                      read_aot_data_input_namelist, &
    &                      get_dimension_aot_data, &
    &                      get_aot_grid_and_data, &
    &                      lon_aot, &
    &                      lat_aot, &
    &                      aot_data, &
    &                      aot_grid
  USE mo_aot_target_fields, ONLY: allocate_aot_target_fields,&
    &                              aot_tg
  
  USE mo_agg_aot, ONLY: agg_aot_data_to_target_grid

  USE mo_aot_output_nc, ONLY: write_netcdf_buffer_aot, &
    &                         write_netcdf_cosmo_grid_aot, &
    &                         write_netcdf_icon_grid_aot


  IMPLICIT NONE
  
  CHARACTER(len=filename_max) :: netcdf_filename
  CHARACTER(len=filename_max) :: filename

  CHARACTER(len=filename_max) :: namelist_grid_def

  CHARACTER(len=filename_max) :: input_namelist_file
  CHARACTER(len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition

  CHARACTER(len=filename_max) :: input_glc2000_namelist_file 
  CHARACTER(len=filename_max) :: glc2000_file


  CHARACTER (len=filename_max) :: raw_data_aot_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_aot_filename !< filename temperature climatology raw data

  CHARACTER (len=filename_max) :: aot_buffer_file !< name for aerosol buffer file
  CHARACTER (len=filename_max) :: aot_output_file !< name for aerosol output file

  INTEGER :: i, ilev, ip, iplev, ic, in, iclev, istartlev
  INTEGER :: nj
  INTEGER :: j,k !< counter
  INTEGER :: l,m !< counter
  REAL (KIND=wp) :: undefined
  INTEGER        :: undef_int

  !--------------------------------------------------------------------------------------
  INTEGER (KIND=i8) :: nlon_glc2000 !< number of grid elements in zonal direction for glc2000 data
  INTEGER (KIND=i8) :: nlat_glc2000 !< number of grid elements in meridional direction for glc2000 data
  !--------------------------------------------------------------------------------------

  
   INTEGER (KIND=i8) :: ntype !< number of types of aerosols
   INTEGER (KIND=i8) :: nrows !< number of rows
   INTEGER (KIND=i8) :: ncolumns !< number of columns
   INTEGER (KIND=i8) :: ntime !< number of times


  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid

  ! Print the default information to stdout:
  CALL info_define ('aot_to_buffer')      ! Pre-define the program name as binary name
  CALL info_print ()                      ! Print the information to stdout
  !--------------------------------------------------------------------------------------------------------
  ! get information on target grid, allocate target fields with coordinates and determin the coordinates 
  ! for th target grid

  namelist_grid_def = 'INPUT_grid_org'
  CALL  init_target_grid(namelist_grid_def)

  PRINT *,'target grid tg: ',tg%ie, tg%je, tg%ke, tg%minlon, tg%maxlon, tg%minlat, tg%maxlat
  !------------------------------------------------------------------------------------

  ! get information about aerosol data
  input_namelist_file='INPUT_AOT'


  CALL read_namelists_extpar_aerosol(input_namelist_file, &
   &                                  raw_data_aot_path, &
   &                                  raw_data_aot_filename, &
   &                                  aot_buffer_file, &
   &                                  aot_output_file)


   filename = TRIM(raw_data_aot_path) // TRIM(raw_data_aot_filename)

   PRINT *,'filename ',TRIM(filename)

   ! inquire dimensions

   CALL  get_dimension_aot_data(filename, &
                                     nrows,        &
                                     ncolumns,     &
                                     ntime,        &
                                     ntype)

   PRINT *, 'nrows: ',nrows
   PRINT *, 'ncolumns: ',ncolumns
   PRINT *, 'ntime: ',ntime
   PRINT *, 'ntype: ',ntype
   ! allocate aot raw data fields

   CALL allocate_aot_data(nrows,ncolumns,ntime,ntype)


   ! read in aot raw data

   CALL get_aot_grid_and_data(filename, &
                                     nrows,        &
                                     ncolumns,     &
                                     ntime,        &
                                     ntype,        &
                                     aot_grid,     &
                                     lon_aot,      &
                                     lat_aot,      &
                                     aot_data)
    ! allocate target grid fields for aerosol optical thickness

    CALL allocate_aot_target_fields(tg, ntime, ntype)
    
    undefined = -999.0_wp
    undef_int = -999

    aot_tg  =  undefined  ! set target grid values to undefined

    PRINT *,'call agg_aot_data_to_target_grid'
    CALL  agg_aot_data_to_target_grid(nrows,ncolumns,ntime,ntype)

    !write out data
    netcdf_filename =  TRIM(aot_output_file)
    PRINT *,'tg%igrid_type: ', tg%igrid_type

    SELECT CASE(tg%igrid_type)

    CASE(igrid_icon) ! ICON GRID
        PRINT *,'write cosmo output to ',TRIM(aot_output_file)


        CALL write_netcdf_icon_grid_aot(netcdf_filename,  &
   &                                     icon_grid,         &
   &                                     tg,               &
   &                                     undefined, &
   &                                     undef_int,   &
   &                                     lon_geo,     &
   &                                     lat_geo, &
   &                                     ntype,           &
   &                                     ntime,        &
   &                                     aot_tg)



        CASE(igrid_cosmo) ! COSMO grid
        PRINT *,'write cosmo output to ',TRIM(aot_output_file)

        CALL write_netcdf_cosmo_grid_aot(netcdf_filename,  &
   &                                     cosmo_grid,       &
   &                                     tg,               &
   &                                     undefined, &
   &                                     undef_int,   &
   &                                     lon_geo,     &
   &                                     lat_geo, &
   &                                     ntype,           &
   &                                     ntime,        &
   &                                     aot_tg)


        CASE(igrid_gme) ! GME grid   

       END SELECT

        !write out data

       print *,'write output to ',  TRIM(aot_buffer_file)

       netcdf_filename = TRIM(aot_buffer_file)
        print *,'write output to ', TRIM(netcdf_filename)
       
       CALL write_netcdf_buffer_aot(netcdf_filename, &
   &                                     tg,         &
   &                                     undefined,  &
   &                                     undef_int,  &
   &                                     lon_geo,    &
   &                                     lat_geo,    &
   &                                     ntype,      &
   &                                     ntime,      &
   &                                     aot_tg)

       CALL deallocate_aot_data()


  PRINT *,'============= aot_to_buffer done ==============='

  

END PROGRAM extpar_aot_to_buffer
