!+  Fortran main program for consistency check of external parameters
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  small bug fixes accroding to Fortran compiler warnings
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran main program for consistency check of external parameters 
!>  
!> Purpose: read in the external paraemeters and check for consistency, especially for the land-sea mask.
!> \author Hermann Asensio
PROGRAM extpar_consistency_check

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

USE mo_gme_grid, ONLY: gme_grid

USE  mo_icon_grid_data, ONLY: icon_grid !< structure which contains the definition of the ICON grid
USE  mo_icon_grid_data, ONLY: icon_grid_region
USE  mo_icon_grid_data, ONLY: icon_grid_level
USE  mo_icon_grid_data, ONLY: nvertex_dom  
USE  mo_icon_grid_data, ONLY: ncells_dom
USE  mo_icon_grid_data, ONLY: icon_domain_grid


!  USE mo_icon_grid_routines, ONLY: allocate_icon_grid
USE mo_icon_grid_routines, ONLY: get_icon_grid_info
USE mo_icon_grid_routines, ONLY: inq_grid_dims,            &
                                inq_domain_dims,          &
                                read_grid_info_part,      &
                                read_domain_info_part,    &
                                read_gridref_nl

USE mo_search_icongrid,   ONLY: walk_to_nc,              &
                                find_nc_dom1,            &
                                find_nc


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

USE mo_icon_domain, ONLY: max_dom



USE mo_io_units,          ONLY: filename_max

USE mo_exception,         ONLY: message_text, message, finish

USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_math_constants,  ONLY: pi, pi_2, dbl_eps,rad2deg

USE mo_read_extpar_namelists, ONLY: read_namelists_extpar_check

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
USE mo_soil_data, ONLY: undef_soiltype, default_soiltype, soiltype_ice, soiltype_water

USE   mo_soil_tg_fields, ONLY:  fr_land_soil
USE   mo_soil_tg_fields, ONLY:  soiltype_fao
USE   mo_soil_tg_fields, ONLY:  allocate_soil_target_fields

USE mo_soil_output_nc, ONLY: write_netcdf_soil_cosmo_grid
USE mo_soil_output_nc, ONLY: write_netcdf_soil_icon_grid
USE mo_soil_output_nc, ONLY: write_netcdf_soil_buffer

USE mo_soil_output_nc, ONLY: read_netcdf_soil_buffer

USE mo_target_grid_routines, ONLY: init_target_grid

USE mo_glc2000_tg_fields, ONLY: allocate_glc2000_target_fields

USE mo_glc2000_tg_fields, ONLY:  fr_land_glc2000, &
    &      glc2000_class_fraction,    &
    &      glc2000_class_npixel, &
    &      glc2000_tot_npixel, &
    &      ice_glc2000, &
    &      z0_glc2000, &
    &      root_glc2000, &
    &      plcov_mn_glc2000, &
    &      plcov_mx_glc2000, &
    &      lai_mn_glc2000, &
    &      lai_mx_glc2000, &
    &      rs_min_glc2000, &
    &      urban_glc2000,  &
    &      for_d_glc2000,  &
    &      for_e_glc2000, &
    &      emissivity_glc2000

USE mo_glcc_lookup_tables, ONLY: nclass_glcc
USE mo_glcc_lookup_tables, ONLY: init_glcc_lookup_tables
USE mo_glcc_lookup_tables, ONLY: glcc_legend
USE mo_glcc_lookup_tables, ONLY: ilookup_table_glcc, &
  &                              i_gme_lookup_table_glcc, &
  &                              i_cosmo_lookup_table_glcc, &
  &                              i_experimental_lookup_table_glcc
USE mo_glcc_lookup_tables, ONLY: z0_lt_glcc, lnz0_lt_glcc, plc_mn_lt_glcc, plc_mx_lt_glcc

USE mo_glcc_lookup_tables, ONLY: lai_mn_lt_glcc, lai_mx_lt_glcc, rd_lt_glcc, emiss_lt_glcc, rs_min_lt_glcc         

USE mo_glcc_tg_fields, ONLY:  fr_land_glcc, &
      &       glcc_class_fraction,    &
      &        glcc_class_npixel, &
      &        glcc_tot_npixel, &
      &        ice_glcc, &
      &        z0_glcc, &
      &        root_glcc, &
      &        plcov_mn_glcc, &
      &        plcov_mx_glcc, &
      &        lai_mn_glcc, &
      &        lai_mx_glcc, &
      &        rs_min_glcc, &
      &        urban_glcc,  &
      &        for_d_glcc,  &
      &        for_e_glcc, &
      &        emissivity_glcc, &
      &        allocate_glcc_target_fields

USE mo_glc2000_lookup_tables, ONLY: ilookup_table_glc2000, &
  &                                 get_name_glc2000_lookup_tables, &
  &                                 name_lookup_table_glc2000

USE mo_landuse_output_nc, ONLY: read_netcdf_buffer_glc2000
USE mo_landuse_output_nc, ONLY: read_netcdf_buffer_glcc

USE mo_landuse_routines, ONLY: read_namelists_extpar_land_use


USE mo_lu_tg_fields, ONLY: fr_land_lu, &
  &        ice_lu, &
  &        z0_lu, &
  &        z0_tot, &
  &        root_lu, &
  &        plcov_mn_lu, &
  &        plcov_mx_lu, &
  &        lai_mn_lu, &
  &        lai_mx_lu, &
  &        rs_min_lu, &
  &        urban_lu,  &
  &        for_d_lu,  &
  &        for_e_lu, &
  &        emissivity_lu, &
  &        fr_ocean_lu

USE mo_lu_tg_fields, ONLY: allocate_lu_target_fields

USE mo_ndvi_tg_fields, ONLY: ndvi_field, &
    &                                ndvi_max, &
    &                                ndvi_field_mom, &
    &                                ndvi_ratio_mom, &
    &                                allocate_ndvi_target_fields

USE mo_ndvi_data, ONLY: ntime_ndvi
USE mo_ndvi_data, ONLY: undef_ndvi, minimal_ndvi

USE mo_ndvi_output_nc, ONLY: read_netcdf_buffer_ndvi

USE mo_globe_tg_fields, ONLY:  fr_land_globe,       &
    &                         hh_globe,            &
    &                         stdh_globe,          &
    &                         theta_globe,         &
    &                         aniso_globe,         &
    &                         slope_globe,         &
    &                         z0_topo,             &
    &                         allocate_globe_target_fields

USE mo_globe_tg_fields, ONLY: add_parameters_domain, &
    &        vertex_param, &
    &        allocate_additional_hh_param
   
USE mo_globe_output_nc, ONLY: read_netcdf_buffer_globe

USE mo_aot_target_fields, ONLY: allocate_aot_target_fields,&
    &                              aot_tg
    
USE mo_aot_output_nc, ONLY: read_netcdf_buffer_aot

USE mo_cru_target_fields, ONLY: allocate_cru_target_fields,&
    &                              crutemp

USE mo_cru_output_nc, ONLY: read_netcdf_buffer_cru

USE mo_aot_data, ONLY: ntype_aot, ntime_aot


USE mo_flake_tg_fields, ONLY: fr_lake, &
  &       lake_depth,    &
  &       flake_tot_npixel, &
  &       allocate_flake_target_fields
USE mo_flake_data, ONLY: flake_depth_undef !< default value for undefined lake depth
USE mo_flake_data, ONLY: flake_depth_default !< default value for default lake depth, 10 [m]
USE mo_flake_data, ONLY: DWD_max_lake_depth !< Maximum lake depth in [m] for FLAKE

USE mo_flake_output_nc, ONLY: read_netcdf_buffer_flake

USE mo_extpar_output_nc, ONLY: write_netcdf_buffer_extpar, &
  &                        write_netcdf_cosmo_grid_extpar, &
  &                        write_netcdf_icon_grid_extpar

USE mo_extpar_output_grib, ONLY: write_cosmo_grid_extpar_grib, &
  &                              write_gme_grid_extpar_grib

USE mo_math_constants, ONLY: deg2rad, rad2deg
USE mo_physical_constants, ONLY: re ! av. radius of the earth [m]


  IMPLICIT NONE

  CHARACTER (len=filename_max) :: namelist_grid_def
  CHARACTER (len=filename_max) :: netcdf_output_filename

  CHARACTER (len=filename_max) :: grib_output_filename
  CHARACTER (len=filename_max) :: grib_template

  CHARACTER (len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition
  CHARACTER (len=filename_max) :: namelist_globe_data_input !< file with input namelist with GLOBE data information

  
  CHARACTER (len=filename_max) :: netcdf_out_filename      !< filename for netcdf file with data

  CHARACTER (len=filename_max) :: netcdf_in_filename      !< filename for netcdf file with data

  !-----------------------------------------------------------------------------------------------------------------------
  CHARACTER (len=filename_max) :: namelist_file !< filename with namelists for for EXTPAR settings
  ! soil
  CHARACTER (len=filename_max) :: soil_buffer_file  !< name for soil buffer file
  CHARACTER (len=filename_max) :: soil_output_file  !< name for soil output file
  CHARACTER (len=filename_max) :: soil_buffer_file_consistent !< name for soil buffer file after consistency check
  CHARACTER (len=filename_max) :: soil_output_file_consistent !< name for soil output file after consistency check

  CHARACTER (len=filename_max) :: soil_buffer_cons_output !< name for soil output file after consistency check

  CHARACTER (len=filename_max) :: raw_data_soil_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_soil_filename !< filename soil raw data



  ! orography
  CHARACTER (len=filename_max) :: orography_buffer_file !< name for orography buffer file
  CHARACTER (len=filename_max) :: orography_output_file !< name for orography output file
  
  CHARACTER (len=filename_max) :: raw_data_orography_path        !< path to raw data

  CHARACTER (len=filename_max) :: orography_buffer_cons_output  !< name for orography output file after consistency check

  ! land use

  CHARACTER (len=filename_max) :: raw_data_glc2000_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_glc2000_filename !< filename glc2000 raw data

  CHARACTER (len=filename_max) :: glc2000_buffer_file !< name for glc2000 buffer file
  CHARACTER (len=filename_max) :: glc2000_output_file !< name for glc2000 output file
  CHARACTER (len=filename_max) :: glc2000_buffer_cons_output  !< name for glc200 output file after consistency check

  CHARACTER (len=filename_max) :: raw_data_glcc_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_glcc_filename !< filename glc2000 raw data

  CHARACTER (len=filename_max) :: glcc_buffer_file    !< name for glcc buffer file
  CHARACTER (len=filename_max) :: glcc_output_file    !< name for glcc output file
  CHARACTER (len=filename_max) :: glcc_buffer_cons_output  !< name for glcc output file after consistency check

  CHARACTER(len=filename_max) :: input_glc2000_namelist_file
  !-----------------------------------------------------------------------------------------------------------------------
  ! NDVI
  CHARACTER (len=filename_max) :: raw_data_ndvi_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_ndvi_filename !< filename NDVI raw data

  CHARACTER (len=filename_max) :: ndvi_buffer_file !< name for NDVI buffer file
  CHARACTER (len=filename_max) :: ndvi_output_file !< name for NDVI output file
  CHARACTER (len=filename_max) :: ndvi_buffer_cons_output  !< name for ndvi output file after consistency check

  ! temperature climatology
  CHARACTER (len=filename_max) :: raw_data_t_clim_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_t_clim_filename !< filename temperature climatology raw data


  CHARACTER (len=filename_max) :: t_clim_buffer_file !< name for temperature climatology buffer
  CHARACTER (len=filename_max) :: t_clim_output_file !< name for temperature climatology output file
  CHARACTER (len=filename_max) :: t_clim_buffer_cons_output  !< name for t_clim output file after consistency check

  ! aerosol optical thickness
  
  CHARACTER (len=filename_max) :: raw_data_aot_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_aot_filename !< filename temperature climatology raw data

  CHARACTER (len=filename_max) :: flake_buffer_file  !< name for flake buffer file
  CHARACTER (len=filename_max) :: flake_buffer_cons_output  !< name for flake output file after consistency check

  CHARACTER (len=filename_max) :: aot_buffer_file !< name for aerosol buffer file
  CHARACTER (len=filename_max) :: aot_output_file !< name for aerosol output file

  CHARACTER (len=filename_max) :: aot_buffer_cons_output  !< name for aot output file after consistency check

  REAL (KIND=wp) :: point_lon_geo !< longitude of a point in geographical system
  REAL (KIND=wp) :: point_lat_geo !< latitude of a point in geographical system

  REAL(KIND=wp) :: undefined !< value to indicate undefined grid elements 
  INTEGER (KIND=i4) :: undef_int   !< value for undefined integer

  REAL(KIND=wp), PARAMETER :: undefined_lu = 0.0_wp !< value to indicate undefined land use grid elements 

  REAL(KIND=wp) :: fill_value_real !< value to indicate undefined grid elements 
  INTEGER (KIND=i4) :: fill_value_int   !< value for undefined integer

  INTEGER (KIND=i4) :: default_value !< default value

  INTEGER (KIND=i4) :: index_tile   !< index for dummy test
  TYPE(geographical_coordinates) :: DWD_location !< geographical coordinates of DWD for dummy test

  TYPE(geographical_coordinates) :: ur   !< upper right point for test block
  TYPE(geographical_coordinates) :: ll   !< lower left point for test block

  INTEGER (KIND=i4), ALLOCATABLE :: time(:) !< array with time axis values (needed for netcdf cf convention)
  INTEGER (KIND=i4), PARAMETER :: mpy=12     !< month per year
  INTEGER (KIND=i4):: nmonth  !< index for month

  INTEGER :: i !< counter
  INTEGER :: j !< counter
  INTEGER :: k !< counter
  INTEGER :: t !< counter
  INTEGER :: ie !< counter
  INTEGER :: je !< counter
  INTEGER :: ke !< counter

  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid

  ! variables for the ICON grid
  INTEGER :: n_dom                        !< number of model domains
  INTEGER :: first_dom                    !< index of first (global) model domain 
  INTEGER :: idom                         !< counter for domains
  
  ! variables for the "Erdmann Heise Formel"
  REAL (KIND=wp) :: dnorm  !< scale factor 
  REAL (KIND=wp) :: zlnorm = 2250.    !< scale factor [m]
  REAL (KIND=wp) :: alpha  = 1.E-05 !< scale factor [1/m] 
  REAL (KIND=wp) :: factor !< factor
  REAL (KIND=wp) :: zhp = 10.0    !< height of Prandtl-layer [m]
  REAL (KIND=wp) :: zlnhp      !< ln of height of Prandtl-layer [m]



  !HA tests
  REAL :: timestart
  REAL :: timeend
  REAL :: timediff

  INTEGER :: db_ice_counter


  ! Print the default information to stdout:
  CALL info_define ('extpar_consistency_check')      ! Pre-define the program name as binary name
  CALL info_print ()                     ! Print the information to stdout
  !--------------------------------------------------------------------------------------------------------
  !--------------------------------------------------------------------------------------------------------
  !--------------------------------------------------------------------------------------------------------
  ! get information on target grid, allocate target fields with coordinates and determin the coordinates 
  ! for th target grid

  namelist_grid_def = 'INPUT_grid_org'
  CALL  init_target_grid(namelist_grid_def)
  PRINT *,' target grid tg: ',tg
  igrid_type = tg%igrid_type

  
SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID
     PRINT *,'icon_grid%ncell: ',icon_grid%ncell
     PRINT *,'icon_grid%nvertex: ',icon_grid%nvertex

     CALL  allocate_additional_hh_param(icon_grid%nvertex)

END SELECT


  !--------------------------------------------------------------------------------------------------------
  ! get info on raw data file
  namelist_file = 'INPUT_LU'

  !---------------------------------------------------------------------------
  CALL read_namelists_extpar_land_use(namelist_file, &
    &                                 raw_data_glc2000_path, &
    &                                 raw_data_glc2000_filename, &
    &                                 ilookup_table_glc2000, &
    &                                 glc2000_buffer_file, &
    &                                 glc2000_output_file)

  CALL get_name_glc2000_lookup_tables(ilookup_table_glc2000, name_lookup_table_glc2000)
  PRINT *,'name_lookup_table_glc2000: ',TRIM(name_lookup_table_glc2000)
  !--------------------------------------------------------------------------------------------------------
  ! get filenames from namelist
  !--------------------------------------------------------------------------------------------------------
  namelist_file = 'INPUT_CHECK'

  CALL read_namelists_extpar_check(namelist_file, &
                                     grib_output_filename, &
                                     netcdf_output_filename, &
                                     orography_buffer_file, &
                                     soil_buffer_file, &
                                     glc2000_buffer_file, &
                                     glcc_buffer_file, &
                                     flake_buffer_file, &
                                     ndvi_buffer_file, &
                                     t_clim_buffer_file, &
                                     aot_buffer_file)


  ! get information about GLC2000 data

  CALL allocate_glc2000_target_fields(tg)
  PRINT *,'GLC2000 fields allocated'

  CALL allocate_glcc_target_fields(tg)
  PRINT *,'GLCC fields allocated'
  
  ! allocate Land use target fields
  CALL allocate_lu_target_fields(tg)
  PRINT *,'Land Use fields allocated'

  CALL allocate_soil_target_fields(tg)
  PRINT *,'soil fields allocated'

  CALL allocate_ndvi_target_fields(tg,ntime_ndvi)
  PRINT *,'ntime_ndvi ', ntime_ndvi
  PRINT *,'NDVI fields allocated'

  CALL allocate_globe_target_fields(tg)
  PRINT *,'GLOBE fields allocated'

  CALL allocate_aot_target_fields(tg, ntime_aot, ntype_aot)
  PRINT *,'aot fields allocated'

  CALL allocate_cru_target_fields(tg)
  PRINT *,'cru temperature field allocated'

  CALL allocate_flake_target_fields(tg)
  PRINT *,'flake parameter fields allocated'

  !--------------------------------------------------------------------------------------------------------
  ! Start Input

  PRINT *,'Read in Land Use data'
  PRINT *,'read ', TRIM(glc2000_buffer_file)

  CALL read_netcdf_buffer_glc2000(glc2000_buffer_file,  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     fr_land_glc2000, &
    &                                     glc2000_class_fraction,    &
    &                                     glc2000_class_npixel, &
    &                                     glc2000_tot_npixel, &
    &                                     ice_glc2000, &
    &                                     z0_glc2000, &
    &                                     root_glc2000, &
    &                                     plcov_mn_glc2000, &
    &                                     plcov_mx_glc2000, &
    &                                     lai_mn_glc2000, &
    &                                     lai_mx_glc2000, &
    &                                     rs_min_glc2000, &
    &                                     urban_glc2000,  &
    &                                     for_d_glc2000,  &
    &                                     for_e_glc2000, &
    &                                     emissivity_glc2000)
   PRINT *,'read ', TRIM(glcc_buffer_file)
  CALL read_netcdf_buffer_glcc(glcc_buffer_file,  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     fr_land_glcc, &
    &                                     glcc_class_fraction,    &
    &                                     glcc_class_npixel, &
    &                                     glcc_tot_npixel, &
    &                                     ice_glcc, &
    &                                     z0_glcc, &
    &                                     root_glcc, &
    &                                     plcov_mn_glcc, &
    &                                     plcov_mx_glcc, &
    &                                     lai_mn_glcc, &
    &                                     lai_mx_glcc, &
    &                                     rs_min_glcc, &
    &                                     urban_glcc,  &
    &                                     for_d_glcc,  &
    &                                     for_e_glcc, &
    &                                     emissivity_glcc)



  PRINT *,'Read in soil data'
  CALL read_netcdf_soil_buffer(soil_buffer_file,  &
   &                                     tg,         &
   &                                     undefined, &
   &                                     undef_int,   &
   &                                     fr_land_soil,       &
   &                                     soiltype_fao)

  PRINT *,'Read in NDVI data'
  CALL read_netcdf_buffer_ndvi(ndvi_buffer_file,  &
   &                                     tg,         &
   &                                     ntime_ndvi, &
   &                                     undefined, &
   &                                     undef_int,   &
   &                                     ndvi_max,  &
   &                                     ndvi_field_mom,&
   &                                     ndvi_ratio_mom)

   
   PRINT *,'Read in orography data'
  
   SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID

     CALL read_netcdf_buffer_globe(orography_buffer_file,  &
       &                                     tg,         &
       &                                     undefined, &
       &                                     undef_int,   &
       &                                     fr_land_globe, &
       &                                     hh_globe,            &
       &                                     stdh_globe,          &
       &                                     theta_globe,         &
       &                                     aniso_globe,         &
       &                                     slope_globe,         &
       &                                     z0_topo,             &
       &                                     vertex_param)

     CASE DEFAULT


     CALL read_netcdf_buffer_globe(orography_buffer_file,  &
       &                                     tg,         &
       &                                     undefined, &
       &                                     undef_int,   &
       &                                     fr_land_globe, &
       &                                     hh_globe,            &
       &                                     stdh_globe,          &
       &                                     theta_globe,         &
       &                                     aniso_globe,         &
       &                                     slope_globe,         &
       &                                     z0_topo)



   END SELECT


   PRINT *,'Read in aot data'

   CALL read_netcdf_buffer_aot(aot_buffer_file,  &
   &                                     tg,         &
   &                                     ntype_aot,           &
   &                                     ntime_aot,        &
   &                                     aot_tg)



 
   CALL read_netcdf_buffer_cru(t_clim_buffer_file,  &
    &                                     tg,         &
    &                                     crutemp)

    PRINT *,'Read in FLAKE'
  CALL read_netcdf_buffer_flake(flake_buffer_file,  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     flake_tot_npixel)



!------------------------------------------------------------------------------------------
!------------- land use data --------------------------------------------------------------
!------------------------------------------------------------------------------------------
      PRINT *,'determine land-sea mask'
      CALL CPU_TIME(timestart)

      WHERE (lat_geo >=-56.) ! glc2000 data only from -56 degrees north to 90 degrees north, excluding Antarctica
        fr_land_lu = fr_land_glc2000
      ELSEWHERE
        fr_land_lu = fr_land_glcc
      ENDWHERE
      
      !set land-sea mask, spread at 0.5 due to poor accuracy of values in GRIB files
       WHERE (fr_land_lu < 0.5) 
        fr_land_lu = MIN(0.49,fr_land_lu)
      ELSEWHERE !   fr_land_glc2000 >= 0.5
        fr_land_lu = MAX(0.51,fr_land_lu)
      ENDWHERE


      CALL CPU_TIME(timeend)
      timediff = timeend - timestart
      PRINT *,'determine land-sea mask, WHERE, done in: ', timediff
      WHERE (lat_geo >=-56.) ! glc2000 data only from -56 degrees north to 90 degrees north, excluding Antarctica
        ice_lu            = ice_glc2000
        z0_lu             = z0_glc2000
        z0_tot            = z0_glc2000 + z0_topo
        root_lu           = root_glc2000
        plcov_mn_lu       = plcov_mn_glc2000
        plcov_mx_lu       = plcov_mx_glc2000
        lai_mn_lu         = lai_mn_glc2000
        lai_mx_lu         = lai_mx_glc2000
        rs_min_lu         = rs_min_glc2000
        urban_lu          = urban_glc2000
        for_d_lu          = for_d_glc2000
        for_e_lu          = for_e_glc2000
        emissivity_lu     = emissivity_glc2000
      ELSEWHERE
        ice_lu            = ice_glcc
        z0_lu             = z0_glcc 
        z0_tot            = z0_glcc + z0_topo
        root_lu           = root_glcc
        plcov_mn_lu       = plcov_mn_glcc
        plcov_mx_lu       = plcov_mx_glcc
        lai_mn_lu         = lai_mn_glcc
        lai_mx_lu         = lai_mx_glcc
        rs_min_lu         = rs_min_glcc
        urban_lu          = urban_glcc
        for_d_lu          = for_d_glcc
        for_e_lu          = for_e_glcc
        emissivity_lu     = emissivity_glcc
      ENDWHERE
      
!------------------------------------------------------------------------------------------
!------------- land use data consistency  -------------------------------------------------
!------------------------------------------------------------------------------------------
      WHERE (fr_land_lu < 0.5)  ! set vegetation to undefined (0.) for water grid elements
        ! z0 and emissivity are not set to undefined_lu
        ice_lu            = undefined_lu
        root_lu           = undefined_lu
        plcov_mn_lu       = undefined_lu
        plcov_mx_lu       = undefined_lu
        lai_mn_lu         = undefined_lu
        lai_mx_lu         = undefined_lu
        rs_min_lu         = undefined_lu
        urban_lu          = undefined_lu
        for_d_lu          = undefined_lu
        for_e_lu          = undefined_lu
      ENDWHERE

!------------------------------------------------------------------------------------------
!------------- soil data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

     ! undef_soiltype   = 0 ! \TODO read undef_soiltype from netcdf file (_Fill_Value)
     ! default_soiltype = 5 ! default soil type loam
     ! soiltype_ice     = 1   !< soiltype for ice
     ! soiltype_water   = 9   !< soiltype for water
      PRINT *,'Soil data consistency check'

      WHERE (fr_land_lu < 0.5)  ! set water soiltype for water grid elements
        soiltype_fao = soiltype_water
      ELSEWHERE ! fr_land_lu >= 0.5, i.e. a land grid element
      ! (soiltyp(:,:) > 8 .OR. soiltyp(:,:) < 1))
        WHERE ((soiltype_fao == undef_soiltype).OR.(soiltype_fao > 8) ) ! land grid elements must have a valid soiltype
         !  WHERE ( (lat_geo < -60.).OR.(lat_geo > 65.) ) ! Arctic and Antarctica
         !     soiltype_fao = soiltype_ice  ! set soil type to ice for Arctic or Antarctic undefinded points
           WHERE ( (lat_geo < -60.) ) ! Antarctica
             soiltype_fao = soiltype_ice  ! set soil type to ice for Antarctic undefinded points

           ELSEWHERE  ! rest of the World 
              soiltype_fao = default_soiltype ! set default soiltype to loam
           ENDWHERE   
        ENDWHERE
      ENDWHERE

      CALL CPU_TIME(timeend)
      timediff = timeend - timestart
      PRINT *,'soil data consitency check, WHERE, done in: ', timediff

      db_ice_counter = 0

      DO k=1,tg%ke
      DO j=1,tg%je
      DO i=1,tg%ie
        IF  ( (soiltype_fao(i,j,k) /= soiltype_ice) .AND.  & 
           &    (fr_land_lu(i,j,k) > 0.5).AND. (ice_lu(i,j,k) ==  fr_land_lu(i,j,k)) ) THEN
             soiltype_fao(i,j,k) = soiltype_ice 
             db_ice_counter = db_ice_counter +1
        ENDIF
      ENDDO
      ENDDO
      ENDDO
!------------------------------------------------------------------------------------------
!------------- soil data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
!------------- flake data consistency  ----------------------------------------------------
!------------------------------------------------------------------------------------------

      PRINT *,'flake data consistency check'
      CALL CPU_TIME(timestart)

     ! determine "fraction ocean" first before considering "fraction lake"
     ! fr_ocean should be determined by ocean model if available
     ! so use (1. - lsm_ocean_model) as mask instead of fr_land_globe from the orography data
     WHERE (fr_land_globe < 0.99)
       fr_ocean_lu = 1. - fr_land_lu
       fr_lake = 0.0
     ELSEWHERE
       fr_ocean_lu = 0.0
       fr_lake = 1. - fr_land_lu
     ENDWHERE

     ! set Death Sea to "ocean water"
     WHERE ((hh_globe < -390.).AND. &
      &     (lon_geo > 35.).AND.(lon_geo < 36.).AND. &
      &     (lat_geo > 31.).AND.(lat_geo < 32.) )
       fr_ocean_lu = 1. - fr_land_lu
       fr_lake = 0.0
     ENDWHERE
     
     ! set Caspian Sea to "ocean water"
     WHERE ((hh_globe < 33.).AND. &
      &     (lon_geo > 46.).AND.(lon_geo < 55.).AND. &
      &     (lat_geo > 36.).AND.(lat_geo < 48.) )
       fr_ocean_lu = 1. - fr_land_lu
       fr_lake = 0.0
     ENDWHERE
     ! here fr_ocean+fr_lake+fr_land = 1


     ! check consistency for "lake depth"
     WHERE (fr_land_lu >= 0.5)  
       lake_depth = flake_depth_undef ! set lake depth to flake_depth_undef (-1 m)
     ENDWHERE 
     WHERE (fr_ocean_lu >= 0.5)  
       lake_depth = flake_depth_undef ! set lake depth to flake_depth_undef (-1 m)
     ENDWHERE 

     WHERE ((fr_lake > 0.5).AND.(lake_depth < 0.0))
       lake_depth = flake_depth_default ! set lake depth to default value (10 m)
     ENDWHERE ! 
     ! restrict lake depth to maximum value
     WHERE (lake_depth > DWD_max_lake_depth)
      lake_depth = DWD_max_lake_depth
     END WHERE


      
      CALL CPU_TIME(timeend)
      timediff = timeend - timestart
      PRINT *,'flake data consitency check, WHERE, done in: ', timediff

!------------------------------------------------------------------------------------------
!------------- NDVI data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

      ! set default NDVI values for land grid elements with so far undefined values or very small NDVI values
      PRINT *,'NDVI data consistency check'
      
      CALL CPU_TIME(timestart)
      !minimal_ndvi = 0.09 ! bare soil value
      !undef_ndvi   = 0.0  ! no vegetation

      WHERE (fr_land_lu < 0.5) ! set undefined NDVI value (0.0) for water grid elements
        ndvi_max = undef_ndvi
      ELSEWHERE ! fr_land_lu >= 0.5
        WHERE (ndvi_max <= minimal_ndvi) ! small NDVI values at land grid elements
          ndvi_max = minimal_ndvi
        ENDWHERE
      ENDWHERE
        
      FORALL (t=1:mpy) ! mpy = month per year = 12
        WHERE (fr_land_lu < 0.5) ! set undefined NDVI value (0.0) for water grid elements
          ndvi_field_mom(:,:,:,t) = undef_ndvi
          ndvi_ratio_mom(:,:,:,t) = undef_ndvi
         ELSEWHERE ! fr_land_lu >= 0.5
           WHERE (ndvi_max(:,:,:) <= minimal_ndvi) ! small NDVI values at land grid elements
              ndvi_field_mom(:,:,:,t) = minimal_ndvi
              ndvi_ratio_mom(:,:,:,t) = 1.0  ! minimal_ndvi / minimal_ndvi ! ndvi_max set to minimal_ndvi
            ENDWHERE 
            WHERE (ndvi_field_mom(:,:,:,t) <= minimal_ndvi) ! small NDVI values at land grid elements
              ndvi_field_mom(:,:,:,t) = minimal_ndvi
              ndvi_ratio_mom(:,:,:,t) = minimal_ndvi / ndvi_max(:,:,:)
            ENDWHERE
        ENDWHERE
      END FORALL
       
     CALL CPU_TIME(timeend)
     timediff = timeend - timestart
     PRINT *,'NDVI data consitency check, WHERE, done in:  ', timediff


!------------------------------------------------------------------------------------------
!------------- NDVI data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
!------------- data output ----------------------------------------------------------------
!------------------------------------------------------------------------------------------
fill_value_real = -999.
fill_value_int = -999

SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID
        
       PRINT *,'write out ', TRIM(netcdf_output_filename)

       CALL write_netcdf_icon_grid_extpar(TRIM(netcdf_output_filename),  &
    &                                     icon_grid,       &
    &                                     tg,         &
    &                                     fill_value_real, &
    &                                     fill_value_int,   &
    &                                     TRIM(name_lookup_table_glc2000), &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, &
    &                                     glc2000_class_fraction,    &
    &                                     ice_lu, &
    &                                     z0_tot, &
    &                                     root_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     soiltype_fao, &
    &                                     ndvi_max,  &
    &                                     ndvi_field_mom,&
    &                                     ndvi_ratio_mom, &
    &                                     hh_globe,            &
    &                                     stdh_globe,          &
    &                                     theta_globe,         &
    &                                     aniso_globe,         &
    &                                     slope_globe,   &
    &                                     vertex_param,  &
    &                                     aot_tg, &
    &                                     crutemp )



     CASE(igrid_cosmo) ! COSMO grid


      PRINT *,'write out ', TRIM(netcdf_output_filename)
       CALL  write_netcdf_cosmo_grid_extpar(TRIM(netcdf_output_filename),  &
    &                                     cosmo_grid,       &
    &                                     tg,         &
    &                                     fill_value_real, &
    &                                     fill_value_int,   &
    &                                     TRIM(name_lookup_table_glc2000), &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, &
    &                                     glc2000_class_fraction,    &
    &                                     ice_lu, &
    &                                     z0_tot, &
    &                                     z0_lu, &
    &                                     z0_topo,  &
    &                                     root_lu, &
    &                                     plcov_mn_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mn_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     soiltype_fao, &
    &                                     ndvi_max,  &
    &                                     ndvi_field_mom,&
    &                                     ndvi_ratio_mom, &
    &                                     hh_globe,            &
    &                                     stdh_globe,          &
    &                                     theta_globe,         &
    &                                     aniso_globe,         &
    &                                     slope_globe,   &
    &                                     aot_tg, &
    &                                     crutemp )
     
     PRINT *,'write out ', TRIM(grib_output_filename)
     CALL  write_cosmo_grid_extpar_grib(TRIM(grib_output_filename),  &
    &                                     cosmo_grid,       &
    &                                     tg,         &
    &                                     fill_value_real, &
    &                                     fill_value_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, & !&                                     ice_lu, &
    &                                     z0_tot, &
    &                                     root_lu, &
    &                                     plcov_mn_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mn_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     soiltype_fao, &
    &                                     ndvi_max,  &
    &                                     ndvi_field_mom,&
    &                                     ndvi_ratio_mom, &
    &                                     hh_globe,            &
    &                                     stdh_globe,          &
    &                                     theta_globe,         &
    &                                     aniso_globe,         &
    &                                     slope_globe,   &
    &                                     aot_tg, &
    &                                     crutemp )

  
     CASE(igrid_gme) ! GME grid  
     PRINT *,'write out ', TRIM(grib_output_filename)
     CALL  write_gme_grid_extpar_grib(TRIM(grib_output_filename),  &
    &                                     gme_grid,       &
    &                                     tg,         &
    &                                     fill_value_real, &
    &                                     fill_value_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, & !&                                     ice_lu, &
    &                                     z0_tot, &
    &                                     root_lu, &
    &                                     plcov_mn_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mn_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     soiltype_fao, &
    &                                     ndvi_max,  &
    &                                     ndvi_field_mom,&
    &                                     ndvi_ratio_mom, &
    &                                     hh_globe,            &
    &                                     stdh_globe,          &
    &                                     theta_globe,         &
    &                                     aniso_globe,         &
    &                                     slope_globe,   &
    &                                     aot_tg, &
    &                                     crutemp )

END SELECT


        

END PROGRAM extpar_consistency_check
