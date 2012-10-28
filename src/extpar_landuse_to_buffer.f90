!+ Fortran main program to aggregate land use data to a target grid 
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
! V1_3         2011/04/19 Hermann Asensio
! introduce Globcover 2009 land use data set for external parameters
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran main program to aggregate land use data to a target grid
!!
!! @par extpar_landuse_to_buffer 
!!
!! 
!! This program aggregates the GLC2000 land use data and the GLCC data to a given target grid (COSMO/ICON).
!! The desired external parameters are mapped from the GLC2000 land use classes with look-up tables 
!! (see module mo_glc2000_data) and avereaged to the target grid cell (see module mo_agg_glc2000 for details),
!! this is also done for the GLCC data (which cover the whole earth, while GLC2000 data does not cover Antarctica.
!! 
!!
!! @author
!!     Hermann Asensio
!!     (DWD)
!!
!!
!!
PROGRAM extpar_landuse_to_buffer
  
  ! Load the library information data:
  USE info_extpar, ONLY: info_define, info_readnl, info_print


  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  USE mo_grid_structures, ONLY: target_grid_def,   &
    &                            reg_lonlat_grid,   &
    &                            rotated_lonlat_grid
  
  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme

  USE mo_target_grid_data, ONLY: lon_geo, &
    &                            lat_geo, &
    &                            no_raw_data_pixel, &
    &                            allocate_com_target_fields
  
  USE mo_target_grid_data, ONLY: tg
  
  USE mo_target_grid_routines, ONLY: init_target_grid

  USE mo_icon_grid_data, ONLY: ICON_grid  !< structure which contains the definition of the ICON grid
                                ! icon_grid_region, &
                                ! icon_grid_level
 
  USE mo_icon_grid_routines, ONLY: allocate_icon_grid

  USE  mo_cosmo_grid, ONLY: COSMO_grid, &
    &                       lon_rot, &
    &                       lat_rot, &
    &                       allocate_cosmo_rc, &
    &                       get_cosmo_grid_info, &
    &                       calculate_cosmo_domain_coordinates

  USE mo_base_geometry,    ONLY:  geographical_coordinates, &
    &                             cartesian_coordinates

  USE mo_icon_domain,          ONLY: icon_domain, &
    &                             grid_cells,               &
    &                             grid_vertices,            &
    &                             construct_icon_domain,    &
    &                             destruct_icon_domain

  USE mo_icon_domain, ONLY: max_dom
  
  USE mo_io_units,          ONLY: filename_max

  USE mo_exception,         ONLY: message_text, message, finish

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_search_icongrid,   ONLY: walk_to_nc,              &
                                  find_nc_dom1,            &
                                  find_nc

  USE mo_icon_grid_routines,ONLY: inq_grid_dims,            &
    &                              inq_domain_dims,          &
    &                              read_grid_info_part,      &
    &                              read_domain_info_part,    &
    &                              read_gridref_nl
  
  USE mo_additional_geometry,   ONLY: cc2gc,                  &
    &                            gc2cc,                  &
    &                            arc_length,             &
    &                            cos_arc_length,         &
    &                            inter_section,          &
    &                            vector_product,         &
    &                            point_in_polygon_sp


  USE mo_math_constants,  ONLY: pi, pi_2, dbl_eps,rad2deg

  USE mo_landuse_routines, ONLY: read_namelists_extpar_land_use

  USE mo_landuse_routines, ONLY:  get_dimension_glcc_data, &
    &                             get_lonlat_glcc_data, &
    &                             get_dimension_glc2000_data,       &
    &                             get_lonlat_glc2000_data



 USE mo_glc2000_data, ONLY: glc2000_grid, &
    &                         lon_glc2000,  &
    &                         lat_glc2000,  &
    &                         allocate_raw_glc2000_fields

  USE mo_glcc_data, ONLY: glcc_grid, &
 &         lon_glcc,  &
 &         lat_glcc,  &
 &         allocate_raw_glcc_fields

  USE mo_glc2000_lookup_tables, ONLY: glc2000_legend
  USE mo_glc2000_lookup_tables, ONLY: nclass_glc2000
  USE mo_glc2000_lookup_tables, ONLY: i_gme_lookup_table, &
    &                                 i_cosmo_lookup_table,  &
    &                                 i_experimental_lookup_table
  USE mo_glc2000_lookup_tables, ONLY: init_glc2000_lookup_tables

  USE mo_glc2000_lookup_tables, ONLY:   z0_lt_glc2000, lnz0_lt_glc2000, plc_mn_lt_glc2000, plc_mx_lt_glc2000, & 
    &               lai_mn_lt_glc2000, lai_mx_lt_glc2000, rd_lt_glc2000, emiss_lt_glc2000, rs_min_lt_glc2000   

USE mo_glcc_lookup_tables, ONLY: nclass_glcc
USE mo_glcc_lookup_tables, ONLY: init_glcc_lookup_tables
USE mo_glcc_lookup_tables, ONLY: glcc_legend
USE mo_glcc_lookup_tables, ONLY: ilookup_table_glcc, &
  &                              i_gme_lookup_table_glcc, &
  &                              i_cosmo_lookup_table_glcc, &
  &                              i_experimental_lookup_table_glcc
USE mo_glcc_lookup_tables, ONLY: z0_lt_glcc, lnz0_lt_glcc, plc_mn_lt_glcc, plc_mx_lt_glcc

USE mo_glcc_lookup_tables, ONLY: lai_mn_lt_glcc, lai_mx_lt_glcc, rd_lt_glcc, emiss_lt_glcc, rs_min_lt_glcc         

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

  USE mo_agg_glc2000, ONLY : agg_glc2000_data_to_target_grid

  USE mo_agg_glcc, ONLY : agg_glcc_data_to_target_grid

  USE mo_lu_tg_fields, ONLY :  i_lu_globcover, i_lu_glc2000, i_lu_glcc
  USE mo_lu_tg_fields, ONLY: allocate_lu_target_fields, allocate_add_lu_fields
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
  &        fr_ocean_lu, &
  &        lu_class_fraction,    &
  &        lu_class_npixel, &
  &        lu_tot_npixel



  USE mo_landuse_output_nc, ONLY: write_netcdf_buffer_glc2000, &
    &                             write_netcdf_cosmo_grid_glc2000, &
    &                             write_netcdf_icon_grid_glc2000

  USE mo_landuse_output_nc, ONLY: write_netcdf_buffer_glcc, &
    &                             write_netcdf_cosmo_grid_glcc, &
    &                             write_netcdf_icon_grid_glcc
  USE mo_landuse_output_nc, ONLY: write_netcdf_buffer_lu             
  USE mo_globcover_lookup_tables, ONLY: nclass_globcover
  USE mo_globcover_lookup_tables, ONLY: init_globcover_lookup_tables
  USE mo_landuse_routines, ONLY: get_dimension_globcover_data,       &
    &                             get_lonlat_globcover_data

  USE mo_globcover_data, ONLY: globcover_grid, &
    &                         lon_globcover,  &
    &                         lat_globcover,  &
    &                         allocate_raw_globcover_fields

 USE mo_agg_globcover, ONLY : agg_globcover_data_to_target_grid

  IMPLICIT NONE
  
  CHARACTER(len=filename_max) :: filename
  CHARACTER(len=filename_max) :: netcdf_filename

  CHARACTER(len=filename_max) :: input_namelist_file
  CHARACTER(len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition

  CHARACTER(len=filename_max) :: namelist_grid_def

  CHARACTER (len=filename_max) :: namelist_globe_data_input !< file with input namelist with GLOBE data information
  CHARACTER(len=filename_max) :: input_lu_namelist_file
  CHARACTER(len=filename_max) :: lu_file

  CHARACTER (len=filename_max) :: raw_data_lu_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_lu_filename !< filename glc2000 raw data
  CHARACTER(len=filename_max) :: glcc_file

  CHARACTER (len=filename_max) :: lu_buffer_file !< name for glc2000 buffer file
  CHARACTER (len=filename_max) :: lu_output_file !< name for glc2000 output file

  CHARACTER (len=filename_max) :: raw_data_glcc_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_glcc_filename !< filename glcc raw data

  CHARACTER (len=filename_max) :: glcc_buffer_file !< name for glcc buffer file
  CHARACTER (len=filename_max) :: glcc_output_file !< name for glcc output file

  INTEGER :: i,j,k !< counter

  REAL (KIND=wp) :: undefined
  REAL (KIND=wp) :: tg_southern_bound

  LOGICAL :: l_use_glcc=.FALSE.

  INTEGER :: undef_int

  INTEGER (KIND=i8) :: nlon_globcover !< number of grid elements in zonal direction for globcover data
  INTEGER (KIND=i8) :: nlat_globcover !< number of grid elements in meridional direction for globcover data

  INTEGER (KIND=i8) :: nlon_glc2000 !< number of grid elements in zonal direction for glc2000 data
  INTEGER (KIND=i8) :: nlat_glc2000 !< number of grid elements in meridional direction for glc2000 data

  INTEGER (KIND=i8) :: nlon_glcc !< number of grid elements in zonal direction for glcc data
  INTEGER (KIND=i8) :: nlat_glcc !< number of grid elements in meridional direction for glcc data

  !--------------------------------------------------------------------------------------

  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid
  INTEGER  :: i_landuse_data !<integer switch to choose a land use raw data set
  INTEGER  :: ilookup_table_lu !< integer switch to choose a lookup table
  INTEGER  :: nclass_lu !< number of land use classes 

  
  ! Print the default information to stdout:
  CALL info_define ('extpar_landuse_to_buffer')      ! Pre-define the program name as binary name
  CALL info_print ()                     ! Print the information to stdout
  !--------------------------------------------------------------------------------------------------------

  namelist_grid_def = 'INPUT_grid_org'
  CALL init_target_grid(namelist_grid_def)

  igrid_type = tg%igrid_type
  tg_southern_bound=MINVAL(lat_geo) ! get southern boundary of target grid
  CALL allocate_lu_target_fields(tg)
  print *,'Grid defined, lu target fields allocated'

  !------------------------------------------------------------------------------------

  ! get information about landuse data

  ! get info on raw data file
  input_lu_namelist_file = 'INPUT_LU'

  !---------------------------------------------------------------------------
  CALL read_namelists_extpar_land_use(input_lu_namelist_file, &
    &                                 i_landuse_data, &
    &                                 raw_data_lu_path, &
    &                                 raw_data_lu_filename, &
    &                                 ilookup_table_lu, &
    &                                 lu_buffer_file, &
    &                                 lu_output_file, &
    &                                 raw_data_glcc_path, &
    &                                  raw_data_glcc_filename, &
    &                                  ilookup_table_glcc,         &
    &                                  glcc_buffer_file, &
    &                                  glcc_output_file)


  PRINT *,'raw_data_glcc_filename: ',TRIM(raw_data_glcc_filename)
  lu_file = TRIM(raw_data_lu_path) // TRIM(raw_data_lu_filename)

  glcc_file = TRIM(raw_data_glcc_path) // TRIM(raw_data_glcc_filename)

  PRINT *,'lu_file: ', TRIM(lu_file)
  PRINT *,'glcc file: ', TRIM(glcc_file)

  SELECT CASE (i_landuse_data)
    CASE (i_lu_globcover)
      nclass_lu = nclass_globcover

      CALL get_dimension_globcover_data(nlon_globcover, &
        &                                  nlat_globcover)
      CALL allocate_raw_globcover_fields(nlat_globcover,nlon_globcover)
      CALL allocate_add_lu_fields(tg,nclass_globcover)
      CALL get_lonlat_globcover_data( &
        &                              nlon_globcover, &
        &                              nlat_globcover, &
        &                              lon_globcover,  &
        &                              lat_globcover,  &
        &                              globcover_grid)
        !HA debug
        PRINT *,'globcover_grid: ',globcover_grid
        ! If southern boundary of target grid is south of southern boundary of Globcover data
        ! (Globcover 2009 does not include Antarctica) then also process GLCC data)
        IF (tg_southern_bound < globcover_grid%end_lat_reg) THEN
          l_use_glcc=.TRUE.
          CALL allocate_glcc_target_fields(tg)
        ENDIF  
    CASE (i_lu_glc2000)
      nclass_lu = nclass_glc2000

      CALL get_dimension_glc2000_data(lu_file, &
        &                                  nlon_glc2000, &
        &                                  nlat_glc2000)
      CALL allocate_raw_glc2000_fields(nlat_glc2000,nlon_glc2000)
      CALL allocate_add_lu_fields(tg,nclass_glc2000)
      CALL get_lonlat_glc2000_data(lu_file, &
        &                              nlon_glc2000, &
        &                              nlat_glc2000, &
        &                              lon_glc2000,  &
        &                              lat_glc2000,  &
        &                              glc2000_grid)
      ! If southern boundary of target grid is south of southern boundary of GLC2000 data
      ! (GLC2000 does not include Antarctica) then also process GLCC data)
      IF(tg_southern_bound < glc2000_grid%end_lat_reg) THEN
        l_use_glcc=.TRUE.
        CALL allocate_glcc_target_fields(tg)
      ENDIF 
    CASE (i_lu_glcc)
      nclass_lu = nclass_glcc
  END SELECT

  IF (l_use_glcc.OR.(i_landuse_data==i_lu_glcc)) THEN
     
      CALL get_dimension_glcc_data(glcc_file, &
        &                           nlon_glcc, &
        &                           nlat_glcc)

      CALL allocate_raw_glcc_fields(nlat_glcc, nlon_glcc)

      CALL  get_lonlat_glcc_data(glcc_file, &
       &                                   nlon_glcc, &
       &                                   nlat_glcc, &
       &                                   lon_glcc,  &
       &                                   lat_glcc,  &
       &                                   glcc_grid)

  ENDIF

  undefined = 0.0_wp
  PRINT *,'aggregate land use data to target grid'
 SELECT CASE (i_landuse_data)
    CASE(i_lu_globcover)

    CALL agg_globcover_data_to_target_grid(lu_file,ilookup_table_lu,undefined,       &
    &                                        tg,                                         &
    &                                        nclass_globcover,                             &
    &                                        lu_class_fraction, &
    &                                        lu_class_npixel, &
    &                                        lu_tot_npixel,   &
    &                                        fr_land_lu ,     &
    &                                        ice_lu,          &
    &                                        z0_lu, &
    &                                        root_lu, &
    &                                        plcov_mn_lu, &
    &                                        plcov_mx_lu, &
    &                                        lai_mn_lu,   &
    &                                        lai_mx_lu, &
    &                                        rs_min_lu, &
    &                                        urban_lu,  &
    &                                        for_d_lu,  &
    &                                        for_e_lu, &
    &                                        emissivity_lu    )


    CASE(i_lu_glc2000)

    CALL agg_glc2000_data_to_target_grid(lu_file,ilookup_table_lu,undefined,       &
    &                                        tg,                                         &
    &                                        nclass_glc2000,                             &
    &                                        lu_class_fraction, &
    &                                        lu_class_npixel, &
    &                                        lu_tot_npixel,   &
    &                                        fr_land_lu ,     &
    &                                        ice_lu,          &
    &                                        z0_lu, &
    &                                        root_lu, &
    &                                        plcov_mn_lu, &
    &                                        plcov_mx_lu, &
    &                                        lai_mn_lu,   &
    &                                        lai_mx_lu, &
    &                                        rs_min_lu, &
    &                                        urban_lu,  &
    &                                        for_d_lu,  &
    &                                        for_e_lu, &
    &                                        emissivity_lu    )

   CASE(i_lu_glcc)

    CALL agg_glcc_data_to_target_grid(lu_file,ilookup_table_lu,undefined,       &
    &                                        tg,                                         &
    &                                        nclass_glcc,                             &
    &                                        lu_class_fraction, &
    &                                        lu_class_npixel, &
    &                                        lu_tot_npixel,   &
    &                                        fr_land_lu ,     &
    &                                        ice_lu,          &
    &                                        z0_lu, &
    &                                        root_lu, &
    &                                        plcov_mn_lu, &
    &                                        plcov_mx_lu, &
    &                                        lai_mn_lu,   &
    &                                        lai_mx_lu, &
    &                                        rs_min_lu, &
    &                                        urban_lu,  &
    &                                        for_d_lu,  &
    &                                        for_e_lu, &
    &                                        emissivity_lu    )

  END SELECT

  IF (l_use_glcc) THEN ! additionally process GLCC data
    CALL agg_glcc_data_to_target_grid(glcc_file,ilookup_table_glcc,undefined,  &
      &                                        tg,                              &
      &                                        nclass_glcc,                     &
      &                                        glcc_class_fraction, &
      &                                        glcc_class_npixel, &
      &                                        glcc_tot_npixel,   &
      &                                        fr_land_glcc ,     &
      &                                        ice_glcc,          &
      &                                        z0_glcc, &
      &                                        root_glcc, &
      &                                        plcov_mn_glcc, &
      &                                        plcov_mx_glcc, &
      &                                        lai_mn_glcc,   &
      &                                        lai_mx_glcc, &
      &                                        rs_min_glcc, &
      &                                        urban_glcc,  &
      &                                        for_d_glcc,  &
      &                                        for_e_glcc, &
      &                                        emissivity_glcc    )

  ENDIF
    PRINT *,'aggregation of land use data done'

  !--------------------------------------------------------------------------------
  ! output
   undefined = -999.0_wp
   undef_int = -999

   netcdf_filename = TRIM(lu_buffer_file)
   print *, 'GLC2000 buffer filename: ',TRIM(netcdf_filename)

   
   CALL write_netcdf_buffer_lu(TRIM(netcdf_filename),  &
    &                                     tg,         &
    &                                     i_landuse_data, &
    &                                     ilookup_table_lu, &
    &                                     nclass_lu, &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, &
    &                                     lu_class_fraction,    &
    &                                     lu_class_npixel, &
    &                                     lu_tot_npixel, &
    &                                     ice_lu, &
    &                                     z0_lu, &
    &                                     root_lu, &
    &                                     plcov_mn_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mn_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu)

    IF (l_use_glcc) THEN !
      netcdf_filename = TRIM(glcc_buffer_file)
      print *, 'GLCC buffer filename: ',TRIM(netcdf_filename)
   CALL write_netcdf_buffer_glcc(TRIM(netcdf_filename),  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
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
   ENDIF


END PROGRAM extpar_landuse_to_buffer

