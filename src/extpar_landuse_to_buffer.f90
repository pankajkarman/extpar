!+ Fortran main program to aggregate land use data to a target grid 
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
  USE mo_icon_grid_data, ONLY: icon_domain_grid
 
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
  USE mo_glc2000_lookup_tables, ONLY: ilookup_table_glc2000, &
    &                                 i_gme_lookup_table,    &
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


  USE mo_landuse_output_nc, ONLY: write_netcdf_buffer_glc2000, &
    &                             write_netcdf_cosmo_grid_glc2000, &
    &                             write_netcdf_icon_grid_glc2000

  USE mo_landuse_output_nc, ONLY: write_netcdf_buffer_glcc, &
    &                             write_netcdf_cosmo_grid_glcc, &
    &                             write_netcdf_icon_grid_glcc
               
  
  IMPLICIT NONE
  
  CHARACTER(len=filename_max) :: filename
  CHARACTER(len=filename_max) :: netcdf_filename

  CHARACTER(len=filename_max) :: input_namelist_file
  CHARACTER(len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition

  CHARACTER(len=filename_max) :: namelist_grid_def

  CHARACTER (len=filename_max) :: namelist_globe_data_input !< file with input namelist with GLOBE data information
  CHARACTER(len=filename_max) :: input_glc2000_namelist_file 
  CHARACTER(len=filename_max) :: glc2000_file

  CHARACTER (len=filename_max) :: raw_data_glc2000_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_glc2000_filename !< filename glc2000 raw data
  CHARACTER(len=filename_max) :: glcc_file

  CHARACTER (len=filename_max) :: glc2000_buffer_file !< name for glc2000 buffer file
  CHARACTER (len=filename_max) :: glc2000_output_file !< name for glc2000 output file

 CHARACTER (len=filename_max) :: raw_data_glcc_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_glcc_filename !< filename glcc raw data

  CHARACTER (len=filename_max) :: glcc_buffer_file !< name for glcc buffer file
  CHARACTER (len=filename_max) :: glcc_output_file !< name for glcc output file


  INTEGER :: ilev, ip, iplev, ic, in, iclev, istartlev

  ! Namelist variables
  INTEGER :: grid_root                    !< number of partitions of the icosahedron
  INTEGER :: start_lev                    !< level of (first) global model domain
  INTEGER :: n_dom                        !< number of model domains
  INTEGER :: parent_id(max_dom-1)         !< id of parent model domain


  INTEGER                      :: i_nc       !< number of cells
  INTEGER                      :: i_ne       !< number of edges
  INTEGER                      :: i_nv       !< number of vertices
  INTEGER                      :: nc_p_e     !< number of cells per edge
  INTEGER                      :: nv_p_c     !< number of vertices per cell
  INTEGER                      :: ne_p_v     !< number of edges per vertex
  INTEGER                      :: nchilds    !< number of child cells per cell

  INTEGER                      :: n_childdom !< actual number of child domains

  INTEGER :: first_dom
  INTEGER :: idom

  TYPE(icon_domain) , ALLOCATABLE, TARGET :: icon_grid_all(:)

  INTEGER, ALLOCATABLE :: level_region(:)   ! level of region

  TYPE(geographical_coordinates) :: tpoint

  INTEGER :: start_id 
  INTEGER :: nearest_cell_id

  INTEGER :: nj
  INTEGER :: nb_cell_id
  TYPE(cartesian_coordinates)  :: neighbour_cc     !> coordinates of a neighbour cell centre in cartesian system
  REAL(KIND=wp)                :: sp               !> cos arc length of  of geodesic arc with endpoints x0,x1 (normalized scalar product of the two points)
  REAL(KIND=wp)                :: sp_max
  TYPE(geographical_coordinates) :: target_geo_co    !> target coordinates in geographical system of point for which the nearest ICON grid cell is to be determined
  TYPE(cartesian_coordinates)  :: target_cc_co     !>  target coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined

  INTEGER, ALLOCATABLE :: nearest_cell_ids(:)    !< array with ids of nearest cell for the domains
  TYPE(cartesian_coordinates), ALLOCATABLE :: polygon(:)
  TYPE(cartesian_coordinates)              :: point
  TYPE(cartesian_coordinates)              :: out_point
  TYPE(geographical_coordinates)           :: out_point_geo
  TYPE(geographical_coordinates), ALLOCATABLE :: poly_geo(:)

  INTEGER                                  :: inflag

  INTEGER                                  :: vert_index
  INTEGER                                  :: ivert

  TYPE(cartesian_coordinates), ALLOCATABLE :: test_poly(:)
  TYPE(cartesian_coordinates)              :: test_point
  TYPE(geographical_coordinates)           :: test_point_geo
  TYPE(cartesian_coordinates)              :: test_out_point
  TYPE(geographical_coordinates)           :: test_out_point_geo
  TYPE(geographical_coordinates), ALLOCATABLE :: test_poly_geo(:)

  INTEGER :: i,j,k !< counter
  INTEGER (KIND=i8) :: icell

  REAL (KIND=wp) :: undefined
  INTEGER :: undef_int


  INTEGER (KIND=i8) :: nlon_glc2000 !< number of grid elements in zonal direction for glc2000 data
  INTEGER (KIND=i8) :: nlat_glc2000 !< number of grid elements in meridional direction for glc2000 data

  INTEGER (KIND=i8) :: nlon_glcc !< number of grid elements in zonal direction for glcc data
  INTEGER (KIND=i8) :: nlat_glcc !< number of grid elements in meridional direction for glcc data

  !--------------------------------------------------------------------------------------

  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid

  
  ! Print the default information to stdout:
  CALL info_define ('aot_to_buffer')      ! Pre-define the program name as binary name
  CALL info_print ()                     ! Print the information to stdout
  !--------------------------------------------------------------------------------------------------------



  namelist_grid_def = 'INPUT_grid_org'
  CALL init_target_grid(namelist_grid_def)

  PRINT *,' target grid tg: ',tg

  igrid_type = tg%igrid_type

  CALL allocate_glc2000_target_fields(tg)

  CALL allocate_glcc_target_fields(tg)
  print *,'Grid defined, target fields allocated'

  !------------------------------------------------------------------------------------
  !------------------------------------------------------------------------------------


  ! get information about GLC2000 data

  ! get info on raw data file
  input_glc2000_namelist_file = 'INPUT_LU'

  !---------------------------------------------------------------------------
  CALL read_namelists_extpar_land_use(input_glc2000_namelist_file, &
    &                                 raw_data_glc2000_path, &
    &                                 raw_data_glc2000_filename, &
    &                                 ilookup_table_glc2000, &
    &                                 glc2000_buffer_file, &
    &                                 glc2000_output_file, &
    &                                 raw_data_glcc_path, &
                                      raw_data_glcc_filename, &
                                      ilookup_table_glcc,         &
                                      glcc_buffer_file, &
                                      glcc_output_file)


  PRINT *,'raw_data_glcc_filename: ',TRIM(raw_data_glcc_filename)
  glc2000_file = TRIM(raw_data_glc2000_path) // TRIM(raw_data_glc2000_filename)

  glcc_file = TRIM(raw_data_glcc_path) // TRIM(raw_data_glcc_filename)

  PRINT *,'glc2000_file: ', TRIM(glc2000_file)
  PRINT *,'glcc file: ', TRIM(glcc_file)

  CALL get_dimension_glc2000_data(glc2000_file, &
    &                                  nlon_glc2000, &
    &                                  nlat_glc2000)

  print *,'nlon_glc2000: ',nlon_glc2000
  print *,'nlat_glc2000: ',nlat_glc2000

  CALL get_dimension_glcc_data(glcc_file, &
    &                           nlon_glcc, &
    &                           nlat_glcc)
  print *,'nlon_glcc: ',nlon_glcc
  print *,'nlat_glcc: ',nlat_glcc

  

   ! &                             get_lonlat_glcc_data
  CALL allocate_raw_glc2000_fields(nlat_glc2000,nlon_glc2000)
  CALL allocate_raw_glcc_fields(nlat_glcc, nlon_glcc)


  CALL  get_lonlat_glc2000_data(glc2000_file, &
                                      nlon_glc2000, &
                                      nlat_glc2000, &
                                      lon_glc2000,  &
                                      lat_glc2000,  &
                                      glc2000_grid)

  PRINT *,'glc2000_grid: ', glc2000_grid
  PRINT *,'MINVAL(lon_glc2000) :', MINVAL(lon_glc2000)
  PRINT *,'MAXVAL(lon_glc2000) :', MAXVAL(lon_glc2000)

  PRINT *,'MINVAL(lat_glc2000) :', MINVAL(lat_glc2000)
  PRINT *,'MAXVAL(lat_glc2000) :', MAXVAL(lat_glc2000)

  PRINT *,'glc2000_legend: ', glc2000_legend

   CALL  get_lonlat_glcc_data(glcc_file, &
                                      nlon_glcc, &
                                      nlat_glcc, &
                                      lon_glcc,  &
                                      lat_glcc,  &
                                      glcc_grid)

  PRINT *,'glc2000_grid: ', glcc_grid
  PRINT *,'MINVAL(lon_glcc) :', MINVAL(lon_glcc)
  PRINT *,'MAXVAL(lon_glcc) :', MAXVAL(lon_glcc)

  PRINT *,'MINVAL(lat_glcc) :', MINVAL(lat_glcc)
  PRINT *,'MAXVAL(lat_glcc) :', MAXVAL(lat_glcc)

  PRINT *,'glcc_legend: ', glcc_legend

  
  PRINT *,'target grid '
  PRINT *,'MINVAL(lon_geo) :', MINVAL(lon_geo)
  PRINT *,'MAXVAL(lon_geo) :', MAXVAL(lon_geo)

  PRINT *,'MINVAL(lat_geo) :', MINVAL(lat_geo)
  PRINT *,'MAXVAL(lat_geo) :', MAXVAL(lat_geo)




  undefined = 0.0_wp


  PRINT *,'CALL agg_glc2000_data_to_target_grid'

  CALL agg_glc2000_data_to_target_grid(glc2000_file,ilookup_table_glc2000,undefined,       &
    &                                        tg,                                         &
    &                                        nclass_glc2000,                             &
    &                                        glc2000_class_fraction, & 
    &                                        glc2000_class_npixel, &
    &                                        glc2000_tot_npixel,   &
    &                                        fr_land_glc2000 ,     &  
    &                                        ice_glc2000,          &
    &                                        z0_glc2000, &
    &                                        root_glc2000, &
    &                                        plcov_mn_glc2000, &
    &                                        plcov_mx_glc2000, &
    &                                        lai_mn_glc2000,   &
    &                                        lai_mx_glc2000, &
    &                                        rs_min_glc2000, &
    &                                        urban_glc2000,  &
    &                                        for_d_glc2000,  &
    &                                        for_e_glc2000, &
    &                                        emissivity_glc2000    ) 

  PRINT *,'agg_glc2000_data_to_target_grid done'

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



  PRINT *,'agg_glc2000_data_to_target_grid done'


  !--------------------------------------------------------------------------------
  ! output
   undefined = -999.0_wp
   undef_int = -999

   netcdf_filename = TRIM(glc2000_buffer_file)
   print *, 'GLC2000 buffer filename: ',TRIM(netcdf_filename)

   
   CALL write_netcdf_buffer_glc2000(TRIM(netcdf_filename),  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
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




    SELECT CASE(igrid_type)

      CASE(igrid_icon) ! ICON GRID
        
        netcdf_filename = TRIM(glc2000_output_file)
        PRINT *,'write out icon data ', TRIM(netcdf_filename)

               
        CALL write_netcdf_icon_grid_glc2000(TRIM(netcdf_filename),  &
    &                                     icon_grid,       &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
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



         
      CASE(igrid_cosmo) ! COSMO grid

         netcdf_filename = TRIM(glc2000_output_file)
         PRINT *,'write out cosmo data ', TRIM(netcdf_filename)

         

        CALL write_netcdf_cosmo_grid_glc2000(TRIM(netcdf_filename),  &
    &                                     cosmo_grid,       &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
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




      CASE(igrid_gme) ! GME grid   

    END SELECT

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



    SELECT CASE(igrid_type)

      CASE(igrid_icon) ! ICON GRID
        
        netcdf_filename = TRIM(glcc_output_file)
        PRINT *,'write out ', TRIM(netcdf_filename)
        CALL  write_netcdf_icon_grid_glcc(TRIM(netcdf_filename),  &
    &                                     icon_grid,       &
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


         
      CASE(igrid_cosmo) ! COSMO grid

         netcdf_filename = TRIM(glcc_output_file)
         PRINT *,'write out ', TRIM(netcdf_filename)
         CALL write_netcdf_cosmo_grid_glcc(TRIM(netcdf_filename),  &
    &                                     cosmo_grid,       &
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
  



      CASE(igrid_gme) ! GME grid   

    END SELECT



END PROGRAM extpar_landuse_to_buffer
