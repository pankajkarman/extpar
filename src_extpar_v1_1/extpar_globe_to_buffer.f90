!+ Fortran main program to read in GLOBE orography data and aggregate to target grid
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!   small bug fixes accroding to Fortran compiler warnings        
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran main program to read in GLOBE orography data and aggregate to target grid
!>  
!! @par extpar_globe_to_buffer 
!!
!! This program read in the GLOBE orography data set and aggregates the orographic height to the target grid 
!! and computes the subgrid-scale orography parameters (SSO) required by the SSO-parameterization.
!!
!> Purpose: read in GLOBE orography data and aggregate to COSMO grid
!> \author Hermann Asensio
PROGRAM extpar_globe_to_buffer

  ! Load the library information data:
  USE info_extpar, ONLY: info_define, info_readnl, info_print


  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4


  USE mo_target_grid_data, ONLY: lon_geo, &
    &                            lat_geo, &
    &                            no_raw_data_pixel

  USE mo_target_grid_data, ONLY: tg  !< structure with target grid description

  USE mo_target_grid_routines, ONLY: init_target_grid

  USE mo_grid_structures, ONLY: target_grid_def,  &
    &                            reg_lonlat_grid,  &
    &                            rotated_lonlat_grid
  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme

  USE mo_cosmo_grid, ONLY: COSMO_grid, &
    &                       lon_rot, &
    &                       lat_rot, &
    &                       allocate_cosmo_rc, &
    &                       get_cosmo_grid_info, &
    &                       calculate_cosmo_domain_coordinates

  USE mo_icon_grid_data, ONLY: ICON_grid, & !< structure which contains the definition of the ICON grid
    &                          nvertex_dom,  &
    &                          ncells_dom
    
  USE mo_icon_grid_data, ONLY: icon_domain_grid

  USE mo_icon_grid_routines, ONLY: allocate_icon_grid

  USE mo_icon_grid_routines, ONLY: inq_grid_dims,            &
    &                             inq_domain_dims,          &
    &                             read_grid_info_part,      &
    &                             read_domain_info_part,    &
    &                             read_gridref_nl

  USE mo_search_icongrid,   ONLY: walk_to_nc,              &
    &                             find_nc_dom1,            &
    &                             find_nc

  USE mo_base_geometry,    ONLY:  geographical_coordinates, &
    &                              cartesian_coordinates

  USE mo_additional_geometry,   ONLY: cc2gc,                  &
    &                           gc2cc,                  &
    &                           arc_length,             &
    &                           cos_arc_length,         &
    &                           inter_section,          &
    &                           vector_product,         &
    &                           point_in_polygon_sp

  USE mo_icon_domain,          ONLY: icon_domain, &
    &                           grid_cells,               &
    &                           grid_vertices,            &
    &                           construct_icon_domain,    &
    &                           destruct_icon_domain

  USE mo_icon_domain, ONLY: max_dom
 
  USE mo_io_units,          ONLY: filename_max

  USE mo_exception,         ONLY: message_text, message, finish

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_math_constants,  ONLY: pi, pi_2, dbl_eps,rad2deg

  USE mo_globe_routines, ONLY: read_namelists_extpar_orography

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

  USE mo_globe_tg_fields, ONLY: globe_buffer

  USE mo_globe_data,       ONLY:  globe_tiles_grid, &
    &                              globe_grid,       &
    &                              ntiles_gl,        &
    &                              nc_tot

  USE mo_globe_routines, ONLY:  read_globe_data_input_namelist, &
    &                           det_globe_tiles_grid,           &
    &                           det_globe_grid,                  &
    &                           get_globe_tile_nr,              &
    &                           get_globe_tile_block_indices

  USE mo_agg_globe, ONLY: agg_globe_data_to_target_grid

  USE mo_globe_output_nc, ONLY: write_netcdf_buffer_globe
  USE mo_globe_output_nc, ONLY: write_netcdf_icon_grid_globe
  USE mo_globe_output_nc, ONLY: write_netcdf_cosmo_grid_globe

  IMPLICIT NONE

  CHARACTER(len=filename_max) :: filename
  CHARACTER(len=filename_max) :: netcdf_filename
  CHARACTER(len=filename_max) :: namelist_grid_def

  CHARACTER(len=filename_max) :: input_namelist_file
  CHARACTER (len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition
  CHARACTER (len=filename_max) :: namelist_globe_data_input !< file with input namelist with GLOBE data information
    
  CHARACTER (len=filename_max) :: raw_data_path        !< path to raw data
  CHARACTER (LEN=filename_max) :: globe_files(1:ntiles_gl)  !< filenames globe raw data

  CHARACTER (len=filename_max) :: orography_buffer_file !< name for orography buffer file
  CHARACTER (len=filename_max) :: orography_output_file !< name for orography output file

  CHARACTER (len=filename_max) :: raw_data_orography_path        !< path to raw data
      
  CHARACTER (len=filename_max) :: netcdf_out_filename      !< filename for netcdf file with GLOBE data on COSMO grid

  REAL (KIND=wp) :: point_lon_geo !< longitude of a point in geographical system
  REAL (KIND=wp) :: point_lat_geo !< latitude of a point in geographical system

  REAL(KIND=wp) :: undefined !< value to indicate undefined grid elements 
  INTEGER (KIND=i4) :: undef_int   !< value for undefined integer
  INTEGER (KIND=i4) :: default_value !< default value
  INTEGER (KIND=i4) :: index_tile   !< index for dummy test
  TYPE(geographical_coordinates) :: DWD_location !< geographical coordinates of DWD for dummy test
  
  INTEGER (KIND=i4) :: globe_startrow(1:ntiles_gl)    !< startrow indeces for each GLOBE tile
  INTEGER (KIND=i4) :: globe_endrow(1:ntiles_gl)      !< endrow indeces for each GLOBE tile
  INTEGER (KIND=i4) :: globe_startcolumn(1:ntiles_gl)  !< starcolumn indeces for each GLOBE tile

  INTEGER (KIND=i4) :: globe_endcolumn(1:ntiles_gl)   !< endcolumn indeces for each GLOBE tile
  TYPE(geographical_coordinates) :: ur   !< upper right point for test block
  TYPE(geographical_coordinates) :: ll   !< lower left point for test block

  INTEGER :: k !< counter
  INTEGER :: ie !< counter
  INTEGER :: je !< counter
  INTEGER :: ke !< counter
  INTEGER, PARAMETER :: nrows = 300   ! parameter for h_band dimension
  INTEGER (KIND=i4) :: h_band(1:nc_tot,1:nrows) = 0  !< GLOBE altitude data along a parallel band woth nrows
  INTEGER (KIND=i4) :: startrow_index = 0      !< the index of the GLOBE data row of the first data block row
  INTEGER (KIND=i4) :: endrow_index  = 0       !< the index of the GLOBE data row of the last data block row
  INTEGER (KIND=i4) :: startcolumn_index    !< the index of the startcolumn of data to read in
  INTEGER (KIND=i4) :: endcolumn_index      !< the index of the endcolumn of data to read in
  INTEGER  (KIND=i4) :: point_lon_index !< longitude index of point for regular lon-lat grid
  INTEGER  (KIND=i4) :: point_lat_index !< latitude index of point for regular lon-lat grid

  REAL (KIND=wp) :: globe_target_value  !< interpolated altitude from GLOBE data

  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid

  ! variables for the ICON grid
  INTEGER :: n_dom                        !< number of model domains
  INTEGER :: first_dom                    !< index of first (global) model domain 
  INTEGER :: nvertex  !< total number of vertices
  INTEGER :: nv ! counter

 ! Print the default information to stdout:
  CALL info_define ('globe_to_buffer')      ! Pre-define the program name as binary name
  CALL info_print ()                     ! Print the information to stdout
  !--------------------------------------------------------------------------------------------------------
 
  namelist_grid_def = 'INPUT_grid_org'
  CALL init_target_grid(namelist_grid_def)

  PRINT *,' target grid tg: ',tg

  igrid_type = tg%igrid_type
  ! get information on target grid
  ! get GLOBE raw data information
  !--------------------------------------------------------------------------------------------------------
  ! read namlelist with globe data information

  namelist_globe_data_input = 'INPUT_ORO'
  CALL read_namelists_extpar_orography(namelist_globe_data_input, &
    &                                     raw_data_orography_path,&
    &                                     globe_files,           &
    &                                     orography_buffer_file, &
    &                                     orography_output_file)

  CALL det_globe_tiles_grid(globe_tiles_grid)
  !HA debug
  DO k=1,ntiles_gl
    print *,'GLOBE files: ', TRIM(globe_files(k))
  ENDDO
    print *,'globe_tiles_grid(1): ', globe_tiles_grid(1)

  CALL det_globe_grid(globe_grid)
  !HA debug
  print *,'globe_grid: ', globe_grid


  ! allocate globe fields for target grid
  !--------------------------------------------------------------------------------------------------------
  CALL allocate_globe_target_fields(tg)

  ! allocate additional fields for icon grid
  !--------------------------------------------------------------------------------------------------------

  SELECT CASE(igrid_type)
    CASE(igrid_icon) ! ICON GRID
    ! allocate addtional target fields
    nvertex = icon_domain_grid%nverts
    CALL  allocate_additional_hh_param(nvertex)
  END SELECT

  ! call the aggregation routine
  !--------------------------------------------------------------------------------------------------------
   PRINT *,'CALL agg_globe_data_to_target_grid'
   CALL agg_globe_data_to_target_grid(globe_tiles_grid, &
     &                                      globe_grid,       &
     &                                      tg,               &
     &                                      globe_files,      &
     &                                      hh_globe,         &
     &                                      stdh_globe,       &
     &                                      theta_globe,      &
     &                                      aniso_globe,      &
     &                                      slope_globe,      &
     &                                      fr_land_globe,    &
     &                                      z0_topo,         &
     &                                      no_raw_data_pixel)
   
   ! if the target domain has a higher resolution of than the GLOBE data set (30'') some grid elements might not
   ! be set by the routine agg_globe_data_to_target_grid, (no_raw_data_pixel(ie,je,ke) == 0 in this case
   ! loop overa all grid elements to check and perform a bilinear interplation if necessary
   k = 0
   undefined = -999.9

       PRINT *,'Maximum number of GLOBE raw data pixel in a target grid element: '
       PRINT *,'MAXVAL(no_raw_data_pixel): ', MAXVAL(no_raw_data_pixel)

       PRINT *,'Minimal number of GLOBE raw data pixel in a target grid element: '
       PRINT *,'MINVAL(no_raw_data_pixel): ', MINVAL(no_raw_data_pixel)

        PRINT *,'Maximum stdh_globe: '
       PRINT *,'MAXVAL(stdh_globe): ', MAXVAL(stdh_globe)

       PRINT *,'Minimal stdh_globe: '
       PRINT *,'MINVAL(stdh_globe): ', MINVAL(stdh_globe)

  ! consistency for small grid sizes, do not use estimates of variance for small sample size
   IF ( (MAXVAL(no_raw_data_pixel)< 10).OR. (MINVAL(no_raw_data_pixel)==0)) THEN
      stdh_globe  = 0.  
      theta_globe = 0.
      aniso_globe = 0.
      slope_globe = 0.
      z0_topo     = 0.
   ENDIF

!   DO ke=1,tg%ke
!   DO je=1,tg%je
!   DO ie=1,tg%ie
!
!     IF (no_raw_data_pixel(ie,je,ke) == 0 ) THEN ! interpolate from raw data in this case
!       k = k + 1 ! count number of grid element for which a bilinear interpolation is done
!       point_lon_geo = lon_geo(ie,je,ke)
!       point_lat_geo = lat_geo(ie,je,ke)
!       CALL bilinear_interpol_globe_to_target_point(globe_grid, &
!         &                                 globe_files,      &
!         &                                 undefined,    &
!         &                                 point_lon_geo,      &
!         &                                 point_lat_geo,      &
!         &                                 nrows,              &
!         &                                 h_band,    &
!         &                                 startrow_index,     &
!         &                                 endrow_index,       &
!         &                                 globe_target_value)
!
!       hh_globe(ie,je,ke) = globe_target_value
!       PRINT *,'HA debug bilinterpol, globe_target_value: ', globe_target_value
!       PRINT *,'HE debug bilinterpol, globe_grid: ', globe_grid
!       PRINT *,'HE debug bilinterpol, globe_grid%dlat_reg: ', globe_grid%dlat_reg
!       PRINT *,'HE debug bilinterpol, globe_grid%dlon_reg: ', globe_grid%dlon_reg
!       PRINT *,'HA debug bilinterpol, ie,je,ke: ', ie,je,ke
!       fr_land_globe(ie,je,ke) = undefined
!     ENDIF
!
!   ENDDO
!   ENDDO
!   ENDDO
!   print *,'bilinear interpolation of GLOBE data used for no of grid cells: ',k

   
   ! consistency for small grid sizes, do not use estimates of variance for small sample size

   IF ( (MAXVAL(no_raw_data_pixel)< 10).OR. (MINVAL(no_raw_data_pixel)==0)) THEN

      stdh_globe  = 0.  
      theta_globe = 0.
      aniso_globe = 0.
      slope_globe = 0.
      z0_topo     = 0.


   ENDIF


   DO ke=1,tg%ke
   DO je=1,tg%je
   DO ie=1,tg%ie
      IF (no_raw_data_pixel(ie,je,ke) < 1 ) THEN
         stdh_globe(ie,je,ke)  = 0.  
         theta_globe(ie,je,ke) = 0.
         aniso_globe(ie,je,ke) = 0.
         slope_globe(ie,je,ke) = 0.
         z0_topo(ie,je,ke)     = 0.
      ENDIF
   ENDDO
   ENDDO
   ENDDO


!   SELECT CASE(igrid_type)
!     CASE(igrid_icon) ! ICON GRID
!     nvertex = icon_domain_grid%nverts
!     je=1
!     ke=1
!     DO nv=1, nvertex
!       k=0
!       IF (vertex_param%npixel_vert(nv,je,ke) == 0) THEN ! interpolate from raw data in this case
!         k = k + 1 ! count number of grid element for which a bilinear interpolation is done
!          point_lon_geo =  rad2deg * icon_domain_grid%verts%vertex(nv)%lon 
!          point_lat_geo =  rad2deg * icon_domain_grid%verts%vertex(nv)%lat
!          CALL bilinear_interpol_globe_to_target_point(globe_grid, &
!            &                                 globe_files,      &
!            &                                 undefined,    &
!            &                                 point_lon_geo,      &
!            &                                 point_lat_geo,      &
!            &                                 nrows,              &
!            &                                 h_band,    &
!            &                                 startrow_index,     &
!            &                                 endrow_index,       &
!            &                                 globe_target_value)
!
!          vertex_param%hh_vert(nv,je,ke) = globe_target_value
!       ENDIF  
!     ENDDO
!     print *,'bilinear interpolation of GLOBE data used for no of grid vertices: ',k
!   END SELECT
   !HA debug
   PRINT *,'agg_globe_data_to_target_grid finished'

   ! output to netcdf file
   undefined = -999.9
   undef_int = -999

   netcdf_filename = TRIM(orography_buffer_file)
   print *, 'filename: ',TRIM(netcdf_filename)

   SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID

      CALL write_netcdf_buffer_globe(netcdf_filename,  &
     &                                     tg,         &
     &                                     undefined, &
     &                                     undef_int,   &
     &                                     lon_geo,     &
     &                                     lat_geo, &
     &                                     fr_land_globe, &
     &                                     hh_globe,            &
     &                                     stdh_globe,          &
     &                                     theta_globe,         &
     &                                     aniso_globe,         &
     &                                     slope_globe,         &
     &                                     z0_topo,             &
     &                                     vertex_param)


     CASE DEFAULT


     CALL write_netcdf_buffer_globe(netcdf_filename,  &
       &                                     tg,         &
       &                                     undefined, &
       &                                     undef_int,   &
       &                                     lon_geo,     &
       &                                     lat_geo, &
       &                                     fr_land_globe, &
       &                                     hh_globe,            &
       &                                     stdh_globe,          &
       &                                     theta_globe,         &
       &                                     aniso_globe,         &
       &                                     slope_globe,        &
       &                                     z0_topo)
   END SELECT


   SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID
        
       netcdf_filename = TRIM(orography_output_file)
       PRINT *,'write out ', TRIM(netcdf_filename)

       CALL write_netcdf_icon_grid_globe(netcdf_filename,  &
     &                                     icon_grid,       &
     &                                     tg,         &
     &                                     undefined, &
     &                                     undef_int,   &
     &                                     lon_geo,     &
     &                                     lat_geo, &
     &                                     fr_land_globe, &
     &                                     hh_globe,            &
     &                                     stdh_globe,          &
     &                                     theta_globe,         &
     &                                     aniso_globe,         &
     &                                     slope_globe,         &
     &                                     z0_topo,             &
     &                                     vertex_param)

     CASE(igrid_cosmo) ! COSMO grid

       netcdf_filename = TRIM(orography_output_file)
       PRINT *,'write out ', TRIM(netcdf_filename)

       CALL write_netcdf_cosmo_grid_globe(netcdf_filename,  &
     &                                     cosmo_grid,       &
     &                                     tg,         &
     &                                     undefined, &
     &                                     undef_int,   &
     &                                     lon_geo,     &
     &                                     lat_geo, &
     &                                     fr_land_globe, &
     &                                     hh_globe,            &
     &                                     stdh_globe,          &
     &                                     theta_globe,         &
     &                                     aniso_globe,         &
     &                                     slope_globe,         &
     &                                     z0_topo)


     CASE(igrid_gme) ! GME grid   
   END SELECT

   print *,'globe_to_buffer finished.'
        

END PROGRAM extpar_globe_to_buffer
