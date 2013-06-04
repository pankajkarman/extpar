!+ Fortran main program to read in GLOBE orography data and aggregate to target grid
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!   small bug fixes accroding to Fortran compiler warnings        
! V1_2         2011/03/25 Hermann Asensio
!  update to support ICON refinement grids
! V1_4         2011/04/21 Anne Roches     
!  implementation of orography smoothing  
! V1_7         2013/01/25 Guenther Zaengl 
!   Parallel threads for ICON and COSMO using Open-MP, 
!   Several bug fixes and optimizations for ICON search algorithm, 
!   particularly for the special case of non-contiguous domains; 
!   simplified namelist control for ICON  
!
! @VERSION@    @DATE@     Martina Messmer
!  adaptation in order to allow user to chose between GLOBE and ASTER topography
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran main program to read in GLOBE orography data and aggregate to target grid
!>  
!! @par extpar_globe_to_buffer 
!!
!! This program reads in the GLOBE orography data set and aggregates the orographic height to the target grid 
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

  USE mo_icon_grid_data, ONLY: ICON_grid !< structure which contains the definition of the ICON grid
    
  USE mo_icon_grid_data, ONLY: icon_grid_region


  USE mo_base_geometry,    ONLY:  geographical_coordinates, &
    &                             cartesian_coordinates

  USE mo_additional_geometry,   ONLY: cc2gc,            &
    &                           gc2cc,                  &
    &                           arc_length,             &
    &                           cos_arc_length,         &
    &                           inter_section,          &
    &                           vector_product,         &
    &                           point_in_polygon_sp

  USE mo_icon_domain,          ONLY: icon_domain,         &
    &                           grid_cells,               &
    &                           grid_vertices,            &
    &                           construct_icon_domain,    &
    &                           destruct_icon_domain

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
    &                         allocate_globe_target_fields,&
    &                         slope_asp_globe,     &
    &                         slope_ang_globe,     &
    &                         horizon_globe,       &
    &                         skyview_globe

  USE mo_globe_tg_fields, ONLY: add_parameters_domain, &
    &        vertex_param, &
    &        allocate_additional_hh_param

! mes > -------------------------------------------------------------
!  USE mo_globe_data,       ONLY:  globe_tiles_grid, &
!    &                              globe_grid,       &
!    &                              ntiles_gl,        &
!    &                              nc_tot

USE mo_topo_data,        ONLY:  topo_aster,        &
    &                           topo_gl,           &
    &                           topography,        &    
    &                           topo_tiles_grid,   &
    &                           topo_grid,         &
    &                           ntiles,            &
    &                           max_tiles,         &
    &                           nc_tot,            &
    &                           nr_tot,            &
    &                           nc_tile,           &
    &                           tiles_lon_min,     &
    &                           tiles_lon_max,     &
    &                           tiles_lat_min,     &
    &                           tiles_lat_max,     &
    &                           aster_lat_min,     &
    &                           aster_lat_max,     &
    &                           aster_lon_min,     &
    &                           aster_lon_max,     &
    &                           num_tiles,         &
    &                           allocate_topo_data,&
    &                           fill_topo_data,    &
    &                           lradtopo,          &
    &                           nhori
! mes < -------------------------------------------------------------

  USE mo_globe_routines, ONLY:  read_globe_data_input_namelist, &
    &                           det_globe_tiles_grid,           &
    &                           det_globe_grid,                  &
    &                           get_globe_tile_nr,              &
    &                           get_globe_tile_block_indices

  USE mo_agg_globe, ONLY: agg_globe_data_to_target_grid

  USE mo_globe_output_nc, ONLY: write_netcdf_buffer_globe
  USE mo_globe_output_nc, ONLY: write_netcdf_icon_grid_globe
  USE mo_globe_output_nc, ONLY: write_netcdf_cosmo_grid_globe
!roa >
  USE mo_oro_filter, ONLY: read_namelists_extpar_orosmooth
  USE mo_lradtopo,   ONLY: read_namelists_extpar_lradtopo, &
    &                      compute_lradtopo
!roa <

  IMPLICIT NONE

  CHARACTER(len=filename_max) :: filename
  CHARACTER(len=filename_max) :: netcdf_filename
  CHARACTER(len=filename_max) :: namelist_grid_def

  CHARACTER(len=filename_max) :: input_namelist_file
  CHARACTER (len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition
  CHARACTER (len=filename_max) :: namelist_globe_data_input !< file with input namelist with GLOBE data information
!roa >
  CHARACTER (len=filename_max) :: namelist_oro_smooth       !< file with orography smoothing information (switches)
  CHARACTER (len=filename_max) :: namelist_lrad             !< file with opo information (switches)
!roa <  

    
  CHARACTER (len=filename_max) :: raw_data_path        !< path to raw data
  CHARACTER (LEN=24) :: topo_files(1:max_tiles)  !< filenames globe raw data

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
  
  INTEGER (KIND=i4) :: globe_startrow(1:ntiles)    !< startrow indeces for each GLOBE tile
  INTEGER (KIND=i4) :: globe_endrow(1:ntiles)      !< endrow indeces for each GLOBE tile
  INTEGER (KIND=i4) :: globe_startcolumn(1:ntiles)  !< starcolumn indeces for each GLOBE tile

  INTEGER (KIND=i4) :: globe_endcolumn(1:ntiles)   !< endcolumn indeces for each GLOBE tile
  TYPE(geographical_coordinates) :: ur   !< upper right point for test block
  TYPE(geographical_coordinates) :: ll   !< lower left point for test block

  INTEGER :: k !< counter
  INTEGER :: ie !< counter
  INTEGER :: je !< counter
  INTEGER :: ke !< counter


!!!!!!!!!mes >
!  INTEGER, PARAMETER :: nrows = 300   ! parameter for h_band dimension
!  INTEGER (KIND=i4) :: h_band(1:nc_tot,1:nrows) = 0  !< GLOBE altitude data along a parallel band woth nrows
!!!!!!!!!mes <


  INTEGER (KIND=i4) :: startrow_index = 0      !< the index of the GLOBE data row of the first data block row
  INTEGER (KIND=i4) :: endrow_index  = 0       !< the index of the GLOBE data row of the last data block row
  INTEGER (KIND=i4) :: startcolumn_index    !< the index of the startcolumn of data to read in
  INTEGER (KIND=i4) :: endcolumn_index      !< the index of the endcolumn of data to read in
  INTEGER  (KIND=i4) :: point_lon_index !< longitude index of point for regular lon-lat grid
  INTEGER  (KIND=i4) :: point_lat_index !< latitude index of point for regular lon-lat grid

  REAL (KIND=wp) :: globe_target_value  !< interpolated altitude from GLOBE data

  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid

  ! variables for the ICON grid
  INTEGER :: nvertex  !< total number of vertices
  INTEGER :: nv ! counter

 !mes > -------------------------
 INTEGER (KIND=i4) :: itopo_type           !< use 1 for GLOBE data and 2 for ASTER data

  !roa >
  LOGICAL           :: &
    lfilter_oro,     &
    lxso_first
  
  INTEGER(KIND=i4)  :: &
    ilow_pass_oro,   & 
    numfilt_oro,     &
    ifill_valley,    &
    ilow_pass_xso,   &
    numfilt_xso

  REAL(KIND=wp)     :: &
    eps_filter,      &
    rfill_valley,    &   
    rxso_mask     
!roa <


 ! Print the default information to stdout:
  CALL info_define ('globe_to_buffer')      ! Pre-define the program name as binary name
  CALL info_print ()                     ! Print the information to stdout
  !--------------------------------------------------------------------------------------------------------
 
  namelist_grid_def = 'INPUT_grid_org'

  namelist_lrad     = 'INPUT_RADTOPO'

  CALL read_namelists_extpar_lradtopo(namelist_lrad,lradtopo,nhori)

  CALL init_target_grid(namelist_grid_def,lrad=lradtopo)

  PRINT *,'target grid tg: ',tg%ie, tg%je, tg%ke, tg%minlon, tg%maxlon, tg%minlat, tg%maxlat


  igrid_type = tg%igrid_type
  ! get information on target grid
  ! get GLOBE raw data information

  ! Checks
   IF (igrid_type /= igrid_cosmo) lradtopo = .FALSE.

  !--------------------------------------------------------------------------------------------------------

  ! read namelist with globe data information

  namelist_globe_data_input = 'INPUT_ORO'
  CALL read_namelists_extpar_orography(namelist_globe_data_input, &
    &                                     raw_data_orography_path,&
    &                                     topo_files,             &  !mes>
    &                                     itopo_type,             &
    &                                     orography_buffer_file,  &
    &                                     orography_output_file)



                 
  CALL num_tiles(itopo_type,ntiles,topography)          ! gives back the number of tiles that are available 16 for GLOBE or 36 for ASTER
  
!mes <

! mes >
  CALL allocate_topo_data(ntiles)                  ! allocates the data using ntiles

  CALL fill_topo_data(raw_data_orography_path,topo_files, &! the allocated vectors need to be filled with the respective value.
                                           tiles_lon_min, &
                                           tiles_lon_max, &    
                                           tiles_lat_min, &
                                           tiles_lat_max, &
                                           nc_tot,        &
                                           nr_tot,        &
                                           nc_tile)

 !aster_lon_min, &
 !                                          aster_lon_max, &
 !                                          aster_lat_min, &
 !                                          aster_lat_max, &
  SELECT CASE(topography)
    CASE(topo_aster)
      PRINT*,'edges of ASTER domain: ', aster_lon_min,' ', aster_lon_max,' ', aster_lat_min,' ',aster_lat_max
    END SELECT
! mes <

!mes >
  PRINT*,' topography :', itopo_type
  print*, lon_geo(tg%ie,tg%je,tg%ke), lon_geo(1,1,1)
  print*, lat_geo(tg%ie,tg%je,tg%ke), lat_geo(1,1,1)

  SELECT CASE (itopo_type)
  CASE (topo_aster)
   IF (lon_geo (tg%ie,tg%je,tg%ke) > aster_lon_max .OR. lon_geo(1,1,1) < aster_lon_min) THEN
   PRINT*, 'ASTER min lon is: ', aster_lon_min, ' and ASTER max lon is: ', aster_lon_max
   CALL abort_extpar('The chosen longitude edges are not within the ASTER domain.')
   END IF
   IF (lat_geo(tg%ie,tg%je,tg%ke) > aster_lat_max .OR. lat_geo(1,1,1) < aster_lat_min) THEN
   PRINT*, 'ASTER min lat is: ', aster_lat_min, ' and ASTER max lat is: ', aster_lat_max
   CALL abort_extpar('The chosen latitude edges are not within the ASTER domain.')
   END IF
  END SELECT


!roa >
  namelist_oro_smooth = 'INPUT_OROSMOOTH'
  CALL read_namelists_extpar_orosmooth(namelist_oro_smooth,      &
                                           lfilter_oro,          &
                                           ilow_pass_oro,        &
                                           numfilt_oro,          &
                                           eps_filter,           &
                                           ifill_valley,         &
                                           rfill_valley,         &
                                           ilow_pass_xso,        &
                                           numfilt_xso,          &
                                           lxso_first,           &
                                           rxso_mask)
!roa <

  IF (lradtopo .AND. (.NOT. lfilter_oro)) THEN
    PRINT *,' Warning *** lradtopo should not be used without orography filtering *** '
    PRINT *,'                            (consistency problem)                        ' 
  ENDIF



  CALL det_globe_tiles_grid(topo_tiles_grid)
  !HA debug
  DO k=1,ntiles
    print *,'GLOBE files: ', TRIM(topo_files(k))
  ENDDO
    print *,'topo_tiles_grid(1): ', topo_tiles_grid(1)

  CALL det_globe_grid(topo_grid)
  !HA debug
  print *,'topo_grid: ', topo_grid


  ! allocate globe fields for target grid
  !--------------------------------------------------------------------------------------------------------
  CALL allocate_globe_target_fields(tg)

  ! allocate additional fields for icon grid
  !--------------------------------------------------------------------------------------------------------

  SELECT CASE(igrid_type)
    CASE(igrid_icon) ! ICON GRID
    ! allocate addtional target fields
    nvertex = icon_grid_region%nverts
    CALL  allocate_additional_hh_param(nvertex)
  END SELECT

  ! call the aggregation routine
  !--------------------------------------------------------------------------------------------------------
   PRINT *,'CALL agg_globe_data_to_target_grid'
   CALL agg_globe_data_to_target_grid(topo_tiles_grid, &
     &                                      topo_grid,       &
     &                                      tg,               &
     &                                      topo_files,      &
!roa>
     &                                      lfilter_oro,      &
     &                                      ilow_pass_oro,    &
     &                                      numfilt_oro,      &
     &                                      eps_filter,       &
     &                                      ifill_valley,     &
     &                                      rfill_valley,     &
     &                                      ilow_pass_xso,    &
     &                                      numfilt_xso,      &
     &                                      lxso_first,       &
     &                                      rxso_mask,        & 
!roa<
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
!          CALL bilinear_interpol_globe_to_target_point(topo_files, globe_grid, &
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

   ! compute the lradtopo parameters if needed
   IF ( lradtopo ) THEN
     CALL compute_lradtopo(nhori,tg,hh_globe,slope_asp_globe,slope_ang_globe,horizon_globe,skyview_globe)
   ENDIF


   !HA debug
   PRINT *,'agg_globe_data_to_target_grid finished'

   ! output to netcdf file
   undefined = -999.9
   undef_int = -999

   netcdf_filename = TRIM(orography_buffer_file)
   print *, 'filename: ',TRIM(netcdf_filename)

   SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID

      CALL write_netcdf_buffer_globe(netcdf_filename,     &
     &                                     tg,            &
     &                                     undefined,     &
     &                                     undef_int,     &
     &                                     lon_geo,       &
     &                                     lat_geo,       &
     &                                     fr_land_globe, &
     &                                     hh_globe,      &
     &                                     stdh_globe,    &
     &                                     theta_globe,   &
     &                                     aniso_globe,   &
     &                                     slope_globe,   &
     &                                     z0_topo,       &
     &                                     lradtopo,      &
     &                                     nhori,         &
     &                                     vertex_param=vertex_param)


     CASE DEFAULT

     IF (lradtopo) THEN
       CALL write_netcdf_buffer_globe(netcdf_filename,        &
         &                                     tg,            &
         &                                     undefined,     &
         &                                     undef_int,     &
         &                                     lon_geo,       &
         &                                     lat_geo,       &
         &                                     fr_land_globe, &
         &                                     hh_globe,      &
         &                                     stdh_globe,    &
         &                                     theta_globe,   &
         &                                     aniso_globe,   &
         &                                     slope_globe,   &
         &                                     z0_topo,       &
         &                                     lradtopo,      &
         &                                     nhori,         &
         &                                     slope_asp_globe=slope_asp_globe, &
         &                                     slope_ang_globe= slope_ang_globe,&
         &                                     horizon_globe=horizon_globe,     &
         &                                     skyview_globe=skyview_globe)
     ELSE

       CALL write_netcdf_buffer_globe(netcdf_filename,          &
         &                                     tg,              &
         &                                     undefined,       &
         &                                     undef_int,       &
         &                                     lon_geo,         &
         &                                     lat_geo,         &
         &                                     fr_land_globe,   &
         &                                     hh_globe,        &
         &                                     stdh_globe,      &
         &                                     theta_globe,     &
         &                                     aniso_globe,     &
         &                                     slope_globe,     &
         &                                     z0_topo,         &
         &                                     lradtopo,        &
         &                                     nhori)
     ENDIF


   END SELECT


   SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID
        
       netcdf_filename = TRIM(orography_output_file)
       PRINT *,'write out ', TRIM(netcdf_filename)

       CALL write_netcdf_icon_grid_globe(netcdf_filename,       &
     &                                     icon_grid,           &
     &                                     tg,                  &
     &                                     undefined,           &
     &                                     undef_int,           &
     &                                     lon_geo,             &
     &                                     lat_geo,             &
     &                                     fr_land_globe,       &
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

       IF(lradtopo) THEN
         CALL write_netcdf_cosmo_grid_globe(netcdf_filename,      &
       &                                     cosmo_grid,          &
       &                                     tg,                  &
       &                                     undefined,           &
       &                                     undef_int,           &
       &                                     lon_geo,             &
       &                                     lat_geo,             &
       &                                     fr_land_globe,       &
       &                                     hh_globe,            &
       &                                     stdh_globe,          &
       &                                     theta_globe,         &
       &                                     aniso_globe,         &
       &                                     slope_globe,         &
       &                                     z0_topo,             &
       &                                     lradtopo,            &
       &                                     nhori,               &
       &                                     slope_asp_globe=slope_asp_globe,   &
       &                                     slope_ang_globe=slope_ang_globe,   &
       &                                     horizon_globe=horizon_globe,       &
       &                                     skyview_globe=skyview_globe)

       ELSE

         CALL write_netcdf_cosmo_grid_globe(netcdf_filename, &
       &                                     cosmo_grid,     &
       &                                     tg,             &
       &                                     undefined,      &
       &                                     undef_int,      &
       &                                     lon_geo,        &
       &                                     lat_geo,        &
       &                                     fr_land_globe,  &
       &                                     hh_globe,       &
       &                                     stdh_globe,     &
       &                                     theta_globe,    &
       &                                     aniso_globe,    &
       &                                     slope_globe,    &
       &                                     z0_topo,        &
       &                                     lradtopo,       &
       &                                     nhori)
       ENDIF


     CASE(igrid_gme) ! GME grid   
   END SELECT

   print *,'globe_to_buffer finished.'
   PRINT *, achar(27)//'[32m DONE'//achar(27)//'[0m'  !mes     

END PROGRAM extpar_globe_to_buffer

