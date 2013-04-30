!+ Fortran module to aggregate GLOBE orogrphy data to the target grid 
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  correct calculation of standard deviation of height
! V1_2         2011/03/25 Hermann Asensio
!  update to support ICON refinement grids
!  bug fix in interpolation routine and handling of undefined GLOBE values
! @VERSION@    @DATE@     Anne Roches
!  implementation of orography smoothing
! @VERSION@    @DATE@     Martina Messmer
!  Change all 'globe' to topo in globe_files, remove all 'globe' in 
!  change mo_GLOBE_data to mo_topo_data, globe_tiles_grid to 
!  topo_tiles_grid, globe_files to topo_files, globe_grid to
!  topo_grid and change ntiles_gl to ntiles to obtain a more 
!  dynamical code.
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module to aggregate GLOBE orogrphy data to the target grid
!> \author Hermann Asensio
MODULE mo_agg_globe
  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i4
  USE mo_kind, ONLY: i8
  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar
  USE mo_io_units,          ONLY: filename_max
  !> data type structures form module GRID_structures
  USE mo_grid_structures, ONLY: reg_lonlat_grid, &
    &                           rotated_lonlat_grid
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid
  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme
  USE mo_search_ll_grid, ONLY: find_reg_lonlat_grid_element_index
  USE mo_search_ll_grid, ONLY: find_rotated_lonlat_grid_element_index

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: agg_globe_data_to_target_grid
 ! PUBLIC :: bilinear_interpol_globe_to_target_point
  CONTAINS
    !> aggregate GLOBE orography to target grid
    SUBROUTINE agg_globe_data_to_target_grid(topo_tiles_grid, &
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
      &                                      hh_target,         &
      &                                      stdh_target,       &
      &                                      theta_target,      &
      &                                      aniso_target,      &
      &                                      slope_target,      &
      &                                      fr_land_globe,    &
      &                                      z0_topo,          &
      &                                      no_raw_data_pixel)
    USE mo_topo_data, ONLY : ntiles,   &  !< there are 16 GLOBE tiles 
                             max_tiles
       
    USE mo_topo_data, ONLY: nc_tot !< number of total GLOBE columns un a latitude circle (43200)
    USE mo_topo_data, ONLY: nr_tot !< total number of rows in GLOBE data (21600)
!mes >
    USE mo_topo_data, ONLY: get_fill_value   !< determines the _FillValue of either GLOBE or ASTER
    USE mo_topo_data, ONLY: topography
    USE mo_topo_data, ONLY: topo_gl
    USE mo_topo_data, ONLY: topo_aster
!mes <

    USE mo_grid_structures, ONLY: reg_lonlat_grid  !< Definition of Data Type to describe a regular (lonlat) grid
    USE mo_grid_structures, ONLY: rotated_lonlat_grid !< Definition of Data Type to describe a rotated lonlat grid
    USE mo_grid_structures, ONLY: target_grid_def  !< Definition of data type with target grid definition
    
   USE mo_utilities_extpar, ONLY: uv2uvrot          !< This routine converts the components u and v from the real geographical system to the rotated system
   USE mo_globe_routines, ONLY: get_globe_tile_block_indices
   USE mo_globe_routines, ONLY: open_netcdf_GLOBE_tile
   USE mo_globe_routines, ONLY: close_netcdf_GLOBE_tile
   USE mo_globe_routines, ONLY: get_globe_data_parallel
   USE mo_globe_routines, ONLY: get_globe_data_block
   USE mo_globe_routines, ONLY: det_band_gd

   USE mo_gme_grid, ONLY: sync_diamond_edge
   USE mo_gme_grid, ONLY: gme_real_field, gme_int_field
   USE mo_gme_grid, ONLY: cp_buf2gme, cp_gme2buf
   ! USE global data fields (coordinates)
   USE mo_target_grid_data, ONLY: lon_geo, & !< longitude coordinates of the grid in the geographical system 
     &                              lat_geo !< latitude coordinates of the grid in the geographical system

   ! USE structure which contains the definition of the COSMO grid
   USE  mo_cosmo_grid, ONLY: COSMO_grid !< structure which contains the definition of the COSMO grid
   ! USE structure which contains the definition of the ICON grid
   USE mo_gme_grid, ONLY: gme_grid
   USE mo_gme_grid, ONLY: xn, rlon_gme, rlat_gme
   USE mo_search_gme_grid, ONLY: pp_ll2gp
   USE  mo_icon_grid_data, ONLY: ICON_grid !< structure which contains the definition of the ICON grid
   ! USE icon domain structure wich contains the ICON coordinates (and parent-child indices etc)
   USE mo_icon_grid_data, ONLY: icon_grid_region, icon_dom_nr
   ! use additional parameters for height on vertices
   ! as a test the fields are loaded from a module instead of passing in the subroutine call
   USE mo_globe_tg_fields, ONLY: add_parameters_domain !< data structure
   USE mo_globe_tg_fields, ONLY: vertex_param          !< this structure contains the fields 
                                                           !! vertex_param%npixel_vert
   ! USE modules to search in ICON grid
   USE mo_search_icongrid, ONLY: find_nc, &
                                    walk_to_nc, &
                                    find_nchild_nlev, &
                                    find_nearest_vert

   USE mo_icon_domain,     ONLY: icon_domain

   ! structure for geographica coordintaes
   USE mo_base_geometry,   ONLY: geographical_coordinates
   USE mo_base_geometry,   ONLY: cartesian_coordinates
   USE mo_additional_geometry,   ONLY: cc2gc,                  &
    &                                  gc2cc

   USE mo_math_constants, ONLY: pi, rad2deg, deg2rad
   USE mo_physical_constants, ONLY: re !< av. radius of the earth [m]

   USE mo_bilinterpol, ONLY: get_4_surrounding_raw_data_indices, &
    &                        calc_weight_bilinear_interpol, &
    &                        calc_value_bilinear_interpol
!roa >
   USE mo_oro_filter, ONLY: do_orosmooth
!roa<


   TYPE(reg_lonlat_grid) :: topo_tiles_grid(1:ntiles) !< structure with defenition of the raw data grid for the 16 GLOBE tiles
   TYPE(target_grid_def), INTENT(IN)      :: tg              !< !< structure with target grid description

   TYPE(reg_lonlat_grid) :: topo_grid                !< structure with defenition of the raw data grid for the whole GLOBE dataset
   CHARACTER (LEN=24), INTENT(IN) :: topo_files(1:max_tiles)  !< filenames globe raw data
   !roa>
   LOGICAL, INTENT(IN) :: lfilter_oro  !< oro smoothing to be performed? (TRUE/FALSE) 
   INTEGER(KIND=i4), INTENT(IN) :: ilow_pass_oro            !< type of oro smoothing and 
                                                            !  stencil width (1,4,5,6,8)
   INTEGER(KIND=i4), INTENT(IN) :: numfilt_oro              !< number of applications of the filter
   REAL(KIND=wp),    INTENT(IN) :: eps_filter               !< smoothing param ("strength" of the filtering)
   INTEGER(KIND=i4), INTENT(IN) :: ifill_valley             !< fill valleys before or after oro smoothing 
                                                            !  (1: before, 2: after)
   REAL(KIND=wp),    INTENT(IN) :: rfill_valley             !< mask for valley filling (threshold value)
   INTEGER(KIND=i4), INTENT(IN) :: ilow_pass_xso            !< type of oro eXtra SmOothing for steep
                                                            !  orography and stencil width (1,4,5,6,8)
   INTEGER(KIND=i4), INTENT(IN) :: numfilt_xso              !< number of applications of the eXtra filter
   LOGICAL,          INTENT(IN) :: lxso_first               !< eXtra SmOothing before or after oro
                                                            !  smoothing? (TRUE/FALSE)
   REAL(KIND=wp),    INTENT(IN) :: rxso_mask                !< mask for eXtra SmOothing (threshold value)
!roa<

   REAL(KIND=wp), INTENT(OUT)   :: hh_target(1:tg%ie,1:tg%je,1:tg%ke)  !< mean height of target grid element
       REAL(KIND=wp), INTENT(OUT)   :: stdh_target(1:tg%ie,1:tg%je,1:tg%ke)  !< standard deviation of subgrid scale orographic height 
   REAL(KIND=wp), INTENT(OUT)   :: theta_target(1:tg%ie,1:tg%je,1:tg%ke) !< sso parameter, angle of principal axis
   REAL(KIND=wp), INTENT(OUT)   :: aniso_target(1:tg%ie,1:tg%je,1:tg%ke) !< sso parameter, anisotropie factor
   REAL(KIND=wp), INTENT(OUT)   :: slope_target(1:tg%ie,1:tg%je,1:tg%ke) !< sso parameter, mean slope
   REAL(KIND=wp), INTENT(OUT)   :: z0_topo(1:tg%ie,1:tg%je,1:tg%ke) !< roughness length due to orography
   REAL(KIND=wp), INTENT(OUT)   :: fr_land_globe(1:tg%ie,1:tg%je,1:tg%ke) !< fraction land
   INTEGER (KIND=i8), INTENT(OUT) :: no_raw_data_pixel(1:tg%ie,1:tg%je,1:tg%ke)  !< number of raw data pixel for a target grid element

   ! local variables
   REAL (KIND=wp)  :: lon_globe(1:nc_tot)   !< longitude coordinates of the GLOBE grid
   REAL (KIND=wp)  :: lat_globe(1:nr_tot)   !< latititude coordinates of the GLOBE grid

!!!!!!!!mes >
!   INTEGER (KIND=i4), PARAMETER :: nc_tot_p1 = nc_tot +1
   INTEGER (KIND=i4):: nc_tot_p1
!!!!!!!!mes <


   INTEGER  :: ncids_globe(1:ntiles)  !< ncid for the GLOBE tiles, the netcdf files have to be opened by a previous call of open_netcdf_GLOBE_tile
   INTEGER (KIND=i4) :: h_parallel(1:nc_tot)  !< one line with GLOBE data
   INTEGER (KIND=i4) :: h_3rows(1:nc_tot,1:3) !< three rows with GLOBE data

!!!!!!!!mes >
!   INTEGER (KIND=i4) :: hh(0:nc_tot_p1,1:3) !< topographic height for gradient calculations
   INTEGER (KIND=i4) :: hh(0:nc_tot+1,1:3) !< topographic height for gradient calculations
!!!!!!!!mes <

   REAL(KIND=wp)   :: dhdx(1:nc_tot)  !< x-gradient for one latitude row
   REAL(KIND=wp)   :: dhdy(1:nc_tot)  !< y-gradient for one latitude row

   REAL(KIND=wp)   :: dhdxdx(1:nc_tot)  !< x-gradient square for one latitude row
   REAL(KIND=wp)   :: dhdydy(1:nc_tot)  !< y-gradient square for one latitude row
   REAL(KIND=wp)   :: dhdxdy(1:nc_tot)  !< dxdy for one latitude row
   REAL(KIND=wp)   :: hh1_target(1:tg%ie,1:tg%je,1:tg%ke)  !< mean height of grid element
   REAL(KIND=wp)   :: hh2_target(1:tg%ie,1:tg%je,1:tg%ke)  !< square mean height of grid element
!roa >
   REAL(KIND=wp)   :: hsmooth(1:tg%ie,1:tg%je,1:tg%ke)  !< mean smoothed height of grid element
!roa <


   REAL(KIND=wp)   :: h11(1:tg%ie,1:tg%je,1:tg%ke) !< help variables
   REAL(KIND=wp)   :: h12(1:tg%ie,1:tg%je,1:tg%ke) !< help variables
   REAL(KIND=wp)   :: h22(1:tg%ie,1:tg%je,1:tg%ke) !< help variables
   REAL(KIND=wp)   :: zh11, zh12, zh22
   INTEGER (KIND=i8) :: ndata(1:tg%ie,1:tg%je,1:tg%ke)  !< number of raw data pixel with land point

   TYPE(geographical_coordinates) :: target_geo_co  !< structure for geographical coordinates of raw data pixel
   INTEGER (KIND=i8) :: nearest_cell_id  !< result of icon grid search
   INTEGER (KIND=i8) :: nearest_vert_id  !< result of icon grid search for vertices
   INTEGER :: n_dom ! number of Icon domains
   INTEGER (KIND=i4) :: undef_topo
   INTEGER (KIND=i4) :: default_globe
   INTEGER :: i,j,k,l ! counters
   INTEGER (KIND=i8) :: ie, je, ke  ! indices for grid elements
   INTEGER (KIND=i8) :: i_vert, j_vert, k_vert ! indeces for ICON grid vertices
   INTEGER :: nv ! counter
   INTEGER :: nt      ! counter
   INTEGER :: j_n, j_c, j_s ! counter for northern, central and southern row
   INTEGER :: j_new ! counter for swapping indices j_n, j_c, j_s
   INTEGER :: j_r           ! counter for row
   INTEGER  :: mlat ! row number for GLOBE data
   REAL(KIND=wp)   ::  dx, dy, dx0    !  grid distance for gradient calculation (in [m])
   REAL(KIND=wp)   ::  d2x, d2y  ! 2 times grid distance for gradient calculation (in [m])
   REAL(KIND=wp)   :: row_lat(1:3)    ! latitude of the row for the topographic height array hh
   REAL(KIND=wp)   :: lat0
   REAL(KIND=wp) :: znorm, znfi2sum, zarg ! help variables for the estiamtion of the variance
   REAL(KIND=wp) :: K_lm, L_lm, M_lm      ! variables to determine angle of principal axis, anisotropy and slope after Lott and Miller 96
   REAL(KIND=wp) :: K_lm_prime, L_lm_prime, M_lm_prime      ! variables to determine angle of principal axis, anisotropy and slope after Lott and Miller 96
   REAL (KIND=wp) :: theta                ! angle of principle axis
   REAL (KIND=wp) :: theta_rot            ! angle of principle axis in the rotated system
   REAL (KIND=wp) :: theta_u, theta_v     ! help variables for the rotation of theta into the rotated system
   REAL (KIND=wp) :: theta_urot, theta_vrot! help variables for the rotation of theta into the rotated system
   REAL (KIND=wp) :: gamma_lm                ! ansisotropy factor
   REAL (KIND=wp) :: gamma_lm2               ! anisotropy factor square
   REAL (KIND=wp) :: zaehler     ! help variable
   REAL (KIND=wp) :: nenner      ! help variable
   REAL (KIND=wp) :: sigma   ! slope parameter
   REAL (KIND=wp) :: bound_north_cosmo !< northern boundary for COSMO target domain
   REAL (KIND=wp) :: bound_south_cosmo !< southern boundary for COSMO target domain
   TYPE(reg_lonlat_grid) :: ta_grid !< structure with definition of the target area grid (dlon must be the same as for the whole GLOBE dataset)
   INTEGER (KIND=i4), ALLOCATABLE :: h_block(:,:) !< a block of GLOBE altitude data
   INTEGER :: block_row_start
   INTEGER :: block_row
   INTEGER :: errorcode !< error status variable
   ! test with walk_to_nc at start
   INTEGER (KIND=i8) :: start_cell_id = 1 !< start cell id 
   TYPE(cartesian_coordinates)  :: target_cc_co     !< coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined
   LOGICAL :: l_child_dom     ! logical switch if child domain exists
   INTEGER :: child_dom_id   ! id of child domain
    !variables for GME search
   INTEGER :: nip1 ! grid mesh dimension 
   REAL (KIND=wp)  :: zx,zy,zz ! cartesian coordinates of point
   REAL (KIND=wp), SAVE  :: spd_t = 1. ! threshold value for scalar product 
   INTEGER :: kd ! diamond containing point
   INTEGER :: kj1,kj2   ! nodal indices of nearest grid point
   ! on entry, kj1 and kj2 are first guess values
   REAL (KIND=wp), SAVE  :: sp =1.! scalar product between point and nearest GME nodal point
   LOGICAL :: ldebug=.FALSE.
   ! global data flag
   LOGICAL :: gldata=.TRUE. ! GLOBE data are global
   REAL (KIND=wp) :: point_lon_geo       !< longitude coordinate in geographical system of input point 
   REAL (KIND=wp) :: point_lat_geo       !< latitude coordinate in geographical system of input point
   REAL(KIND=wp)   :: point_lon, point_lat
   INTEGER (KIND=i8) :: western_column     !< the index of the western_column of raw data 
   INTEGER (KIND=i8) :: eastern_column     !< the index of the eastern_column of raw data 
   INTEGER (KIND=i8) :: northern_row       !< the index of the northern_row of raw data 
   INTEGER (KIND=i8) :: southern_row       !< the index of the southern_row of raw data 
   REAL (KIND=wp) :: globe_point_sw
   REAL (KIND=wp) :: globe_point_se
   REAL (KIND=wp) :: globe_point_ne
   REAL (KIND=wp) :: globe_point_nw
   REAL (KIND=wp) :: bwlon !< weight for bilinear interpolation
   REAL (KIND=wp) :: bwlat !< weight for bilinear interpolation
   REAL (KIND=wp) :: globe_target_value  !< interpolated altitude from GLOBE data
   REAL (KIND=wp) :: fr_land_pixel  !< interpolated fr_land from GLOBE data
   ! variables for the "Erdmann Heise Formel"
   REAL (KIND=wp) :: dnorm  !< scale factor 
   REAL (KIND=wp) :: zlnorm = 2250.    !< scale factor [m]
   REAL (KIND=wp) :: alpha  = 1.E-05 !< scale factor [1/m] 
   REAL (KIND=wp) :: factor !< factor
   REAL           :: zhp = 10.0    !< height of Prandtl-layer [m]
   REAL (KIND=wp) :: zlnhp      !< ln of height of Prandtl-layer [m]
   REAL (KIND=wp) :: z0_globe   !< rougness length according to Erdmann Heise Formula

!mes >

   CHARACTER(LEN=24) :: topo_file_1
   nc_tot_p1 = nc_tot + 1
   topo_file_1 = topo_files(1)
!mes <

   SELECT CASE(tg%igrid_type)
   CASE(igrid_icon)  ! ICON GRID
       ke = 1
       n_dom = ICON_grid%n_dom ! number of Icon domains (grid refinement areas)
       nearest_cell_id = 0_i8 !< result of icon grid search, set to 0 at start
       nearest_vert_id = 0_i8 !< set to 0 at start
   CASE(igrid_cosmo)  ! COSMO GRID
       ke = 1
       bound_north_cosmo = MAXVAL(lat_geo) + 0.05_wp  ! add some "buffer"
       bound_north_cosmo = MIN(bound_north_cosmo,90.0_wp)
       bound_south_cosmo = MINVAL(lat_geo) - 0.05_wp  ! add some "buffer"
       bound_south_cosmo = MAX(bound_south_cosmo,-90.0_wp)
       n_dom = 1 ! no grid refinements
   CASE(igrid_gme)  ! GME GRID
       n_dom = 1 ! no grid refinements
   END SELECT

   j_n = 1 ! index for northern row
   j_c = 2 ! index for central row
   j_s = 3 ! index for southern row

!mes >
   CALL get_fill_value(topo_file_1,undef_topo)

!   undef_globe = -500 ! \TODO read undef value from netcdf attribute
! mes <
   default_globe = 0

   SELECT CASE(topography)
    CASE(topo_aster)
      hh = default_globe
      h_3rows = default_globe
    CASE(topo_gl)
      hh = undef_topo
      h_3rows = undef_topo
   END SELECT

   ! initialize some variables
   no_raw_data_pixel     = 0
   ndata      = 0
   z0_topo    = 0.0
   hh_target   = 0.0
   hh1_target  = 0.0
   hh2_target  = 0.0
   stdh_target = 0.0
   theta_target = 0.0
   aniso_target = 0.0
   slope_target = 0.0
   h11         = 0.0
   h12         = 0.0
   h22         = 0.0
!roa >
   hsmooth     = 0.0
!roa <

   IF (tg%igrid_type == igrid_icon) THEN ! Icon grid
       vertex_param%hh_vert = 0.0
       vertex_param%npixel_vert = 0
   ENDIF
   
   ! calculate the longitude coordinate of the GLOBE columns
   DO i=1,nc_tot
     lon_globe(i) = topo_grid%start_lon_reg + (i-1) * topo_grid%dlon_reg
   ENDDO

   ! calculate the latitiude coordinate of the GLOBE columns
   DO j=1,nr_tot
     lat_globe(j) = topo_grid%start_lat_reg + (j-1) * topo_grid%dlat_reg
   ENDDO
       !HA debug:
       print *,'lat_globe(1): ', lat_globe(1)
       print *,'lat_globe(nr_tot) ', lat_globe(nr_tot)

   nt = 1
   dx0 =  topo_tiles_grid(nt)%dlon_reg * deg2rad * re ! longitudinal distance between to GLOBE grid elemtens at equator 
   print *, 'dx0: ',dx0
   dy = topo_tiles_grid(nt)%dlat_reg * deg2rad * re ! latitudinal distance  between to GLOBE grid elemtens ! note the negative increment, as direction of data from north to south
   print *,'dy: ',dy
   d2y = 2. * dy

   print *,'open GLOBE netcdf files'
   ! first open the GLOBE netcdf files
   DO nt=1,ntiles
     CALL open_netcdf_GLOBE_tile(topo_files(nt), ncids_globe(nt))
   ENDDO
   mlat = 1
   block_row_start = mlat

   CALL det_band_gd(topo_grid,block_row_start, ta_grid)
   PRINT *,'first call of det_band_gd'
   PRINT *,'ta_grid: ',ta_grid
    
   IF(ALLOCATED(h_block)) THEN
      DEALLOCATE(h_block, STAT=errorcode)
      IF(errorcode/=0) CALL abort_extpar('Cant deallocate the h_block')
   ENDIF
   ALLOCATE (h_block(1:ta_grid%nlon_reg,1:ta_grid%nlat_reg), STAT=errorcode)
    IF(errorcode/=0) CALL abort_extpar('Cant allocate h_block')

   CALL get_globe_data_block(topo_file_1,     &   !mes ><
      &                       ta_grid,         &
      &                       topo_tiles_grid, &
      &                       ncids_globe, &
      &                       h_block)

   block_row = 0 

   print *,'Start loop over GLOBE rows'
   !-----------------------------------------------------------------------------
   globe_rows: DO mlat=1,nr_tot            !mes ><
   !globe_rows: DO mlat=1,2000
   !-----------------------------------------------------------------------------
   !-----------------------------------------------------------------------------
   IF (MOD(mlat,100)==0) PRINT *, 'GLOBE row:', mlat
   block_row= block_row + 1
   IF(block_row > ta_grid%nlat_reg) THEN ! read in new block
     block_row_start = mlat
     block_row = 1
     CALL det_band_gd(topo_grid,block_row_start, ta_grid)
     PRINT *,'next call of det_band_gd'
     PRINT *,'ta_grid: ',ta_grid
     IF(ALLOCATED(h_block)) THEN
        DEALLOCATE(h_block, STAT=errorcode)
        IF(errorcode/=0) CALL abort_extpar('Cant deallocate the h_block')
     ENDIF
     ALLOCATE (h_block(1:ta_grid%nlon_reg,1:ta_grid%nlat_reg), STAT=errorcode)
     IF(errorcode/=0) CALL abort_extpar('Cant allocate h_block')
      CALL get_globe_data_block(topo_file_1,     &            !mes ><
        &                       ta_grid,         &
        &                       topo_tiles_grid, &
        &                       ncids_globe, &
        &                       h_block)
   ENDIF

   IF (mlat==1) THEN  !first row of globe data
     !CALL get_globe_data_parallel(mlat, ncids_globe, h_parallel)
     h_parallel(1:nc_tot) = h_block(1:nc_tot,block_row)
     row_lat(j_c) = topo_grid%start_lat_reg + (mlat-1) * topo_grid%dlat_reg

     h_3rows(1:nc_tot,j_c) = h_parallel(1:nc_tot)  ! put data to "central row"
     hh(1:nc_tot,j_c) = h_parallel(1:nc_tot)  ! put data to "central row"
     hh(0,j_c)        = h_parallel(nc_tot) ! western wrap at -180/180 degree longitude
     hh(nc_tot_p1,j_c) = h_parallel(1)      ! eastern wrap at -180/180 degree longitude
   ENDIF
   row_lat(j_s) = topo_grid%start_lat_reg + mlat * topo_grid%dlat_reg  !  ((mlat+1)-1)

   IF (tg%igrid_type == igrid_cosmo) THEN ! CASE COSMO grid
     IF ((row_lat(j_s) > bound_north_cosmo).OR.(row_lat(j_s) < bound_south_cosmo) ) THEN ! raw data out of target grid
       CYCLE globe_rows
     ENDIF
   ENDIF ! COSMO grid

   IF(mlat /= nr_tot) THEN !  read raw data south of "central" row except when you are at the most southern raw data line     !mes ><
     h_parallel(1:nc_tot) = h_block(1:nc_tot,block_row)
     h_3rows(1:nc_tot,j_s) = h_parallel(1:nc_tot)
     hh(1:nc_tot,j_s) = h_parallel(1:nc_tot) ! put data to "southern row"
     hh(0,j_s)        = h_parallel(nc_tot) ! western wrap at -180/180 degree longitude
     hh(nc_tot_p1,j_s) = h_parallel(1)      ! eastern wrap at -180/180 degree longitude
   ENDIF
   dx      = dx0 * COS(row_lat(j_c) * deg2rad)  ! longitudinal distance between to GLOBE grid elemtens
   d2x = 2. * dx
   d2y = 2. * dy
   IF (mlat==1) THEN ! most northern row of raw data
     j_n = j_c  ! put the index of "northern row" to the same index as "central row"
     d2y = dy   ! adjust d2y in this case too
   ELSEIF (mlat==nr_tot) THEN ! most southern row of raw data   !mes ><
     j_s = j_c  ! put the index of "southern row" to the same index as "central row"
     d2y = dy   ! adjust d2y in this case too
   ENDIF

   ! set undefined vaulues to 0 altitude (default)
    WHERE (hh == undef_topo)  
     hh = default_globe
   END WHERE
    WHERE (h_parallel == undef_topo)
     h_parallel = default_globe
   END WHERE


   ! calculate gradients of hh
   DO i=1,nc_tot
    dhdx(i) = (hh(i+1,j_c) - hh(i-1,j_c))/d2x  ! centered differences as gradient, except for mlat=1 and mlat= 21600
    dhdy(i) = (hh(i,j_n) - hh(i,j_s))/d2y 
   ENDDO

   dhdxdx(1:nc_tot) = dhdx(1:nc_tot) * dhdx(1:nc_tot) ! x-gradient square
   dhdydy(1:nc_tot) = dhdy(1:nc_tot) * dhdy(1:nc_tot) ! y-gradient square
   dhdxdy(1:nc_tot) = dhdx(1:nc_tot) * dhdy(1:nc_tot) ! dx*dy

   !------------------------------------------------------------
   DO i=1,nc_tot ! loop over one latitude circle of the raw data
   !------------------------------------------------------------
     point_lon = lon_globe(i) 
     point_lat = row_lat(j_c)

     ! call here the attribution of raw data pixel to target grid for different grid types
     SELECT CASE(tg%igrid_type)
       CASE(igrid_icon)  ! ICON GRID

       target_geo_co%lon = point_lon * deg2rad ! note that the ICON coordinates do not have the unit degree but radians
       target_geo_co%lat = point_lat * deg2rad
       nearest_cell_id = 0_i8 !< result of icon grid search, set to 0 at start
       nearest_vert_id = 0_i8 !< set to 0 at start
       target_cc_co = gc2cc(target_geo_co)
       CALL walk_to_nc(icon_grid_region(icon_dom_nr),   &
                          target_cc_co,     &
                          start_cell_id,    &
                          ICON_grid%nvertex_per_cell, &
                          nearest_cell_id)
       start_cell_id = nearest_cell_id ! save for next search

       ! additional get the nearest vertex index for accumulating height values there

       CALL  find_nearest_vert(icon_grid_region(icon_dom_nr), &
                          target_cc_co,                  &
                          nearest_cell_id,        &
                          ICON_grid%nvertex_per_cell,    &
                          nearest_vert_id)
         
       ke = 1

       ! additional get the nearest vertex index for accumulating height values there

       ! aggregate the vertex parameter here
       i_vert = nearest_vert_id
       j_vert = 1
       k_vert = 1
       IF ((i_vert /=0).AND.(j_vert /=0).AND.(k_vert /=0)) THEN ! raw data pixel within target grid
         vertex_param%npixel_vert(i_vert,j_vert,k_vert) =  &
         vertex_param%npixel_vert(i_vert,j_vert,k_vert) + 1

         vertex_param%hh_vert(i_vert,j_vert,k_vert) =  &
         vertex_param%hh_vert(i_vert,j_vert,k_vert) +  h_parallel(i)
       ENDIF

       ie =  nearest_cell_id
       je = 1
       ke = 1

       CASE(igrid_cosmo)  ! COSMO GRID

       CALL find_rotated_lonlat_grid_element_index(point_lon, &
                                               point_lat,     &
                                               COSMO_grid,       &
                                               ie,               &
                                               je)
       ke = 1
       CASE(igrid_gme)  ! GME GRID
         nip1 = gme_grid%ni + 1
         CALL pp_ll2gp(xn,point_lon,point_lat,&
                     & nip1,                          &
                     & zx,zy,zz,                      &
                     & spd_t,                         &
                     & kd,kj1,kj2,                    &
                     & sp,ldebug)

         ie = kj1 + 1
         je = kj2
         ke = kd
       END SELECT

       IF ((ie /= 0).AND.(je/=0).AND.(ke/=0))THEN ! raw data pixel within target grid, see output of routine find_rotated_lonlat_grid_element_index
           no_raw_data_pixel(ie,je,ke) = no_raw_data_pixel(ie,je,ke) + 1
           !- summation of variables
! mes >
           SELECT CASE(topography)
            CASE(topo_aster)
             IF (h_3rows(i,j_c) /= default_globe) THEN       
               ndata(ie,je,ke)      = ndata(ie,je,ke) + 1
               hh_target(ie,je,ke)  = hh_target(ie,je,ke) + h_3rows(i,j_c)
               hh2_target(ie,je,ke) = hh2_target(ie,je,ke) + (h_3rows(i,j_c) * h_3rows(i,j_c))
               h11(ie,je,ke)        = h11(ie,je,ke) + dhdxdx(i)
               h12(ie,je,ke)        = h12(ie,je,ke) + dhdxdy(i)
               h22(ie,je,ke)        = h22(ie,je,ke) + dhdydy(i)
             ENDIF
            CASE(topo_gl)
             IF (h_3rows(i,j_c) /= undef_topo) THEN            
               ndata(ie,je,ke)      = ndata(ie,je,ke) + 1
               hh_target(ie,je,ke)  = hh_target(ie,je,ke) + h_3rows(i,j_c)
               hh2_target(ie,je,ke) = hh2_target(ie,je,ke) + (h_3rows(i,j_c) * h_3rows(i,j_c))
               h11(ie,je,ke)        = h11(ie,je,ke) + dhdxdx(i)
               h12(ie,je,ke)        = h12(ie,je,ke) + dhdxdy(i)
               h22(ie,je,ke)        = h22(ie,je,ke) + dhdydy(i)
             ENDIF
           END SELECT
! mes <

       ENDIF

       ENDDO ! loop over one latitude circle of the raw data


       ! swap indices of the hh array for next data row
       j_new = j_n ! the new data will be written in the former "northern" array
       j_n = j_c   ! the "center" row will become "northern" row
       j_c = j_s   ! the "southern" row will become "center" row
       j_s = j_new ! the new data will be written in the "southern" row

       !-----------------------------------------------------------------------------
       !-----------------------------------------------------------------------------
       ENDDO globe_rows
       !-----------------------------------------------------------------------------

       print *,'loop over globe_rows done'

       PRINT *,'Maximum number of GLOBE raw data pixel in a target grid element: '
       PRINT *,'MAXVAL(no_raw_data_pixel): ', MAXVAL(no_raw_data_pixel)
       PRINT *,'Index of target grid element: ', MAXLOC(no_raw_data_pixel)

       PRINT *,'Maximum number of GLOBE land pixel in a target grid element: '
       PRINT *,'MAXVAL(ndata): ', MAXVAL(ndata)
       PRINT *,'Index of target grid element: ', MAXLOC(ndata)

       

       PRINT *,'Minimal number of GLOBE raw data pixel in a target grid element: '
       PRINT *,'MINVAL(no_raw_data_pixel): ', MINVAL(no_raw_data_pixel)
       PRINT *,'Index of target grid element: ', MINLOC(no_raw_data_pixel)

       PRINT *,'Minimal number of GLOBE land pixel in a target grid element: '
       PRINT *,'MINVAL(ndata): ', MINVAL(ndata)
       PRINT *,'Index of target grid element: ', MINLOC(ndata)



           
     SELECT CASE(tg%igrid_type)
     CASE(igrid_gme)  ! in GME grid the diamond edges need to be synrchonized

       ! hh_target
       CALL cp_buf2gme(tg,gme_grid,hh_target,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,hh_target)
       ! hh2_target
       CALL cp_buf2gme(tg,gme_grid,hh2_target,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,hh2_target)
       ! h11
       CALL cp_buf2gme(tg,gme_grid,h11,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,h11)
       ! h12
       CALL cp_buf2gme(tg,gme_grid,h12,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,h12)
       ! h22
       CALL cp_buf2gme(tg,gme_grid,h22,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,h22)

       ! no_raw_data_pixel
       CALL cp_buf2gme(tg,gme_grid,no_raw_data_pixel,gme_int_field)
       CALL sync_diamond_edge(gme_grid, gme_int_field)
       CALL cp_gme2buf(tg,gme_grid,gme_int_field,no_raw_data_pixel)

       ! ndata
       CALL cp_buf2gme(tg,gme_grid,ndata,gme_int_field)
       CALL sync_diamond_edge(gme_grid, gme_int_field)
       CALL cp_gme2buf(tg,gme_grid,gme_int_field,ndata)
     END SELECT

      hh1_target = hh_target ! save values of hh_target for computations of standard deviation

      print *,'Average height'
      ! Average height
      DO ke=1, tg%ke
      DO je=1, tg%je
      DO ie=1, tg%ie
        IF (no_raw_data_pixel(ie,je,ke) /= 0) THEN ! avoid division by zero for small target grids
          hh_target(ie,je,ke) = hh_target(ie,je,ke)/no_raw_data_pixel(ie,je,ke) ! average height, oceans point counted as 0 height
          fr_land_globe(ie,je,ke) =  REAL(ndata(ie,je,ke),wp) / REAL(no_raw_data_pixel(ie,je,ke),wp) ! fraction land
        ELSE
          hh_target(ie,je,ke) = REAL(default_globe)
          fr_land_globe(ie,je,ke) = 0.0
        ENDIF
      ENDDO 
      ENDDO
      ENDDO
!roa>
      hsmooth = hh_target
! oro filt here
      IF (lfilter_oro) THEN
         CALL do_orosmooth(tg,                                 &
      &                                      hh_target,        &
      &                                      fr_land_globe,    &
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
      &                                      hsmooth           )
      ENDIF
!roa<


      IF (tg%igrid_type == igrid_icon) THEN ! CASE ICON grid
        print *,'Average height for vertices'
       ! Average height for vertices
        DO ke=1, 1
        DO je=1, 1
        DO ie=1, icon_grid_region(icon_dom_nr)%nverts

          IF (vertex_param%npixel_vert(ie,je,ke) /= 0) THEN ! avoid division by zero for small target grids
            vertex_param%hh_vert(ie,je,ke) =  &
            vertex_param%hh_vert(ie,je,ke)/vertex_param%npixel_vert(ie,je,ke) ! average height
          ELSE
             vertex_param%hh_vert(ie,je,ke) = REAL(default_globe)
          ENDIF
        ENDDO 
        ENDDO
        ENDDO
       ENDIF
      print *,'Standard deviation of height'
      !     Standard deviation of height.
      DO ke=1, tg%ke
      DO je=1, tg%je
      DO ie=1, tg%ie
!roa>        
          ! estimation of variance
         IF (lfilter_oro) THEN
            IF (no_raw_data_pixel(ie,je,ke) > 1) THEN
               znorm = 1.0/(no_raw_data_pixel(ie,je,ke)-1)
            ELSE
               znorm = 0.0
            ENDIF
             !!!!! standard deviation of height using oro filt !!!!!
            zarg = znorm * (hh2_target(ie,je,ke) &
                    -2 * hsmooth(ie,je,ke) * hh1_target(ie,je,ke)                   &
                    + no_raw_data_pixel(ie,je,ke) * hsmooth(ie,je,ke)**2)
         ELSE
            IF (no_raw_data_pixel(ie,je,ke) > 1) THEN
               znorm = 1.0/(no_raw_data_pixel(ie,je,ke) * (no_raw_data_pixel(ie,je,ke)-1))
            ELSE
               znorm = 0.0
            ENDIF
            znfi2sum = no_raw_data_pixel(ie,je,ke) * hh2_target(ie,je,ke) 
            zarg     = ( znfi2sum - (hh1_target(ie,je,ke)*hh1_target(ie,je,ke))) * znorm
         ENDIF
            zarg = MAX(zarg,0.0_wp) ! truncation errors may cause zarg < 0.0
            stdh_target(ie,je,ke) = SQRT(zarg)
!roa<

      ENDDO
      ENDDO
      ENDDO

       print *,'SSO parameters'
      ! SSO parameters
      ! angle of principal axis
      DO ke=1, tg%ke
      DO je=1, tg%je
      DO ie=1, tg%ie
        IF (no_raw_data_pixel(ie,je,ke) > 1) THEN
            znorm = 1.0/no_raw_data_pixel(ie,je,ke) 
        ELSE
            znorm = 0.
        ENDIF
        IF (stdh_target(ie,je,ke) > 10.0) THEN ! avoid trivial case of sea point
          ! SSO parameters
          zh11 = h11(ie,je,ke) * znorm 
          zh12 = h12(ie,je,ke) * znorm 
          zh22 = h22(ie,je,ke) * znorm 
          
          ! calculation of angle of principal axis
          !----------------------------------------------------------------------------------
          ! Equation (A.1) of Lott and Miller, 1996
          K_lm = 0.5 * (zh11 + zh22)
          L_lm = 0.5 * (zh11 - zh22) 
          M_lm = zh12

          ! angle of principle axis
          theta = 0.5 * ATAN2(M_lm,L_lm)    ! Lott and Miller 1996, equation (A.2)
          SELECT CASE(tg%igrid_type)
            CASE(igrid_icon)  ! ICON GRID
              theta_target(ie,je,ke) = theta  
            CASE(igrid_cosmo)  ! COSMO GRID
              ! compute theta in the rotated grid
              theta_u = COS(theta)
              theta_v = SIN(theta)
              point_lon = lon_geo(ie,je,ke)
              point_lat = lat_geo(ie,je,ke)
              CALL uv2uvrot(theta_u, theta_v,           &
                            point_lat, point_lon,       &
                            COSMO_grid%pollat, COSMO_grid%pollon, &
                            theta_urot, theta_vrot)
              theta_rot = ATAN2(theta_vrot,theta_urot)            ! angle of principle axis in the rotated system
              ! Restrict the range of values of theta to [-pi/2,pi/2]
              IF (theta_rot < -pi/2.) theta_rot = theta_rot + pi
              IF (theta_rot >  pi/2.) theta_rot = theta_rot - pi
              theta_target(ie,je,ke) = theta_rot  ! angle of principle axis in the rotated system
            CASE(igrid_gme) ! GME GRID
              theta_target(ie,je,ke) = theta
          END SELECT
          !----------------------------------------------------------------------------------
          ! calculation of anisotropy factor
          !----------------------------------------------------------------------------------
          K_lm_prime = K_lm
          L_lm_prime = SQRT(L_lm*L_lm + M_lm*M_lm)
          M_lm_prime = 0.0
          zaehler = K_lm_prime - L_lm_prime
          nenner  = K_lm_prime + L_lm_prime
          IF (zaehler <= EPSILON(zaehler) ) zaehler = 0.0
          IF (nenner  <= EPSILON(nenner) )  nenner  = EPSILON(nenner)
          gamma_lm2 = zaehler / nenner
          gamma_lm = SQRT(gamma_lm2) ! Lott and Miller 1996, equation (A.3)
          aniso_target(ie,je,ke) = gamma_lm  ! anisotropy factor
          !----------------------------------------------------------------------------------
          ! Calculation of slope parameter
          !----------------------------------------------------------------------------------
           sigma = K_lm_prime + L_lm_prime
           sigma = SQRT(sigma) ! Lott and Miller 1996, equation (A.5)
           slope_target(ie,je,ke) = sigma
          ELSE  ! seapoints or other points with STDH <= 10 m
            theta_target(ie,je,ke) = 0.0
            aniso_target(ie,je,ke) = 0.0
            slope_target(ie,je,ke) = 0.0
          ENDIF
      ENDDO
      ENDDO
      ENDDO
      !----------------------------------------------------------------------------------
      ! calculate roughness length
      ! first zo_topo with "Erdmann Heise formula"
      !----------------------------------------------------------------------------------
       
       SELECT CASE(tg%igrid_type)
         CASE(igrid_icon)  ! ICON GRID
           dnorm = 60000.         ! dummy value for normation of Erdmann Heise formula
         CASE(igrid_cosmo)  ! COSMO GRID
             dnorm = cosmo_grid%dlon_rot * deg2rad * re ! average grid size for Erdman Heise formula, in [m]
         CASE(igrid_gme)  ! GME GRID
           dnorm = 60000.         ! dummy value for normation of Erdmann Heise formula
       END SELECT
       !---------------------------------------------------------------------------------
       ! Erdman Heise Formel
       !---------------------------------------------------------------------------------
       zlnhp = ALOG(zhp)
       factor= alpha*ATAN(dnorm/zlnorm) !  alpha  = 1.E-05 [1/m] ,  zlnorm = 2250 [m]  
       DO ke=1, tg%ke
       DO je=1, tg%je
       DO ie=1, tg%ie
         z0_globe = factor*stdh_target(ie,je,ke)**2
         z0_globe = MIN(z0_globe,zhp-1.0_wp)
         z0_topo(ie,je,ke) = z0_globe
       ENDDO
       ENDDO
       ENDDO

       !roa>
       ! set the orography variable hh_target to the smoothed orography variable
       ! hsmooth in case of orogrpahy smoothing in extpar
       IF (lfilter_oro) THEN
          hh_target (:,:,:) = hsmooth (:,:,:)
       ENDIF


       ! bilinear interpolation of the orography in case of target grid points having
       ! no corresponding points in GLOBE
!roa<
       DO ke=1, tg%ke
       DO je=1, tg%je
       DO ie=1, tg%ie
         IF (no_raw_data_pixel(ie,je,ke) == 0) THEN  ! bilinear interpolation to target grid
 
           point_lon_geo = lon_geo(ie,je,ke)
           point_lat_geo = lat_geo(ie,je,ke)
 
           CALL bilinear_interpol_globe_to_target_point(topo_files, & !mes ><
             &                                      topo_grid, &
             &                                      topo_tiles_grid, &
             &                                      ncids_globe,      &
             &                                      lon_globe,        &
             &                                      lat_globe,        &
             &                                      point_lon_geo,      &
             &                                      point_lat_geo,      &
             &                                      fr_land_pixel,      &
             &                                      globe_target_value)
 
           fr_land_globe(ie,je,ke) = fr_land_pixel
           hh_target(ie,je,ke) = globe_target_value
            
           stdh_target(ie,je,ke)  = 0.  
           theta_target(ie,je,ke) = 0.
           aniso_target(ie,je,ke) = 0.
           slope_target(ie,je,ke) = 0.
           z0_topo(ie,je,ke)     = 0.
         ENDIF
       ENDDO
       ENDDO
       ENDDO

       SELECT CASE(tg%igrid_type)
         CASE(igrid_icon) ! ICON GRID
         je=1
         ke=1
         DO nv=1, icon_grid_region(icon_dom_nr)%nverts
           IF (vertex_param%npixel_vert(nv,je,ke) == 0) THEN ! interpolate from raw data in this case
             point_lon_geo =  rad2deg * icon_grid_region(icon_dom_nr)%verts%vertex(nv)%lon
             point_lat_geo =  rad2deg * icon_grid_region(icon_dom_nr)%verts%vertex(nv)%lat

             CALL bilinear_interpol_globe_to_target_point(topo_files,  & !mes ><
               &                                      topo_grid, &
               &                                      topo_tiles_grid, &
               &                                      ncids_globe,      &
               &                                      lon_globe,        &
               &                                      lat_globe,        &
               &                                      point_lon_geo,      &
               &                                      point_lat_geo,      &
               &                                      fr_land_pixel,      &
               &                                      globe_target_value)

              vertex_param%hh_vert(nv,je,ke) = globe_target_value
           ENDIF  
         ENDDO
       END SELECT
       ! close the GLOBE netcdf files
       DO nt=1,ntiles
          CALL close_netcdf_GLOBE_tile(ncids_globe(nt))
       ENDDO
       PRINT *,'GLOBE netcdf files closed'
       PRINT *,'Subroutine agg_globe_data_to_target_grid done'
       END SUBROUTINE agg_globe_data_to_target_grid

       !----------------------------------------------------------------------------------------------------------------
       
       !> subroutine for bilenar interpolation from GLOBE data (regular lonlat grid) to a single target point
       !!
       !! the GLOBE data are passed to the subroutine in the globe_data_block 2D-Array, which is re-read 
       !! from the raw data file if the target point is out of the range of the data block. 
       !! (If the data block is not too small, repeated I/O to the hard disk is avoided, reading from memory is much faster.)
       !! 
       !! the definition of the regular lon-lat grid requires 
       !! - the coordinates of the north-western point of the domain ("upper left") startlon_reg_lonlat and startlat_reg_lonlat
       !! - the increment dlon_reg_lonlat and dlat_reg_lonlat(implict assuming that the grid definiton goes from the west to the east and from the north to the south)
       !! - the number of grid elements nlon_reg_lonlat and nlat_reg_lonlat for both directions
       SUBROUTINE bilinear_interpol_globe_to_target_point(topo_files, & !mes ><
                                               topo_grid,             &
                                               topo_tiles_grid,       &
                                               ncids_globe,           &
                                               lon_globe,             &
                                               lat_globe,             &
                                               point_lon_geo,         &
                                               point_lat_geo,         &
                                               fr_land_pixel,         &
                                               globe_target_value)
       
       USE mo_topo_data, ONLY: ntiles  !< there are 16 GLOBE tiles 
       USE mo_topo_data, ONLY: nc_tot      !< number of total GLOBE columns at a latitude circle (43200)
       USE mo_topo_data, ONLY: nr_tot      !< number of total GLOBE rows at a latitude circle (43200)
!mes >
       USE mo_topo_data, ONLY: get_fill_value  ! determines the corresponding _FillValue of GLOBE or ASTER
       USE mo_topo_data, ONLY: max_tiles
! mes <
       USE mo_grid_structures, ONLY: reg_lonlat_grid  !< Definition of Data Type to describe a regular (lonlat) grid
       USE mo_grid_structures, ONLY: rotated_lonlat_grid !< Definition of Data Type to describe a rotated lonlat grid
       USE mo_globe_routines, ONLY: open_netcdf_globe_tile
       USE mo_globe_routines, ONLY: close_netcdf_globe_tile
       USE mo_globe_routines, ONLY: get_globe_data_band
       USE mo_globe_routines, ONLY: get_globe_data_block
       USE mo_bilinterpol, ONLY: get_4_surrounding_raw_data_indices, &
          &                        calc_weight_bilinear_interpol, &
          &                        calc_value_bilinear_interpol
       CHARACTER(LEN=24), INTENT(IN)     :: topo_files(1:max_tiles)
       TYPE(reg_lonlat_grid), INTENT(IN) :: topo_grid                !< structure with defenition of the raw data grid for the whole GLOBE dataset
       TYPE(reg_lonlat_grid), INTENT(IN) :: topo_tiles_grid(1:ntiles) !< structure with defenition of the raw data grid for the 16 GLOBE tiles
       INTEGER (KIND=i4), INTENT(IN) :: ncids_globe(1:ntiles)  !< ncid for the GLOBE tiles, the netcdf files have to be opened by a previous call of open_netcdf_GLOBE_tile
       
       REAL (KIND=wp), INTENT(IN) :: lon_globe(1:nc_tot)   !< longitude coordinates of the GLOBE grid
       REAL (KIND=wp), INTENT(IN) :: lat_globe(1:nr_tot)   !< latititude coordinates of the GLOBE grid
       REAL (KIND=wp), INTENT(IN) :: point_lon_geo       !< longitude coordinate in geographical system of input point 
       REAL (KIND=wp), INTENT(IN) :: point_lat_geo       !< latitude coordinate in geographical system of input point
       REAL (KIND=wp), INTENT(OUT) :: fr_land_pixel  !< interpolated fr_land from GLOBE data
       REAL (KIND=wp), INTENT(OUT) :: globe_target_value  !< interpolated altitude from GLOBE data

       ! local variables
       INTEGER (KIND=i4), ALLOCATABLE :: h_block(:,:) !< a block of GLOBE altitude data
       TYPE(reg_lonlat_grid) :: ta_grid !< structure with definition of the target area grid (dlon must be the same as for the whole GLOBE dataset)
       INTEGER :: nt      ! counter
       INTEGER  (KIND=i8) :: point_lon_index !< longitude index of point for regular lon-lat grid
       INTEGER  (KIND=i8) :: point_lat_index !< latitude index of point for regular lon-lat grid
       INTEGER (KIND=i8) :: western_column     !< the index of the western_column of data to read in
       INTEGER (KIND=i8) :: eastern_column     !< the index of the eastern_column of data to read in
       INTEGER (KIND=i8) :: northern_row       !< the index of the northern_row of data to read in
       INTEGER (KIND=i8) :: southern_row       !< the index of the southern_row of data to read in
       REAL (KIND=wp)   :: bwlon  !< weight for bilinear interpolation
       REAL (KIND=wp)   :: bwlat  !< weight for bilinear interpolation
       REAL (KIND=wp) :: south_lat !< southern latitude of GLOBE data pixel for bilinear interpolation 
       REAL (KIND=wp) :: west_lon  !< western longitude of GLOBE data pixel for bilinear interpolation 
       REAL (KIND=wp) :: pixel_lon !< longitude coordinate in geographical system of input point
       REAL (KIND=wp) :: pixel_lat !< latitude coordinate in geographical system of input point
       REAL (KIND=wp) :: globe_pixel_lon !< longitude coordinate in geographical system of globe raw data point
       REAL (KIND=wp) :: globe_pixel_lat !< latitude coordinate in geographical system of globe raw data point
       REAL (KIND=wp)   :: globe_point_sw       !< value of the GLOBE raw data pixel south west
       REAL (KIND=wp)   :: globe_point_se       !< value of the GLOBE raw data pixel south east
       REAL (KIND=wp)   :: globe_point_ne       !< value of the GLOBE raw data pixel north east
       REAL (KIND=wp)   :: globe_point_nw       !< value of the GLOBE raw data pixel north west
       INTEGER :: errorcode
       LOGICAL :: gldata=.TRUE. ! GLOBE data are global
       INTEGER :: ndata
       INTEGER :: nland
       INTEGER (KIND=i4) :: undef_topo
       INTEGER (KIND=i4) :: default_globe
!mes >
       CHARACTER(LEN=24) :: topo_file_1   
!mes >
       topo_file_1 = topo_files(1)

       CALL get_fill_value(topo_file_1,undef_topo)

!       print*, undef_topo
!       undef_globe = -500 ! \TODO read undef value from netcdf attribute
!mes <
       default_globe = 0

       ! get four surrounding raw data indices
       CALL  get_4_surrounding_raw_data_indices(topo_grid, &
         &                                      lon_globe,     &
         &                                      lat_globe,     &
         &                                      gldata,        &
         &                                      point_lon_geo, &
         &                                      point_lat_geo, &
         &                                      western_column,&
         &                                      eastern_column,&
         &                                      northern_row,  &
         &                                      southern_row)
         !print *,'western_column, eastern_column, northern_row, southern_row'  
         !print *, western_column, eastern_column, northern_row, southern_row  

         
       ta_grid%dlon_reg = topo_grid%dlon_reg
       ta_grid%dlat_reg = topo_grid%dlat_reg
       ta_grid%nlon_reg = eastern_column - western_column + 1
       ta_grid%nlat_reg = southern_row - northern_row + 1
       ta_grid%start_lon_reg = lon_globe(western_column)
       ta_grid%end_lon_reg = lon_globe(eastern_column)
       ta_grid%start_lat_reg = lat_globe(northern_row)
       ta_grid%end_lat_reg  = lat_globe(southern_row) 
          
       ! calculate weight for bilinear interpolation
       CALL calc_weight_bilinear_interpol(point_lon_geo, &
         &                                 point_lat_geo, &
         &                                 lon_globe(western_column), &
         &                                 lon_globe(eastern_column), &
         &                                 lat_globe(northern_row),   &
         &                                 lat_globe(southern_row),   &
         &                                 bwlon,         &
         &                                 bwlat)

       ALLOCATE (h_block(western_column:eastern_column,northern_row:southern_row), STAT=errorcode)
       IF(errorcode/=0) CALL abort_extpar('Cant allocate h_block')
       CALL get_globe_data_block(topo_file_1,     &   !mes ><
         &                       ta_grid,         &
         &                       topo_tiles_grid, &
         &                       ncids_globe, &
         &                       h_block)
       ! check for undefined GLOBE data, which indicate ocean grid element

       IF( h_block(western_column,southern_row) == undef_topo) THEN
          globe_point_sw = 0.0
          h_block(western_column,southern_row) = default_globe
       ELSE
          globe_point_sw = 1.0
       ENDIF

       IF( h_block(eastern_column,southern_row) == undef_topo) THEN
          globe_point_se = 0.0
          h_block(eastern_column,southern_row) = default_globe
       ELSE
          globe_point_se = 1.0
       ENDIF
       
       IF( h_block(eastern_column,northern_row) == undef_topo) THEN
          globe_point_ne = 0.0
          h_block(eastern_column,northern_row) = default_globe
       ELSE
          globe_point_ne = 1.0
       ENDIF

       IF( h_block(western_column,northern_row) == undef_topo) THEN
          globe_point_nw = 0.0
          h_block(western_column,northern_row) = default_globe 
       ELSE
          globe_point_nw = 1.0
       ENDIF


       ! perform the interpolation, first for fraction land
       fr_land_pixel = calc_value_bilinear_interpol(bwlon, &
                                                 & bwlat, &
                                                 & globe_point_sw, &
                                                 & globe_point_se, &
                                                 & globe_point_ne, &
                                                 & globe_point_nw)

       globe_point_sw = h_block(western_column,southern_row)
       globe_point_se = h_block(eastern_column,southern_row)
       globe_point_ne = h_block(eastern_column,northern_row)
       globe_point_nw = h_block(western_column,northern_row)

       ! perform the interpolation for height
       globe_target_value = calc_value_bilinear_interpol(bwlon, &
                                                 & bwlat, &
                                                 & globe_point_sw, &
                                                 & globe_point_se, &
                                                 & globe_point_ne, &
                                                 & globe_point_nw)

       END SUBROUTINE bilinear_interpol_globe_to_target_point




!----------------------------------------------------------------------------------------------------------------



END MODULE mo_agg_globe

