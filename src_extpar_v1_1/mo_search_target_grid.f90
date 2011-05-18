!+ Fortran Module to search index in target grid
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran Module to search index in target grid
!! Fortran module to find nearest grid element index in target grid for external parameters
!! 
!! Depending of the type of the grid, the nearest grid point search for the ICON grid or the 
!! Cosmo-grid are called
!!
MODULE mo_search_target_grid


  USE mo_kind,            ONLY: wp, i4, i8
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: find_nearest_target_grid_element


  CONTAINS

  SUBROUTINE find_nearest_target_grid_element(  point_lon_geo, &
    &                                           point_lat_geo, &
    &                                           tg,            &
    &                                           tg_el_ie,      &
    &                                           tg_el_je,      &
    &                                           tg_el_ke)

  USE mo_grid_structures, ONLY: target_grid_def
  USE mo_grid_structures, ONLY: reg_lonlat_grid
  USE mo_grid_structures, ONLY: target_grid_def

  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme

  USE mo_search_ll_grid, ONLY: find_reg_lonlat_grid_element_index
  USE mo_search_ll_grid, ONLY: find_rotated_lonlat_grid_element_index

  ! USE structure which contains the definition of the COSMO grid
  USE  mo_cosmo_grid, ONLY: COSMO_grid !< structure which contains the definition of the COSMO grid

  ! USE structure which contains the definition of the ICON grid
  USE  mo_icon_grid_data, ONLY: ICON_grid !< structure which contains the definition of the ICON grid

  USE mo_gme_grid, ONLY: gme_grid
  USE mo_gme_grid, ONLY: xn, rlon_gme, rlat_gme
  USE mo_search_gme_grid, ONLY: pp_ll2gp
  !USE mo_gme_grid, ONLY: xns

  ! USE icon domain structure wich contains the ICON coordinates (and parent-child indices etc)
  ! USE mo_icon_grid_data, ONLY:  icon_grid_region
  ! USE mo_icon_grid_data, ONLY:  icon_grid_level
  USE mo_icon_grid_data, ONLY: icon_domain_grid


  ! USE modules to search in ICON grid
  USE mo_search_icongrid, ONLY: find_nc
  USE mo_search_icongrid, ONLY: walk_to_nc
  USE mo_search_icongrid, ONLY: find_nchild_nlev
  USE mo_search_icongrid, ONLY: find_nearest_vert

  USE mo_icon_domain,     ONLY: icon_domain

  USE mo_base_geometry,   ONLY: geographical_coordinates
  USE mo_base_geometry,   ONLY: cartesian_coordinates

  USE mo_additional_geometry,   ONLY: gc2cc

  USE mo_math_constants, ONLY: deg2rad, pi

  
  REAL (KIND=wp), INTENT(in) :: point_lon_geo       !< longitude coordinate in geographical system of input point 
  REAL (KIND=wp), INTENT(in) :: point_lat_geo       !< latitude coordinate in geographical system of input point
  TYPE(target_grid_def), INTENT(IN) :: tg           !< structure with target grid description

  INTEGER (KIND=i8), INTENT(OUT) :: tg_el_ie        !< Index tg_el_ie of target grid element nearest to the input point
  INTEGER (KIND=i8), INTENT(OUT) :: tg_el_je        !< Index tg_el_je of target grid element nearest to the input point
  INTEGER (KIND=i8), INTENT(OUT) :: tg_el_ke        !< Index tg_el_ke of target grid element nearest to the input point

  ! local variables
   TYPE(geographical_coordinates) :: target_geo_co  !< structure for geographical coordinates of raw data pixel
   TYPE(cartesian_coordinates)  :: target_cc_co     !< coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined
   INTEGER (KIND=i8) :: nearest_cell_id
   INTEGER (KIND=i8), SAVE :: start_cell_id = 1 !< start cell id 

   ! variables for GME search
   INTEGER :: nip1 ! grid mesh dimension 
   REAL (KIND=wp)  :: zx,zy,zz ! cartesian coordinates of point
   REAL (KIND=wp), SAVE  :: spd_t = 1. ! threshold value for scalar product 
   INTEGER, SAVE :: kd = 1  ! diamond containing point
   INTEGER, SAVE :: kj1 = 0 ! nodal indices of nearest grid point
   INTEGER, SAVE :: kj2 = 1 ! on entry, kj1 and kj2 are first guess values
   REAL (KIND=wp), SAVE  :: sp =1.! scalar product between point and nearest GME nodal point
   LOGICAL :: ldebug=.FALSE.

       tg_el_ie = 0_i8 ! default settings
       tg_el_je = 0_i8
       tg_el_ke = 0_i8

       SELECT CASE(tg%igrid_type)
       CASE(igrid_icon)  ! ICON GRID

         target_geo_co%lon = point_lon_geo * deg2rad ! note that the ICON coordinates do not have the unit degree but radians
         target_geo_co%lat = point_lat_geo * deg2rad

         target_cc_co = gc2cc(target_geo_co) ! transform the geographical coordinates of the point to cartesian coordinates

         nearest_cell_id = 0_i8 !< result of icon grid search, set to 0 at start

         CALL walk_to_nc(icon_domain_grid,   &
           &             target_cc_co,     &
           &             start_cell_id,    &
           &             ICON_grid%nvertex_per_cell, &
           &             nearest_cell_id)
         start_cell_id = nearest_cell_id ! save for next search

         tg_el_ie = nearest_cell_id
         tg_el_je = 1_i8
         tg_el_ke = 1_i8

       CASE(igrid_cosmo)  ! COSMO GRID

         CALL find_rotated_lonlat_grid_element_index(point_lon_geo, &
           &                                    point_lat_geo,     &
           &                                    COSMO_grid,        &
           &                                    tg_el_ie,          &
           &                                    tg_el_je)
         tg_el_ke = 1_i8

       CASE(igrid_gme)  ! GME GRID
         nip1 = gme_grid%ni + 1
        !!HA debug:
        ! IF (point_lat_geo < 75.0 ) THEN
        ! PRINT *,'point_lon_geo: ', point_lon_geo
        ! print *,'point_lat_geo: ', point_lat_geo
        ! ldebug = .TRUE.
        ! ENDIF
         ! kd,kj1,kj2 are used as 'first guess' as input and resulting indices as output
         CALL pp_ll2gp(xn,point_lon_geo,point_lat_geo,&
           &           nip1,                          &
           &           zx,zy,zz,                      &
           &           spd_t,                         &
           &           kd,kj1,kj2,                    &
           &           sp,ldebug)

         tg_el_ie = kj1 + 1
         tg_el_je = kj2
         tg_el_ke = kd

        ! IF (point_lat_geo < 75.0 ) THEN
        ! PRINT *,'HA debug'
        ! PRINT *,'tg_el_ie, tg_el_je, tg_el_ke ', tg_el_ie, tg_el_je, tg_el_ke
        ! stop
        ! ENDIF

       END SELECT

  END  SUBROUTINE find_nearest_target_grid_element


END MODULE mo_search_target_grid


