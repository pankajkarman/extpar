!+ Fortran module to aggregate aerosol optical thickness raw data to target grid
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
!> Fortran module to aggregate aerosol optical thickness raw data to target grid
!!
!! This module interpolates monthly aerosol optical thicknesses for five different type of aerosols 
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
!!  Contribution of different aerosol species to the global aerosol extinction optical thickness: Estimates from model results</a>.
!! J. Geophys. Res., <b>102</b>, 23895-23915.
!> \author Hermann Asensio
MODULE mo_agg_aot

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4



  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_bilinterpol, ONLY:  get_4_surrounding_raw_data_indices, &
    &                        calc_weight_bilinear_interpol, &
    &                        calc_value_bilinear_interpol



IMPLICIT NONE

PRIVATE

PUBLIC :: agg_aot_data_to_target_grid

    CONTAINS

    !> Subroutine to aggregate aerosol optical thickness data to the target grid
    SUBROUTINE agg_aot_data_to_target_grid(nrows,ncolumns,ntime,ntype)
      !-------------------------------------------------------------------------------------
      ! list of modules which are used as "input"
      USE mo_grid_structures, ONLY: target_grid_def   !< type definition of structure for tg
      !> data type structures form module GRID_structures
      USE mo_grid_structures, ONLY: reg_lonlat_grid, &
        &                           rotated_lonlat_grid, &
        &                           gme_triangular_grid
     USE mo_grid_structures, ONLY: igrid_gme, igrid_icon, igrid_cosmo

      ! USE icon domain structure wich contains the ICON coordinates (and parent-child indices etc)
      USE mo_icon_grid_data, ONLY:  icon_grid_region
      USE mo_icon_grid_data, ONLY:  icon_grid_level

      ! USE structure which contains the definition of the ICON grid
      USE  mo_icon_grid_data, ONLY: icon_grid !< structure which contains the definition of the ICON grid

      ! USE structure which contains the definition of the COSMO grid
      USE  mo_cosmo_grid, ONLY: cosmo_grid !< structure which contains the definition of the COSMO grid

      USE mo_base_geometry,   ONLY: geographical_coordinates
      USE mo_base_geometry,   ONLY: cartesian_coordinates
      USE mo_math_constants, ONLY: pi, rad2deg, eps
      USE mo_physical_constants, ONLY: re
      USE mo_additional_geometry,   ONLY: cc2gc,                  &
        &                                  gc2cc
      USE mo_search_ll_grid, ONLY: find_reg_lonlat_grid_element_index

      ! USE global data fields (coordinates)
      USE mo_target_grid_data, ONLY: lon_geo, & !< longitude coordinates of the COSMO grid in the geographical system 
        &                            lat_geo !< latitude coordinates of the COSMO grid in the geographical system
      USE mo_target_grid_data, ONLY: tg !< structure with target grid description

      ! "input" fields
      USE mo_aot_data, ONLY : lon_aot, &
        &                     lat_aot, &
        &                     aot_data, &
        &                     aot_grid
 
      !-------------------------------------------------------------------------------------
      ! list of modules and fields for "output"

      USE mo_aot_target_fields, ONLY: aot_tg

                                  
      INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
      INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns
      INTEGER (KIND=i8), INTENT(IN) :: ntype !< number of types of aerosols
      INTEGER (KIND=i8), INTENT(IN) :: ntime !< number of times
      REAL (KIND=wp) :: undefined            !< undef value
 
      INTEGER (KIND=i8) :: undefined_integer ! undef value
      REAL (KIND=wp)    :: default_real
 
      REAL (KIND=wp) :: deg2rad ! degree to radian

      INTEGER :: i,j,k,l ! counters
      INTEGER (KIND=i8) :: ie, je, ke  ! indices for target grid elements

      INTEGER :: idom  ! counter
      INTEGER :: n_dom ! number of Icon domains

      INTEGER :: nlon

      REAL(KIND=wp)   :: point_lon, point_lat
      TYPE(geographical_coordinates) :: target_geo_co  !< structure for geographical coordinates of raw data pixel
      TYPE(cartesian_coordinates)  :: target_cc_co     !< coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined
      INTEGER (KIND=i8) :: start_cell_id = 1 !< start cell id
      LOGICAL :: l_child_dom     ! logical switch if child domain exists
      INTEGER :: child_dom_id   ! id of child domain
 
      INTEGER        :: k_error     ! error return code

      REAL (KIND=wp) :: bound_north_cosmo !< northern boundary for COSMO target domain
      REAL (KIND=wp) :: bound_south_cosmo !< southern boundary for COSMO target domain

      REAL (KIND=wp) :: point_lon_geo       !< longitude coordinate in geographical system of input point 
      REAL (KIND=wp) :: point_lat_geo       !< latitude coordinate in geographical system of input point
      
       
      INTEGER (KIND=i8) :: western_column     !< the index of the western_column of raw data 
      INTEGER (KIND=i8) :: eastern_column     !< the index of the eastern_column of raw data 
      INTEGER (KIND=i8) :: northern_row       !< the index of the northern_row of raw data 
      INTEGER (KIND=i8) :: southern_row       !< the index of the southern_row of raw data 

      INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid

      ! global data flag
      LOGICAL :: gldata=.TRUE. ! AOT data are global


      REAL (KIND=wp) :: bwlon !< weight for bilinear interpolation
      REAL (KIND=wp) :: bwlat !< weight for bilinear interpolation

      REAL (KIND=wp) :: data_array_sw(ntime,ntype) !< data array values at south-western point
      REAL (KIND=wp) :: data_array_se(ntime,ntype) !< data array values at south-eastern point
      REAL (KIND=wp) :: data_array_ne(ntime,ntype) !< data array values at north-eastern point
      REAL (KIND=wp) :: data_array_nw(ntime,ntype) !< data array values at north-western point
      REAL (KIND=wp) :: target_array_value(ntime,ntype) !< interpolated values


      PRINT *,'entering agg_aot_data_to_target_grid'

      target_array_value = -999.

      PRINT *,'MINVAL(lon_geo)', MINVAL(lon_geo)
      PRINT *,'MAXVAL(lon_geo)', MAXVAL(lon_geo)

      PRINT *,'MINVAL(lat_geo)', MINVAL(lat_geo)
      PRINT *,'MAXVAL(lat_geo)', MAXVAL(lat_geo)

      ! loop through all target grid elements
       DO i=1,tg%ie
       DO j=1,tg%je
       DO k=1,tg%ke

         point_lon_geo = lon_geo(i,j,k) 
         point_lat_geo = lat_geo(i,j,k)

         ! get four surrounding raw data indices
         !PRINT *,'CALL  get_4_surrounding_raw_data_indices'

         CALL  get_4_surrounding_raw_data_indices(   aot_grid, &
                                                   lon_aot,           &
                                                   lat_aot,           &
                                                   gldata,            &
                                                   point_lon_geo,      &
                                                   point_lat_geo,      &
                                                   western_column,     &
                                                   eastern_column,     &
                                                   northern_row,       &
                                                   southern_row)
         ! for the bilinear interpolation, use the four data pixels
         ! data(western_column,northern_row)
         ! data(western_column,southern_row)
         ! data(eastern_column,southern_row)
         ! data(eastern_column,northern_row)

          !PRINT *,'western_column: ', western_column
          !PRINT *,'eastern_column: ', eastern_column
          !PRINT *,'northern_row: ', northern_row
          !PRINT *,'southern_row: ', southern_row



         ! calculate weight for bilinear interpolation
         target_array_value = -999.
         if ((western_column /= 0) ) then  ! point is not out of data grid range

         CALL calc_weight_bilinear_interpol(point_lon_geo, &
                                        point_lat_geo, &
                                        lon_aot(western_column),      &
                                        lon_aot(eastern_column),      &
                                        lat_aot(northern_row),     &
                                        lat_aot(southern_row),     &
                                        bwlon,         &
                                        bwlat)
       ! the weights are bwlon and bwlat
       !  PRINT *,'bwlon, bwlat', bwlon,bwlat
       ! put all relevent data to an array



       data_array_sw(1:ntime,1:ntype) = aot_data(western_column,southern_row,1:ntime,1:ntype)  ! (ntime, ntype) for each grid point
       data_array_se(1:ntime,1:ntype) = aot_data(eastern_column,southern_row,1:ntime,1:ntype)
       data_array_ne(1:ntime,1:ntype) = aot_data(eastern_column,northern_row,1:ntime,1:ntype)
       data_array_nw(1:ntime,1:ntype) = aot_data(western_column,northern_row,1:ntime,1:ntype)

       !  print *,'data_array_sw(1,1): ', data_array_sw(1,1) 
       !  print *,'data_array_se(1,1): ', data_array_se(1,1)
       !  print *,'data_array_ne(1,1): ', data_array_ne(1,1)
       !  print *,'data_array_nw(1,1): ', data_array_nw(1,1) 


         ! perform the interpolation

         target_array_value = calc_value_bilinear_interpol(bwlon, bwlat, &
                                       data_array_sw, data_array_se, data_array_ne, data_array_nw)
        ENDIF
         !print *,'target_array_value(1,1): ', target_array_value(1,1)
         aot_tg(i,j,k,1:ntime,1:ntype) = target_array_value(1:ntime,1:ntype)

       ENDDO
       ENDDO
       ENDDO ! loop through all target grid elements


    END SUBROUTINE agg_aot_data_to_target_grid


END MODULE mo_agg_aot
