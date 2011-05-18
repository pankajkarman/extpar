!+ Fortran module to aggregate NDVI data to the target grid
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
!> Fortran module to aggregate NDVI data to the target grid
!> \author Hermann Asensio
MODULE mo_agg_ndvi

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i4
  USE mo_kind, ONLY: i8


  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar



  USE mo_grid_structures, ONLY: reg_lonlat_grid, &
    &                           rotated_lonlat_grid, &
    &                           target_grid_def, &
    &                           icosahedral_triangular_grid

  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme


  USE mo_search_ll_grid, ONLY: find_reg_lonlat_grid_element_index, &
    &                          find_rotated_lonlat_grid_element_index

  USE mo_search_target_grid, ONLY: find_nearest_target_grid_element

IMPLICIT NONE

PRIVATE
!
PUBLIC :: agg_ndvi_data_to_target_grid

    CONTAINS

    !> Subroutine to aggregate NDVI data to target grid
    SUBROUTINE agg_ndvi_data_to_target_grid(tg,undefined, path_ndvi_file)

       USE mo_ndvi_data, ONLY: ndvi_raw_data_grid, &
                               ndvi_field_row_mom, &
                               ndvi_field_row, &
                               lon_ndvi, &
                               lat_ndvi, &
                               ntime_ndvi
                               
       USE mo_ndvi_tg_fields, ONLY: ndvi_field, &
                                    ndvi_max, &
                                    ndvi_field_mom, &
                                    ndvi_ratio_mom

       USE mo_ndvi_routines, ONLY: open_netcdf_NDVI_data, &
                                   close_netcdf_NDVI_data, &
                                   get_one_row_NDVI_data, &
                                   get_pixel_NDVI_data

       USE mo_target_grid_data, ONLY: lon_geo, &
                                      lat_geo, &
                                      no_raw_data_pixel
        
    USE mo_gme_grid, ONLY: gme_grid
    USE mo_gme_grid, ONLY: sync_diamond_edge
    USE mo_gme_grid, ONLY: gme_real_field, gme_int_field
    USE mo_gme_grid, ONLY: cp_buf2gme, cp_gme2buf


       ! USE structure which contains the definition of the COSMO grid
       USE  mo_cosmo_grid, ONLY: COSMO_grid !< structure which contains the definition of the COSMO 

      ! USE icon domain structure wich contains the ICON coordinates (and parent-child indices etc)
      USE mo_icon_grid_data, ONLY:  icon_grid_region
      USE mo_icon_grid_data, ONLY:  icon_grid_level

      ! USE structure which contains the definition of the ICON grid
      USE  mo_icon_grid_data, ONLY: ICON_grid !< structure which contains the definition of the ICON grid

      USE mo_bilinterpol, ONLY: get_4_surrounding_raw_data_indices, &
                                calc_weight_bilinear_interpol, &
                                calc_value_bilinear_interpol





       IMPLICIT NONE

       TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description

       REAL (KIND=wp), INTENT(IN) :: undefined  !< undefined value

       CHARACTER (len=*), INTENT(in) :: path_ndvi_file         !< filename with path for NDVI raw data


      ! local variables

       INTEGER (KIND=i4)    :: time_index            !< the index of the time (month) to read in

       INTEGER :: ncid_ndvi

       REAL (KIND=wp) :: default_value

    INTEGER (KIND=i8) :: ie   !< index value for longitude
    INTEGER (KIND=i8)  :: je   !< index value for latitude
    INTEGER (KIND=i8)  :: ke   !< counter
    INTEGER (KIND=i8) :: i,j,k !< counter
    INTEGER :: row_index !< counter for NDVI data row
    INTEGER :: column_index !< counter for NDVI data column
    REAL (KIND=wp) :: northern_bound !< northern boundary for NDVI data to read for COSMO grid domain
    REAL (KIND=wp) :: southern_bound !< southern boundary for NDVI data to read for COSMO grid domain

    INTEGER :: northern_bound_index !< northern boundary for NDVI data to read for COSMO grid domain
    INTEGER :: southern_bound_index !< southern boundary for NDVI data to read for COSMO grid domain


     REAL (KIND=wp) ::  ndvi_sum(1:tg%ie,1:tg%je,1:tg%ke) !< field of target grid with sum of NDVI values

    INTEGER (KIND=i4) :: point_rot_lon_index          !< longitude index of point for rotated lon-lat grid
    INTEGER (KIND=i4) :: point_rot_lat_index          !< latitude index of point for rotated lon-lat grid

    
    INTEGER (KIND=i4) :: point_reg_lon_index          !< longitude index of point for regular lon-lat grid
    INTEGER (KIND=i4) :: point_reg_lat_index          !< latitude index of point for regular lon-lat grid

    INTEGER (KIND=i4) :: nlon_reg !< number of columns
    INTEGER (KIND=i4) :: nlat_reg !< number of rows

    REAL (KIND=wp) :: point_lon_geo       !< longitude coordinate in geographical system of input point 
    REAL (KIND=wp) :: point_lat_geo       !< latitude coordinate in geographical system of input point

    REAL(KIND=wp)   :: point_lon, point_lat
         
   INTEGER (KIND=i8) :: western_column     !< the index of the western_column of raw data 
   INTEGER (KIND=i8) :: eastern_column     !< the index of the eastern_column of raw data 
   INTEGER (KIND=i8) :: northern_row       !< the index of the northern_row of raw data 
   INTEGER (KIND=i8) :: southern_row       !< the index of the southern_row of raw data 

    REAL (KIND=wp) :: bwlon !< weight for bilinear interpolation
    REAL (KIND=wp) :: bwlat !< weight for bilinear interpolation

    REAL (KIND=wp)   :: ndvi_point_sw       !< value of the NDVI raw data pixel south west
    REAL (KIND=wp)   :: ndvi_point_se       !< value of the NDVI raw data pixel south east
    REAL (KIND=wp)   :: ndvi_point_ne       !< value of the NDVI raw data pixel north east
    REAL (KIND=wp)   :: ndvi_point_nw       !< value of the NDVI raw data pixel north west

    REAL (KIND=wp)   :: target_value


    ! matrix to save search results

    INTEGER (KIND=i8) :: map_ie(ndvi_raw_data_grid%nlon_reg, ndvi_raw_data_grid%nlat_reg)
    INTEGER (KIND=i8) :: map_je(ndvi_raw_data_grid%nlon_reg, ndvi_raw_data_grid%nlat_reg)
    INTEGER (KIND=i8) :: map_ke(ndvi_raw_data_grid%nlon_reg, ndvi_raw_data_grid%nlat_reg)

    ! global data flag
    LOGICAL :: gldata=.TRUE. ! NDVI data are global





    ndvi_field = undefined ! set cosmo_ndvi_field to undefined value at the start
    ndvi_field_mom = undefined
    default_value = 0.
   ! ndvi_field = default_value
   ! ndvi_field_mom = default_value
    
    !HA debug:
    print *,' ndvi_raw_data_grid: ', ndvi_raw_data_grid
    print *,' ndvi_raw_data_grid%start_lat_reg: ', ndvi_raw_data_grid%start_lat_reg
    print *,' ndvi_raw_data_grid%dlat_reg: ', ndvi_raw_data_grid%dlat_reg
    print *,' ndvi_raw_data_grid%nlat_reg: ', ndvi_raw_data_grid%nlat_reg
    
    ! determine northern and southern boundary for NDVI data to read for COSMO grid domain
    northern_bound =  MAXVAL(lat_geo) + 1.* ndvi_raw_data_grid%dlat_reg ! One row of NDVI data north of northern boundary of COSMO grid domain
    northern_bound = MIN(90._wp, northern_bound)                                 ! Check for the poles
   
   print *, 'northern_bound: ', northern_bound
   
    northern_bound_index = NINT((northern_bound - ndvi_raw_data_grid%start_lat_reg)/&
                                 ndvi_raw_data_grid%dlat_reg) +1  ! calculate index for regular lon-lat NDVI grid
  
  print *, 'northern_bound_index: ', northern_bound_index
  
  if (northern_bound_index < 1) then 
        northern_bound_index = 1 !< check for bounds
    else if (northern_bound_index > ndvi_raw_data_grid%nlat_reg) then
        northern_bound_index=ndvi_raw_data_grid%nlat_reg
    endif
    southern_bound = MINVAL(lat_geo) - 1.* ndvi_raw_data_grid%dlat_reg ! One row of NDVI data south of southern boundary of COSMO grid domain
    southern_bound = MAX(-90._wp, southern_bound)                               ! Check for the poles

    print *, 'southern_bound: ', southern_bound


    southern_bound_index = NINT((southern_bound - ndvi_raw_data_grid%start_lat_reg)/&
                                 ndvi_raw_data_grid%dlat_reg) +1  ! calculate index for regular lon-lat NDVI grid
    print *, 'southern_bound_index: ', southern_bound_index
    if (southern_bound_index < 1) then 
        southern_bound_index = 1 !< check for bounds
    else if (southern_bound_index > ndvi_raw_data_grid%nlat_reg) then
        southern_bound_index=ndvi_raw_data_grid%nlat_reg
    endif

    !HA debug:
    print *,' MAXVAL(lat_geo): ', MAXVAL(lat_geo)
    print *,' MINVAL(lat_geo): ', MINVAL(lat_geo)
    print *, 'northern_bound: ', northern_bound
    print *, 'southern_bound: ', southern_bound

    print *, 'northern_bound_index: ', northern_bound_index
    print *, 'southern_bound_index: ', southern_bound_index
    print *,'lon_ndvi(northern_bound_index): ', lat_ndvi(northern_bound_index)
    print *,'lon_ndvi(southern_bound_index): ', lat_ndvi(southern_bound_index)


    nlon_reg = ndvi_raw_data_grid%nlon_reg
    nlat_reg = ndvi_raw_data_grid%nlat_reg

   ! open netcdf file with NDVI data
    CALL open_netcdf_NDVI_data(path_ndvi_file, &
                               ncid_ndvi)

    ! read in NDVI data row by row and assign NDVI raw data pixel to COSMO grid
    ! start loop over NDVI raw data
    time_loop: DO time_index=1,12
    no_raw_data_pixel = 0 ! set count to 0
    ndvi_sum = 0.  ! set sum to 0

    data_rows: DO row_index=northern_bound_index,southern_bound_index
                    ! get NDVI raw data row
                    
                    CALL get_one_row_NDVI_data(ncid_ndvi,      &
                                nlon_reg,      &
                                nlat_reg,     &  
                                ntime_ndvi,     &
                                row_index,           &
                                time_index,         &
                                ndvi_field_row)



                    column: DO column_index=1, ndvi_raw_data_grid%nlon_reg

                       IF (time_index == 1) THEN
                          point_lon = lon_ndvi(column_index)
                          point_lat = lat_ndvi(row_index)
                          CALL find_nearest_target_grid_element( point_lon, &
                                              &      point_lat, &
                                              &      tg,        &
                                              &      ie,      &
                                              &      je,      &
                                              &      ke)

                          map_ie(column_index, row_index) = ie
                          map_je(column_index, row_index) = je
                          map_ke(column_index, row_index) = ke
                        ELSE
                          ie = map_ie(column_index, row_index)
                          je = map_je(column_index, row_index)
                          ke = map_ke(column_index, row_index)
                        ENDIF

                    IF ((ie /= 0).AND.(je/=0).AND.(ke/=0))THEN

                        no_raw_data_pixel(ie,je,ke) = no_raw_data_pixel(ie,je,ke) + 1                                 ! count raw data pixel within COSMO grid element
                        ndvi_sum(ie,je,ke)  = ndvi_sum(ie,je,ke) + ndvi_field_row(column_index) ! sum data values
                    ENDIF


                  ENDDO column

    END DO data_rows

     SELECT CASE(tg%igrid_type)
     CASE(igrid_gme)  ! in GME grid the diamond edges need to be synrchonized

       ! ndvi_sum
       CALL cp_buf2gme(tg,gme_grid,ndvi_sum,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,ndvi_sum)

      ! no_raw_data_pixel
      CALL cp_buf2gme(tg,gme_grid,no_raw_data_pixel,gme_int_field)
      CALL sync_diamond_edge(gme_grid, gme_int_field)
      CALL cp_gme2buf(tg,gme_grid,gme_int_field,no_raw_data_pixel)
     END SELECT





    !HA debug:
    print *,'MAXVAL(no_raw_data_pixel): ',MAXVAL(no_raw_data_pixel)
    print *,'MINVAL(no_raw_data_pixel): ',MINVAL(no_raw_data_pixel)

    !WHERE (no_raw_data_pixel > 0)
    !    ndvi_field = ndvi_sum / no_raw_data_pixel   ! calculate arithmetic mean
    !END WHERE

    print *,'tg: ',tg

    DO k=1, tg%ke
    DO j=1, tg%je
    DO i=1, tg%ie

    !print *,' i,j,k: ',i,j,k
    !print *,'no_raw_data_pixel(i,j,k): ', no_raw_data_pixel(i,j,k)

     IF (no_raw_data_pixel(i,j,k) /= 0) THEN 
       !-----------------------------------------------------------------------------------------------------------------------------------------
      !   PRINT *,'no_raw_data_pixel(i,j,k) /= 0'
      !   print *,'no_raw_data_pixel(i,j,k): ', no_raw_data_pixel(i,j,k)
      !   print *,'ndvi_sum(i,j,k): ', ndvi_sum(i,j,k)
      !   PRINT *,'SIZE(ndvi_field): ', SIZE(ndvi_field)
      !   PRINT *,'SHAPE(ndvi_field): ', SHAPE(ndvi_field)
      !   
      !   print *,'ndvi_field(i,j,k): ', ndvi_field(i,j,k)
          ndvi_field(i,j,k) = ndvi_sum(i,j,k) / REAL(no_raw_data_pixel(i,j,k))   ! calculate arithmetic mean
      !    PRINT *,' ndvi_field(i,j,k) = ndvi_sum(i,j,k) / REAL(no_raw_data_pixel(i,j,k))'
     ELSE ! bilinear interpolation
       !-----------------------------------------------------------------------------------------------------------------------------------------

       !  print *,'start bilinear interpolation'
         
         point_lon_geo = lon_geo(i,j,k) 
         point_lat_geo = lat_geo(i,j,k)

        ! get four surrounding raw data indices

         CALL  get_4_surrounding_raw_data_indices(   ndvi_raw_data_grid, &
                                                     lon_ndvi,           &
                                                     lat_ndvi,           &
                                                     gldata,             &
                                                     point_lon_geo,      &
                                                     point_lat_geo,      &
                                                     western_column,     &
                                                     eastern_column,     &
                                                     northern_row,       &
                                                     southern_row)
         !print *,'western_column, eastern_column, northern_row, southern_row'  
         !print *, western_column, eastern_column, northern_row, southern_row  

         IF ( (western_column /= 0) .AND. &
              (eastern_column /= 0) .AND. &
              (northern_row /= 0)   .AND. &
              (southern_row /= 0)        ) THEN


           ! get NDVI data for the pixel south west
           point_reg_lon_index = western_column
           point_reg_lat_index = southern_row
           CALL  get_pixel_NDVI_data( ncid_ndvi,           &
                                      nlon_reg,           & 
                                      nlat_reg,           &
                                      ntime_ndvi,          &
                                      point_reg_lon_index,     &
                                      point_reg_lat_index,     &
                                      time_index,          &
                                      ndvi_point_sw)


           ! get NDVI data for the pixel south east
           point_reg_lon_index = eastern_column
           point_reg_lat_index = southern_row
           CALL  get_pixel_NDVI_data( ncid_ndvi,           &
                                      nlon_reg,           & 
                                      nlat_reg,           &
                                      ntime_ndvi,          &
                                      point_reg_lon_index,     &
                                      point_reg_lat_index,     &
                                      time_index,          &
                                      ndvi_point_se)

           ! get NDVI data for the pixel north east
           point_reg_lon_index = eastern_column
           point_reg_lat_index = northern_row
           CALL  get_pixel_NDVI_data( ncid_ndvi,           &
                                      nlon_reg,           & 
                                      nlat_reg,           &
                                      ntime_ndvi,          &
                                      point_reg_lon_index,     &
                                      point_reg_lat_index,     &
                                      time_index,          &
                                      ndvi_point_ne)


           ! get NDVI data for the pixel north west
           point_reg_lon_index = western_column
           point_reg_lat_index = northern_row
           CALL  get_pixel_NDVI_data( ncid_ndvi,           &
                                      nlon_reg,           & 
                                      nlat_reg,           &
                                      ntime_ndvi,          &
                                      point_reg_lon_index,     &
                                      point_reg_lat_index,     &
                                      time_index,          &
                                      ndvi_point_nw)



         ! calculate weight for bilinear interpolation

         CALL calc_weight_bilinear_interpol(point_lon_geo, &
                                            point_lat_geo, &
                                            lon_ndvi(western_column),      &
                                            lon_ndvi(eastern_column),      &
                                            lat_ndvi(northern_row),     &
                                            lat_ndvi(southern_row),     &
                                            bwlon,         &
                                            bwlat)
         ! the weights are bwlon and bwlat
       !  PRINT *,'bwlon, bwlat', bwlon,bwlat

        ! perform the interpolation

         target_value = calc_value_bilinear_interpol(bwlon, bwlat, &
                                           ndvi_point_sw, ndvi_point_se, ndvi_point_ne, ndvi_point_nw)


         ndvi_field(i,j,k) = target_value
       ELSE ! grid element outside target grid
         ndvi_field(i,j,k) = default_value
       ENDIF

       !-----------------------------------------------------------------------------------------------------------------------------------------
       ENDIF

     ENDDO
     ENDDO
     ENDDO

     print *,'ndvi_field determined'
     print *,'time_index:', time_index

    ndvi_field_mom(:,:,:,time_index) = ndvi_field(:,:,:)

    END DO time_loop

    CALL  close_netcdf_NDVI_data(ncid_ndvi)


      ! calculate NDVI Max and NDVI ratio

     ndvi_max = MAXVAL(ndvi_field_mom,4) ! maximum for the "time" dimension

     ndvi_ratio_mom = default_value

     DO k=1, tg%ke
     DO j=1, tg%je
     DO i=1, tg%ie
          IF(ndvi_max(i,j,k) /= 0.) THEN
            DO time_index=1,12
            ndvi_ratio_mom(i,j,k,time_index) = ndvi_field_mom(i,j,k,time_index) / ndvi_max(i,j,k)
            ENDDO
          ENDIF
     ENDDO
     ENDDO
     ENDDO

   ! print *,' subroutine aggregate_ndvi_raw_data_to_cosmo_grid: MAXVAL(cosmo_ndvi_field): ',MAXVAL(cosmo_ndvi_field)


       END SUBROUTINE agg_ndvi_data_to_target_grid


END MODULE mo_agg_ndvi
