!+ Fortran module to aggregate ecoclimap land use data to a target grid
!
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_6         2011/04/19 Gerhard Smiatek
!  Initial release
! V2_0         2013/01/25 Guenther Zaengl 
!   Parallel threads for ICON and COSMO using Open-MP, 
!   Several bug fixes and optimizations for ICON search algorithm, 
!   particularly for the special case of non-contiguous domains; 
!   simplified namelist control for ICON  
!             2013/06/04 Martina Messmer
!   adaptations to the use of tiles   
! V2_0_3       2014/09/17 Burkhardt Rockel
!  Added use of directory information to access raw data files
!
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module to aggregate ecoclimap land use data to a target grid
!!
!> \author Hermann Asensio
! ecoclimap option (gs_09.03.12)
MODULE mo_agg_ecoclimap

  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4

  USE mo_grid_structures,       ONLY: igrid_icon, igrid_cosmo, &
       &                              target_grid_def
                                
  USE mo_search_ll_grid,        ONLY: find_reg_lonlat_grid_element_index
  USE mo_io_units,              ONLY: filename_max
  USE mo_io_utilities,          ONLY: check_netcdf

  USE mo_search_target_grid,    ONLY: find_nearest_target_grid_element

  USE netcdf,                   ONLY:   &
       &                              nf90_open,              &
       &                              nf90_close,             &
       &                              nf90_inq_varid,         &
       &                              nf90_get_var,           &
       &                              NF90_NOWRITE

  USE mo_ecoclimap_data,        ONLY: ecoclimap_grid, &
       &                                 lon_ecoclimap,  &
       &                                 ntime_ecoclimap, &
       &                                 lat_ecoclimap

  USE mo_ecoclimap_lookup_tables, ONLY: name_lookup_table_ecoclimap, &
       &                                init_ecoclimap_lookup_tables, &
       &                                get_name_ecoclimap_lookup_tables, &
       &                                z012_lt_ecoclimap, lnz012_lt_ecoclimap, plc12_lt_ecoclimap, &
       &                                lai12_lt_ecoclimap, rd_lt_ecoclimap, &
       &                                emiss12_lt_ecoclimap, rs_min_lt_ecoclimap, &
       &                                ecoclimap_look_up, &
       &                                forest_type_ecoclimap

  USE mo_math_constants,        ONLY: deg2rad
  USE mo_physical_constants,    ONLY: re
  USE mo_target_grid_data,      ONLY: lon_geo, & !< longitude coordinates of the COSMO grid in the geographical system
       &                              lat_geo, &  !< latitude coordinates of the COSMO grid in the geographical system
       &                              search_res !< resolution of ICON grid search index list

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: agg_ecoclimap_data_to_target_grid

  REAL(KIND=wp), PARAMETER :: rs_min_undef=999. !< undefined value for minimal stomata resistance

  CONTAINS

  !> Subroutine to aggregate ecoclimap data to the target grid
  SUBROUTINE agg_ecoclimap_data_to_target_grid(raw_data_lu_path, & !_br 17.09.14
       &                                        ecoclimap_file,        & !_br 17.09.14
       &                                        ilookup_table_ecoclimap, &
       &                                        undefined,               &
       &                                        tg,                      &
       &                                        nclass_ecoclimap,        &
       &                                        ecoclimap_class_fraction, &
       &                                        ecoclimap_class_npixel, &
       &                                        ecoclimap_tot_npixel,   &
       &                                        fr_land_ecoclimap ,     &
       &                                        ice_ecoclimap,          &
       &                                        z012_ecoclimap,      &
       &                                        root_ecoclimap,       &
       &                                        plcov12_ecoclimap, &
       &                                        lai12_ecoclimap,   &
       &                                        rs_min_ecoclimap, &
       &                                        urban_ecoclimap,  &
       &                                        for_d_ecoclimap,  &
       &                                        for_e_ecoclimap, &
       &                                        emissivity_ecoclimap )



    TYPE(target_grid_def), INTENT(IN)        :: tg  !< structure with target grid description
    CHARACTER (LEN=*), INTENT(IN)            :: ecoclimap_file(:)  !< filename ecoclimap raw data
    INTEGER(KIND=i4), INTENT(IN)             :: ilookup_table_ecoclimap, & 
         &                                      nclass_ecoclimap
    REAL (KIND=wp), INTENT(IN)               :: undefined            !< undef value

    INTEGER (KIND=i4), INTENT(OUT)           :: ecoclimap_class_npixel(:,:,:,:), &  
         &                                      ecoclimap_tot_npixel(:,:,:)  


    REAL (KIND=wp), INTENT(OUT)              :: ecoclimap_class_fraction(:,:,:,:), &   
         &                                      fr_land_ecoclimap(:,:,:), &  !< fraction land due to ecoclimap raw data
         &                                      ice_ecoclimap(:,:,:), &      !< fraction of ice due to ecoclimap raw data
         &                                      z012_ecoclimap(:,:,:,:), &       !< roughness length due to ecoclimap land use data
         &                                      root_ecoclimap(:,:,:), &     !< root depth due to ecoclimap land use data
         &                                      plcov12_ecoclimap(:,:,:,:), & !< plant cover maximum due to ecoclimap land use data
         &                                      lai12_ecoclimap(:,:,:,:), &   !< Leaf Area Index maximum due to ecoclimap land use data
         &                                      rs_min_ecoclimap(:,:,:), &   !< minimal stomata resistance due to ecoclimap land use data
         &                                      urban_ecoclimap(:,:,:), &    !< urban fraction due to ecoclimap land use data
         &                                      for_d_ecoclimap(:,:,:), &    !< deciduous forest (fraction) due to ecoclimap land use data
         &                                      for_e_ecoclimap(:,:,:), &    !< evergreen forest (fraction) due to ecoclimap land use data
         &                                      emissivity_ecoclimap(:,:,:) !< longwave emissivity due to ecoclimap land use da


    !local variables
    CHARACTER (LEN=filename_max)            :: raw_data_lu_path        !< path to raw data !_br 17.09.14
    CHARACTER (LEN=80) :: varname  !< name of variable

    INTEGER (KIND=i4)                       :: undefined_integer, &  ! undef value
         &                                     i_lu, j_lu, & 
         &                                     ie, je, ke, &   ! indices for target grid elements
         &                                     start_cell_id, &  !< ID of starting cell for ICON search
         &                                     i1, i2, & 
         &                                     k,l, &  ! counters
         &                                     i_col, j_row, &  ! counter
         &                                     ecoclimap_data_row(ecoclimap_grid%nlon_reg), & 
         &                                     ecoclimap_data_pixel(1:1,1:1), & 
         &                                     lu, &   ! land use class
         &                                     nclass, &  ! index in array of ecoclimap tables
         &                                     ncid_ecoclimap, &                              !< netcdf unit file number
         &                                     varid_ecoclimap, &                !< id of variable
         &                                     nlon, & 
         &                                     k_error     ! error return code

    INTEGER (KIND=i4), ALLOCATABLE          :: ie_vec(:), je_vec(:), ke_vec(:)  ! indices for target grid elements

    REAL (KIND=wp)                          :: default_real, & 
         &                                     a_weight(1:tg%ie,1:tg%je,1:tg%ke), &  !< area weight of all raw data pixels in target grid
         &                                     a_class(1:tg%ie,1:tg%je,1:tg%ke,1:nclass_ecoclimap), &  
         &                                     apix, &       !< area of a raw data pixel
         &                                     apix_e, &       !< area of a raw data pixel at equator
         &                                      point_lon, point_lat, & 
         &                                     pland, &           !< land cover                      (-)
         &                                     pice, &            !< ice fraction                    (-)
         &                                     plnz0(ntime_ecoclimap), &           !< logarithm of roughness length   (m)
         &                                     proot, &           !< root depth                      (m)
         &                                     p12(ntime_ecoclimap), &             !<  plant cover             (-)
         &                                     plai12(ntime_ecoclimap), &          !<  leaf area index         (m**2/m**2)
         &                                     purb, &            !< urbanisation                    (-)
         &                                     pfor_d, &          !< deciduous forest                (-)
         &                                     pfor_e, &          !< evergreen forest                (-)
         &                                     pemissivity12(ntime_ecoclimap), &     !< surface thermal emissivity      (-)
         &                                     prs_min, &         !< minimum stomata resistance      (s/m)
         &                                     hp, &  ! height of Prandtl-layer
         &                                     lnhp, & 
         &                                     pwz0(ntime_ecoclimap), &  ! weighted summand for z0
         &                                     area_tot, &    ! total area
         &                                     area_land, &   ! area with land
         &                                     area_plcov(ntime_ecoclimap), &  ! area covered with plants
         &                                     bound_north_cosmo, &  !< northern boundary for COSMO target domain
         &                                     bound_south_cosmo, &  !< southern boundary for COSMO target domain
         &                                     bound_west_cosmo, &   !< western  boundary for COSMO target domain
         &                                     bound_east_cosmo  !< eastern  boundary for COSMO target domain

    ! Some stuff for OpenMP parallelization
    INTEGER(KIND=i4)                       :: num_blocks, ib, il, blk_len, istartlon, iendlon, nlon_sub, ishift
    !$   INTEGER :: omp_get_max_threads, omp_get_thread_num, thread_id
    !$   INTEGER (KIND=i4), ALLOCATABLE :: start_cell_arr(:)

    CALL logging%info('Enter routine: agg_ecoclimap_data_to_target_grid')

    apix_e  = re * re * deg2rad* ABS(ecoclimap_grid%dlon_reg) * deg2rad * ABS(ecoclimap_grid%dlat_reg)
    ! area of ecoclimap raw data pixel at equator

    hp   = 30.0      ! height of Prandtl-layer
    lnhp = LOG(hp)

    default_real = 0.0
    undefined_integer= NINT(undefined)

    ecoclimap_class_fraction = default_real
    ecoclimap_class_npixel   = undefined_integer
    ecoclimap_tot_npixel = undefined_integer

    a_weight = default_real
    a_class  = default_real

    SELECT CASE(tg%igrid_type)
      CASE(igrid_icon)  ! ICON GRID
        ke = 1
      CASE(igrid_cosmo)  ! COSMO GRID
        ke = 1
        bound_north_cosmo = MAXVAL(lat_geo) + 0.05_wp  ! add some "buffer"
        bound_north_cosmo = MIN(bound_north_cosmo,90._wp)
        bound_south_cosmo = MINVAL(lat_geo) - 0.05_wp  ! add some "buffer"
        bound_south_cosmo = MAX(bound_south_cosmo,-90._wp)

        bound_east_cosmo = MAXVAL(lon_geo) + 0.25_wp  ! add some "buffer"
        bound_east_cosmo = MIN(bound_east_cosmo,180.0_wp)
        bound_west_cosmo = MINVAL(lon_geo) - 0.25_wp  ! add some "buffer"
        bound_west_cosmo = MAX(bound_west_cosmo,-180.0_wp)
    END SELECT

    ! init lookup tables
    CALL init_ecoclimap_lookup_tables(raw_data_lu_path, & !_br 17.09.14
         &      nclass_ecoclimap, &   !_br 17.09.14
         &      z012_lt_ecoclimap,            &
         &      lnz012_lt_ecoclimap,          &
         &      plc12_lt_ecoclimap,        &
         &      lai12_lt_ecoclimap,        &
         &      rd_lt_ecoclimap,            &
         &      emiss12_lt_ecoclimap,         &
         &      rs_min_lt_ecoclimap,        &
         &      forest_type_ecoclimap)

    CALL get_name_ecoclimap_lookup_tables(ilookup_table_ecoclimap, name_lookup_table_ecoclimap)
    ! open netcdf file
    CALL check_netcdf( nf90_open(TRIM(ecoclimap_file(1)),NF90_NOWRITE, ncid_ecoclimap))

    varname = 'landuse' ! I know that the ecoclimap data are stored in a variable called 'Band1'

    CALL check_netcdf( nf90_inq_varid(ncid_ecoclimap, TRIM(varname), varid_ecoclimap))
    nlon = ecoclimap_grid%nlon_reg
    ALLOCATE(ie_vec(nlon),je_vec(nlon),ke_vec(nlon))
    ie_vec(:) = 0
    je_vec(:) = 0
    ke_vec(:) = 0
    start_cell_id = 1

    ! Determine start and end longitude of search
    istartlon = 1
    iendlon = ecoclimap_grid%nlon_reg
    IF (tg%igrid_type == igrid_icon) THEN
      DO i_col = 1, ecoclimap_grid%nlon_reg
        point_lon = lon_ecoclimap(i_col)
        IF (point_lon < tg%minlon) istartlon = i_col + 1
        IF (point_lon > tg%maxlon) THEN
          iendlon = i_col - 1
          EXIT
        ENDIF
      ENDDO
    ELSE IF (tg%igrid_type == igrid_cosmo) THEN
      DO i_col = 1, ecoclimap_grid%nlon_reg
        point_lon = lon_ecoclimap(i_col)
        IF (point_lon < bound_west_cosmo) istartlon = i_col + 1
        IF (point_lon > bound_east_cosmo) THEN
          iendlon = i_col - 1
          EXIT
        ENDIF
      ENDDO
    ENDIF
    nlon_sub = iendlon - istartlon + 1

    num_blocks = 1
    !$   num_blocks = omp_get_max_threads()
    IF (MOD(nlon_sub,num_blocks)== 0) THEN
      blk_len = nlon_sub/num_blocks
    ELSE
      blk_len = nlon_sub/num_blocks + 1
    ENDIF
    !$   ALLOCATE(start_cell_arr(num_blocks))
    !$   start_cell_arr(:) = 1

    WRITE(message_text,*) 'nlon_sub: ',nlon_sub,' num_blocks: ',num_blocks, ' blk_len: ',blk_len
    CALL logging%info(message_text)

    CALL logging%info('Start loop over ecoclimap rows...')

    ! loop over rows of ecoclimap dataset
    rows: DO j_row=1,ecoclimap_grid%nlat_reg
      !rows: DO j_row=1,100
      point_lat = lat_ecoclimap(j_row)

      IF (tg%igrid_type == igrid_cosmo) THEN ! CASE COSMO grid, save some I/O from hard disk if you are out or the target domain
        IF ((point_lat > bound_north_cosmo).OR.(point_lat < bound_south_cosmo) ) THEN ! raw data out of target grid
          CYCLE rows
        ENDIF
      ELSE IF (tg%igrid_type == igrid_icon) THEN 
        IF (point_lat > tg%maxlat .OR. point_lat < tg%minlat) THEN
          CYCLE rows
        ENDIF
      ENDIF ! COSMO grid

      ! read in pixels
      CALL check_netcdf(nf90_get_var(ncid_ecoclimap, varid_ecoclimap,  ecoclimap_data_row,  &
           &               start=(/1,j_row/),count=(/nlon,1/)))


      apix = apix_e * COS(point_lat * deg2rad) ! area of raw data pixel (in [m**2])
      ie_vec(istartlon:iendlon) = 0
      IF (tg%igrid_type /= igrid_icon) THEN
        je_vec(:) = 0
        ke_vec(:) = 0
      ENDIF

!$OMP PARALLEL DO PRIVATE(ib,il,i_col,i1,i2,ishift,point_lon,thread_id,start_cell_id)
      DO ib = 1, num_blocks

        !$     thread_id = omp_get_thread_num()+1
        !$     start_cell_id = start_cell_arr(thread_id)
        ishift = istartlon-1+(ib-1)*blk_len

        columns1: DO il = 1,blk_len
          i_col = ishift+il
          IF (i_col > iendlon) CYCLE columns1

          ! find the corresponding target grid indices
          point_lon = lon_ecoclimap(i_col)

          ! Reset start cell when entering a new row or when the previous data point was outside
          ! the model domain
          IF (tg%igrid_type == igrid_icon .AND. (il == 1 .OR. start_cell_id == 0)) THEN
            i1 = NINT(point_lon*search_res)
            i2 = NINT(point_lat*search_res)
            start_cell_id = tg%search_index(i1,i2)
            IF (start_cell_id == 0) EXIT ! in this case, the whole row is empty; may happen with merged (non-contiguous) domains
          ENDIF

          CALL  find_nearest_target_grid_element( point_lon,     &
               &      point_lat,     &
               &      tg,            &
               &      start_cell_id, &
               &      ie_vec(i_col), &
               &      je_vec(i_col), &
               &      ke_vec(i_col)  )

        ENDDO columns1
        !$     start_cell_arr(thread_id) = start_cell_id
      ENDDO
!$OMP END PARALLEL DO

      columns2: DO i_col=istartlon,iendlon

        ie = ie_vec(i_col)
        je = je_vec(i_col)
        ke = ke_vec(i_col)

        IF ((ie /= 0).AND.(je/=0).AND.(ke/=0))THEN
          ! raw data pixel within target grid, see output of routine find_rotated_lonlat_grid_element_index
          !- summation of variables
          ecoclimap_tot_npixel(ie,je,ke) = ecoclimap_tot_npixel(ie,je,ke) + 1
          a_weight(ie,je,ke) = a_weight(ie,je,ke) + apix  ! sum up area for weight
          lu = ecoclimap_data_row(i_col)                        ! land use class

          if (lu < 0) lu = 256 + lu 

          CALL ecoclimap_look_up(lu, &
               &      nclass_ecoclimap, &
               &      plc12_lt_ecoclimap,        &
               &      lai12_lt_ecoclimap,        &
               &      rd_lt_ecoclimap,            &
               &      emiss12_lt_ecoclimap,         &
               &      rs_min_lt_ecoclimap,        &
               &      forest_type_ecoclimap,      &
               &      pland,          &
               &      pice,           &
               &      plnz0,          &
               &      proot,          &
               &      p12,            &
               &      plai12,         &
               &      purb,           &
               &      pfor_d,         &
               &      pfor_e,         &
               &      pemissivity12,    &
               &      prs_min,        &
               &      k_error)




          IF (k_error == 0) THEN ! valid land use class

            nclass = lu  

            ecoclimap_class_npixel(ie,je,ke,nclass) = ecoclimap_class_npixel(ie,je,ke,nclass) + 1
            a_class(ie,je,ke,nclass) = a_class(ie,je,ke,nclass) + apix   ! sum area of valid land use pixels
            IF (pland >  0.0) THEN ! only for land pixel

              ! weighted with whole area
              fr_land_ecoclimap(ie,je,ke) = fr_land_ecoclimap(ie,je,ke) + apix * pland
              ice_ecoclimap(ie,je,ke) = ice_ecoclimap(ie,je,ke) + apix * pice
              urban_ecoclimap(ie,je,ke) = urban_ecoclimap(ie,je,ke) + apix * purb

              ! z0 is averaged logarithmic

              DO k = 1, ntime_ecoclimap 
                IF ( lnhp /= plnz0(k)) THEN ! z0 is averaged logarithmic
                  pwz0(k) = 1./(lnhp - plnz0(k))
                ELSE
                  pwz0(k) = 0.
                ENDIF
              ENDDO

              ! the following fields are weighted with the plant cover
              root_ecoclimap(ie,je,ke) = root_ecoclimap(ie,je,ke) + apix * p12(1) * proot


              DO k = 1, ntime_ecoclimap 
                z012_ecoclimap(ie,je,ke,k)    = z012_ecoclimap(ie,je,ke,k) + apix * pwz0(k)
                plcov12_ecoclimap(ie,je,ke,k) = plcov12_ecoclimap(ie,je,ke,k) + apix * p12(k)
                lai12_ecoclimap(ie,je,ke,k)   = lai12_ecoclimap(ie,je,ke,k) + apix * p12(k) * plai12(k)
              END DO
              emissivity_ecoclimap(ie,je,ke) =  emissivity_ecoclimap(ie,je,ke) + apix * &   !_br 21.02.14 splitted too long line
                   SUM(pemissivity12(1:ntime_ecoclimap))/REAL(ntime_ecoclimap,wp) !_br 21.02.14
              rs_min_ecoclimap(ie,je,ke) = rs_min_ecoclimap(ie,je,ke) + apix * p12(1) * prs_min
              for_d_ecoclimap(ie,je,ke) = for_d_ecoclimap(ie,je,ke) + apix * p12(1) * pfor_d
              for_e_ecoclimap(ie,je,ke) = for_e_ecoclimap(ie,je,ke) + apix * p12(1) * pfor_e

            END IF

          ENDIF
        ENDIF

        ! end loops
      ENDDO columns2
    ENDDO rows

    DEALLOCATE(ie_vec,je_vec,ke_vec)
    !$  DEALLOCATE(start_cell_arr)

    CALL logging%info('...done')

    ! calculate ecoclimap_class_fraction (ecoclimap_class_fraction/ecoclimap_class_npixel)
    DO ke=1, tg%ke
      DO je=1, tg%je
        DO ie=1, tg%ie
          area_tot = a_weight(ie,je,ke)
          area_land = fr_land_ecoclimap(ie,je,ke)

          ! weight by total area
          IF (area_tot > 0.0) THEN
            DO l=1,nclass_ecoclimap
              ecoclimap_class_fraction(ie,je,ke,l) =  a_class(ie,je,ke,l) /  area_tot
              ! area fraction of each land use class
            ENDDO
          ENDIF

          IF (area_land > 0.0 ) THEN

            area_plcov(:) = plcov12_ecoclimap(ie,je,ke,:) ! area covered with plants
            !area_plcov(:) = MAX(area_plcov(:), eps)      ! area covered with plants

            DO k = 1, ntime_ecoclimap 
              z012_ecoclimap(ie,je,ke,k) = z012_ecoclimap(ie,je,ke,k) / area_land
              z012_ecoclimap(ie,je,ke,k) = hp * EXP(-1./z012_ecoclimap(ie,je,ke,k))
            ENDDO
            ! weight by total area
            fr_land_ecoclimap(ie,je,ke) = fr_land_ecoclimap(ie,je,ke) / area_tot
            ice_ecoclimap(ie,je,ke)   = ice_ecoclimap(ie,je,ke)   / area_tot
            urban_ecoclimap(ie,je,ke) = urban_ecoclimap(ie,je,ke) / area_tot
            emissivity_ecoclimap(ie,je,ke) = emissivity_ecoclimap(ie,je,ke) / area_tot

            ! weight by land area
            for_d_ecoclimap(ie,je,ke) = for_d_ecoclimap(ie,je,ke) / area_land
            for_e_ecoclimap(ie,je,ke) = for_e_ecoclimap(ie,je,ke) / area_land
            root_ecoclimap(ie,je,ke) = root_ecoclimap(ie,je,ke) / area_land
            rs_min_ecoclimap(ie,je,ke) = rs_min_ecoclimap(ie,je,ke) / area_land

            DO k = 1, ntime_ecoclimap 
              plcov12_ecoclimap(ie,je,ke,k) = plcov12_ecoclimap(ie,je,ke,k) / area_land
            ENDDO
            ! weight by area covered with plants
            ! here wee need to see each month  correct root and rs_min 
            DO k = 1, ntime_ecoclimap                     
              IF (area_plcov(k) > 0.0) THEN
                lai12_ecoclimap(ie,je,ke,k) = lai12_ecoclimap(ie,je,ke,k) / area_plcov(k)
              ELSE ! may occur for ice grid elements
                lai12_ecoclimap(ie,je,ke,k) = undefined
              ENDIF
            ENDDO
          ELSE IF ( area_tot > 0.0) THEN ! only sea pixels were found
            fr_land_ecoclimap(ie,je,ke) = undefined
            z012_ecoclimap(ie,je,ke,:)  = undefined
            ice_ecoclimap(ie,je,ke)     = undefined
            urban_ecoclimap(ie,je,ke)   = undefined
            for_d_ecoclimap(ie,je,ke)   = undefined
            for_e_ecoclimap(ie,je,ke)   = undefined
            plcov12_ecoclimap(ie,je,ke,:)= undefined
            root_ecoclimap(ie,je,ke)    = undefined
            lai12_ecoclimap(ie,je,ke,:)  = undefined
            rs_min_ecoclimap(ie,je,ke)  = undefined

          ENDIF
        ENDDO
      ENDDO
    ENDDO ! ke

    DO ke=1, tg%ke
      DO je=1, tg%je
        DO ie=1, tg%ie
          IF (ecoclimap_tot_npixel(ie,je,ke) == 0) THEN ! nearest neighbor search

            point_lon = lon_geo(ie,je,ke)
            point_lat = lat_geo(ie,je,ke)
            CALL find_reg_lonlat_grid_element_index(point_lon,      &
                 &                                     point_lat,      &
                 &                                     ecoclimap_grid,  &
                 &                                     i_lu,    &
                 &                                     j_lu)
            ! get data
            IF ((i_lu /= 0).AND.(j_lu/=0))THEN

              i_col = i_lu
              j_row = j_lu
              CALL check_netcdf(nf90_get_var(ncid_ecoclimap, varid_ecoclimap,  ecoclimap_data_pixel,  &
                   &               start=(/ i_col,j_row /),count=(/ 1,1 /)))

              lu = ecoclimap_data_pixel(1,1)

              if (lu < 0) lu = 256 + lu

              CALL ecoclimap_look_up(lu, &
                   &      nclass_ecoclimap, &
                   &      plc12_lt_ecoclimap,        &
                   &      lai12_lt_ecoclimap,        &
                   &      rd_lt_ecoclimap,            &
                   &      emiss12_lt_ecoclimap,         &
                   &      rs_min_lt_ecoclimap,        &
                   &      forest_type_ecoclimap,      &
                   &      pland,          &
                   &      pice,           &
                   &      plnz0,          &
                   &      proot,          &
                   &      p12,            &
                   &      plai12,         &
                   &      purb,           &
                   &      pfor_d,         &
                   &      pfor_e,         &
                   &      pemissivity12,    &
                   &      prs_min,        &
                   &      k_error)
            ELSE
              lu = 0
              k_error = 1
            ENDIF

            IF (k_error == 0) THEN ! valid land use class
              apix = apix_e * COS(point_lat * deg2rad) ! area of raw data pixel (in [m**2])

              nclass = lu


              ecoclimap_class_npixel(ie,je,ke,nclass) = ecoclimap_class_npixel(ie,je,ke,nclass) + 1
              a_class(ie,je,ke,nclass) = a_class(ie,je,ke,nclass) + apix   ! sum area of valid land use pixels
              IF (pland >  0.0) THEN ! only for land pixel
                fr_land_ecoclimap(ie,je,ke) = pland
                ice_ecoclimap(ie,je,ke) =  pice
                urban_ecoclimap(ie,je,ke) = purb
                DO k = 1, 12
                  IF ( lnhp /= plnz0(k)) THEN ! log z0
                    pwz0(k)= 1./(lnhp - plnz0(k))
                  ELSE
                    pwz0(k) = 0.
                  ENDIF
                END DO
                DO k = 1, ntime_ecoclimap 
                  z012_ecoclimap(ie,je,ke,k)      =  pwz0(k)
                  z012_ecoclimap(ie,je,ke,k) = hp * EXP(-1./z012_ecoclimap(ie,je,ke,k))
                  plcov12_ecoclimap(ie,je,ke,k) = p12(k)
                  lai12_ecoclimap(ie,je,ke,k) = plai12(k)
                END DO
                emissivity_ecoclimap(ie,je,ke) =  SUM(pemissivity12(1:ntime_ecoclimap)) / REAL(ntime_ecoclimap,wp) 
                root_ecoclimap(ie,je,ke) = proot
                rs_min_ecoclimap(ie,je,ke) = prs_min
                for_d_ecoclimap(ie,je,ke) = pfor_d * p12(1)
                for_e_ecoclimap(ie,je,ke) = pfor_e * p12(1)
                IF (pice == 1.0) THEN  ! ice land use class
                  root_ecoclimap(ie,je,ke) = undefined
                  lai12_ecoclimap(ie,je,ke,:) = undefined
                  rs_min_ecoclimap(ie,je,ke) = rs_min_undef
                ENDIF
              ENDIF
            ELSE ! not a valid land use class
              fr_land_ecoclimap(ie,je,ke) = undefined
              z012_ecoclimap(ie,je,ke,:)      = undefined
              ice_ecoclimap(ie,je,ke)     = undefined
              urban_ecoclimap(ie,je,ke)   = undefined
              for_d_ecoclimap(ie,je,ke)   = undefined
              for_e_ecoclimap(ie,je,ke)   = undefined
              plcov12_ecoclimap(ie,je,ke,:)= undefined
              root_ecoclimap(ie,je,ke)    = undefined
              lai12_ecoclimap(ie,je,ke,:)  = undefined
              rs_min_ecoclimap(ie,je,ke)  = undefined
            ENDIF
          ENDIF ! nearest neighbour search
        ENDDO
      ENDDO
    ENDDO

    ! close netcdf file
    CALL check_netcdf( nf90_close(ncid_ecoclimap))

    CALL logging%info('Exit routine: agg_ecoclimap_data_to_target_grid')

  END SUBROUTINE agg_ecoclimap_data_to_target_grid


END MODULE mo_agg_ecoclimap
