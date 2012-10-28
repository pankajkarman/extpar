!+ Fortran module to aggregate GLC2000 land use data to a target grid 
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
! @VERSION@    @DATE@     Hermann Asensio
!  clean up
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module to aggregate GLC2000 land use data to a target grid 
!!
!> \author Hermann Asensio
MODULE mo_agg_glc2000

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar


  !> data type structures form module GRID_structures
  USE mo_grid_structures, ONLY: reg_lonlat_grid, &
    &                           rotated_lonlat_grid
  
  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme

  USE mo_search_ll_grid, ONLY: find_reg_lonlat_grid_element_index, &
    &                          find_rotated_lonlat_grid_element_index
  USE mo_io_units,          ONLY: filename_max
  USE mo_io_utilities, ONLY: check_netcdf

  USE mo_search_target_grid, ONLY: find_nearest_target_grid_element


  USE netcdf,      ONLY :   &
    & nf90_open,              &
    & nf90_close,             &
    & nf90_inq_varid,         &
    & nf90_get_var,           &
    & NF90_NOWRITE,           &
    & nf90_noerr,             &
    & nf90_strerror

  

  IMPLICIT NONE

  PRIVATE
  

  PUBLIC :: agg_glc2000_data_to_target_grid

  REAL(KIND=wp), PARAMETER :: rs_min_undef=999. !< undefined value for minimal stomata resistance


  CONTAINS

  !> Subroutine to aggregate glc2000 data to the target grid
  SUBROUTINE agg_glc2000_data_to_target_grid(glc2000_file,ilookup_table_glc2000,undefined,       &
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
    
    
    
    
    
  !-------------------------------------------------------------------------------------
  ! list of modules which are used as "input"
  USE mo_grid_structures, ONLY: target_grid_def   !< type definition of structure for tg

  ! raw data grid description, here for GLC2000 data
  USE mo_glc2000_data, ONLY: glc2000_grid, &
    &                          lon_glc2000,  &
    &                          lat_glc2000

  USE mo_glc2000_lookup_tables, ONLY: name_lookup_table_glc2000
  USE mo_glc2000_lookup_tables, ONLY: i_gme_lookup_table,    &
    &                                 i_cosmo_lookup_table,  &
    &                                 i_experimental_lookup_table
  USE mo_glc2000_lookup_tables, ONLY: init_glc2000_lookup_tables, &
    &                                 get_name_glc2000_lookup_tables
  USE mo_glc2000_lookup_tables, ONLY:   z0_lt_glc2000, lnz0_lt_glc2000, plc_mn_lt_glc2000, plc_mx_lt_glc2000, & 
    &               lai_mn_lt_glc2000, lai_mx_lt_glc2000, rd_lt_glc2000, emiss_lt_glc2000, rs_min_lt_glc2000   


  USE mo_glc2000_lookup_tables, ONLY: glc2000_look_up

    USE mo_gme_grid, ONLY: gme_grid
    USE mo_gme_grid, ONLY: sync_diamond_edge
    USE mo_gme_grid, ONLY: gme_real_field, gme_int_field
    USE mo_gme_grid, ONLY: cp_buf2gme, cp_gme2buf



    ! USE icon domain structure wich contains the ICON coordinates (and parent-child indices etc)
    !USE mo_icon_grid_data, ONLY:  icon_grid_region
    !USE mo_icon_grid_data, ONLY:  icon_grid_level

    ! USE structure which contains the definition of the ICON grid
    USE  mo_icon_grid_data, ONLY: ICON_grid !< structure which contains the definition of the ICON grid

    ! USE structure which contains the definition of the COSMO grid
    USE  mo_cosmo_grid, ONLY: COSMO_grid !< structure which contains the definition of the COSMO grid

    USE mo_math_constants, ONLY: pi, rad2deg, deg2rad, eps
    USE mo_physical_constants, ONLY: re
    ! USE global data fields (coordinates)
    USE mo_target_grid_data, ONLY: lon_geo, & !< longitude coordinates of the COSMO grid in the geographical system 
      &                            lat_geo !< latitude coordinates of the COSMO grid in the geographical system

     CHARACTER (LEN=filename_max), INTENT(IN) :: glc2000_file  !< filename glc2000 raw data
     INTEGER, INTENT(IN) :: ilookup_table_glc2000
     REAL (KIND=wp), INTENT(IN) :: undefined            !< undef value

     TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description
     INTEGER, INTENT(IN) :: nclass_glc2000 !< GLC2000 has 23 classes for the land use description
     REAL (KIND=wp), INTENT(OUT)  :: glc2000_class_fraction(:,:,:,:)  !< fraction for each glc2000 class on target grid (dimension (ie,je,ke,nclass_glc2000))

    INTEGER (KIND=i8), INTENT(OUT) :: glc2000_class_npixel(:,:,:,:) !< number of raw data pixels for each glc2000 class on target grid (dimension (ie,je,ke,nclass_glc2000))


    INTEGER (KIND=i8), INTENT(OUT) :: glc2000_tot_npixel(:,:,:)  !< total number of glc2000 raw data pixels on target grid (dimension (ie,je,ke))


    REAL (KIND=wp), INTENT(OUT)  :: fr_land_glc2000(:,:,:) !< fraction land due to glc2000 raw data
    REAL (KIND=wp), INTENT(OUT)  :: ice_glc2000(:,:,:)     !< fraction of ice due to glc2000 raw data
    REAL (KIND=wp), INTENT(OUT)  :: z0_glc2000(:,:,:)      !< roughness length due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: root_glc2000(:,:,:)    !< root depth due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: plcov_mx_glc2000(:,:,:)!< plant cover maximum due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: plcov_mn_glc2000(:,:,:)!< plant cover minimum due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: lai_mx_glc2000(:,:,:)  !< Leaf Area Index maximum due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: lai_mn_glc2000(:,:,:)  !< Leaf Area Index minimum due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: rs_min_glc2000(:,:,:)  !< minimal stomata resistance due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: urban_glc2000(:,:,:)   !< urban fraction due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: for_d_glc2000(:,:,:)   !< deciduous forest (fraction) due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: for_e_glc2000(:,:,:)   !< evergreen forest (fraction) due to glc2000 land use data
    REAL (KIND=wp), INTENT(OUT)  :: emissivity_glc2000(:,:,:) !< longwave emissivity due to glc2000 land use da



     
     INTEGER (KIND=i8) :: undefined_integer ! undef value
     REAL (KIND=wp)    :: default_real


     INTEGER :: i,j,k,l ! counters
     INTEGER :: i_col, j_row ! counter
     INTEGER (KIND=i8) :: i_lu, j_lu
     INTEGER (KIND=i8) :: ie, je, ke  ! indices for target grid elements

     INTEGER :: idom  ! counter

     INTEGER (KIND=i8) :: ndata(1:tg%ie,1:tg%je,1:tg%ke)  !< number of raw data pixel with land point
     REAL (KIND=wp)    :: a_weight(1:tg%ie,1:tg%je,1:tg%ke) !< area weight of all raw data pixels in target grid
     REAL (KIND=wp)    :: a_class(1:tg%ie,1:tg%je,1:tg%ke,1:nclass_glc2000) !< area for each land use class grid  in target grid element (for a area weight)
     
     REAL (KIND=wp)    :: latw      !< latitude weight (for area weighted mean)
     REAL (KIND=wp)    :: apix      !< area of a raw data pixel
     REAL (KIND=wp)    :: apix_e      !< area of a raw data pixel at equator

     INTEGER :: glc2000_data_row(glc2000_grid%nlon_reg)
     INTEGER :: glc2000_data_pixel(1:1,1:1)
     INTEGER :: lu  ! land use class
     INTEGER :: ncid_glc2000                             !< netcdf unit file number
     CHARACTER (LEN=80) :: varname  !< name of variable
     INTEGER :: varid_glc2000               !< id of variable
     INTEGER :: nlon

     REAL(KIND=wp)   :: point_lon, point_lat

      REAL (KIND=wp) :: pland          !< land cover                      (-)
      REAL (KIND=wp) :: pice           !< ice fraction                    (-)
      REAL (KIND=wp) :: plnz0          !< logarithm of roughness length   (m)
      REAL (KIND=wp) :: proot          !< root depth                      (m)
      REAL (KIND=wp) :: pmn            !< minimal plant cover             (-)
      REAL (KIND=wp) :: pmx            !< maximum plant cover             (-)
      REAL (KIND=wp) :: plaimn         !< minimal leaf area index         (m**2/m**2)
      REAL (KIND=wp) :: plaimx         !< maximum leaf area index         (m**2/m**2)
      REAL (KIND=wp) :: purb           !< urbanisation                    (-)
      REAL (KIND=wp) :: pfor_d         !< deciduous forest                (-)
      REAL (KIND=wp) :: pfor_e         !< evergreen forest                (-)
      REAL (KIND=wp) :: pemissivity    !< surface thermal emissivity      (-)
      REAL (KIND=wp) :: prs_min        !< minimum stomata resistance      (s/m)

     INTEGER        :: k_error     ! error return code

     REAL           :: hp ! height of Prandtl-layer
     REAL (KIND=wp) :: lnhp
     REAL (KIND=wp) :: pwz0 ! weighted summand for z0

     REAL (KIND=wp) :: area_tot   ! total area
     REAL (KIND=wp) :: area_land  ! area with land
     REAL (KIND=wp) :: area_plcov ! area covered with plants

     REAL (KIND=wp) :: bound_north_cosmo !< northern boundary for COSMO target domain
     REAL (KIND=wp) :: bound_south_cosmo !< southern boundary for COSMO target domain


     apix_e  = re * re * deg2rad* ABS(glc2000_grid%dlon_reg) * deg2rad * ABS(glc2000_grid%dlat_reg) ! area of GLC2000 raw data pixel at equator
     PRINT *,'area pixel at equator: ',apix_e

     hp   = 30.0      ! height of Prandtl-layer
     lnhp = ALOG(hp)

     default_real = 0.0
     undefined_integer= NINT(undefined)

     glc2000_class_fraction = default_real
     glc2000_class_npixel   = undefined_integer
     glc2000_tot_npixel = undefined_integer
     ndata = undefined_integer

     a_weight = default_real
     a_class  = default_real
     
     SELECT CASE(tg%igrid_type)
       CASE(igrid_icon)  ! ICON GRID
           ke = 1
       CASE(igrid_cosmo)  ! COSMO GRID
           ke = 1
           bound_north_cosmo = MAXVAL(lat_geo) + 0.05  ! add some "buffer"
           bound_north_cosmo = MIN(bound_north_cosmo,90.)
           bound_south_cosmo = MINVAL(lat_geo) - 0.05  ! add some "buffer"
           bound_south_cosmo = MAX(bound_south_cosmo,-90.)

       CASE(igrid_gme)  ! GME GRID

     END SELECT

     ! init lookup tables
     CALL init_glc2000_lookup_tables(nclass_glc2000, &
        &      ilookup_table_glc2000, &
        &      z0_lt_glc2000,            &
        &      lnz0_lt_glc2000,          &
        &      plc_mn_lt_glc2000,        &
        &      plc_mx_lt_glc2000,        &
        &      lai_mn_lt_glc2000,        &
        &      lai_mx_lt_glc2000,        &
        &      rd_lt_glc2000,            &
        &      emiss_lt_glc2000,         &
        &      rs_min_lt_glc2000)

      CALL get_name_glc2000_lookup_tables(ilookup_table_glc2000, name_lookup_table_glc2000)  
     ! open netcdf file 
     PRINT *,'open ',TRIM(glc2000_file)
     CALL check_netcdf( nf90_open(TRIM(glc2000_file),NF90_NOWRITE, ncid_glc2000))

     varname = 'glc2000byte' ! I know that the longitude coordinates for the GLC2000 data are stored in a variable called 'glc2000byte'

     CALL check_netcdf( nf90_inq_varid(ncid_glc2000, TRIM(varname), varid_glc2000))
     nlon = glc2000_grid%nlon_reg

     PRINT *,'Start loop over glc2000 dataset '
     ! loop over rows of GLC2000 dataset
     rows: DO j_row=1,glc2000_grid%nlat_reg
     !rows: DO j_row=1,100
       point_lat = lat_glc2000(j_row)
        
       IF (tg%igrid_type == igrid_cosmo) THEN ! CASE COSMO grid, save some I/O from hard disk if you are out or the target domain
         IF ((point_lat > bound_north_cosmo).OR.(point_lat < bound_south_cosmo) ) THEN ! raw data out of target grid
           CYCLE rows
         ENDIF
       ENDIF ! COSMO grid
        
       ! read in pixels
       !HA debug
       IF (MOD(j_row,500) == 0  ) THEN
       PRINT *,'nlon: ', nlon
       PRINT *,'j_row: ', j_row
       !HA debug
       ENDIF
       CALL check_netcdf(nf90_get_var(ncid_glc2000, varid_glc2000,  glc2000_data_row,  &
         &               start=(/1,j_row/),count=(/nlon,1/)))
       apix = apix_e * COS(point_lat * deg2rad) ! area of raw data pixel (in [m**2])

  columns: DO i_col=1, glc2000_grid%nlon_reg
  ! find the corresponding target grid indices
       point_lon = lon_glc2000(i_col) 

       CALL  find_nearest_target_grid_element( point_lon, &
                                              &      point_lat, &
                                              &      tg,        &
                                              &      ie,      &
                                              &      je,      &
                                              &      ke)

       IF ((ie /= 0).AND.(je/=0).AND.(ke/=0))THEN 
         ! raw data pixel within target grid, see output of routine find_rotated_lonlat_grid_element_index
         !- summation of variables
         glc2000_tot_npixel(ie,je,ke) = glc2000_tot_npixel(ie,je,ke) + 1
         a_weight(ie,je,ke) = a_weight(ie,je,ke) + apix  ! sum up area for weight
         lu = glc2000_data_row(i_col)                        ! land use class
         !IF ( (lu /= 20).OR.(lu /= 23)) THEN ! 20 is "water", 23 is "undefined" in the GLC2000 data set
         !   ndata(ie,je,ke)        = ndata(ie,je,ke) + 1
         !   fr_land_glc2000(ie,je,ke) = fr_land_glc2000(ie,je,ke) + apix
         !ENDIF

              CALL glc2000_look_up(lu, &
            &      nclass_glc2000, &
            &      lnz0_lt_glc2000,          &
            &      plc_mn_lt_glc2000,        &
            &      plc_mx_lt_glc2000,        &
            &      lai_mn_lt_glc2000,        &
            &      lai_mx_lt_glc2000,        &
            &      rd_lt_glc2000,            &
            &      emiss_lt_glc2000,         &
            &      rs_min_lt_glc2000,        &
            &      pland,          &
            &      pice,           &
            &      plnz0,          &
            &      proot,          &
            &      pmn,            &
            &      pmx,            &
            &      plaimn,         &
            &      plaimx,         &
            &      purb,           &
            &      pfor_d,         &
            &      pfor_e,         &
            &      pemissivity,    &
            &      prs_min,        &
            &      k_error)


!glc2000_look_up(lu,pland,pice,plnz0,proot, pmx,  &
!            &                  plaimx,purb,pfor_d,pfor_e,  &  
!            &                  pemissivity, prs_min,       &
!            &                  k_error)

          IF (k_error == 0) THEN ! valid land use class

            glc2000_class_npixel(ie,je,ke,lu) = glc2000_class_npixel(ie,je,ke,lu) + 1
            a_class(ie,je,ke,lu) = a_class(ie,je,ke,lu) + apix   ! sum area of valid land use pixels 
                                                                    !(use as weight later)
            emissivity_glc2000(ie,je,ke) =  emissivity_glc2000(ie,je,ke) + apix * pemissivity
            IF (pland >  0.0) THEN ! only for land pixel

            ! weighted with whole area
            fr_land_glc2000(ie,je,ke) = fr_land_glc2000(ie,je,ke) + apix * pland
            ice_glc2000(ie,je,ke) = ice_glc2000(ie,je,ke) + apix * pice
            urban_glc2000(ie,je,ke) = urban_glc2000(ie,je,ke) + apix * purb
            ! z0 is averaged logarithmic
            IF ( lnhp /= plnz0) THEN ! z0 is averaged logarithmic
              pwz0 = 1./(lnhp - plnz0)
            ELSE
              pwz0 = 0.
            ENDIF
            z0_glc2000(ie,je,ke)      = z0_glc2000(ie,je,ke) + apix * pwz0
            plcov_mn_glc2000(ie,je,ke) = plcov_mn_glc2000(ie,je,ke) + apix * pmn
            plcov_mx_glc2000(ie,je,ke) = plcov_mx_glc2000(ie,je,ke) + apix * pmx

            ! the following fields are weighted with the plant cover
            root_glc2000(ie,je,ke) = root_glc2000(ie,je,ke) + apix * pmx * proot
            lai_mn_glc2000(ie,je,ke) = lai_mn_glc2000(ie,je,ke) + apix * pmx * plaimn
            lai_mx_glc2000(ie,je,ke) = lai_mx_glc2000(ie,je,ke) + apix * pmx * plaimx
            rs_min_glc2000(ie,je,ke) = rs_min_glc2000(ie,je,ke) + apix * pmx * prs_min
            for_d_glc2000(ie,je,ke) = for_d_glc2000(ie,je,ke) + apix * pmx * pfor_d
            for_e_glc2000(ie,je,ke) = for_e_glc2000(ie,je,ke) + apix * pmx * pfor_e
                     
          END IF

        ENDIF
      ENDIF

      ! end loops
    ENDDO columns
    ENDDO rows
   
    
     SELECT CASE(tg%igrid_type)
     CASE(igrid_gme)  ! in GME grid the diamond edges need to be synrchonized

       ! fr_land_glc2000
       CALL cp_buf2gme(tg,gme_grid,fr_land_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,fr_land_glc2000)
       ! ice_glc2000
       CALL cp_buf2gme(tg,gme_grid,ice_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,ice_glc2000)
       ! urban_glc2000
       CALL cp_buf2gme(tg,gme_grid,urban_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,urban_glc2000)
       ! emissivity_glc2000
       CALL cp_buf2gme(tg,gme_grid,emissivity_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,emissivity_glc2000)
       ! z0_glc2000
       CALL cp_buf2gme(tg,gme_grid,z0_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,z0_glc2000)
       ! plcov_mn_glc2000
       CALL cp_buf2gme(tg,gme_grid,plcov_mn_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,plcov_mn_glc2000)
       ! plcov_mx_glc2000
       CALL cp_buf2gme(tg,gme_grid,plcov_mx_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,plcov_mx_glc2000)
       ! root_glc2000
       CALL cp_buf2gme(tg,gme_grid,root_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,root_glc2000)
       ! lai_mn_glc2000
       CALL cp_buf2gme(tg,gme_grid,lai_mn_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,lai_mn_glc2000)
       ! lai_mx_glc2000
       CALL cp_buf2gme(tg,gme_grid,lai_mx_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,lai_mx_glc2000)
       ! rs_min_glc2000
       CALL cp_buf2gme(tg,gme_grid,rs_min_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,rs_min_glc2000)
       ! for_d_glc2000
       CALL cp_buf2gme(tg,gme_grid,for_d_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,for_d_glc2000)
       ! for_e_glc2000
       CALL cp_buf2gme(tg,gme_grid,for_e_glc2000,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,for_e_glc2000)
       
       ! a_weight
       CALL cp_buf2gme(tg,gme_grid,a_weight,gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,a_weight)
      ! a_class
      DO l=1,nclass_glc2000
       CALL cp_buf2gme(tg,gme_grid,a_class(1:tg%ie,1:tg%je,1:tg%ke,l),gme_real_field)
       CALL sync_diamond_edge(gme_grid, gme_real_field)
       CALL cp_gme2buf(tg,gme_grid,gme_real_field,a_class(1:tg%ie,1:tg%je,1:tg%ke,l))
      ENDDO
      ! glc2000_tot_npixel
      CALL cp_buf2gme(tg,gme_grid,glc2000_tot_npixel,gme_int_field)
      CALL sync_diamond_edge(gme_grid, gme_int_field)
      CALL cp_gme2buf(tg,gme_grid,gme_int_field,glc2000_tot_npixel)
     END SELECT


    ! calculate glc2000_class_fraction (glc2000_class_fraction/glc2000_class_npixel)
    DO ke=1, tg%ke
    DO je=1, tg%je
    DO ie=1, tg%ie
      area_tot = a_weight(ie,je,ke)
      area_land = fr_land_glc2000(ie,je,ke)

      ! weight by total area
      IF (area_tot > 0.0) THEN
        emissivity_glc2000(ie,je,ke) = emissivity_glc2000(ie,je,ke) / area_tot
        DO l=1,nclass_glc2000
          glc2000_class_fraction(ie,je,ke,l) =  a_class(ie,je,ke,l) /  area_tot 
          ! area fraction of each land use class
        ENDDO
      ENDIF

      IF (area_land > 0.0 ) THEN
        area_plcov = plcov_mx_glc2000(ie,je,ke) ! area covered with plants
        !area_plcov = MAX(area_plcov, eps)      ! area covered with plants

        z0_glc2000(ie,je,ke) = z0_glc2000(ie,je,ke) / area_land
        z0_glc2000(ie,je,ke) = hp * EXP(-1./z0_glc2000(ie,je,ke))

        ! weight by total area
        fr_land_glc2000(ie,je,ke) = fr_land_glc2000(ie,je,ke) / area_tot
        ice_glc2000(ie,je,ke)   = ice_glc2000(ie,je,ke)   / area_tot


        ! weight by land area
        urban_glc2000(ie,je,ke) = urban_glc2000(ie,je,ke) / area_land
        for_d_glc2000(ie,je,ke) = for_d_glc2000(ie,je,ke) / area_land
        for_e_glc2000(ie,je,ke) = for_e_glc2000(ie,je,ke) / area_land
        plcov_mn_glc2000(ie,je,ke) = plcov_mn_glc2000(ie,je,ke) / area_land

        plcov_mx_glc2000(ie,je,ke) = plcov_mx_glc2000(ie,je,ke) / area_land

        ! weight by area covered with plants
        IF (area_plcov > 0.0) THEN
          root_glc2000(ie,je,ke) = root_glc2000(ie,je,ke)     / area_plcov
          lai_mn_glc2000(ie,je,ke) = lai_mn_glc2000(ie,je,ke) / area_plcov
          lai_mx_glc2000(ie,je,ke) = lai_mx_glc2000(ie,je,ke) / area_plcov
          rs_min_glc2000(ie,je,ke) = rs_min_glc2000(ie,je,ke) / area_plcov
        ELSE ! may occur for ice grid elements
          root_glc2000(ie,je,ke) = undefined
          lai_mn_glc2000(ie,je,ke) = undefined
          lai_mx_glc2000(ie,je,ke) = undefined
          rs_min_glc2000(ie,je,ke) = rs_min_undef
        ENDIF



      ELSE IF ( area_tot > 0.0) THEN ! only sea pixels were found
        fr_land_glc2000(ie,je,ke) = undefined
        z0_glc2000(ie,je,ke)      = undefined
        ice_glc2000(ie,je,ke)     = undefined
        urban_glc2000(ie,je,ke)   = undefined
        for_d_glc2000(ie,je,ke)   = undefined
        for_e_glc2000(ie,je,ke)   = undefined
        plcov_mx_glc2000(ie,je,ke)= undefined
        plcov_mn_glc2000(ie,je,ke)= undefined

        root_glc2000(ie,je,ke)    = undefined
        lai_mx_glc2000(ie,je,ke)  = undefined
        lai_mn_glc2000(ie,je,ke)  = undefined
        rs_min_glc2000(ie,je,ke)  = undefined

      ENDIF
    ENDDO
    ENDDO
    ENDDO

    DO ke=1, tg%ke
    DO je=1, tg%je
    DO ie=1, tg%ie
      IF (glc2000_tot_npixel(ie,je,ke) == 0) THEN ! nearest neighbor search

      point_lon = lon_geo(ie,je,ke)
      point_lat = lat_geo(ie,je,ke)
       CALL find_reg_lonlat_grid_element_index(point_lon,      &
          &                                     point_lat,      &
          &                                     glc2000_grid,  &
          &                                     i_lu,    &
          &                                     j_lu)
        ! get data
        IF ((i_lu /= 0).AND.(j_lu/=0))THEN 

        i_col = i_lu
        j_row = j_lu
        CALL check_netcdf(nf90_get_var(ncid_glc2000, varid_glc2000,  glc2000_data_pixel,  &
          &               start=(/ i_col,j_row /),count=(/ 1,1 /)))

        lu = glc2000_data_pixel(1,1)

        
              CALL glc2000_look_up(lu, &
            &      nclass_glc2000, &
            &      lnz0_lt_glc2000,          &
            &      plc_mn_lt_glc2000,        &
            &      plc_mx_lt_glc2000,        &
            &      lai_mn_lt_glc2000,        &
            &      lai_mx_lt_glc2000,        &
            &      rd_lt_glc2000,            &
            &      emiss_lt_glc2000,         &
            &      rs_min_lt_glc2000,        &
            &      pland,          &
            &      pice,           &
            &      plnz0,          &
            &      proot,          &
            &      pmn,            &
            &      pmx,            &
            &      plaimn,         &
            &      plaimx,         &
            &      purb,           &
            &      pfor_d,         &
            &      pfor_e,         &
            &      pemissivity,    &
            &      prs_min,        &
            &      k_error)
      ELSE
      lu = 0
      k_error = 1
      ENDIF
       
          IF (k_error == 0) THEN ! valid land use class
            apix = apix_e * COS(point_lat * deg2rad) ! area of raw data pixel (in [m**2])
            glc2000_class_npixel(ie,je,ke,lu) = glc2000_class_npixel(ie,je,ke,lu) + 1
            a_class(ie,je,ke,lu) = a_class(ie,je,ke,lu) + apix   ! sum area of valid land use pixels 
            emissivity_glc2000(ie,je,ke) =  pemissivity
            IF (pland >  0.0) THEN ! only for land pixel
              fr_land_glc2000(ie,je,ke) = pland
              ice_glc2000(ie,je,ke) =  pice
              urban_glc2000(ie,je,ke) = purb
              IF ( lnhp /= plnz0) THEN ! log z0 
                pwz0 = 1./(lnhp - plnz0)
              ELSE
                pwz0 = 0.
              ENDIF
              z0_glc2000(ie,je,ke)      =  pwz0
              z0_glc2000(ie,je,ke) = hp * EXP(-1./z0_glc2000(ie,je,ke))
              plcov_mn_glc2000(ie,je,ke) = pmn
              plcov_mx_glc2000(ie,je,ke) = pmx
              root_glc2000(ie,je,ke) = proot
              lai_mn_glc2000(ie,je,ke) = plaimn
              lai_mx_glc2000(ie,je,ke) = plaimx
              rs_min_glc2000(ie,je,ke) = prs_min
              for_d_glc2000(ie,je,ke) = pfor_d * pmx
              for_e_glc2000(ie,je,ke) = pfor_e * pmx
              IF (pice == 1.0) THEN  ! ice land use class
                root_glc2000(ie,je,ke) = undefined
                lai_mn_glc2000(ie,je,ke) = undefined
                lai_mx_glc2000(ie,je,ke) = undefined
                rs_min_glc2000(ie,je,ke) = rs_min_undef
              ENDIF
            ENDIF
          ELSE ! not a valid land use class
            fr_land_glc2000(ie,je,ke) = undefined
            z0_glc2000(ie,je,ke)      = undefined
            ice_glc2000(ie,je,ke)     = undefined
            urban_glc2000(ie,je,ke)   = undefined
            for_d_glc2000(ie,je,ke)   = undefined
            for_e_glc2000(ie,je,ke)   = undefined
            plcov_mx_glc2000(ie,je,ke)= undefined
            plcov_mn_glc2000(ie,je,ke)= undefined
            root_glc2000(ie,je,ke)    = undefined
            lai_mx_glc2000(ie,je,ke)  = undefined
            lai_mn_glc2000(ie,je,ke)  = undefined
            rs_min_glc2000(ie,je,ke)  = undefined
          ENDIF
      ENDIF ! nearest neighbour search
   ENDDO
   ENDDO
   ENDDO

  
    ! close netcdf file 
    call check_netcdf( nf90_close(ncid_glc2000))



  END SUBROUTINE agg_glc2000_data_to_target_grid


END MODULE mo_agg_glc2000


