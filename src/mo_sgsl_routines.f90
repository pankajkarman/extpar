!+ Fortran module with routines and settings for DEM subgrid-scale slope data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V4_0         2016/07/28 Daniel Luethi
!  Initial release
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with routines and settings for DEM subgrid-scale slope data
!> \author Daniel Luethi
!>
MODULE mo_sgsl_routines

  USE mo_logging
  USE mo_kind,                  ONLY: wp,i4, i2

  USE netcdf,                   ONLY: nf90_close,   &
       &                              nf90_get_att, &
       &                              nf90_get_var, &
       &                              nf90_inq_varid, &
       &                              nf90_nowrite, &
       &                              nf90_open

  USE mo_grid_structures,       ONLY: reg_lonlat_grid

  USE mo_io_utilities,          ONLY: check_netcdf

  USE mo_utilities_extpar,      ONLY: free_un 

  USE mo_sgsl_data,             ONLY: max_tiles, &
         &                            nc_tot,        &      
         &                            nr_tot,        &
         &                            idem_type,     &
         &                            dem_aster,     &
         &                            dem_gl,        &
         &                            demraw_lat_min,&
         &                            demraw_lat_max,&
         &                            demraw_lon_min,&
         &                            ntiles, &
         &                            get_varname, &
         &                            demraw_lon_max

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_namelists_extpar_sg_slope,&
       &    det_sgsl_tiles_grid,            &
       &    det_sgsl_grid,                  &
       &    get_sgsl_tile_block_indices,    &
       &    open_netcdf_sgsl_tile,          &
       &    close_netcdf_sgsl_tile

  PUBLIC :: det_band_gd, get_sgsl_data_block

  CONTAINS

  !---------------------------------------------------------------------------
  !---------------------------------------------------------------------------
  !> subroutine to read namelist for orography data settings for EXTPAR 
  SUBROUTINE read_namelists_extpar_sg_slope (namelist_file,          &
       raw_data_sgsl_path,&
       sgsl_files,             &  !mes>
       ntiles_column,          &
       ntiles_row,             &
       idem_type,             &  
       sgsl_buffer_file)


    CHARACTER (LEN=*), INTENT(IN)     :: namelist_file !< filename with namelists for for EXTPAR settings
    ! orography
    CHARACTER (LEN=1024), INTENT(OUT) :: raw_data_sgsl_path, &        !< path to raw data
         &                               sgsl_files(1:max_tiles), &                     !< filenames globe raw data
         &                               sgsl_buffer_file !< name for subgrid slope buffer file

    INTEGER (KIND=i4), INTENT(OUT)    :: ntiles_column, &     !< number of tile columns
         &                               ntiles_row, &        !< number of tile rows
         &                               idem_type

    INTEGER(KIND=i4)                  :: nuin, ierr, nzylen

    !> namelist with filenames for orography data output
    NAMELIST /sgsl_io_extpar/ sgsl_buffer_file
    !> namelist with information on orography data input
    NAMELIST /sgsl_raw_data/ idem_type, raw_data_sgsl_path, ntiles_column, ntiles_row, sgsl_files

    nuin = free_un()  ! function free_un returns free Fortran unit number
    OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)
    IF (ierr /=  0) THEN
      WRITE(message_text,*)'Cannot open ', TRIM(namelist_file)
      CALL logging%error(message_text,__FILE__, __LINE__) 
    ENDIF

    READ(nuin, NML=sgsl_io_extpar, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist sgsl_io_extpar',__FILE__, __LINE__) 
    ENDIF

    READ(nuin, NML=sgsl_raw_data, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist sgsl_raw_data',__FILE__, __LINE__) 
    ENDIF

    CLOSE(nuin, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      WRITE(message_text,*)'Cannot close ', TRIM(namelist_file)
      CALL logging%error(message_text,__FILE__, __LINE__) 
    ENDIF

    nzylen=LEN_TRIM(raw_data_sgsl_path)
    IF( nzylen > 0 ) THEN
      IF( raw_data_sgsl_path(nzylen:nzylen) /= '/') THEN
        IF( nzylen < LEN(raw_data_sgsl_path) ) THEN
          raw_data_sgsl_path = raw_data_sgsl_path (1:nzylen)//'/'
        ENDIF
      ENDIF
    ENDIF

  END SUBROUTINE read_namelists_extpar_sg_slope

  !> determine GLOBE raw data grid
  !> \author Hermann Asensio
  SUBROUTINE det_sgsl_tiles_grid(sgsl_tiles_grid)
    USE mo_sgsl_data, ONLY : ntiles , &    !< GLOBE raw data has 16 tiles and ASTER has 13
         tiles_lon_min, &
         tiles_lon_max, &
         tiles_lat_min, &
         tiles_lat_max, &
         tiles_ncolumns, &
         tiles_nrows

    TYPE(reg_lonlat_grid), INTENT(OUT) :: sgsl_tiles_grid(1:ntiles) 
    !< structure with definition of the raw data grid for the input tiles

    INTEGER :: k ! counter

    REAL (KIND=wp) :: dlon
    REAL (KIND=wp) :: dlat

    DO k=1,  ntiles ! determine the globe_tile_grid information from the namelist information

      dlon = (tiles_lon_max(k) - tiles_lon_min(k)) / REAL(tiles_ncolumns(k))

      dlat = -1. * (tiles_lat_max(k) - tiles_lat_min(k)) / REAL(tiles_nrows(k))

      ! latitude from north to south, negative increment


      sgsl_tiles_grid(k)%start_lon_reg  = tiles_lon_min(k) + 0.5 * dlon
      sgsl_tiles_grid(k)%end_lon_reg    = tiles_lon_max(k) - 0.5 * dlon

      sgsl_tiles_grid(k)%start_lat_reg  = tiles_lat_max(k) + 0.5 * dlat 
      ! latitude from north to south, note the negative increment!
      sgsl_tiles_grid(k)%end_lat_reg    = tiles_lat_min(k) - 0.5 * dlat 
      ! latitude from north to south, note the negative increment!

      sgsl_tiles_grid(k)%dlon_reg = dlon
      sgsl_tiles_grid(k)%dlat_reg = dlat

      sgsl_tiles_grid(k)%nlon_reg = tiles_ncolumns(k)
      sgsl_tiles_grid(k)%nlat_reg = tiles_nrows(k)

    ENDDO


  END SUBROUTINE det_sgsl_tiles_grid

  !> determine complete(global) GLOBE raw data grid 
  !> \author Hermann Asensio
  SUBROUTINE det_sgsl_grid(sgsl_grid)

    TYPE(reg_lonlat_grid), INTENT(OUT) :: sgsl_grid !< structure with definition of the global data grid of the GLOBE data 
    REAL (KIND=wp)                     :: dlon, dlat

    !as ASTER does not cover the whole globe until now different procedures must be chosen for ASTER and GLOBE
    SELECT CASE(idem_type)
      CASE(dem_aster, dem_gl)

        dlon = (demraw_lon_max - demraw_lon_min) / REAL(nc_tot)

        dlat = -1. * (demraw_lat_max - demraw_lat_min) / REAL(nr_tot)
        ! latitude from north to south, negative increment

        sgsl_grid%start_lon_reg  =  demraw_lon_min + 0.5 * dlon
        sgsl_grid%end_lon_reg    =  demraw_lon_max - 0.5 * dlon


        sgsl_grid%start_lat_reg = demraw_lat_max + 0.5 * dlat 
        ! latitude from north to south, note the negative increment!
        sgsl_grid%end_lat_reg  =  demraw_lat_min - 0.5 * dlat
        ! latitude from north to south, note the negative increment!
    END SELECT

    sgsl_grid%dlon_reg = dlon
    sgsl_grid%dlat_reg = dlat

    sgsl_grid%nlon_reg = nc_tot
    sgsl_grid%nlat_reg = nr_tot

  END SUBROUTINE det_sgsl_grid

  !> determine grid description of band for GLOBE I/O 
  !> \author Hermann Asensio
  SUBROUTINE det_band_gd(sgsl_grid,start_sgsl_row, ta_grid)

    TYPE(reg_lonlat_grid), INTENT(IN) :: sgsl_grid !< structure with definition of the global data grid of the GLOBE data 
    INTEGER, INTENT(IN)               :: start_sgsl_row !< number of the start row of band of sgsl_grid (global domain)
    TYPE(reg_lonlat_grid), INTENT(OUT):: ta_grid !< structure with defenition of the target area grid

    INTEGER(KIND=i4) :: nrows = 500 !< number of rows, set to 500 as default
    ! band from east to west for the whole globe, like the complete sgsl_grid

    ta_grid%dlon_reg = sgsl_grid%dlon_reg
    ta_grid%dlat_reg = sgsl_grid%dlat_reg

    ta_grid%start_lon_reg = sgsl_grid%start_lon_reg
    ta_grid%end_lon_reg =  sgsl_grid%end_lon_reg
    ta_grid%nlon_reg = sgsl_grid%nlon_reg

    ! latitude from north to south, negative increment
    ta_grid%nlat_reg = nrows
    ta_grid%start_lat_reg = sgsl_grid%start_lat_reg + ta_grid%dlat_reg * (start_sgsl_row - 1)  
    ! latitude from north to south, note the negative increment!
    ta_grid%end_lat_reg  =  ta_grid%start_lat_reg + ta_grid%dlat_reg * (nrows - 1)
    ! latitude from north to south, note the negative increment!
    ! check for south pole
    IF (ta_grid%end_lat_reg < sgsl_grid%end_lat_reg) THEN ! band is at south pole
      ta_grid%end_lat_reg =  sgsl_grid%end_lat_reg
      ta_grid%nlat_reg =  NINT(((ta_grid%end_lat_reg - ta_grid%start_lat_reg) / ta_grid%dlat_reg)) + 1
    ENDIF

  END SUBROUTINE det_band_gd

  !----------------------------------------------------------------------------------------------------------------

  !> get startrow, endrow, startcolumn and endcolumn of each GLOBE tile (raw data) for a 
  !! given target area (ta_grid) and
  !! get start_indices (lon, lat) and end_indices of the target area for each GLOBE tile
  !! The GLOBE raw data are split in 16 tiles (ASTER in 36), so the target area may overlap several tiles.
  !! This subroutine determines the necesarry indices to read in the GLOBE/ASTER data into the
  !! target area.
  !! GLOBE/ASTER tiles which are outside the target block will get indices with the value '0'

  SUBROUTINE get_sgsl_tile_block_indices(ta_grid,         &
       &                                     sgsl_tiles_grid, &
       &                                     sgsl_startrow,  &
       &                                     sgsl_endrow,    & 
       &                                     sgsl_startcolumn,&
       &                                     sgsl_endcolumn, &
       &                                     ta_start_ie, &
       &                                     ta_end_ie,   &
       &                                     ta_start_je, &
       &                                     ta_end_je)

    USE mo_sgsl_data, ONLY : ntiles ,     &    !< GLOBE raw data has 16 tiles, ASTER has 36
         tiles_ncolumns,&
         tiles_nrows

    USE mo_grid_structures, ONLY: reg_lonlat_grid  !< Definition of Data Type to describe a regular (lonlat) grid

    !< structure with definition of the target area grid (dlon must be the same as for the whole GLOBE dataset)
    TYPE(reg_lonlat_grid), INTENT(IN)  :: ta_grid 


    TYPE(reg_lonlat_grid), INTENT(IN) :: sgsl_tiles_grid(1:ntiles) 
    !< structure with defenition of the raw data grid for the 16 GLOBE tiles

    INTEGER (KIND=i4), INTENT(OUT) :: sgsl_startrow(1:ntiles)    !< startrow indices for each GLOBE tile
    INTEGER (KIND=i4), INTENT(OUT) :: sgsl_endrow(1:ntiles)      !< endrow indices for each GLOBE tile

    INTEGER (KIND=i4), INTENT(OUT) :: sgsl_startcolumn(1:ntiles)  !< starcolumn indices for each GLOBE tile
    INTEGER (KIND=i4), INTENT(OUT) :: sgsl_endcolumn(1:ntiles)   !< endcolumn indices for each GLOBE tile

    INTEGER (KIND=i4), INTENT(OUT) :: ta_start_ie(1:ntiles)    
    !< indices of target area block for first column of each GLOBE tile
    INTEGER (KIND=i4), INTENT(OUT) :: ta_end_ie(1:ntiles)      
    !< indices of target area block for last column of each GLOBE tile
    INTEGER (KIND=i4), INTENT(OUT) :: ta_start_je(1:ntiles)  
    !< indices of target area block for first row of each GLOBE tile
    INTEGER (KIND=i4), INTENT(OUT) :: ta_end_je(1:ntiles)   
    !< indices of target area block for last row of each GLOBE tile

    INTEGER  :: undefined

    INTEGER (KIND=i4) :: startrow ! startrow for tile
    INTEGER (KIND=i4) :: endrow 
    INTEGER (KIND=i4) :: startcolumn
    INTEGER (KIND=i4) :: endcolumn

    REAL (KIND=wp) :: dlon
    REAL (KIND=wp) :: dlat

    INTEGER :: k

    undefined = 0
    sgsl_startrow     = undefined
    sgsl_endrow       = undefined
    sgsl_startcolumn   = undefined
    sgsl_endcolumn    = undefined
    ta_start_ie = undefined 
    ta_end_ie = undefined
    ta_start_je = undefined
    ta_end_je = undefined

    k=1 ! determin dlon and dlat (are the same for all tiles)
    dlon = ta_grid%dlon_reg
    dlat = ta_grid%dlat_reg
    !dlon = (tiles_lon_max(k) - tiles_lon_min(k)) / REAL(tiles_ncolumns(k))
    !dlat =(tiles_lat_max(k) - tiles_lat_min(k)) / REAL(tiles_nrows(k))

    ! the GLOBE data are diveded in 16 tiles, 
    ! this defines a "dummy grid" to determine the tile index with a function
    ! lon from -180 to 180 with dlon 90 degrees
    ! lat from 100 to -100 with dlat 50 degrees
    DO k = 1,ntiles

      ! get startcolumn for tile k
      startcolumn = NINT((ta_grid%start_lon_reg - sgsl_tiles_grid(k)%start_lon_reg)/dlon) + 1 
      !< here I want nearest index (NINT)
      IF (startcolumn < 1) THEN 
        sgsl_startcolumn(k) = 1
        ! get the start index of the subtile for the target area block
        ta_start_ie(k) = NINT ((sgsl_tiles_grid(k)%start_lon_reg - ta_grid%start_lon_reg)/dlon) + 1 
        !< index of target area block

      ELSE IF (startcolumn > tiles_ncolumns(k)) THEN
        sgsl_startcolumn(k) = 0
        ta_start_ie(k) = 0
      ELSE
        sgsl_startcolumn(k) = startcolumn
        ta_start_ie(k) = 1
      ENDIF

      ! get endcolumn for tile k
      endcolumn = NINT((ta_grid%end_lon_reg - sgsl_tiles_grid(k)%start_lon_reg)/dlon) + 1
      IF (endcolumn > tiles_ncolumns(k)) THEN 
        sgsl_endcolumn(k) = tiles_ncolumns(k)
        ! get the end index of the subtile for the target area block
        ta_end_ie(k) = NINT ((sgsl_tiles_grid(k)%end_lon_reg - ta_grid%start_lon_reg)/dlon) + 1 
        !< index of target area block
      ELSE IF (endcolumn < 1) THEN
        sgsl_endcolumn(k) = 0
        ta_end_ie(k) = 0
      ELSE
        sgsl_endcolumn(k) = endcolumn
        ta_end_ie(k) = ta_grid%nlon_reg
      ENDIF


      ! get startrow for tile k
      startrow = NINT((ta_grid%start_lat_reg - sgsl_tiles_grid(k)%start_lat_reg)/dlat) + 1

      IF (startrow < 1) THEN 
        sgsl_startrow(k) = 1
        ! get the start index of the subtile for the target area block
        ta_start_je(k) = NINT ((sgsl_tiles_grid(k)%start_lat_reg  - ta_grid%start_lat_reg)/dlat) + 1 
        !< index of target area block

      ELSE IF (startrow > tiles_nrows(k)) THEN
        sgsl_startrow(k) = 0
        ta_start_je(k) = 0
      ELSE
        sgsl_startrow(k) = startrow
        ta_start_je(k) = 1
      ENDIF


      ! get endrow for tile k
      endrow   = NINT(( ta_grid%end_lat_reg - sgsl_tiles_grid(k)%start_lat_reg )/dlat)  + 1

      IF (endrow > tiles_nrows(k)) THEN 
        sgsl_endrow(k) = tiles_nrows(k)
        ! get the start index of the subtile for the target area block
        ta_end_je(k) = NINT ((sgsl_tiles_grid(k)%end_lat_reg -  ta_grid%start_lat_reg )/dlat) + 1 
        !< index of target area block

      ELSE IF (endrow < 1) THEN
        sgsl_endrow(k) = 0
        ta_end_je(k) = 0
      ELSE
        sgsl_endrow(k) = endrow
        ta_end_je(k) =  ta_grid%nlat_reg
      ENDIF


    ENDDO


  END SUBROUTINE get_sgsl_tile_block_indices


  !----------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------
  !> open netcdf-file and get netcdf unit file number
  SUBROUTINE open_netcdf_sgsl_tile(path_sgsl_tile, &
       ncid)
    CHARACTER (len=*), INTENT(in) :: path_sgsl_tile         !< filename with path to GLOBE tile
    INTEGER, INTENT(out)          :: ncid                             !< netcdf unit file number

    !! open netcdf file 
    call check_netcdf( nf90_open(TRIM(path_sgsl_tile),NF90_NOWRITE, ncid))

  END SUBROUTINE open_netcdf_sgsl_tile

  !> close netcdf-file 
  SUBROUTINE close_netcdf_sgsl_tile(ncid)
    INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number

    !! close netcdf file 
    call check_netcdf( nf90_close( ncid))

  END SUBROUTINE close_netcdf_sgsl_tile

  !-------------------------------------------------------------------------------
  !> get GLOBE data block for a given target area from the tile block indices  
  SUBROUTINE get_sgsl_data_block(sgsl_file_1,     &   !mes ><
       &                             ta_grid,         &
       &                             sgsl_tiles_grid, &
       &                             ncids_sgsl, &
       &                             sgsl_block)


    CHARACTER (len=*), INTENT(IN)      :: sgsl_file_1

    TYPE(reg_lonlat_grid), INTENT(IN)  :: ta_grid, & 
         &                                sgsl_tiles_grid(1:ntiles) 

    INTEGER(KIND=i4) , INTENT(IN)      :: ncids_sgsl(1:ntiles)  

    REAL (KIND=wp), INTENT(OUT)        :: sgsl_block(1:ta_grid%nlon_reg,1:ta_grid%nlat_reg) !< a block of GLOBE altitude data 

    !local variables
    INTEGER (KIND=i4)                  :: sgsl_startrow(1:ntiles), & !< startrow indices for each GLOBE tile
         &                                sgsl_endrow(1:ntiles), & !< endrow indices for each GLOBE tile
         &                                sgsl_startcolumn(1:ntiles), & !< starcolumn indices for each GLOBE tile
         &                                sgsl_endcolumn(1:ntiles), & !< endcolumn indices for each GLOBE tile
         &                                ta_start_ie(1:ntiles), &    !< indices of target area block for first column of each GLOBE tile
         &                                ta_end_ie(1:ntiles), &      !< indices of target area block for last column of each GLOBE tile
         &                                ta_start_je(1:ntiles), &  !< indices of target area block for first row of each GLOBE tile
         &                                ta_end_je(1:ntiles), &   !< indices of target area block for last row of each GLOBE tile
         &                                varid, &               !< id of variable
         &                                nrows, & !< number of rows ! dimensions for raw_sgsl_block
         &                                ncolumns, & !< number of columns ! dimensions for raw_sgsl_block
         &                                k, & ! counter
         &                                errorcode !< error status variable

    INTEGER (KIND=i2)                 :: fill_value
    INTEGER (KIND=i2), ALLOCATABLE    :: raw_sgsl_block(:,:) !< a block with GLOBE data
    REAL (KIND=wp)                    :: scale_factor
    CHARACTER (LEN=80)                :: varname  !< name of variable

    CALL get_varname(sgsl_file_1,varname)

    !       varname = 'S_ORO'  
    ! I know that in the S_ORO is used for the subgrid-scale slope data
    CALL check_netcdf(nf90_inq_varid(ncids_sgsl(1),TRIM(varname),varid)) ! get the varid of the altitude variable
    CALL check_netcdf(nf90_get_att(ncids_sgsl(1),varid, & 
         '_FillValue', fill_value))
    CALL check_netcdf(nf90_get_att(ncids_sgsl(1),varid, & 
         'scale_factor', scale_factor))
    sgsl_block = fill_value*scale_factor

    CALL get_sgsl_tile_block_indices( ta_grid,         &
         &                                sgsl_tiles_grid,& 
         &                                sgsl_startrow,  &
         &                                sgsl_endrow,    & 
         &                                sgsl_startcolumn,&
         &                                sgsl_endcolumn,  &
         &                                ta_start_ie, &
         &                                ta_end_ie,   &
         &                                ta_start_je, &
         &                                ta_end_je)


    DO k=1,ntiles
      IF ((sgsl_startrow(k)/=0).AND.(sgsl_startcolumn(k)/=0)) THEN
        nrows = sgsl_endrow(k) - sgsl_startrow(k) + 1
        ncolumns = sgsl_endcolumn(k) - sgsl_startcolumn(k) + 1

        ALLOCATE (raw_sgsl_block(1:ncolumns,1:nrows), STAT=errorcode)
        IF(errorcode/=0) CALL logging%error('Cant allocate the array raw_sgsl_block',__FILE__,__LINE__)
        
        raw_sgsl_block=fill_value

        ! get the data into the raw_sgsl_block
        CALL check_netcdf(nf90_get_var(ncids_sgsl(k),varid,raw_sgsl_block,     & 
             &     start=(/sgsl_startcolumn(k),sgsl_startrow(k)/),count=(/ncolumns,nrows/)))

        sgsl_block(ta_start_ie(k):ta_end_ie(k),ta_start_je(k):ta_end_je(k)) = &
             raw_sgsl_block(1:ncolumns,1:nrows) * scale_factor

        DEALLOCATE (raw_sgsl_block, STAT=errorcode)
        IF(errorcode/=0) CALL logging%error('Cant deallocate the array raw_sgsl_block',__FILE__,__LINE__)

      ENDIF
    ENDDO

  END SUBROUTINE get_sgsl_data_block

END MODULE mo_sgsl_routines

