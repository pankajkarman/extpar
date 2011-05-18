!+ Fortran module with routines and settings for GLOBE orography data
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
!> Fortran module with routines and settings for GLOBE orography data
!> \author Hermann Asensio
!>
MODULE mo_globe_routines

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i8, &
                   i4

USE netcdf,      ONLY :   &
  nf90_open,              &
  nf90_close,             &
  nf90_inquire,           &
  nf90_inquire_dimension, &
  nf90_inquire_variable,  &
  nf90_inq_attname,       &
  nf90_inquire_attribute, &
  nf90_get_att,           &
  nf90_inquire_dimension, &
  nf90_inq_varid,          &
  nf90_get_var,            &
  nf90_noerr,              &
  nf90_strerror

USE netcdf,      ONLY:     &
  nf90_create,             &
  nf90_def_dim,            &
  nf90_def_var,            &
  nf90_enddef,             &
  nf90_redef,              &
  nf90_put_att,            &
  nf90_put_var

 
USE netcdf,      ONLY :   &
  NF90_CHAR,               &
  NF90_DOUBLE,             &
  NF90_FLOAT,              &
  NF90_INT,                &
  NF90_BYTE,               &
  NF90_SHORT


USE netcdf,      ONLY :   &
  NF90_GLOBAL,             &
  NF90_UNLIMITED,          &
  NF90_CLOBBER,            &
  NF90_NOWRITE


!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_grid_structures,        ONLY: reg_lonlat_grid
USE mo_base_geometry,          ONLY: geographical_coordinates

USE mo_io_units,          ONLY: filename_max


USE mo_io_utilities,           ONLY: check_netcdf

IMPLICIT NONE

PRIVATE

PUBLIC :: read_globe_data_input_namelist,  &
          read_namelists_extpar_orography, &
          det_globe_tiles_grid,            &
          det_globe_grid,                  &
          get_globe_tile_nr,               &
          get_globe_tile_block_indices,    &
          open_netcdf_GLOBE_tile,          &
          close_netcdf_GLOBE_tile,         &
          get_globe_data_band,             &
          get_globe_data_parallel

PUBLIC :: det_band_gd, get_globe_data_block

CONTAINS

!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!> subroutine to read namelist for orography data settings for EXTPAR 
SUBROUTINE read_namelists_extpar_orography(namelist_file,        &
                                         raw_data_orography_path,&
                                         globe_files,            &
                                         orography_buffer_file,  &
                                         orography_output_file)

  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number
  USE mo_GLOBE_data, ONLY : ntiles_gl    !< GLOBE raw data has 16 tiles


  
  CHARACTER (len=filename_max), INTENT(IN) :: namelist_file !< filename with namelists for for EXTPAR settings
  ! orography

  
  CHARACTER (len=filename_max), INTENT(OUT) :: raw_data_orography_path        !< path to raw data
  CHARACTER (LEN=filename_max), INTENT(OUT) :: globe_files(1:ntiles_gl)  !< filenames globe raw data

  CHARACTER (len=filename_max), INTENT(OUT) :: orography_buffer_file !< name for orography buffer file
  CHARACTER (len=filename_max), INTENT(OUT) :: orography_output_file !< name for orography output file




  !> namelist with filenames for orography data output
  NAMELIST /orography_io_extpar/ orography_buffer_file, orography_output_file
  !> namelist with information on orography data input
  NAMELIST /orography_raw_data/ raw_data_orography_path, globe_files






   INTEGER           :: nuin !< unit number
   INTEGER (KIND=i4) :: ierr !< error flag


   nuin = free_un()  ! functioin free_un returns free Fortran unit number
   OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)

   READ(nuin, NML=orography_io_extpar, IOSTAT=ierr)
   READ(nuin, NML=orography_raw_data, IOSTAT=ierr)

   CLOSE(nuin)


END SUBROUTINE read_namelists_extpar_orography
!---------------------------------------------------------------------------

        !> read namelist with settings for GLOBE raw data grid
        !> \author Hermann Asensio
       SUBROUTINE read_globe_data_input_namelist(input_namelist_file,       &
                                                 globe_files)

         USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number
         USE mo_GLOBE_data, ONLY : ntiles_gl    !< GLOBE raw data has 16 tiles

           CHARACTER (LEN=*), INTENT(IN)  :: input_namelist_file !< file with input namelist 
           CHARACTER (LEN=12), INTENT(OUT) :: globe_files(1:ntiles_gl)  !< filenames globe raw data
           INTEGER :: nfiles                                   ! number of files 

           !>Define the namelist group
           NAMELIST /GLOBE_files_info/ nfiles, globe_files

           INTEGER (KIND=i4) :: ierr !< error flag
           INTEGER                  :: nuin !< unit number

              nuin = free_un()  ! functioin free_un returns free Fortran unit number
              open(nuin,FILE=TRIM(input_namelist_file), IOSTAT=ierr)
              !print *, ierr
              read(nuin, NML=GLOBE_files_info, IOSTAT=ierr)
              !print *, ierr

              close(nuin)

            END SUBROUTINE read_globe_data_input_namelist

            
        !> determine GLOBE raw data grid
        !> \author Hermann Asensio
       SUBROUTINE det_globe_tiles_grid(globe_tiles_grid)
         USE mo_GLOBE_data, ONLY : ntiles_gl , &    !< GLOBE raw data has 16 tiles
                                globe_tiles_lon_min, &
                                globe_tiles_lon_max, &
                                globe_tiles_lat_min, &
                                globe_tiles_lat_max, &
                                globe_tiles_ncolumns, &
                                globe_tiles_nrows
         TYPE(reg_lonlat_grid), INTENT(OUT) :: globe_tiles_grid(1:ntiles_gl) !< structure with defenition of the raw data grid for the 16 GLOBE tiles

         INTEGER :: k ! counter

         REAL (KIND=wp) :: lon0
         REAL (KIND=wp) :: lat0

         REAL (KIND=wp) :: dlon
         REAL (KIND=wp) :: dlat

            DO k=1,  ntiles_gl ! determine the globe_tile_grid information from the namelist information

            dlon = (globe_tiles_lon_max(k) - globe_tiles_lon_min(k)) / FLOAT(globe_tiles_ncolumns(k))

            dlat = -1. * (globe_tiles_lat_max(k) - globe_tiles_lat_min(k)) / FLOAT(globe_tiles_nrows(k))
            ! latitude from north to south, negative increment

            globe_tiles_grid(k)%start_lon_reg  = globe_tiles_lon_min(k) + 0.5 * dlon
            globe_tiles_grid(k)%end_lon_reg    = globe_tiles_lon_max(k) - 0.5 * dlon

            
            globe_tiles_grid(k)%start_lat_reg  = globe_tiles_lat_max(k) + 0.5 * dlat ! latitude from north to south, note the negative increment!
            globe_tiles_grid(k)%end_lat_reg    = globe_tiles_lat_min(k) - 0.5 * dlat ! latitude from north to south, note the negative increment!
            globe_tiles_grid(k)%dlon_reg = dlon
            globe_tiles_grid(k)%dlat_reg = dlat

            globe_tiles_grid(k)%nlon_reg = globe_tiles_ncolumns(k)
            globe_tiles_grid(k)%nlat_reg = globe_tiles_nrows(k)



            ENDDO

       END SUBROUTINE det_globe_tiles_grid



       !> determine complete(global) GLOBE raw data grid 
       !> \author Hermann Asensio
       SUBROUTINE det_globe_grid(globe_grid)
         USE mo_globe_data, ONLY :  nc_tot, &    
           &                        nr_tot
         TYPE(reg_lonlat_grid), INTENT(OUT) :: globe_grid !< structure with definition of the global data grid of the GLOBE data 
         INTEGER :: k ! counter

         REAL (KIND=wp) :: lon0
         REAL (KIND=wp) :: lat0
         REAL (KIND=wp) :: dlon
         REAL (KIND=wp) :: dlat


            dlon = 360. / FLOAT(nc_tot)

            dlat = -1. * 180. / FLOAT(nr_tot)
            ! latitude from north to south, negative increment

            globe_grid%start_lon_reg  = -180. + 0.5 * dlon
            globe_grid%end_lon_reg    =  180. - 0.5 * dlon

            
            globe_grid%start_lat_reg = 90. + 0.5 * dlat ! latitude from north to south, note the negative increment!
            globe_grid%end_lat_reg  = -90. - 0.5 * dlat ! latitude from north to south, note the negative increment!
            globe_grid%dlon_reg = dlon
            globe_grid%dlat_reg = dlat

            globe_grid%nlon_reg = nc_tot
            globe_grid%nlat_reg = nr_tot



       END SUBROUTINE det_globe_grid

       !> determine grid description of band for GLOBE I/O 
       !> \author Hermann Asensio
       SUBROUTINE det_band_gd(globe_grid,start_globe_row, ta_grid)

         TYPE(reg_lonlat_grid), INTENT(IN) :: globe_grid !< structure with definition of the global data grid of the GLOBE data 
         INTEGER, INTENT(IN) :: start_globe_row !< number of the start row of band of globe_grid (global domain)
         TYPE(reg_lonlat_grid), INTENT(OUT) :: ta_grid !< structure with defenition of the target area grid

         INTEGER  :: nrows = 1000 !< number of rows, set to 1000 as default
         ! band from east to west for the whole globe, like the complete globe_grid

         ta_grid%dlon_reg = globe_grid%dlon_reg
         ta_grid%dlat_reg = globe_grid%dlat_reg

         ta_grid%start_lon_reg = globe_grid%start_lon_reg
         ta_grid%end_lon_reg =  globe_grid%end_lon_reg
         ta_grid%nlon_reg = globe_grid%nlon_reg
          
         ! latitude from north to south, negative increment
         ta_grid%nlat_reg = nrows
         ta_grid%start_lat_reg = globe_grid%start_lat_reg + ta_grid%dlat_reg * (start_globe_row - 1)  ! latitude from north to south, note the negative increment!
         ta_grid%end_lat_reg  =  ta_grid%start_lat_reg + ta_grid%dlat_reg * (nrows - 1)! latitude from north to south, note the negative increment!
         ! check for south pole
         IF (ta_grid%end_lat_reg < globe_grid%end_lat_reg) THEN ! band is at south pole
           ta_grid%end_lat_reg =  globe_grid%end_lat_reg
           ta_grid%nlat_reg =  NINT(((ta_grid%end_lat_reg - ta_grid%start_lat_reg) / ta_grid%dlat_reg)) + 1
         ENDIF


       END SUBROUTINE det_band_gd






       !> find GLOBE tile for given geographical coordinates
       ELEMENTAL FUNCTION get_globe_tile_nr(point_geo) RESULT (index_k)
       TYPE(geographical_coordinates), INTENT(IN) :: point_geo !< geographical coordinats of a point [degrees]
       INTEGER (KIND=i4) :: index_k !< index of GLOBE tile which contains point_geo

       ! local variables

       INTEGER (KIND=i4) :: t_i
       INTEGER (KIND=i4) :: t_j

       REAL (KIND=wp) :: lon0_t
       REAL (KIND=wp) :: lat0_t
       REAL (KIND=wp) :: dlon_t
       REAL (KIND=wp) :: dlat_t

       REAL (KIND=wp) :: point_lon_coor


       ! the GLOBE data are diveded in 16 tiles, 
       ! this defines a "dummy grid" to determine the index with a function
       lon0_t = -180. 
       lat0_t = 100.
       dlon_t = 90.
       dlat_t = -50.

       point_lon_coor = point_geo%lon
       IF (point_lon_coor > 180.) THEN  ! shift longitude range
         point_lon_coor = point_lon_coor -360.
       ENDIF

       t_i = INT((point_lon_coor - lon0_t)/dlon_t) + 1 ! get the tile index for the column

       t_j = INT((lat0_t - point_geo%lat)/dlat_t) + 1  ! get the tile index for the row, 
                                                        !note the negative increment (rows from north to south

       !IF( (t_i < 1).OR.(t_i>4).OR.(t_j<1).OR.(t_j>4) ) CALL abort_extpar('point not in data range')

       index_k = (t_j - 1) * 4 + t_i ! the way the 16 element array is sorted (columns first)

       END FUNCTION get_globe_tile_nr
!----------------------------------------------------------------------------------------------------------------

       !> get startrow, endrow, startcolumn and endcolumn of each GLOBE tile (raw data) for a 
       !! given target area (ta_grid) and
       !! get start_indices (lon, lat) and end_indices of the target area for each GLOBE tile
       !! The GLOBE raw data are split in 16 tiles, so the target area may overlap several tiles.
       !! This subroutine determines the necesarry indices to read in the GLOBE data into the
       !! target area.
       !! GLOBE tiles which are outside the target block will get indices with the value '0'
       SUBROUTINE get_globe_tile_block_indices(ta_grid,         &
         &                                     globe_tiles_grid, &
         &                                     globe_startrow,  &
         &                                     globe_endrow,    & 
         &                                     globe_startcolumn,&
         &                                     globe_endcolumn, &
         &                                     ta_start_ie, &
         &                                     ta_end_ie,   &
         &                                     ta_start_je, &
         &                                     ta_end_je)

        USE mo_globe_data, ONLY : ntiles_gl , &    !< GLOBE raw data has 16 tiles
                                globe_tiles_lon_min, &
                                globe_tiles_lon_max, &
                                globe_tiles_lat_min, &
                                globe_tiles_lat_max, &
                                globe_tiles_ncolumns, &
                                globe_tiles_nrows
       USE mo_grid_structures, ONLY: reg_lonlat_grid  !< Definition of Data Type to describe a regular (lonlat) grid

       TYPE(reg_lonlat_grid), INTENT(IN)  :: ta_grid !< structure with definition of the target area grid (dlon must be the same as for the whole GLOBE dataset)


       TYPE(reg_lonlat_grid), INTENT(IN) :: globe_tiles_grid(1:ntiles_gl) !< structure with defenition of the raw data grid for the 16 GLOBE tiles

       INTEGER (KIND=i4), INTENT(OUT) :: globe_startrow(1:ntiles_gl)    !< startrow indices for each GLOBE tile
       INTEGER (KIND=i4), INTENT(OUT) :: globe_endrow(1:ntiles_gl)      !< endrow indices for each GLOBE tile

       INTEGER (KIND=i4), INTENT(OUT) :: globe_startcolumn(1:ntiles_gl)  !< starcolumn indices for each GLOBE tile
       INTEGER (KIND=i4), INTENT(OUT) :: globe_endcolumn(1:ntiles_gl)   !< endcolumn indices for each GLOBE tile

       INTEGER (KIND=i4), INTENT(OUT) :: ta_start_ie(1:ntiles_gl)    !< indices of target area block for first column of each GLOBE tile
       INTEGER (KIND=i4), INTENT(OUT) :: ta_end_ie(1:ntiles_gl)      !< indices of target area block for last column of each GLOBE tile
       INTEGER (KIND=i4), INTENT(OUT) :: ta_start_je(1:ntiles_gl)  !< indices of target area block for first row of each GLOBE tile
       INTEGER (KIND=i4), INTENT(OUT) :: ta_end_je(1:ntiles_gl)   !< indices of target area block for last row of each GLOBE tile

       
       INTEGER (KIND=i4) :: index_k !< index of GLOBE tile which contains point_geo

       ! local variables

       INTEGER  :: i          ! index for tiles (i,j)
       INTEGER  :: j 
       INTEGER  :: t_i_start 
       INTEGER  :: t_i_end
       INTEGER  :: t_j_start
       INTEGER  :: t_j_end

       REAL  :: lon0_t ! startlon for dummy grid
       REAL  :: lat0_t ! startlat for dummy grid
       REAL  :: dlon_t ! dlon for dummy grid
       REAL  :: dlat_t ! dlat for dummy grid

       INTEGER  :: undefined

       REAL (KIND=wp) :: point_lon_coor

       REAL (KIND=wp) :: tb_ll_lon ! longitude coordinate for lower left corner of target block
       REAL (KIND=wp) :: tb_ll_lat ! longitude coordinate for lower left corner of target block

       REAL (KIND=wp) :: tb_ur_lon ! longitude coordinate for upper right corner of target block
       REAL (KIND=wp) :: tb_ur_lat ! longitude coordinate for upper right corner of target block


       INTEGER (KIND=i4) :: startrow ! startrow for tile
       INTEGER (KIND=i4) :: endrow 
       INTEGER (KIND=i4) :: startcolumn
       INTEGER (KIND=i4) :: endcolumn

       REAL (KIND=wp) :: dlon
       REAL (KIND=wp) :: dlat

       REAL (KIND=wp) :: stile_ll_lon ! longitude coordinate for lower left corner of subtile
       REAL (KIND=wp) :: stile_ll_lat ! latitued coordinate for lower left corner of subtile

       REAL (KIND=wp) :: stile_ur_lon ! longitude coordinate for upper right corner of subtile
       REAL (KIND=wp) :: stile_ur_lat ! latitude coordinate for upper right corner of subtile



       INTEGER :: k

       undefined = 0
       globe_startrow     = undefined
       globe_endrow       = undefined
       globe_startcolumn   = undefined
       globe_endcolumn    = undefined
       ta_start_ie = undefined 
       ta_end_ie = undefined
       ta_start_je = undefined
       ta_end_je = undefined

       k=1 ! determin dlon and dlat (are the same for all tiles)
       dlon = ta_grid%dlon_reg
       dlat = ta_grid%dlat_reg
       !dlon = (globe_tiles_lon_max(k) - globe_tiles_lon_min(k)) / FLOAT(globe_tiles_ncolumns(k))
       !dlat =(globe_tiles_lat_max(k) - globe_tiles_lat_min(k)) / FLOAT(globe_tiles_nrows(k))

       ! the GLOBE data are diveded in 16 tiles, 
       ! this defines a "dummy grid" to determine the tile index with a function
       ! lon from -180 to 180 with dlon 90 degrees
       ! lat from 100 to -100 with dlat 50 degrees
       lon0_t = -180. 
       lat0_t = 100.
       dlon_t = 90.
       dlat_t = 50.

       !tb_ll_lon = ta_grid%start_lon_reg
       !IF (tb_ll_lon > 180.) THEN  ! shift longitude range
       !  tb_ll_lon = tb_ll_lon -360.
       !ENDIF
       !tb_ur_lon = ta_grid%end_lon_reg
       !IF (tb_ur_lon > 180.) THEN  ! shift longitude range
       !  tb_ur_lon = tb_ur_lon -360.
       !ENDIF
!
!       t_i_start = NINT((tb_ll_lon - lon0_t)/dlon_t) + 1 ! get the start tile index for the column
!
!       ! tb_ll_lon = lon0_t + dlon_t * (t_i_start - 1)
!
!       t_i_end = NINT((tb_ur_lon - lon0_t)/dlon_t) + 1 ! get the end tile index for the column
!       ! INT should truncate towards zero, which I want in this case here (not nearest index with NINT!)
!
!       !--
!
!       t_j_start = NINT((lat0_t - ta_grid%start_lat_reg)/dlat_t) + 1  ! get the start tile index for the row, 
!                                                        !note the negative increment (rows from north to south)
!       t_j_end = NINT((lat0_t - ta_grid%end_lat_reg)/dlat_t) + 1  ! get the start tile index for the row, 
!                                                        !note the negative increment (rows from north to south)
!
!       !IF( (t_i < 1).OR.(t_i>4).OR.(t_j<1).OR.(t_j>4) ) CALL abort_extpar('point not in data range')
!       !HA debug
!       !print *, 't_i_start, t_i_end: ', t_i_start, t_i_end
!       !print *,'t_j_start, t_j_end: ', t_j_start, t_j_end
       DO j=1, 4 ! loop over the tiles which overlap the target area from north to south (j index)
       DO i=1, 4 ! and from west to east (i index)
!       print *,'i ',i
!       print *,'j ',j
         k = (j - 1) * 4 + i ! the way the 16 element array is sorted (columns first)

!         print *,'k ',k
         ! get startcolumn for tile k
         startcolumn = NINT((ta_grid%start_lon_reg - globe_tiles_grid(k)%start_lon_reg)/dlon) +1 ! here I want nearest index (NINT)
         IF (startcolumn < 1) THEN 
           globe_startcolumn(k) = 1
           ! get the start index of the subtile for the target area block
           ta_start_ie(k) = NINT ((globe_tiles_grid(k)%start_lon_reg - ta_grid%start_lon_reg)/dlon) + 1 ! index of target area block

         ELSE IF (startcolumn > globe_tiles_ncolumns(k)) THEN
           globe_startcolumn(k) = 0
           ta_start_ie(k) = 0
         ELSE
           globe_startcolumn(k) = startcolumn
           ta_start_ie(k) = 1
         ENDIF

         ! get endcolumn for tile k
         endcolumn = NINT((ta_grid%end_lon_reg - globe_tiles_grid(k)%start_lon_reg)/dlon) +1
         IF (endcolumn > globe_tiles_ncolumns(k)) THEN 
           globe_endcolumn(k) = globe_tiles_ncolumns(k)
           ! get the end index of the subtile for the target area block
           stile_ur_lon =  globe_tiles_grid(k)%end_lon_reg ! coordinates [degrees]
           ta_end_ie(k) = NINT ((globe_tiles_grid(k)%end_lon_reg - ta_grid%start_lon_reg)/dlon) + 1                ! index of target area block
         ELSE IF (endcolumn < 1) THEN
           globe_endcolumn(k) = 0
           ta_end_ie(k) = 0
         ELSE
           globe_endcolumn(k) = endcolumn
           ta_end_ie(k) = ta_grid%nlon_reg
         ENDIF


         ! get startrow for tile k
         startrow = NINT((ta_grid%start_lat_reg - globe_tiles_grid(k)%start_lat_reg)/dlat) + 1
         IF (startrow < 1) THEN 
           globe_startrow(k) = 1
           ! get the start index of the subtile for the target area block
           ta_start_je(k) = NINT ((globe_tiles_grid(k)%start_lat_reg  - ta_grid%start_lat_reg)/dlat) + 1 ! index of target area block
         ELSE IF (startrow > globe_tiles_nrows(k)) THEN
           globe_startrow(k) = 0
           ta_start_je(k) = 0
         ELSE
           globe_startrow(k) = startrow
           ta_start_je(k) = 1
         ENDIF


         ! get endrow for tile k
         endrow   = NINT(( ta_grid%end_lat_reg - globe_tiles_grid(k)%start_lat_reg )/dlat) + 1
         IF (endrow > globe_tiles_nrows(k)) THEN 
           globe_endrow(k) = globe_tiles_nrows(k)
           ! get the start index of the subtile for the target area block
           ta_end_je(k) = NINT ((globe_tiles_grid(k)%end_lat_reg -  ta_grid%start_lat_reg )/dlat) + 1 ! index of target area block
         ELSE IF (endrow < 1) THEN
           globe_endrow(k) = 0
           ta_end_je(k) = 0
         ELSE
           globe_endrow(k) = endrow
           ta_end_je(k) =  ta_grid%nlat_reg
         ENDIF

       ENDDO
       ENDDO  ! loop over the tiles 

       END SUBROUTINE get_globe_tile_block_indices


!----------------------------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------------------------
    !> open netcdf-file and get netcdf unit file number
       SUBROUTINE open_netcdf_GLOBE_tile(path_globe_tile, &
                                        ncid)
         CHARACTER (len=*), INTENT(in) :: path_globe_tile         !< filename with path to GLOBE tile
         INTEGER, INTENT(out) :: ncid                             !< netcdf unit file number

        !! open netcdf file 
            call check_netcdf( nf90_open(TRIM(path_globe_tile),NF90_NOWRITE, ncid))

       END SUBROUTINE open_netcdf_GLOBE_tile

        !> close netcdf-file 
       SUBROUTINE close_netcdf_GLOBE_tile(ncid)
         INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number

        !! close netcdf file 
            call check_netcdf( nf90_close( ncid))

       END SUBROUTINE close_netcdf_GLOBE_tile

!----------------------------------------------------------------------------------------------------------------
       !> get globe data on a single circle of latitude
       SUBROUTINE get_globe_data_parallel(mlat, &
                                          ncids_globe, &
                                          h_parallel)
       USE mo_globe_data, ONLY : ntiles_gl  !< there are 16 GLOBE tiles 
       USE mo_globe_data, ONLY : nc_tot     !< total number of columns in GLOBE data: 43200
       USE mo_globe_data, ONLY : nc_tile    !< number of columns in a GLOBE tile
       
       USE mo_globe_data, ONLY : h_tile_row !< variable for height of GLOBE data for a data row of a tile

       INTEGER , INTENT(IN) :: mlat  !< global index of raw data line
       INTEGER , INTENT(IN) :: ncids_globe(1:ntiles_gl)  !< ncid for the GLOBE tiles, the netcdf files have to be opened by a previous call of open_netcdf_GLOBE_tile
       INTEGER (KIND=i4), INTENT(OUT) :: h_parallel(1:nc_tot)     !< GLOBE altitude data along a parallel

       ! local variables
        INTEGER  :: tile_start
        INTEGER  :: tile_end
        INTEGER  :: tile_row

        INTEGER :: varid               !< id of variable
        CHARACTER (LEN=80) :: varname  !< name of variable

 !       INTEGER :: h_tile_row(1:nc_tile) !< variable for height of GLOBE data for a data row

        INTEGER :: k !< counter
        INTEGER :: i !< counter
        INTEGER :: j !< counter
        INTEGER :: os !< counter
        INTEGER :: nt ! counter


        varname = 'altitude'  ! I know that in the GLOBE netcdf files the height data are stored in a variable "altitude"


        SELECT CASE(mlat)

        CASE (1:4800)
          tile_start = 1    ! GLOBE TILE A, or 1
          tile_row   = mlat ! row in the Tiles A, B, C, D
        CASE (4801:10800)
          tile_start = 5    ! GLOBE TILE E, or 5
          tile_row   = mlat - 4800  ! row in the tiles E, F, G, H
        CASE (10801:16800)
          tile_start = 9    ! GLOBE TILE I, or 9
          tile_row   = mlat - 10800 ! row in the tiles I, J, K, L
        CASE (16801:21600)
          tile_start = 13
          tile_row   = mlat - 16800 ! row in the tiles M, N, O, P
        CASE DEFAULT
          CALL abort_extpar('get_globe_data_parallel: mlat not in data range of GLOBE tiles')
        END SELECT

        tile_end = tile_start + 3 ! numbering of GLOBE tiles
        nt = 0
        DO k=tile_start,tile_end
          nt = nt + 1 ! count the number of tiles
          CALL check_netcdf(nf90_inq_varid(ncids_globe(k),TRIM(varname),varid)) ! get the varid of the altitude variable
          CALL check_netcdf(nf90_get_var(ncids_globe(k), varid,  h_tile_row, & ! get the data of one tile row
           &    start=(/1,tile_row/),count=(/nc_tile,1/)))
          os = (nt-1) * nc_tile ! offset for array with data along latitude circle
          !DO i=1,nc_tile
          !  j = i + os
          !  h_parallel(j) = h_tile_row(i)
          !ENDDO
          h_parallel(os+1:os+nc_tile) = h_tile_row(1:nc_tile)

        ENDDO

        


       END SUBROUTINE get_globe_data_parallel
!----------------------------------------------------------------------------------------------------------------
   !> get GLOBE data block for a given target area from the tile block indices
       SUBROUTINE get_globe_data_block(ta_grid,              &
         &                             globe_tiles_grid, &
         &                             ncids_globe, &
         &                             h_block)

       USE mo_grid_structures, ONLY: reg_lonlat_grid  !< Definition of Data Type to describe a regular (lonlat) grid

       USE mo_globe_data, ONLY : ntiles_gl  !< there are 16 GLOBE tiles 
       USE mo_globe_data, ONLY : nc_tot     !< total number of columns in GLOBE data: 43200
       USE mo_globe_data, ONLY : nc_tile    !< number of columns in a GLOBE tile

       TYPE(reg_lonlat_grid), INTENT(IN)  :: ta_grid !< structure with definition of the target area grid (dlon must be the same as for the whole GLOBE dataset)
       TYPE(reg_lonlat_grid), INTENT(IN) :: globe_tiles_grid(1:ntiles_gl) !< structure with defenition of the raw data grid for the 16 GLOBE tiles

       INTEGER , INTENT(IN) :: ncids_globe(1:ntiles_gl)  !< ncid for the GLOBE tiles, the netcdf files have to be opened by a previous call of open_netcdf_GLOBE_tile

       INTEGER (KIND=i4), INTENT(OUT) :: h_block(1:ta_grid%nlon_reg,1:ta_grid%nlat_reg) !< a block of GLOBE altitude data 
       !local variables

       INTEGER (KIND=i4) :: globe_startrow(1:ntiles_gl) !< startrow indices for each GLOBE tile
       INTEGER (KIND=i4) :: globe_endrow(1:ntiles_gl) !< endrow indices for each GLOBE tile
       INTEGER (KIND=i4) :: globe_startcolumn(1:ntiles_gl) !< starcolumn indices for each GLOBE tile
       INTEGER (KIND=i4) :: globe_endcolumn(1:ntiles_gl) !< endcolumn indices for each GLOBE tile

       INTEGER (KIND=i4) :: ta_start_ie(1:ntiles_gl)    !< indices of target area block for first column of each GLOBE tile
       INTEGER (KIND=i4) :: ta_end_ie(1:ntiles_gl)      !< indices of target area block for last column of each GLOBE tile
       INTEGER (KIND=i4) :: ta_start_je(1:ntiles_gl)  !< indices of target area block for first row of each GLOBE tile
       INTEGER (KIND=i4) :: ta_end_je(1:ntiles_gl)   !< indices of target area block for last row of each GLOBE tile


       INTEGER (KIND=i4), ALLOCATABLE :: raw_globe_block(:,:) !< a block with GLOBE data

       INTEGER :: varid               !< id of variable
       CHARACTER (LEN=80) :: varname  !< name of variable

       INTEGER :: nrows !< number of rows ! dimensions for raw_globe_block
       INTEGER :: ncolumns !< number of columns ! dimensions for raw_globe_block



       INTEGER :: k ! counter
       INTEGER :: errorcode !< error status variable


       varname = 'altitude'  ! I know that in the GLOBE netcdf files the height data are stored in a variable "altitude"

       
       
       CALL get_globe_tile_block_indices( ta_grid,         &
         &                                globe_tiles_grid,& 
         &                                globe_startrow,  &
         &                                globe_endrow,    & 
         &                                globe_startcolumn,&
         &                                globe_endcolumn,  &
         &                                ta_start_ie, &
         &                                ta_end_ie,   &
         &                                ta_start_je, &
         &                                ta_end_je)
       !  allocate_raw_globe_fields(nrows,ncolumns)
       ! raw_globe_block

       DO k=1,ntiles_gl
         IF ((globe_startrow(k)/=0).AND.(globe_startcolumn(k)/=0)) THEN
           nrows = globe_endrow(k) - globe_startrow(k) + 1
           ncolumns = globe_endcolumn(k) - globe_startcolumn(k) + 1
           
           ALLOCATE (raw_globe_block(1:ncolumns,1:nrows), STAT=errorcode)
           IF(errorcode/=0) CALL abort_extpar('Cant allocate the array raw_globe_block')
           ! raw_globe_block(ncolumns,nrows)

           CALL check_netcdf(nf90_inq_varid(ncids_globe(k),TRIM(varname),varid)) ! get the varid of the altitude variable
           ! get the data into the raw_globe_block
           CALL check_netcdf(nf90_get_var(ncids_globe(k), varid,  raw_globe_block,     & 
            &     start=(/globe_startcolumn(k),globe_startrow(k)/),count=(/ncolumns,nrows/)))

            h_block(ta_start_ie(k):ta_end_ie(k),ta_start_je(k):ta_end_je(k)) = raw_globe_block(1:ncolumns,1:nrows)
            
           DEALLOCATE (raw_globe_block, STAT=errorcode)
           IF(errorcode/=0) CALL abort_extpar('Cant deallocate the array raw_globe_block')
          
         ENDIF
       ENDDO



       END SUBROUTINE get_globe_data_block




!----------------------------------------------------------------------------------------------------------------


 !> get globe data band on a circle of latitude
       SUBROUTINE get_globe_data_band(mstart, &
                                      nrows,  &
                                      ncids_globe, &
                                      h_band)
       USE mo_GLOBE_data, ONLY : ntiles_gl  !< there are 16 GLOBE tiles 
       USE mo_GLOBE_data, ONLY : nc_tot     !< total number of columns in GLOBE data: 43200
       USE mo_GLOBE_data, ONLY : nc_tile    !< number of columns in a GLOBE tile

       USE mo_GLOBE_data, ONLY : h_tile_row !< variable for height of GLOBE data for a data row of a tile



       INTEGER , INTENT(IN) :: mstart  !< global index of first raw data line
       INTEGER , INTENT(IN) :: nrows   !< total number or row data rows to read in
       INTEGER , INTENT(IN) :: ncids_globe(1:ntiles_gl)  !< ncid for the GLOBE tiles, the netcdf files have to be opened by a previous call of open_netcdf_GLOBE_tile
       INTEGER (KIND=i4), INTENT(OUT) :: h_band(1:nc_tot,1:nrows)     !< GLOBE altitude data along a parallel

       ! local variables
        INTEGER  :: tile_start
        INTEGER  :: tile_end
        INTEGER  :: tile_row

        INTEGER :: varid               !< id of variable
        CHARACTER (LEN=80) :: varname  !< name of variable

!        INTEGER :: h_tile_row(1:nc_tile) !< variable for height of GLOBE data for a data row

        INTEGER :: k !< counter
        INTEGER :: i !< counter
        INTEGER :: j !< counter
        INTEGER :: os !< counter
        INTEGER :: nt ! counter
        INTEGER :: n_row ! counter

        INTEGER :: mlat ! global index of GLOBE raw data row to read in

        INTEGER :: m_end ! global index of last raw data line

        m_end = mstart+nrows


        varname = 'altitude'  ! I know that in the GLOBE netcdf files the height data are stored in a variable "altitude"
        SELECT CASE(mstart)

          CASE (1:4800)
            tile_start = 1    ! GLOBE TILE A, or 1
            !tile_row   = mlat ! row in the Tiles A, B, C, D
          CASE (4801:10800)
            tile_start = 5    ! GLOBE TILE E, or 5
            !tile_row   = mlat - 4800  ! row in the tiles E, F, G, H
          CASE (10801:16800)
            tile_start = 9    ! GLOBE TILE I, or 9
            !tile_row   = mlat - 10800 ! row in the tiles I, J, K, L
          CASE (16801:21600)
            tile_start = 13
            !tile_row   = mlat - 16800 ! row in the tiles M, N, O, P
          CASE DEFAULT
            CALL abort_extpar('get_globe_data_band: mlat not in data range of GLOBE tiles')
          END SELECT

          SELECT CASE(m_end)

          CASE (1:4800)
            tile_end = 4    ! row in the Tiles A, B, C, D
          CASE (4801:10800)
            tile_end = 8    ! row in the tiles E, F, G, H
          CASE (10801:16800)
            tile_end = 12   ! row in the tiles I, J, K, L
          CASE (16801:21600)
            tile_end = 16   ! row in the tiles M, N, O, P
          CASE DEFAULT
            CALL abort_extpar('get_globe_data_band: mlat not in data range of GLOBE tiles')
          END SELECT


        DO n_row=1,nrows
          mlat= mstart + n_row -1 ! global index of GLOBE row


          SELECT CASE(mlat)

          CASE (1:4800)
            tile_start = 1    ! GLOBE TILE A, or 1
            tile_row   = mlat ! row in the Tiles A, B, C, D
          CASE (4801:10800)
            tile_start = 5    ! GLOBE TILE E, or 5
            tile_row   = mlat - 4800  ! row in the tiles E, F, G, H
          CASE (10801:16800)
            tile_start = 9    ! GLOBE TILE I, or 9
            tile_row   = mlat - 10800 ! row in the tiles I, J, K, L
          CASE (16801:21600)
            tile_start = 13
            tile_row   = mlat - 16800 ! row in the tiles M, N, O, P
          CASE DEFAULT
            CALL abort_extpar('get_globe_data_band: mlat not in data range of GLOBE tiles')
          END SELECT

          tile_end = tile_start + 3 ! numbering of GLOBE tiles
          nt = 0
          DO k=tile_start,tile_end
            nt = nt + 1 ! count the number of tiles
            CALL check_netcdf(nf90_inq_varid(ncids_globe(k),TRIM(varname),varid)) ! get the varid of the altitude variable
            CALL check_netcdf(nf90_get_var(ncids_globe(k), varid,  h_tile_row,     & ! get the data of one tile row
                 start=(/1,tile_row/),count=(/nc_tile,1/)))
            os = (nt-1) * nc_tile ! offset for array with data along latitude circle
            h_band(os+1:os+nc_tile,n_row) = h_tile_row(1:nc_tile)

          ENDDO

        ENDDO



       END SUBROUTINE get_globe_data_band
!----------------------------------------------------------------------------------------------------------------




END MODULE mo_globe_routines
