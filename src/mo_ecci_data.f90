!+ Fortran Module with data fields for the ecci data
!
! Current Code Owner: DWD, Hermann Asensio
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_3         2011/04/19 Hermann Asensio
!  Initial release
! V2_0_3       2014/09/17 Burkhardt Rockel
!  Added use of directory information to access raw data files
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran Module with data fields for the Ecci data
!> \author Hermann Asensio
!!
MODULE mo_ecci_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_grid_structures, ONLY: reg_lonlat_grid

! >mes

USE mo_io_utilities,       ONLY:  check_netcdf

USE mo_io_units,           ONLY:  filename_max

USE netcdf,       ONLY :     &
     nf90_open,              &
     nf90_close,             &
     nf90_inquire,           &
     nf90_inquire_dimension, &
     nf90_inquire_variable,  &
     nf90_inq_attname,       &
     nf90_inquire_attribute, &
     nf90_get_att,           &
     nf90_inquire_dimension, &
     nf90_inq_dimid,         &
     nf90_inq_varid,         &
     nf90_get_var,           &
     nf90_noerr,             &
     nf90_strerror,          &
     nf90_create,            &
     nf90_def_dim,           &
     nf90_def_var,           &
     nf90_enddef,            &
     nf90_redef,             &
     nf90_put_att,           &
     nf90_put_var,           &
     NF90_CHAR,              &
     NF90_DOUBLE,            &
     NF90_FLOAT,             &
     NF90_INT,               &
     NF90_BYTE,              &
     NF90_SHORT,             &
     NF90_GLOBAL,            &
     NF90_UNLIMITED,         &
     NF90_CLOBBER,           &
     NF90_NOWRITE

! <mes

IMPLICIT NONE

PRIVATE

PUBLIC :: ecci_grid,                &
          lon_ecci,                 &
          lat_ecci,                 &
          allocate_raw_ecci_fields, &
! >mes
          ntiles_ecci,              &   ! number of tiles in ECCI
          ncolumn_tiles_ecci,                 &   ! number of columns in tile matrix
          nrow_tiles_ecci,                    &   ! number of rows in tile matrix
          len_lu_lon_ecci,                    &   ! Number of columns in land-use dataset
          len_lu_lat_ecci,                    &   ! Number of rows in land-use dataset
          max_tiles_lu_ecci,                  &   ! maximal possible number of tiles that can be read
          nc_tiles_lu_ecci,                   &   ! total number of columns in one ECCI tile
          lu_tiles_lon_min_ecci,              &   ! starting longitude of every ECCI tile
          lu_tiles_lon_max_ecci,              &   ! ending longitude of every ECCI tile
          lu_tiles_lat_min_ecci,              &   ! starting latitude of every ECCI tile
          lu_tiles_lat_max_ecci,              &   ! ending latitude of every ECCI tile
          lu_tiles_ncolumns_ecci,             &   ! number of columns (lonitude increments) in each ECCI tile
          lu_tiles_nrows_ecci,                &   ! number of rows (latitude increments) in each ECCI tile
          ecci_tiles_grid,          &
          fill_ecci_data,           &   ! subroutine (intent(in) and intent(out))
          allocate_ecci_data,       &
          deallocate_landuse_data_ecci

TYPE(reg_lonlat_grid) :: ecci_grid !< structure with defenition of the raw data grid for the whole ecci dataset

REAL (KIND=wp), ALLOCATABLE    :: lon_ecci(:) !< longitude of ecci raw data
REAL (KIND=wp), ALLOCATABLE    :: lat_ecci(:) !< latitude of ecci raw data

SAVE
TYPE(reg_lonlat_grid), ALLOCATABLE  :: ecci_tiles_grid(:)

INTEGER(KIND=i4), ALLOCATABLE :: lu_tiles_ncolumns_ecci(:)
INTEGER(KIND=i4), ALLOCATABLE :: lu_tiles_nrows_ecci(:)
INTEGER(KIND=i4)              :: ntiles_ecci = 6  ! number of ECCI tiles
INTEGER(KIND=i4)              :: ncolumn_tiles_ecci = 3  ! number of used tiles E-W
INTEGER(KIND=i4)              :: nrow_tiles_ecci = 2     ! number of used tiles N-S
INTEGER(KIND=i4)              :: len_lu_lon_ecci
INTEGER(KIND=i4)              :: len_lu_lat_ecci
INTEGER, PARAMETER            :: max_tiles_lu_ecci = 1000
INTEGER(KIND=i4)              :: nc_tiles_lu_ecci

REAL(KIND=wp), ALLOCATABLE    :: lu_tiles_lon_min_ecci(:)
REAL(KIND=wp), ALLOCATABLE    :: lu_tiles_lon_max_ecci(:)
REAL(KIND=wp), ALLOCATABLE    :: lu_tiles_lat_min_ecci(:)
REAL(KIND=wp), ALLOCATABLE    :: lu_tiles_lat_max_ecci(:)
REAL(KIND=wp), ALLOCATABLE    :: raw_lu_line(:)
REAL(KIND=wp), ALLOCATABLE    :: raw_lu_block(:,:)
! <mes

CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_ecci_fields(nrows,ncolumns)
  IMPLICIT NONE
  INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
  INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns

  INTEGER :: errorcode !< error status variable

    ALLOCATE (lon_ecci(1:ncolumns), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_ecci')
    lon_ecci = 0.0

     ALLOCATE (lat_ecci(1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_ecci')
    lat_ecci = 0.0

  END  SUBROUTINE allocate_raw_ecci_fields

  !----------------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------------
! >mes

     SUBROUTINE allocate_ecci_data(ntiles_ecci)   
       IMPLICIT NONE
       INTEGER, INTENT (IN) :: ntiles_ecci       ! number of tiles: 6 for ECCI
       INTEGER :: errorcode

       ALLOCATE (lu_tiles_lon_min_ecci(1:ntiles_ecci), STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant allocate the vector lu_tiles_lon_min_ecci')
       ALLOCATE (lu_tiles_lon_max_ecci(1:ntiles_ecci), STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant allocate the vector lu_tiles_lon_max_ecci')
       ALLOCATE (lu_tiles_lat_min_ecci(1:ntiles_ecci), STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant allocate the vector lu_tiles_lat_min_ecci')
       ALLOCATE (lu_tiles_lat_max_ecci(1:ntiles_ecci), STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant allocate the vector lu_tiles_lat_max_ecci')
       
       ALLOCATE (lu_tiles_ncolumns_ecci(1:ntiles_ecci), STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant allocate the vector lu_tiles_ncolumns_ecci')
       ALLOCATE (lu_tiles_nrows_ecci(1:ntiles_ecci), STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant allocate the vector lu_tiles_nrows_ecci')

       ALLOCATE (ecci_tiles_grid(1:ntiles_ecci), STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant allocate the vector ecci_tiles_grid')
   
   lu_tiles_lon_min_ecci   = 0.0
   lu_tiles_lon_max_ecci   = 0.0
   lu_tiles_lat_min_ecci   = 0.0
   lu_tiles_lat_max_ecci   = 0.0
   lu_tiles_ncolumns_ecci  = 0
   lu_tiles_nrows_ecci     = 0
   
   END SUBROUTINE allocate_ecci_data

! <mes

  !----------------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------------
! >mes

   SUBROUTINE fill_ecci_data(raw_data_lu_path,       &
                                  raw_data_lu_filename,   &
                                  lu_tiles_lon_min_ecci,       &
                                  lu_tiles_lon_max_ecci,       &  ! the allocated vectors need to be filled with the respective value
                                  lu_tiles_lat_min_ecci,       &
                                  lu_tiles_lat_max_ecci,       &
                                  nc_tiles_lu)
   IMPLICIT NONE
   SAVE
   CHARACTER (len=filename_max),INTENT(IN) :: raw_data_lu_path
   CHARACTER (len=filename_max),INTENT(IN) :: raw_data_lu_filename(:)
   REAL(KIND=wp), INTENT(OUT)   :: lu_tiles_lon_min_ecci(1:ntiles_ecci) 
   REAL(KIND=wp), INTENT(OUT)   :: lu_tiles_lon_max_ecci(1:ntiles_ecci)
   REAL(KIND=wp), INTENT(OUT)   :: lu_tiles_lat_min_ecci(1:ntiles_ecci)
   REAL(KIND=wp), INTENT(OUT)   :: lu_tiles_lat_max_ecci(1:ntiles_ecci)
   INTEGER(KIND=i4), INTENT(OUT):: nc_tiles_lu
   CHARACTER(len=2)    :: num
   CHARACTER(len=80)   :: path
   INTEGER(KIND=i4)    :: i, errorcode        ! i is a counter, errorcode is used to check if allocation was successful
   INTEGER(KIND=i4)    :: ncid
   INTEGER(KIND=i4)    :: dimID_lat, dimID_lon, varID_lat, varID_lon                  
   REAL(KIND=wp)       :: half_gridp          ! distance of half a grid point as the grid point is centered on a ECCI pixel
    
   half_gridp = 0.001388888889
!   print*, half_gridp

     DO i = 1,ntiles_ecci
       ! open ECCI file
       CALL check_netcdf(nf90_open(path =TRIM(raw_data_lu_path)//TRIM(raw_data_lu_filename(i)), mode = nf90_nowrite, ncid = ncid))
       CALL check_netcdf(nf90_inq_dimid(ncid,"lon", dimID_lon))
       CALL check_netcdf(nf90_inq_dimid(ncid,"lat", dimID_lat))
       CALL check_netcdf(nf90_inquire_dimension(ncid,dimID_lon, len = lu_tiles_ncolumns_ecci(i)))          
       CALL check_netcdf(nf90_inquire_dimension(ncid,dimID_lat, len = lu_tiles_nrows_ecci(i))) 
       CALL check_netcdf(nf90_inq_varid(ncid, "lon", varID_lon))
       CALL check_netcdf(nf90_inq_varid(ncid, "lat", varID_lat))
       CALL check_netcdf(nf90_get_var(ncid, varID_lon, lu_tiles_lon_min_ecci(i), start = (/1/)))            
       ! reads in the first longitude value of tile i
       CALL check_netcdf(nf90_get_var(ncid, varID_lon, lu_tiles_lon_max_ecci(i), start = (/lu_tiles_ncolumns_ecci(i)/))) 
       ! reads in the last longitude value of tile i
       CALL check_netcdf(nf90_get_var(ncid, varID_lat, lu_tiles_lat_max_ecci(i), start = (/1/)))            
       ! reads in the first latitude value of tile i
       CALL check_netcdf(nf90_get_var(ncid, varID_lat, lu_tiles_lat_min_ecci(i), start = (/lu_tiles_nrows_ecci(i)/))) 
       ! reads in the last latitude value of tile i
       CALL check_netcdf(nf90_close(ncid))  ! the netcdf file is closed again
       lu_tiles_lon_min_ecci(i) = lu_tiles_lon_min_ecci(i) - half_gridp !< half of a grid point must be
       lu_tiles_lon_max_ecci(i) = lu_tiles_lon_max_ecci(i) + half_gridp !< added, as the ECCI data
       lu_tiles_lat_min_ecci(i) = lu_tiles_lat_min_ecci(i) - half_gridp !< is located at the pixel center
       lu_tiles_lat_max_ecci(i) = lu_tiles_lat_max_ecci(i) + half_gridp
       
       len_lu_lon_ecci=lu_tiles_ncolumns_ecci(i)
       len_lu_lat_ecci=lu_tiles_nrows_ecci(i)
#ifdef DEBUG
       print*, 'ECCI TILE ',ntiles_ecci,': NLON,NLAT ',len_lu_lon,len_lu_lat
#endif
     ENDDO

     nc_tiles_lu = lu_tiles_ncolumns_ecci(1)


   END SUBROUTINE fill_ecci_data

! <mes


   SUBROUTINE  deallocate_landuse_data_ecci()

     USE mo_lu_tg_fields, ONLY: fr_land_lu,       &
                       &        ice_lu,           &
                       &        z0_lu,            &
                       &        z0_tot,           &
                       &        root_lu,          &
                       &        plcov_mn_lu,      &
                       &        plcov_mx_lu,      &
                       &        lai_mn_lu,        &
                       &        lai_mx_lu,        &
                       &        rs_min_lu,        &
                       &        urban_lu,         &
                       &        for_d_lu,         &
                       &        for_e_lu,         &
                       &        skinc_lu,         &
                       &        emissivity_lu,    &
                       &        fr_ocean_lu,      &
                       &        lu_class_fraction,&
                       &        lu_class_npixel,  &
                       &        lu_tot_npixel
     
     IMPLICIT NONE     

     INTEGER :: errorcode

       DEALLOCATE (lu_tiles_lon_min_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_tiles_lon_min_ecci')
       DEALLOCATE (lu_tiles_lon_max_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_tiles_lon_max_ecci')
       DEALLOCATE (lu_tiles_lat_min_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_tiles_lat_min_ecci')
       DEALLOCATE (lu_tiles_lat_max_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_tiles_lat_max_ecci')
       DEALLOCATE (lu_tiles_ncolumns_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_tiles_ncolumns_ecci')
       DEALLOCATE (lu_tiles_nrows_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_tiles_nrows_ecci')
       DEALLOCATE (lat_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lat_ecci')
       DEALLOCATE (lon_ecci, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lon_ecci')
       DEALLOCATE (fr_land_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector fr_land_lu')
       DEALLOCATE (ice_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector ice_lu')
       DEALLOCATE (z0_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector z0_lu')
       DEALLOCATE (z0_tot, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector z0_tot')
       DEALLOCATE (root_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector root_lu')
       DEALLOCATE (plcov_mn_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector plcov_mn_lu')
       DEALLOCATE (plcov_mx_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector plcov_mx_lu')
       DEALLOCATE (lai_mn_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lai_mn_lu')
       DEALLOCATE (lai_mx_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lai_mx_lu')
       DEALLOCATE (rs_min_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector rs_min_lu')
       DEALLOCATE (urban_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector urban_lu')
       DEALLOCATE (for_d_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector for_d_lu')
       DEALLOCATE (for_e_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector for_e_lu')
       DEALLOCATE (skinc_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector skinc_lu')
       DEALLOCATE (emissivity_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector emissivity_lu')
       DEALLOCATE (fr_ocean_lu, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector fr_ocean_lu')
       DEALLOCATE (lu_class_fraction, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_class_fraction')
       DEALLOCATE (lu_class_npixel, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_class_npixel')
       DEALLOCATE (lu_tot_npixel, STAT = errorcode)
       IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lu_tot_npixel')

     END SUBROUTINE deallocate_landuse_data_ecci


END MODULE mo_ecci_data

