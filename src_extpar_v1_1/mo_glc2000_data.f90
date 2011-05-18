!+ Fortran Module with data fields for the GLC2000 data
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
!> Fortran Module with data fields for the GLC2000 data
!> \author Hermann Asensio
!!
MODULE mo_glc2000_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_GRID_structures, ONLY: reg_lonlat_grid
                           
IMPLICIT NONE

PRIVATE

PUBLIC :: glc2000_grid, &
          lon_glc2000,  &
          lat_glc2000,  &
          allocate_raw_glc2000_fields
          



TYPE(reg_lonlat_grid) :: glc2000_grid !< structure with defenition of the raw data grid for the whole GLC2000 dataset

REAL (KIND=wp), ALLOCATABLE    :: lon_glc2000(:) !< longitude of glc2000 raw data
REAL (KIND=wp), ALLOCATABLE    :: lat_glc2000(:) !< latitude of glc2000 raw data


CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_glc2000_fields(nrows,ncolumns)
  IMPLICIT NONE
  INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
  INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns

  INTEGER :: errorcode !< error status variable


    ALLOCATE (lon_glc2000(1:ncolumns), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_glc2000')
    lon_glc2000 = 0.0

     ALLOCATE (lat_glc2000(1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_glc2000')
    lat_glc2000 = 0.0



  END  SUBROUTINE allocate_raw_glc2000_fields


END MODULE mo_glc2000_data
