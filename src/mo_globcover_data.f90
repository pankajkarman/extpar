!+ Fortran Module with data fields for the globcover data
!
! Current Code Owner: DWD, Hermann Asensio
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_3         2011/04/19 Hermann Asensio
!  Initial release
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran Module with data fields for the Globcover data
!> \author Hermann Asensio
!!
MODULE mo_globcover_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_GRID_structures, ONLY: reg_lonlat_grid

IMPLICIT NONE

PRIVATE

PUBLIC :: globcover_grid, &
          lon_globcover,  &
          lat_globcover,  &
          allocate_raw_globcover_fields

TYPE(reg_lonlat_grid) :: globcover_grid !< structure with defenition of the raw data grid for the whole globcover dataset

REAL (KIND=wp), ALLOCATABLE    :: lon_globcover(:) !< longitude of globcover raw data
REAL (KIND=wp), ALLOCATABLE    :: lat_globcover(:) !< latitude of globcover raw data


CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_globcover_fields(nrows,ncolumns)
  IMPLICIT NONE
  INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
  INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns

  INTEGER :: errorcode !< error status variable

    ALLOCATE (lon_globcover(1:ncolumns), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_globcover')
    lon_globcover = 0.0

     ALLOCATE (lat_globcover(1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_globcover')
    lat_globcover = 0.0

  END  SUBROUTINE allocate_raw_globcover_fields


END MODULE mo_globcover_data

