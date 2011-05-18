!+ Fortran Module with data fields for the flake data
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
!> Fortran Module with data fields for the flake data
!> \author Hermann Asensio
!!
MODULE mo_flake_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_grid_structures, ONLY: reg_lonlat_grid
                           
IMPLICIT NONE

PRIVATE

PUBLIC :: flake_grid, &
 &         lon_flake,  &
 &         lat_flake,  &
 &         allocate_raw_flake_fields

PUBLIC :: flake_depth_undef, flake_depth_default, DWD_max_lake_depth
          

TYPE(reg_lonlat_grid) :: flake_grid !< structure with defenition of the raw data grid for the whole GLCC dataset

REAL (KIND=wp), ALLOCATABLE    :: lon_flake(:) !< longitude of flake raw data
REAL (KIND=wp), ALLOCATABLE    :: lat_flake(:) !< latitude of flake raw data

REAL (KIND=wp), PARAMETER :: flake_depth_undef = -1. !< default value for undefined lake depth
REAL (KIND=wp), PARAMETER :: flake_depth_default = 10.0 !< default value for default lake depth, 10 [m]
REAL (KIND=wp), PARAMETER :: DWD_max_lake_depth = 50.0 !< Maximum lake depth in [m] for FLAKE




CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_flake_fields(nrows,ncolumns)
  IMPLICIT NONE
  INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
  INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns

  INTEGER :: errorcode !< error status variable
    ALLOCATE (lon_flake(1:ncolumns), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_flake')
    lon_flake = 0.0

     ALLOCATE (lat_flake(1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_flake')
    lat_flake = 0.0

  END  SUBROUTINE allocate_raw_flake_fields

END MODULE mo_flake_data
