!+ Fortran Module with data fields for the GLCC data
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
!> Fortran Module with data fields for the GLCC data
!> \author Hermann Asensio
!!
MODULE mo_glcc_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_grid_structures, ONLY: reg_lonlat_grid
                           
IMPLICIT NONE

PRIVATE

PUBLIC :: glcc_grid, &
 &         lon_glcc,  &
 &         lat_glcc,  &
 &         allocate_raw_glcc_fields
          



TYPE(reg_lonlat_grid) :: glcc_grid !< structure with defenition of the raw data grid for the whole GLCC dataset

REAL (KIND=wp), ALLOCATABLE    :: lon_glcc(:) !< longitude of glcc raw data
REAL (KIND=wp), ALLOCATABLE    :: lat_glcc(:) !< latitude of glcc raw data


CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_glcc_fields(nrows,ncolumns)
  IMPLICIT NONE
  INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
  INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns

  INTEGER :: errorcode !< error status variable


    ALLOCATE (lon_glcc(1:ncolumns), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_glcc')
    lon_glcc = 0.0

     ALLOCATE (lat_glcc(1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_glcc')
    lat_glcc = 0.0



  END  SUBROUTINE allocate_raw_glcc_fields


END MODULE mo_glcc_data
