!+ Fortran module for data specification of target grid for external parameters
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
!> Fortran module for data specification of target grid for external parameters
!> \author Hermann Asensio
MODULE mo_target_grid_data

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4


  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_grid_structures, ONLY: target_grid_def


  IMPLICIT NONE

  PRIVATE

  PUBLIC :: lon_geo
  PUBLIC :: lat_geo
  PUBLIC :: no_raw_data_pixel
  PUBLIC :: tg
  PUBLIC :: allocate_com_target_fields


  REAL (KIND=wp), ALLOCATABLE  :: lon_geo(:,:,:)          !< longitude coordinates of the target grid in the geographical system 
  REAL (KIND=wp), ALLOCATABLE  :: lat_geo(:,:,:)          !< latitude coordinates of the target grid in the geographical system

  INTEGER (KIND=i8), ALLOCATABLE :: no_raw_data_pixel(:,:,:) !< number of raw data pixel which are inside the target grid element

  TYPE(target_grid_def) :: tg !< structure with target grid description


CONTAINS

  !> allocate common fields for target grid
  SUBROUTINE allocate_com_target_fields(tg)
    TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description 

    !local variables
    INTEGER :: errorcode
    INTEGER :: n !< counter
     
    ALLOCATE (lon_geo(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_geo')
    lon_geo = 0.0

    ALLOCATE (lat_geo(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_geo')
    lat_geo = 0.0


    ALLOCATE (no_raw_data_pixel(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array no_raw_data_pixel')
    no_raw_data_pixel = 0

  END SUBROUTINE  allocate_com_target_fields

END Module mo_target_grid_data

