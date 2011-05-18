!+ Fortran module for soil data on target grid for external parameters
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
!> Fortran module for soil data on target grid for external parameters 
!> \author Hermann Asensio
MODULE mo_soil_tg_fields

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i4
  USE mo_kind, ONLY: i8

  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_grid_structures, ONLY: target_grid_def

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: fr_land_soil
  PUBLIC :: soiltype_fao
  PUBLIC :: allocate_soil_target_fields

  REAL(KIND=wp), ALLOCATABLE  :: fr_land_soil(:,:,:) !< fraction land due to FAO Digital Soil map of the World

  INTEGER(KIND=i4), ALLOCATABLE :: soiltype_fao(:,:,:) !< soiltype due to FAO Digital Soil map of the World


  CONTAINS


  !> allocate fields for GLOBE target data 
  SUBROUTINE allocate_soil_target_fields(tg)

    TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description

    INTEGER :: errorcode !< error status variable

    ALLOCATE (fr_land_soil(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array fr_land_soil')
    fr_land_soil = 0.0
      
        
    ALLOCATE (soiltype_fao(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array soiltype_fao')
    soiltype_fao = 3  ! default value for soiltype is 'sand' (3)


  END SUBROUTINE allocate_soil_target_fields

END MODULE mo_soil_tg_fields

