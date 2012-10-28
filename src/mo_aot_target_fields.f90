!+ Fortran module for Aerosol optical thickness data, specification of the target grid fields
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_3         2011/04/19 Hermann Asensio
!  cleanup of code
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for Aerosol optical thickness data, specification of the target grid fields 
!> \author Hermann Asensio
!

MODULE mo_aot_target_fields

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_grid_structures, ONLY: target_grid_def


IMPLICIT NONE

PRIVATE

PUBLIC :: allocate_aot_target_fields
PUBLIC :: aot_tg

REAL (KIND=wp), ALLOCATABLE :: aot_tg(:,:,:,:,:) !< aerosol optical thickness, aot_tg(ie,je,ke,,ntype,ntime) 


CONTAINS

!> allocate fields for TARGET grid
!!
!! the target grid for the GME has 3 dimension (ie,je,jd),
!! the target grid for the COSMO model has 2 dimension (ie,je)
!! the target grid for the ICON model has 1 dimension (ne)
!! depending of the target model the second and third dimension of the target fields should be 
!! allocated with the length 1
  SUBROUTINE allocate_aot_target_fields(tg, ntime, ntype)
  
    TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description
    INTEGER (KIND=i8), INTENT(IN) :: ntime !< number of times
    INTEGER (KIND=i8), INTENT(IN) :: ntype !< number of types of aerosols

    INTEGER :: errorcode !< error status variable
   
    ALLOCATE (aot_tg(1:tg%ie,1:tg%je,1:tg%ke,1:ntype,1:ntime), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array fr_land_glc2000')
    aot_tg = 0.0

  END SUBROUTINE allocate_aot_target_fields


END MODULE mo_aot_target_fields

