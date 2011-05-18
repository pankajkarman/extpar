!+  Fortran module for CRU near surface climatology data on target grid for external parameters
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
!> Fortran module for CRU near surface climatology data on target grid for external parameters 
!> \author Hermann Asensio
MODULE mo_cru_target_fields

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4


  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_grid_structures, ONLY: target_grid_def

  USE mo_io_utilities, ONLY: var_meta_info

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: allocate_cru_target_fields,&
    &       crutemp, &
    &       meta_crutemp

  REAL (KIND=wp), ALLOCATABLE :: crutemp(:,:,:) !< cru climatological temperature , crutemp(ie,je,ke) 

  TYPE(var_meta_info) :: meta_crutemp !< meta information for variable crutemp


  CONTAINS


  !> allocate fields for TARGET grid
  !!
  !! the target grid for the GME has 3 dimension (ie,je,jd),
  !! the target grid for the COSMO model has 2 dimension (ie,je)
  !! the target grid for the ICON model has 1 dimension (ne)
  !! depending of the target model the second and third dimension of the target fields should be 
  !! allocated with the length 1
  SUBROUTINE allocate_cru_target_fields(tg)
    TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description
    INTEGER :: errorcode !< error status variable

    ALLOCATE (crutemp(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array crutemp')
    crutemp = 0.0

    meta_crutemp%varname = 'tem_clim'
    meta_crutemp%n_dim = 3
    ALLOCATE (meta_crutemp%diminfo(meta_crutemp%n_dim), STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array meta_crutemp%diminfo')

    meta_crutemp%diminfo(1)%dimname = 'ie'
    meta_crutemp%diminfo(1)%dimsize = tg%ie
    meta_crutemp%diminfo(2)%dimname = 'je'
    meta_crutemp%diminfo(2)%dimsize = tg%je
    meta_crutemp%diminfo(3)%dimname = 'ke'
    meta_crutemp%diminfo(3)%dimsize = tg%ke
    meta_crutemp%vartype = 2 ! REAL variable
    meta_crutemp%standard_name = 'CRU T'
    meta_crutemp%long_name = 'CRU near surface temperature climatology'
    meta_crutemp%units = ''

  END SUBROUTINE allocate_cru_target_fields


END MODULE mo_cru_target_fields

