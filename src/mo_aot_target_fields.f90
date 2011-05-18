!+ Fortran module for Aerosol optical thickness data, specification of the target grid fields
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

  USE mo_icon_domain, ONLY: icon_domain

  USE mo_grid_structures, ONLY: rotated_lonlat_grid
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid
  USE mo_grid_structures, ONLY: target_grid_def

  USE mo_io_utilities, ONLY: var_meta_info


IMPLICIT NONE

PRIVATE

PUBLIC :: allocate_aot_target_fields,&
          aot_tg, &
          meta_aot_tg
PUBLIC :: aot_buffer

REAL (KIND=wp), ALLOCATABLE :: aot_tg(:,:,:,:,:) !< aerosol optical thickness, aot_tg(ie,je,ke,ntime,ntype) 

TYPE(var_meta_info) :: meta_aot_tg !< meta information for variable aot_tg

!> data structure for buffer with aerosol optical thickness data on target grid
TYPE aot_buffer
   REAL (KIND=wp), ALLOCATABLE :: aot_tg(:,:,:,:,:) !< aerosol optical thickness, aot_tg(ie,je,ke,ntime,ntype) 
END TYPE aot_buffer


       

CONTAINS


!> allocate fields for TARGET grid
!!
!! the target grid for the GME has 3 dimension (ie,je,jd),
!! the target grid for the COSMO model has 2 dimension (ie,je)
!! the target grid for the ICON model has 1 dimension (ne)
!! depending of the target model the second and third dimension of the target fields should be 
!! allocated with the length 1
  subroutine allocate_aot_target_fields(tg, ntime, ntype)
  

    

    TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description
    INTEGER (KIND=i8), INTENT(IN) :: ntime !< number of times
    INTEGER (KIND=i8), INTENT(IN) :: ntype !< number of types of aerosols




    INTEGER :: errorcode !< error status variable

   
    allocate (aot_tg(1:tg%ie,1:tg%je,1:tg%ke,1:ntime,1:ntype), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array fr_land_glc2000')
    aot_tg = 0.0

    meta_aot_tg%varname = 'aot_tg'
    meta_aot_tg%n_dim = 5
     allocate (meta_aot_tg%diminfo(meta_aot_tg%n_dim), STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array meta_aot_tg%diminfo')

    meta_aot_tg%diminfo(1)%dimname = 'ie'
    meta_aot_tg%diminfo(1)%dimsize = tg%ie
    meta_aot_tg%diminfo(2)%dimname = 'je'
    meta_aot_tg%diminfo(2)%dimsize = tg%je
    meta_aot_tg%diminfo(3)%dimname = 'ke'
    meta_aot_tg%diminfo(3)%dimsize = tg%ke
    meta_aot_tg%diminfo(4)%dimname = 'ntime'
    meta_aot_tg%diminfo(4)%dimsize =  ntime
    meta_aot_tg%diminfo(5)%dimname = 'ntype'
    meta_aot_tg%diminfo(5)%dimsize = ntype
    meta_aot_tg%vartype = 2 ! REAL variable
    meta_aot_tg%standard_name = 'aot'
    meta_aot_tg%long_name = 'aerosol optical thickness'
    meta_aot_tg%units = ''







  end subroutine allocate_aot_target_fields




END Module mo_aot_target_fields

