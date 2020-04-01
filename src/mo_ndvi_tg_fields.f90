!+ Fortran module for NDVI data on target grid for external parameters
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
!> Fortran module for NDVI data on target grid for external parameters
!> \author Hermann Asensio
MODULE mo_ndvi_tg_fields

  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4

  USE mo_grid_structures,       ONLY: target_grid_def

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: ndvi_field, &
       &    ndvi_max, &
       &    ndvi_field_mom, &
       &    ndvi_ratio_mom, &
       &    allocate_ndvi_target_fields


  REAL(KIND=wp), ALLOCATABLE  :: ndvi_field(:,:,:), & !< field for ndvi data
       &                         ndvi_max(:,:,:), & !< field for ndvi maximum
       &                         ndvi_field_mom(:,:,:,:), & !< field for monthly mean ndvi data (12 months)
       &                         ndvi_ratio_mom(:,:,:,:) !< field for monthly ndvi ratio (12 months)


  CONTAINS

  !> allocate fields for GLOBE target data
  SUBROUTINE allocate_ndvi_target_fields(tg,nt, l_use_array_cache)

    TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description
    INTEGER (KIND=i4), INTENT(IN)     :: nt !< number of timesteps (12 for monthly mean values)
    LOGICAL, INTENT(in)               :: l_use_array_cache
    
    INTEGER(KIND=i4)                  :: errorcode !< error status variable

    CALL logging%info('Enter routine: allocate_ndvi_target_fields')

if (l_use_array_cache) then
   call allocate_cached('ndvi_field', ndvi_field, [tg%ie,tg%je,tg%ke])
else
   allocate(ndvi_field(tg%ie,tg%je,tg%ke), stat=errorcode)
endif
    IF(errorcode.NE.0) CALL logging%error('Cant allocate the array ndvi_field',__FILE__,__LINE__)
    ndvi_field = 0.0

if (l_use_array_cache) then
   call allocate_cached('ndvi_max', ndvi_max, [tg%ie,tg%je,tg%ke])
else
   allocate(ndvi_max(tg%ie,tg%je,tg%ke), stat=errorcode)
endif
    IF(errorcode.NE.0) CALL logging%error('Cant allocate the array ndvi_max',__FILE__,__LINE__)
    ndvi_max = 0.0

if (l_use_array_cache) then
   call allocate_cached('ndvi_field_mom', ndvi_field_mom, [tg%ie,tg%je,tg%ke,nt])
else
   allocate(ndvi_field_mom(tg%ie,tg%je,tg%ke,nt), stat=errorcode)
endif
    IF(errorcode.NE.0) CALL logging%error('Cant allocate the array ndvi_field_mom',__FILE__,__LINE__)
    ndvi_field_mom = 0.0

if (l_use_array_cache) then
   call allocate_cached('ndvi_ratio_mom', ndvi_ratio_mom, [tg%ie,tg%je,tg%ke,nt])
else
   allocate(ndvi_ratio_mom(tg%ie,tg%je,tg%ke,nt), stat=errorcode)
endif
    IF(errorcode.NE.0) CALL logging%error('Cant allocate the array ndvi_ratio_mom',__FILE__,__LINE__)
    ndvi_ratio_mom = 0.0

  END SUBROUTINE allocate_ndvi_target_fields

END MODULE mo_ndvi_tg_fields
