!+ Fortran module for NDVI data on target grid for external parameters
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_8         2013-03-12 Frank Brenner
!  introduced MODIS albedo dataset(s) as new external parameter(s) 
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for albedo data on target grid for external parameters 
!> \author Frank Brenner, Hermann Asensio
MODULE mo_albedo_tg_fields

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i4
  USE mo_kind, ONLY: i8

  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_grid_structures, ONLY: target_grid_def


  IMPLICIT NONE

  PRIVATE

  PUBLIC :: alb_field, &
            alnid_field, &
            aluvd_field, &
            alb_field_mom, &
            alnid_field_mom, &
            aluvd_field_mom, &
            allocate_alb_target_fields, &
            alb_interpol


         REAL(KIND=wp), ALLOCATABLE  :: alb_field(:,:,:) !< field for albedo data
         REAL(KIND=wp), ALLOCATABLE  :: alnid_field(:,:,:)
         REAL(KIND=wp), ALLOCATABLE  :: aluvd_field(:,:,:)

         REAL(KIND=wp), ALLOCATABLE  :: alb_field_mom(:,:,:,:) !< field for monthly mean albedo data (12 months)
         REAL(KIND=wp), ALLOCATABLE  :: alnid_field_mom(:,:,:,:)
         REAL(KIND=wp), ALLOCATABLE  :: aluvd_field_mom(:,:,:,:)

         REAL(KIND=wp), ALLOCATABLE :: alb_interpol(:,:,:,:) !<  field for interpolated albedo

  CONTAINS

  !> allocate fields for GLOBE target data 
    SUBROUTINE allocate_alb_target_fields(tg,nt)
      IMPLICIT NONE

      TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description
      INTEGER (KIND=i4), INTENT(in) :: nt !< number of timesteps (12 for monthly mean values)

      INTEGER :: errorcode !< error status variable
        
      ALLOCATE (alb_field(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
          IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array alb_field')
      alb_field = 0.07

      ALLOCATE (alnid_field(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
          IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array alnid_field')
      alnid_field = 0.07

      ALLOCATE (aluvd_field(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
          IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array aluvd_field')
      aluvd_field = 0.07

       ALLOCATE (alb_field_mom(1:tg%ie,1:tg%je,1:tg%ke,1:nt), STAT=errorcode)
          IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array alb_field_mom')
      alb_field_mom = 0.0

       ALLOCATE (alnid_field_mom(1:tg%ie,1:tg%je,1:tg%ke,1:nt), STAT=errorcode)
          IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array alnid_field_mom')
      alnid_field_mom = 0.0

       ALLOCATE (aluvd_field_mom(1:tg%ie,1:tg%je,1:tg%ke,1:nt), STAT=errorcode)
          IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array aluvd_field_mom')
      aluvd_field_mom = 0.0

       ALLOCATE (alb_interpol(1:tg%ie,1:tg%je,1:tg%ke,1:nt), STAT=errorcode)
          IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array alb_interpol')
      alb_interpol = 0.0


    END SUBROUTINE allocate_alb_target_fields

END Module mo_albedo_tg_fields
