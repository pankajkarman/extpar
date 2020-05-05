 !+ Fortran module for NDVI data on target grid for external parameters
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_8         2013/03/12 Frank Brenner
!  introduced MODIS albedo dataset(s) as new external parameter(s)
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for albedo data on target grid for external parameters
!> \author Frank Brenner, Hermann Asensio
MODULE mo_albedo_tg_fields

  !> kind parameters are defined in MODULE data_parameters
  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4
  USE mo_array_cache,           ONLY: allocate_cached
  USE mo_grid_structures,       ONLY: target_grid_def


  IMPLICIT NONE

  PRIVATE

  PUBLIC :: alb_dry, &
    &       alb_sat, &
    &       alb_field_mom, &
    &       alnid_field_mom, &
    &       aluvd_field_mom, &
    &       allocate_alb_target_fields, &
    &       alb_interpol



  REAL(KIND=wp), POINTER      :: alb_field_mom(:,:,:,:), & !< field for monthly mean albedo data (12 months)
    &                            alnid_field_mom(:,:,:,:), &
    &                            aluvd_field_mom(:,:,:,:), &
    &                            alb_interpol(:,:,:,:), & !<  field for interpolated albedo
    &                            alb_dry(:,:,:), & !< field for dry soil albedo
    &                            alb_sat(:,:,:) !< field for saturated soil albedo


  CONTAINS

  !> allocate fields for albedo target data
  SUBROUTINE allocate_alb_target_fields(tg,nt,raw_id, l_use_array_cache)

    TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description
    INTEGER (KIND=i4), INTENT(IN)     :: nt, & !< number of timesteps (12 for monthly mean values)
      &                                  raw_id !< type of albedo treatment
    LOGICAL, INTENT(in)               :: l_use_array_cache
    
    INTEGER(KIND=i4)                  :: errorcode !< error status variable

    CALL logging%info('Enter routine: allocate_alb_target_fields')

    IF (l_use_array_cache) THEN
       CALL allocate_cached('alb_field_mom', alb_field_mom, [tg%ie,tg%je,tg%ke,nt])
    ELSE
       allocate(alb_field_mom(tg%ie,tg%je,tg%ke,nt), stat=errorcode)
    ENDIF
    IF(errorcode.NE.0) CALL logging%error('Cant allocate array alb_field_mom',__FILE__,__LINE__)
    alb_field_mom = 0.0

  !> the following fields are always used in the interface write_netcdf_cosmo_grid_extpar
  !> and must be allocated even IF not used
    IF (raw_id == 2) THEN
      IF (l_use_array_cache) THEN
         CALL allocate_cached('alb_dry', alb_dry, [tg%ie,tg%je,tg%ke])
      ELSE
         allocate(alb_dry(tg%ie,tg%je,tg%ke), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array alb_dry',__FILE__,__LINE__)
      alb_dry = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('alb_sat', alb_sat, [tg%ie,tg%je,tg%ke])
      ELSE
         allocate(alb_sat(tg%ie,tg%je,tg%ke), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array alb_sat',__FILE__,__LINE__)
      alb_sat = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('alnid_field_mom', alnid_field_mom, [0,0,0,0])
      ELSE
         allocate(alnid_field_mom(0,0,0,0), stat=errorcode)
      ENDIF
        IF(errorcode.NE.0) CALL logging%error('Cant allocate array alnid_field_mom',__FILE__,__LINE__)
      alnid_field_mom = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('aluvd_field_mom', aluvd_field_mom, [0,0,0,0])
      ELSE
         allocate(aluvd_field_mom(0,0,0,0), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array aluvd_field_mom',__FILE__,__LINE__)
      aluvd_field_mom = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('alb_interpol', alb_interpol, [0,0,0,0])
      ELSE
         allocate(alb_interpol(0,0,0,0), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array alb_interpol',__FILE__,__LINE__)
      alb_interpol = 0.0

    ELSE

      IF (l_use_array_cache) THEN
         CALL allocate_cached('alb_dry', alb_dry, [0,0,0])
      ELSE
         allocate(alb_dry(0,0,0), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array alb_dry',__FILE__,__LINE__)
      alb_dry = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('alb_sat', alb_sat, [0,0,0])
      ELSE
         allocate(alb_sat(0,0,0), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array alb_sat',__FILE__,__LINE__)
      alb_sat = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('alnid_field_mom', alnid_field_mom, [tg%ie,tg%je,tg%ke,nt])
      ELSE
         allocate(alnid_field_mom(tg%ie,tg%je,tg%ke,nt), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array alnid_field_mom',__FILE__,__LINE__)
      alnid_field_mom = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('aluvd_field_mom', aluvd_field_mom, [tg%ie,tg%je,tg%ke,nt])
      ELSE
         allocate(aluvd_field_mom(tg%ie,tg%je,tg%ke,nt), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array aluvd_field_mom',__FILE__,__LINE__)
      aluvd_field_mom = 0.0

      IF (l_use_array_cache) THEN
         CALL allocate_cached('alb_interpol', alb_interpol, [tg%ie,tg%je,tg%ke,nt])
      ELSE
         allocate(alb_interpol(tg%ie,tg%je,tg%ke,nt), stat=errorcode)
      ENDIF
      IF(errorcode.NE.0) CALL logging%error('Cant allocate array alb_interpol',__FILE__,__LINE__)
      alb_interpol = 0.0
    ENDIF

  END SUBROUTINE allocate_alb_target_fields

END MODULE mo_albedo_tg_fields
