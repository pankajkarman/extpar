!+ Fortran module for netcdf output of Albedo data
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
!> Fortran module for netcdf output of Albedo data
!> \author Hermann Asensio, Frank Brenner
MODULE mo_albedo_output_nc

  !> kind parameters are defined in MODULE data_parameters
  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4

  USE mo_var_meta_data,         ONLY: dim_3d_tg, &
    &                                 def_dimension_info_buffer, &
    &                                 def_com_target_fields_meta, &
    &                                 alb_field_mom_meta, &
    &                                 alnid_field_mom_meta, &
    &                                 aluvd_field_mom_meta, &
    &                                 alb_dry_meta, &
    &                                 alb_sat_meta, &
    &                                 def_alb_meta

  USE mo_grid_structures,       ONLY: target_grid_def

  USE mo_io_utilities,          ONLY: netcdf_get_var

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_netcdf_buffer_alb

  CONTAINS

  SUBROUTINE read_netcdf_buffer_alb(netcdf_filename,  &
   &                                     tg,         &
   &                                     ntime, &
   &                                     alb_field_mom, &
   &                                     alnid_field_mom, &
   &                                     aluvd_field_mom, &
   &                                     alb_dry, &
   &                                     alb_sat)



    CHARACTER (len=*), INTENT(IN)         :: netcdf_filename !< filename for the netcdf file
    TYPE(target_grid_def), INTENT(IN)     :: tg !< structure with target grid description
    INTEGER (KIND=i4), INTENT(OUT)        :: ntime !< number of times of input data (12 monthly mean values)
    REAL (KIND=wp), INTENT(OUT), OPTIONAL :: alb_field_mom(:,:,:,:), & !< field for monthly mean albedo data (12 months)
      &                                      alnid_field_mom(:,:,:,:), &
      &                                      aluvd_field_mom(:,:,:,:), &
      &                                      alb_dry(:,:,:), &
      &                                      alb_sat(:,:,:)

    CALL logging%info('Enter routine: read_netcdf_buffer_alb')
    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)

    ! dim_3d_tg
    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_3d_tg)
    ! lon_geo_meta and lat_geo_meta

    !define albedo meta information, related variables for netcdf output
    CALL def_alb_meta(ntime,dim_3d_tg)

    IF (PRESENT(alb_field_mom)) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),alb_field_mom_meta,alb_field_mom)
    ENDIF
    IF (PRESENT(alnid_field_mom)) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),alnid_field_mom_meta,alnid_field_mom)
    ENDIF
    IF (PRESENT(aluvd_field_mom)) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),aluvd_field_mom_meta,aluvd_field_mom)
    ENDIF

    IF (PRESENT(alb_dry)) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),alb_dry_meta,alb_dry)
    ENDIF
    IF (PRESENT(alb_sat)) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),alb_sat_meta,alb_sat)
    ENDIF

    CALL logging%info('Exit routine: read_netcdf_buffer_alb')

  END SUBROUTINE read_netcdf_buffer_alb

END MODULE mo_albedo_output_nc
