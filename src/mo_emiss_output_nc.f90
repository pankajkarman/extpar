!+ Fortran module for netcdf output of EMISS data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_2         2011/03/25 Hermann Asensio
!  small bug fix: remove some USE statements for unused global variables
! V1_3         2011/04/19 Hermann Asensio
!  change netcdf output:  time variable
! V1_4         2011/04/21 Anne Roche
!  clean up
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for netcdf output of EMISS data
!> \author Hermann Asensio
MODULE mo_emiss_output_nc

  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4

  USE mo_grid_structures,       ONLY: target_grid_def

  USE mo_io_utilities,          ONLY: netcdf_get_var

  USE mo_var_meta_data,         ONLY: dim_3d_tg, &
       &                              def_dimension_info_buffer, & 
       &                              def_com_target_fields_meta, &   
       &                              def_emiss_meta, & 
       &                              emiss_field_mom_meta, &
       &                              emiss_ratio_mom_meta, &
       &                              emiss_max_meta


  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_netcdf_buffer_emiss

  CONTAINS

  SUBROUTINE read_netcdf_buffer_emiss(netcdf_filename,  &
       &                              tg,         &
       &                              ntime, &
       &                              emiss_max,  &
       &                              emiss_field_mom,&
       &                              emiss_ratio_mom)


    CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
    TYPE(target_grid_def), INTENT(IN)  :: tg !< structure with target grid description
    INTEGER (KIND=i4), INTENT(INOUT)   :: ntime !< number of times of emiss data (12 monthly mean values)
    REAL (KIND=wp), INTENT(OUT)        :: emiss_max(:,:,:), &  !< field for emiss maximum
         &                                emiss_field_mom(:,:,:,:), &  !< field for monthly mean emiss data (12 months)
         &                                emiss_ratio_mom(:,:,:,:) !< field for monthly emiss ratio (12 months)

    CALL logging%info('Enter routine: read_netcdf_buffer_emiss')

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)
    ! dim_3d_tg
    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_3d_tg)
    ! lon_geo_meta and lat_geo_meta
    !define meta information for various EMISS data related variables for netcdf output
    CALL def_emiss_meta(ntime,dim_3d_tg)
    ! dim_emiss_tg, emiss_max_meta, emiss_field_mom_meta, emiss_ratio_mom_meta

    CALL netcdf_get_var(TRIM(netcdf_filename),emiss_max_meta,emiss_max)

    CALL netcdf_get_var(TRIM(netcdf_filename),emiss_field_mom_meta,emiss_field_mom)

    CALL netcdf_get_var(TRIM(netcdf_filename),emiss_ratio_mom_meta,emiss_ratio_mom)

    CALL logging%info('Exit routine: read_netcdf_buffer_emiss')

  END SUBROUTINE read_netcdf_buffer_emiss

END Module mo_emiss_output_nc
