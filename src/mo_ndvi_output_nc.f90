!+ Fortran module for netcdf output of NDVI data
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
!> Fortran module for netcdf output of NDVI data
!> \author Hermann Asensio
MODULE mo_ndvi_output_nc

  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4

  USE mo_grid_structures,       ONLY: target_grid_def

  USE mo_io_utilities,          ONLY: netcdf_get_var

  USE mo_var_meta_data,         ONLY: dim_3d_tg, &
       &                              def_dimension_info_buffer, &
       &                              def_com_target_fields_meta, &  
       &                              ndvi_max_meta, &
       &                              ndvi_field_mom_meta, &
       &                              ndvi_ratio_mom_meta, &
       &                              def_ndvi_meta
                               
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_netcdf_buffer_ndvi

  CONTAINS

  SUBROUTINE read_netcdf_buffer_ndvi(netcdf_filename,  &
       &                             tg,         &
       &                             ntime, &
       &                             ndvi_max,  &
       &                             ndvi_field_mom,&
       &                             ndvi_ratio_mom)


    CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
    TYPE(target_grid_def), INTENT(IN)  :: tg !< structure with target grid description
    INTEGER (KIND=i4), INTENT(INOUT)   :: ntime !< number of times of ndvi data (12 monthly mean values)

    REAL (KIND=wp), INTENT(OUT)        :: ndvi_max(:,:,:), & !< field for ndvi maximum
         &                                ndvi_field_mom(:,:,:,:), & !< field for monthly mean ndvi data (12 months)
         &                                ndvi_ratio_mom(:,:,:,:) !< field for monthly ndvi ratio (12 months)

    CALL logging%info('Enter routine: read_netcdf_buffer_ndvi')

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)
    ! dim_3d_tg
    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_3d_tg)
    ! lon_geo_meta and lat_geo_meta
    !define meta information for various NDVI data related variables for netcdf output
    CALL def_ndvi_meta(ntime,dim_3d_tg)
    ! dim_ndvi_tg, ndvi_max_meta, ndvi_field_mom_meta, ndvi_ratio_mom_meta

    CALL netcdf_get_var(TRIM(netcdf_filename),ndvi_max_meta,ndvi_max)

    CALL netcdf_get_var(TRIM(netcdf_filename),ndvi_field_mom_meta,ndvi_field_mom)

    CALL netcdf_get_var(TRIM(netcdf_filename),ndvi_ratio_mom_meta,ndvi_ratio_mom)
    
    CALL logging%info('Exit routine: read_netcdf_buffer_ndvi')

   END SUBROUTINE read_netcdf_buffer_ndvi

END MODULE mo_ndvi_output_nc
