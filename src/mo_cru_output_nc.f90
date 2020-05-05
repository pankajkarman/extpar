!+ Fortran module with netcdf output routines for CRU data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_4         2011/04/21 Hermann Asensio
!  Hermann Asensio
! V2_0         2013/06/04 Martina Messmer
!  add a new parameter for CRU temperature elevation (CLM Community)
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with netcdf output routines for CRU data
!> \author Hermann Asensio
MODULE mo_cru_output_nc
  
  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4
                                
  USE mo_io_utilities,          ONLY: netcdf_get_var

  USE mo_var_meta_data,        ONLY: dim_3d_tg,                  &
       &                             def_dimension_info_buffer,  &
       &                             crutemp_meta,               &
       &                             def_crutemp_meta,           &
       &                             cruelev_meta,               &
       &                             def_cruelev_meta

  USE mo_grid_structures,      ONLY: target_grid_def

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_netcdf_buffer_cru

  CONTAINS

  SUBROUTINE read_netcdf_buffer_cru(netcdf_filename,  &
       &                            tg,         &
       &                            crutemp,    &
       &                            cruelev)

    CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
    TYPE(target_grid_def), INTENT(IN)  :: tg !< structure with target grid description
    REAL(KIND=wp), INTENT(OUT)         :: crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke)
    REAL(KIND=wp), OPTIONAL,INTENT(OUT):: cruelev(:,:,:)  !< cru elevation , cruelev(ie,je,ke)

    CALL logging%info('Enter routine: read_netcdf_buffer_cru')

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)

    ! define meta information for variable crutemp for netcdf output
    CALL def_crutemp_meta(dim_3d_tg)
    ! crutemp_meta

    CALL netcdf_get_var(TRIM(netcdf_filename),crutemp_meta,crutemp)

    IF(PRESENT(cruelev)) THEN
      CALL def_cruelev_meta(dim_3d_tg)
      CALL netcdf_get_var(TRIM(netcdf_filename),cruelev_meta,cruelev)
    ENDIF

    CALL logging%info('Exit routine: read_netcdf_buffer_cru')
  END SUBROUTINE read_netcdf_buffer_cru

END MODULE mo_cru_output_nc
