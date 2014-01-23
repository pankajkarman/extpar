!+ Fortran Module with data fields for the globcover data
!
! Current Code Owner: DWD, Hermann Asensio
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_3         2011/04/19 Hermann Asensio
!  Initial release
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!! ECOCLIMAP option gs_08.03.12  programm not operational yet
MODULE mo_ecoclimap_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_GRID_structures, ONLY: reg_lonlat_grid

IMPLICIT NONE

PRIVATE

PUBLIC :: ecoclimap_grid, &
          lon_ecoclimap,  &
          lat_ecoclimap,  &
          ntime_ecoclimap, &
          allocate_raw_ecoclimap_fields, &
          deallocate_ecoclimap_fields

TYPE(reg_lonlat_grid) :: ecoclimap_grid !< structure with defenition of the raw data grid for the whole globcover dataset

REAL (KIND=wp), ALLOCATABLE    :: lon_ecoclimap(:) !< longitude of ecoclimap raw data
REAL (KIND=wp), ALLOCATABLE    :: lat_ecoclimap(:) !< latitude of ecoclimap raw data
INTEGER (KIND=i4)              :: ntime_ecoclimap = 12 !< number of timesteps (12 for monthly mean values)

CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_ecoclimap_fields(nrows,ncolumns)
  IMPLICIT NONE
  INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
  INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns

  INTEGER :: errorcode !< error status variable

    ALLOCATE (lon_ecoclimap(1:ncolumns), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_ecoclimap')
    lon_ecoclimap = 0.0

     ALLOCATE (lat_ecoclimap(1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_ecoclimap')
    lat_ecoclimap = 0.0

  END  SUBROUTINE allocate_raw_ecoclimap_fields

  SUBROUTINE deallocate_ecoclimap_fields()

    USE mo_ecoclimap_tg_fields, ONLY : fr_land_ecoclimap, &
                                       ecoclimap_class_fraction,    &
                                       ecoclimap_class_npixel, &
                                       ecoclimap_tot_npixel, &
                                       ice_ecoclimap, &
                                       z012_ecoclimap, &
                                       z012tot_ecoclimap, &
                                       root_ecoclimap, &
                                       plcov12_ecoclimap, &
                                       lai12_ecoclimap, &
                                       rs_min_ecoclimap, &
                                       urban_ecoclimap,  &
                                       for_d_ecoclimap,  &
                                       for_e_ecoclimap, &
                                       emissivity_ecoclimap

    IMPLICIT NONE

    INTEGER :: errorcode

    DEALLOCATE (lat_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lat_ecoclimap')
    DEALLOCATE (lon_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the vector lon_ecoclimap')
    DEALLOCATE (fr_land_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array fr_land_ecoclimap')
    DEALLOCATE (ecoclimap_tot_npixel, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array ecoclimap_tot_npixel')
    DEALLOCATE (ecoclimap_class_fraction, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array ecoclimap_class_fraction')
    DEALLOCATE (ecoclimap_class_npixel, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array ecoclimap_class_npixel')
    DEALLOCATE (ice_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array ice_ecoclimap')
    DEALLOCATE (z012_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array z012_ecoclimap')
    DEALLOCATE (z012tot_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array z012tot_ecoclimap')
    DEALLOCATE (root_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array root_ecoclimap')
    DEALLOCATE (plcov12_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array plcov12_ecoclimap')
    DEALLOCATE (lai12_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array lai12_ecoclimap')
    DEALLOCATE (rs_min_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array rs_min_ecoclimap')
    DEALLOCATE (urban_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array urban_ecoclimap')
    DEALLOCATE (for_e_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array for_e_ecoclimap')
    DEALLOCATE (for_d_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array for_d_ecoclimap')
    DEALLOCATE (emissivity_ecoclimap, STAT = errorcode)
    IF (errorcode.NE.0) CALL abort_extpar('Cant deallocate the array emissivity_ecoclimap')

  END SUBROUTINE deallocate_ecoclimap_fields
  


END MODULE mo_ecoclimap_data

