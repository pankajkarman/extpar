!+ Fortran module with data fields for NDVI data
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
!> Fortran module with data fields for NDVI data
!> \author Hermann Asensio
MODULE mo_ndvi_data

  USE mo_logging
  USE mo_kind,                  ONLY: wp,i4

  USE mo_grid_structures,       ONLY: reg_lonlat_grid

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: undef_ndvi, minimal_ndvi, ntime_ndvi

  !< longitide coordinates of the soil grid in the geographical (lonlat) system, dimension (nlon_reg)

  INTEGER (KIND=i4)            :: ntime_ndvi = 12 !< number of timesteps (12 for monthly mean values)

  REAL (KIND=wp)               :: undef_ndvi = 0.0, &  !< undefined value for NDVI data
       &                          minimal_ndvi = 0.09 !< minimal NDVI value bare soil value

END MODULE mo_ndvi_data
