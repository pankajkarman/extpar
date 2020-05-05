!+ Fortran module with data fields for EMISS data
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
!> Fortran module with data fields for EMISS data
!> \author Hermann Asensio
MODULE mo_emiss_data

  USE mo_logging
  USE mo_kind,                  ONLY: wp,i4

  IMPLICIT NONE

  PUBLIC :: ntime_emiss,minimal_emiss, undef_emiss
                           
  INTEGER (KIND=i4)            :: ntime_emiss = 12 !< number of timesteps (12 for monthly mean values)

  REAL (KIND=wp)               :: undef_emiss = 0.0, &   !< undefined value for EMISS data
       &                          minimal_emiss = 0.0 !< minimal EMISS value 

END MODULE mo_emiss_data
