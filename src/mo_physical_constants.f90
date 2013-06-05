!+ Module determines physical constants to be used by ICON
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
!>
!!  Module determines physical constants to be used by ICON
!! 
!!  Values are taken from Williamson et al (1992).
!! 
!! @par Revision History
!!  Developed  by Luis Kornblueh and Luca Bonaventura (2002-3)
!!  Modified to ProTeX-style by  Luca Bonaventura and Thomas Heinze (2004).
!!  Modified according to style guide by Thomas Heinze (2005-06-24):
!!   - module renamed from mo_constants to mo_physical_constants
!!   - eps moved to mo_math_constants
!!   - su0 renamed to u0 (as in Williamson et al. (1992) paper)
!!  Adding units to comments by Thomas Heinze (2005-07-26):
!!  Modification by Thomas Heinze (2006-02-21):
!!  - renamed m_modules to mo_modules
!!  Modification by Hui Wan (2007-01-12):
!!  - added more constants from ECHAM5
!!  Modification by Hui Wan (2007-01-16):
!!  - parameter u0 moved to <i>mo_sw_testcases</i>
!! 
MODULE mo_physical_constants

  USE mo_kind,            ONLY: wp 

  IMPLICIT NONE

! !VERSION CONTROL:
  CHARACTER(len=*), PARAMETER, PRIVATE :: version = '$Id: mo_physical_constants.f90,v 1.11 2013-04-16 11:09:20 for0adm Exp $'

  PUBLIC

 ! Physical constants

  REAL (wp), PARAMETER :: re      = 6.371229e6_wp  ! av. radius of the earth [m]
  REAL (wp), PARAMETER :: rre     = 1._wp/re
  REAL (wp), PARAMETER :: grav    = 9.80616_wp     ! av. gravitational const.[m/s^2]
  REAL (wp), PARAMETER :: rgrav   = 1._wp/grav
  REAL (wp), PARAMETER :: omega   = 7.29212e-5_wp  ! angular velocity of earth [1/s]


  REAL (wp), PARAMETER :: argas = 8.314409_wp     ! universal gas constant in J/K/mol
  REAL (wp), PARAMETER :: avo   = 6.022045e23_wp  ! Avogadro constant in 1/mol
  REAL (wp), PARAMETER :: ak    = 1.380662e-23_wp ! Boltzmann constant in J/K
  REAL (wp), PARAMETER :: stbo  = 5.67E-8_wp      ! Stephan-Boltzmann constant in W/m2/K4

! ! Molar weights in g/mol

  REAL (wp), PARAMETER :: amco2 = 44.011_wp      ! molecular weight of carbon dioxide
  REAL (wp), PARAMETER :: amch4 = 16.043_wp      ! molecular weight of methane
  REAL (wp), PARAMETER :: amo3  = 47.9982_wp     ! molecular weight of ozone
  REAL (wp), PARAMETER :: amn2o = 44.013_wp      ! molecular weight of N2O
  REAL (wp), PARAMETER :: amc11 =137.3686_wp     ! molecular weight of CFC11
  REAL (wp), PARAMETER :: amc12 =120.9140_wp     ! molecular weight of CFC12
  REAL (wp), PARAMETER :: amw   = 18.0154_wp     ! molecular weight of water vapor
  REAL (wp), PARAMETER :: amd   = 28.970_wp      ! molecular weight of dry air

! ! Dry air and water vapour thermodynamic constants

  REAL (wp), PARAMETER :: cpd   = 1004.64_wp     ! specific heat of dry air at constant
                                                 ! pressure in J/K/kg
  REAL (wp), PARAMETER :: cpv   = 1869.46_wp     ! specific heat of water vapour at
                                                 ! constant pressure in J/K/kg
  REAL (wp), PARAMETER :: rd    = 287.04_wp      ! gas constant for dry air in J/K/kg
  REAL (wp), PARAMETER :: rv    = 461.51_wp      ! gas constant for water vapour
                                                 ! in J/K/kg
  REAL (wp), PARAMETER :: rcpd  = 1._wp/cpd      ! auxiliary constant in K*kg/J
  REAL (wp)            :: vtmpc1= rv/rd-1._wp    ! dimensionless auxiliary constant
  REAL (wp)            :: vtmpc2= cpv/cpd-1._wp  ! dimensionless auxiliary constant
  REAL (wp), PARAMETER :: p0sl  = 100000.0_wp    ! reference pressure for Exner  
                                                 ! function (Pa)
  REAL (wp)            :: cvd   = cpd-rd         ! specific heat of dry air at constant
                                                 ! volume in J/K/kg
  REAL (wp)            :: rd_o_cpd = rd/cpd      ! rd/cpd 
  REAL (wp)            :: cvd_o_rd = (cpd-rd)/rd ! cvd/rd

! ! H2O related constants, liquid density, phase change constants

  REAL (wp), PARAMETER :: rhoh2o= 1000._wp       ! density of liquid water in kg/m3
  REAL (wp), PARAMETER :: alv   = 2.5008e6_wp    ! latent heat for vaporisation in J/kg
  REAL (wp), PARAMETER :: als   = 2.8345e6_wp    ! latent heat for sublimation in J/kg
  REAL (wp), PARAMETER :: alf   = als-alv        ! latent heat for fusion in J/kg
  REAL (wp), PARAMETER :: clw   = 4186.84_wp     ! specific heat for liquid waterJ/K/kg
  REAL (wp), PARAMETER :: tmelt = 273.15_wp      ! melting temperature of ice/snow

! !Constants used for computation of saturation mixing ratio
!  over liquid water (*c_les*) or ice(*c_ies*)

  REAL (wp), PARAMETER :: c1es  = 610.78_wp              !
  REAL (wp), PARAMETER :: c2es  = c1es*rd/rv             !
  REAL (wp), PARAMETER :: c3les = 17.269_wp              !
  REAL (wp), PARAMETER :: c3ies = 21.875_wp              !
  REAL (wp), PARAMETER :: c4les = 35.86_wp               !
  REAL (wp), PARAMETER :: c4ies = 7.66_wp                !
  REAL (wp), PARAMETER :: c5les = c3les*(tmelt-c4les)    !
  REAL (wp), PARAMETER :: c5ies = c3ies*(tmelt-c4ies)    !
  REAL (wp), PARAMETER :: c5alvcp = c5les*alv/cpd        !
  REAL (wp), PARAMETER :: c5alscp = c5ies*als/cpd        !
  REAL (wp), PARAMETER :: alvdcp  = alv/cpd              !
  REAL (wp), PARAMETER :: alsdcp  = als/cpd              !
  

!EOP  
!--------------------------------------------------------------------  
!BOC  

END MODULE mo_physical_constants

!EOC 
