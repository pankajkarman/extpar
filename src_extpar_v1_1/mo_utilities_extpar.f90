!+ Fortran module for utility routines
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
!> Fortran module for utility routines
!>
!> This module is based on the lm_f90 module utilities.f90 by Ulrich Schaettler
!> with some additional routines by Hermann Asensio. The module provides service utilities.
!!
!! Description:
!!  This module provides service utilities for the model. All routines are 
!!   written in a manner that also other models can use it. That means:
!!     - no routine uses other modules, except the declarations for the 
!!       KIND-type parameter; the data access is by parameter list only
!!     - no routine allocates dynamic memory; work space needed is
!!       provided via the parameter list
!!     - no derived data types are used
!!
!!     - free_un:
!!       Function to get free FORTRAN unit number. 
!!     
!!     - abort_extpar:
!!       Subroutine to abort generation of external parameter program
!!
!!     - phirot2phi:
!!       Converts phi from the rotated system to phi in the real
!!       geographical system.
!!
!!     - phi2phirot:
!!       Converts phi from the real geographical system to phi
!!       in the rotated system.
!!
!!     - rlarot2rla:
!!       Converts lambda from the rotated system to lambda in the real
!!       geographical system.
!!
!!     - rla2rlarot:
!!       Converts lambda from the real geographical system to lambda 
!!       in the rotated system.
!!
!!     - uvrot2uv:
!!       Converts the wind components u and v from the rotated system
!!       to the real geographical system.
!!
!!     - uvrot2uv_vec:
!!       the same as above, but for a whole 2D field (in vectorized form).
!!
!!     - uv2uvrot:
!!       Converts the wind components u and v from the real geographical
!!       system to the rotated system.
!!
!!     - uv2uvrot_vec:
!!       the same as above, but for a whole 2D field (in vectorized form).
!!
!!     - uv2df:
!!       Converts the wind components u and v to wind direction and speed.
!!
!!     - uv2df_vec:
!!       the same as above, but for a whole 2D field (in vectorized form).
!!
!!
MODULE  mo_utilities_extpar

USE mo_kind , ONLY :   &
    wp,    & ! KIND-type parameter for real variables
    ireals,    & ! KIND-type parameter for real variables
    iintegers, & ! KIND-type parameter for standard integer variables
    irealgrib, & ! KIND-type parameter for real variables in the grib library
    idouble,   & ! KIND-type parameter for double precision real variables
    isingle      ! KIND-type parameter for single precision real variables

!==============================================================================

IMPLICIT NONE

PUBLIC

!==============================================================================


CONTAINS


!> Function to get free FORTRAN unit number
!!
!! Method: use INQUIRE for unit number
INTEGER FUNCTION free_un()
IMPLICIT NONE
integer :: unit_no
logical :: is_open

free_un = 1 ! start with unit 1
is_open=.true.
 un_search: DO unit_no=1,999 
  INQUIRE (UNIT=unit_no, OPENED=is_open)
  IF (.NOT. is_open ) then
    free_un = unit_no
    exit un_search
  END IF
 END DO un_search

 IF (is_open) CALL abort_extpar('No free FORTRAN unit!')

END FUNCTION free_un


!> Subroutine to abort generation of external parameter program
SUBROUTINE abort_extpar(errorstring)
  CHARACTER (LEN=*), INTENT(in) :: errorstring !< error message

  print *,'Abort generation of external parameters'
  print *, errorstring(1:LEN_TRIM(errorstring))
  print *,'STOP'
  STOP

END SUBROUTINE abort_extpar

!> Subroutine to get coordinates of rotated soutpole from coordinates of rotated northpol
SUBROUTINE get_rot_spol_coor(npollon,npollat,spollon,spollat)
  REAL (KIND=WP), INTENT(IN)  :: npollon !< longitude of the rotated north pole
  REAL (KIND=WP), INTENT(IN)  :: npollat !< latitude of the rotated north pole
  REAL (KIND=WP), INTENT(OUT) :: spollon !< longitude of the rotated south pole
  REAL (KIND=WP), INTENT(OUT) :: spollat !< latitude of the rotated south pole

  spollat = -1. * npollat ! latitude coordinate of rotated southpole

  spollon = npollon + 180. ! longitude coordinate of rotated soutpole
  IF (spollon > 180.00) THEN ! shift the longitude range
    spollon = spollon - 360.
  ENDIF

END SUBROUTINE get_rot_spol_coor






!> Function for rotation of geographical coordinates
!>
!! Description:
!!   This function converts phi from one rotated system to phi in another
!!   system. The other system
!!   can also be a rotated one, where polgam is the angle between the two
!!   north poles.
!!   If polgam =0 , the other system is the real geographical
!!   system.
!!
!! Method:
!!   Transformation formulas for converting between these two systems.
!!
FUNCTION  phirot2phi ( phirot, rlarot, polphi, pollam, polgam )

REAL (KIND=wp), INTENT (IN)      ::    polphi !< latitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::    pollam !< longitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::    phirot !< latitude in the rotated system
REAL (KIND=wp), INTENT (IN)      ::    rlarot !< longitude in the rotated system

REAL (KIND=wp), INTENT (IN)      ::        &
  polgam       !< angle between the north poles of the systems

REAL (KIND=wp)                   ::        &
  phirot2phi  !< latitude in the geographical system

! Local variables
REAL (KIND=wp)                   ::        &
  zsinpol, zcospol, zphis, zrlas, zarg, zgam

REAL (KIND=wp), PARAMETER        ::        &
  zrpi18 = 57.2957795_wp,                  &
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------

! Begin function phirot2phi

  zsinpol     = SIN (zpir18 * polphi)
  zcospol     = COS (zpir18 * polphi)
 
  zphis       = zpir18 * phirot
  IF (rlarot > 180.0_wp) THEN
    zrlas = rlarot - 360.0_wp
  ELSE
    zrlas = rlarot
  ENDIF
  zrlas       = zpir18 * zrlas

  IF (polgam /= 0.0_wp) THEN
    zgam  = zpir18 * polgam
    zarg  = zsinpol*SIN (zphis) +                                           &
        zcospol*COS(zphis) * ( COS(zrlas)*COS(zgam) - SIN(zgam)*SIN(zrlas) )
  ELSE
    zarg  = zcospol * COS (zphis) * COS (zrlas) + zsinpol * SIN (zphis)
  ENDIF
 
  phirot2phi  = zrpi18 * ASIN (zarg)

END FUNCTION phirot2phi

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------

!> This routine converts phi from the real geographical system to phi in the rotated system
FUNCTION  phi2phirot ( phi, rla, polphi, pollam )

!------------------------------------------------------------------------------
! Description:
!   This routine converts phi from the real geographical system to phi
!   in the rotated system.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
!------------------------------------------------------------------------------
! Parameter list:

REAL (KIND=wp), INTENT (IN)      ::    polphi !< latitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::    pollam !< longitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::    phi !< latitude in the geographical system
REAL (KIND=wp), INTENT (IN)      ::    rla !< longitude in the geographical system

REAL (KIND=wp)                   ::        &
  phi2phirot !< longitude in the rotated system

! Local variables
REAL (KIND=wp)                       ::    &
  zsinpol, zcospol, zlampol, zphi, zrla, zarg1, zarg2, zrla1

REAL (KIND=wp), PARAMETER            ::    &
  zrpi18 = 57.2957795_wp,                  & !
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------

! Begin function phi2phirot

  zsinpol  = SIN (zpir18 * polphi)
  zcospol  = COS (zpir18 * polphi)
  zlampol  =      zpir18 * pollam
  zphi     =      zpir18 * phi
  IF (rla > 180.0_wp) THEN
    zrla1  = rla - 360.0_wp
  ELSE
    zrla1  = rla
  ENDIF
  zrla     = zpir18 * zrla1

  zarg1    = SIN (zphi) * zsinpol
  zarg2    = COS (zphi) * zcospol * COS (zrla - zlampol)

  phi2phirot = zrpi18 * ASIN (zarg1 + zarg2)

END FUNCTION phi2phirot

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!> This function converts lambda from one rotated system to lambda in another.
!!
!! Description:
!!   This function converts lambda from one rotated system to lambda in another
!!   system. The other system
!!   can also be a rotated one, where polgam is the angle between the two
!!   north poles.
!!   If polgam =0 , the other system is the real geographical
!!   system.
FUNCTION  rlarot2rla (phirot, rlarot, polphi, pollam, polgam)

!
! Method:
!   Transformation formulas for converting between these two systems.
!
! Modules used:    NONE
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! Declarations:
!
!------------------------------------------------------------------------------

! Parameter list:
REAL (KIND=wp), INTENT (IN)      ::   polphi !< latitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::   pollam !< longitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::   phirot !< latitude in the rotated system
REAL (KIND=wp), INTENT (IN)      ::   rlarot !< longitude in the rotated system

REAL (KIND=wp), INTENT (IN)      ::        &
  polgam      !< angle between the north poles of the systems

REAL (KIND=wp)                   ::        &
  rlarot2rla  !< latitude in the geographical system

! Local variables
REAL (KIND=wp)                   ::        &
  zsinpol, zcospol, zlampol, zphis, zrlas, zarg1, zarg2, zgam

REAL (KIND=wp), PARAMETER        ::        &
  zrpi18 = 57.2957795_wp,                  & !
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------

! Begin function rlarot2rla

  zsinpol = SIN (zpir18 * polphi)
  zcospol = COS (zpir18 * polphi)

  zlampol = zpir18 * pollam
  zphis   = zpir18 * phirot
  IF (rlarot > 180.0_wp) THEN
    zrlas = rlarot - 360.0_wp
  ELSE
    zrlas = rlarot
  ENDIF
  zrlas   = zpir18 * zrlas

  IF (polgam /= 0.0_wp) THEN
    zgam    = zpir18 * polgam
    zarg1   = SIN (zlampol) *                                                &
      (- zsinpol*COS(zphis) * (COS(zrlas)*COS(zgam) - SIN(zrlas)*SIN(zgam))  &
       + zcospol * SIN(zphis))                                               &
    - COS (zlampol)*COS(zphis) * (SIN(zrlas)*COS(zgam) + COS(zrlas)*SIN(zgam))

    zarg2   = COS (zlampol) *                                                &
      (- zsinpol*COS(zphis) * (COS(zrlas)*COS(zgam) - SIN(zrlas)*SIN(zgam))  &
       + zcospol * SIN(zphis))                                               &
    + SIN (zlampol)*COS(zphis) * (SIN(zrlas)*COS(zgam) + COS(zrlas)*SIN(zgam))
  ELSE
    zarg1   = SIN (zlampol) * (-zsinpol * COS(zrlas) * COS(zphis)  +    &
                                zcospol *              SIN(zphis)) -    &
              COS (zlampol) *             SIN(zrlas) * COS(zphis)
    zarg2   = COS (zlampol) * (-zsinpol * COS(zrlas) * COS(zphis)  +    &
                                zcospol *              SIN(zphis)) +   &
              SIN (zlampol) *             SIN(zrlas) * COS(zphis)
  ENDIF
 
  IF (zarg2 == 0.0) zarg2 = 1.0E-20_wp
 
  rlarot2rla = zrpi18 * ATAN2(zarg1,zarg2)
 
END FUNCTION rlarot2rla

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!>  This routine converts lambda from the real geographical system to lambda in the rotated system.
FUNCTION  rla2rlarot ( phi, rla, polphi, pollam, polgam )

!------------------------------------------------------------------------------
!
! Description:
!   This routine converts lambda from the real geographical system to lambda 
!   in the rotated system.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
!------------------------------------------------------------------------------
!
! Parameter list:
REAL (KIND=wp), INTENT (IN)      ::   polphi !< latitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::   pollam !< longitude of the rotated north pole
REAL (KIND=wp), INTENT (IN)      ::   phi    !< latitude in geographical system
REAL (KIND=wp), INTENT (IN)      ::   rla        !< longitude in geographical system

REAL (KIND=wp), INTENT (IN)      ::        &
  polgam      !< angle between the north poles of the systems

REAL (KIND=wp)                   ::        &
  rla2rlarot !< latitude in the the rotated system

! Local variables
REAL (KIND=wp)                       ::    &
  zsinpol, zcospol, zlampol, zphi, zrla, zarg1, zarg2, zrla1

REAL (KIND=wp), PARAMETER            ::    &
  zrpi18 = 57.2957795_wp,                  & !
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------

! Begin function rla2rlarot

  zsinpol  = SIN (zpir18 * polphi)
  zcospol  = COS (zpir18 * polphi)
  zlampol  =      zpir18 * pollam
  zphi     =      zpir18 * phi
  IF (rla > 180.0_wp) THEN
    zrla1  = rla - 360.0_wp
  ELSE
    zrla1  = rla
  ENDIF
  zrla     = zpir18 * zrla1

  zarg1    = - SIN (zrla-zlampol) * COS(zphi)
  zarg2    = - zsinpol * COS(zphi) * COS(zrla-zlampol) + zcospol * SIN(zphi)

  IF (zarg2 == 0.0) zarg2 = 1.0E-20_wp

  rla2rlarot = zrpi18 * ATAN2 (zarg1,zarg2)

  IF (polgam /= 0.0_wp ) THEN
    rla2rlarot = polgam + rla2rlarot
    IF (rla2rlarot > 180._wp) rla2rlarot = rla2rlarot -360._wp
  ENDIF

END FUNCTION rla2rlarot

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!>  This routine converts the wind components u and v from the rotated system to the real geographical system.
SUBROUTINE uvrot2uv (urot, vrot, rlat, rlon, pollat, pollon, u, v)


!------------------------------------------------------------------------------
!
! Description:
!   This routine converts the wind components u and v from the rotated system
!   to the real geographical system.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
!------------------------------------------------------------------------------

! Parameter list:
REAL (KIND=wp), INTENT (IN)          ::  urot      !< wind components in the rotated grid
REAL (KIND=wp), INTENT (IN)          ::  vrot      !< wind components in the rotated grid
REAL (KIND=wp), INTENT (IN)          ::  rlat      !< latitude in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::  rlon      !< longitude in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::  pollat    !< latitude  of the north pole of the rotated grid
REAL (KIND=wp), INTENT (IN)          ::  pollon    !<  longitude of the north pole of the rotated grid

REAL (KIND=wp), INTENT (OUT)         ::   u        !< wind components in the true geographical system
REAL (KIND=wp), INTENT (OUT)         ::   v        !< wind components in the true geographical system

! Local variables

REAL (KIND=wp)                       ::    &
  zsinpol, zcospol, zlonp, zlat, zarg1, zarg2, znorm

REAL (KIND=wp)                       ::    &
  zrpi18 = 57.2957795_wp,       & !
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------
! Begin subroutine uvrot2uv
!------------------------------------------------------------------------------

! Converting from degree to radians
  zsinpol = SIN(pollat * zpir18)
  zcospol = COS(pollat * zpir18)
  zlonp   = (pollon-rlon) * zpir18
  zlat    =         rlat  * zpir18

  zarg1   = zcospol*SIN(zlonp)
  zarg2   = zsinpol*COS(zlat) - zcospol*SIN(zlat)*COS(zlonp)
  znorm   = 1.0/SQRT(zarg1**2 + zarg2**2)

! Convert the u- and v-components
  u       =   urot*zarg2*znorm + vrot*zarg1*znorm
  v       = - urot*zarg1*znorm + vrot*zarg2*znorm

END SUBROUTINE uvrot2uv

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!> This routine converts the wind components u and v from the rotated system to the real geographical system.
!> This is the vectorized form.
SUBROUTINE uvrot2uv_vec(u, v, rlat, rlon, pollat, pollon, idim, jdim)

!------------------------------------------------------------------------------
!
! Description:
!   This routine converts the wind components u and v from the rotated
!   system to the real geographical system. This is the vectorized form
!   of the routine above, i.e. the computation is for a whole 2D field.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Parameter list:
INTEGER (KIND=iintegers), INTENT(IN)     ::    &
  idim        !< dimensions of the field
INTEGER (KIND=iintegers), INTENT(IN)     ::    &
  jdim        !< dimensions of the field
  

REAL (KIND=wp), INTENT (INOUT)       ::    &
  u  (idim,jdim) !< wind components 
REAL (KIND=wp), INTENT (INOUT)       ::    &
  v  (idim,jdim)    !< wind components 

REAL (KIND=wp), INTENT (IN)          ::  rlat(idim,jdim)      !< latitude in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::  rlon (idim,jdim)     !< longitude in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::  pollat    !< latitude  of the north pole of the rotated grid
REAL (KIND=wp), INTENT (IN)          ::  pollon    !<  longitude of the north pole of the rotated grid


! Local variables
REAL (KIND=wp)                       ::    &
  zsinpol, zcospol, zlonp, zlat, zarg1, zarg2, znorm, zugeo, zvgeo

INTEGER (KIND=iintegers)                 ::    i, j
REAL (KIND=wp)                       ::    &
  zrpi18 = 57.2957795_wp,       & !
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------
! Begin Subroutine uvrot2uv_vec
!------------------------------------------------------------------------------

! Converting from degree to radians
  zsinpol = SIN(pollat * zpir18)
  zcospol = COS(pollat * zpir18)

  DO j = 1, jdim
    DO i = 1, idim

      zlonp   = (pollon-rlon(i,j)) * zpir18
      zlat    =         rlat(i,j)  * zpir18

      zarg1   = zcospol*SIN(zlonp)
      zarg2   = zsinpol*COS(zlat) - zcospol*SIN(zlat)*COS(zlonp)
      znorm   = 1.0/SQRT(zarg1**2 + zarg2**2)

      ! Convert the u- and v-components
      zugeo   =  u(i,j)*zarg2*znorm + v(i,j)*zarg1*znorm
      zvgeo   = -u(i,j)*zarg1*znorm + v(i,j)*zarg2*znorm
      u(i,j) = zugeo
      v(i,j) = zvgeo

    ENDDO
  ENDDO

END SUBROUTINE uvrot2uv_vec

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!> This routine converts the wind components u and v from the real geographical system to the rotated system.
SUBROUTINE uv2uvrot(u, v, rlat, rlon, pollat, pollon, urot, vrot)

!------------------------------------------------------------------------------
!
! Description:
!   This routine converts the wind components u and v from the real
!   geographical system to the rotated system.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Parameter list:
REAL (KIND=wp), INTENT (IN)          ::    &
  u  !< wind components in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::    &
 v    !< wind components in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::    &
rlat !< coordinates in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::    &
rlon !< coordinates in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::    &
 pollat !< latitude of the north pole of the rotated grid
REAL (KIND=wp), INTENT (IN)          ::    &
pollon    !< longitude of the north pole of the rotated grid

REAL (KIND=wp), INTENT (OUT)         ::    &
  urot !< wind components in the rotated grid 
REAL (KIND=wp), INTENT (OUT)         ::    &
vrot        !< wind components in the rotated grid             

! Local variables

REAL (KIND=wp)                       ::    &
  zsinpol, zcospol, zlonp, zlat, zarg1, zarg2, znorm

REAL (KIND=wp)                       ::    &
  zrpi18 = 57.2957795_wp,       & !
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------
! Begin Subroutine uv2uvrot
!------------------------------------------------------------------------------

  zsinpol = SIN(pollat * zpir18)
  zcospol = COS(pollat * zpir18)
  zlonp   = (pollon-rlon) * zpir18
  zlat    =         rlat  * zpir18

  zarg1   = zcospol*SIN(zlonp)
  zarg2   = zsinpol*COS(zlat) - zcospol*SIN(zlat)*COS(zlonp)
  znorm   = 1.0_wp/SQRT( zarg1**2 + zarg2**2 )

! Transform the u and v wind components
  urot   =  u*zarg2*znorm - v*zarg1*znorm
  vrot   =  u*zarg1*znorm + v*zarg2*znorm

END SUBROUTINE uv2uvrot

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!>  This routine converts the wind components u and v from the real  geographical system to the rotated system. This is the vectorized form.
SUBROUTINE uv2uvrot_vec(u, v, rlat, rlon, pollat, pollon, idim, jdim)

!------------------------------------------------------------------------------
!
! Description:
!   This routine converts the wind components u and v from the real
!   geographical system to the rotated system. This is the vectorized form
!   of the routine above, i.e. the computation is for a whole 2D field.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Parameter list:
INTEGER (KIND=iintegers), INTENT(IN)     ::    &
  idim        !< dimensions of the field
INTEGER (KIND=iintegers), INTENT(IN)     ::    &
  jdim        !< dimensions of the field

REAL (KIND=wp), INTENT (INOUT)       ::    &
  u  (idim,jdim) !< wind components 
REAL (KIND=wp), INTENT (INOUT)       ::    &
   v  (idim,jdim)  !< wind components 

REAL (KIND=wp), INTENT (IN)          ::  rlat(idim,jdim)      !< latitude in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::  rlon (idim,jdim)     !< longitude in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::  pollat    !< latitude  of the north pole of the rotated grid
REAL (KIND=wp), INTENT (IN)          ::  pollon    !<  longitude of the north pole of the rotated grid

! Local variables
REAL (KIND=wp)                       ::    &
  zsinpol, zcospol, zlonp, zlat, zarg1, zarg2, znorm, zurot, zvrot

INTEGER (KIND=iintegers)                 ::    i, j
REAL (KIND=wp)                       ::    &
  zrpi18 = 57.2957795_wp,       & !
  zpir18 = 0.0174532925_wp

!------------------------------------------------------------------------------
! Begin Subroutine uv2uvrot_vec
!------------------------------------------------------------------------------

  zsinpol = SIN ( pollat * zpir18 )
  zcospol = COS ( pollat * zpir18 )

  DO j = 1, jdim
    DO i = 1, idim

      zlonp   = ( pollon - rlon(i,j) ) * zpir18
      zlat    =            rlat(i,j)   * zpir18

      zarg1   = zcospol*SIN(zlonp)
      zarg2   = zsinpol*COS(zlat) - zcospol*SIN(zlat)*COS(zlonp)
      znorm   = 1.0_wp/SQRT( zarg1**2 + zarg2**2 )

      ! Transform the u and v wind components
      zurot =  u(i,j)*zarg2*znorm - v(i,j)*zarg1*znorm
      zvrot =  u(i,j)*zarg1*znorm + v(i,j)*zarg2*znorm
      u(i,j) = zurot
      v(i,j) = zvrot

    ENDDO
  ENDDO

END SUBROUTINE uv2uvrot_vec

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!> This routine computes wind speed and wind direction from the wind components.
SUBROUTINE uv2df (u, v, d, f)

!------------------------------------------------------------------------------
!
! Description:
!   This routine computes wind speed and wind direction from the wind
!   components.
!
! Method:
!   Straightforward.
!
!------------------------------------------------------------------------------
!
! Parameter list:
REAL (KIND=wp), INTENT (IN)          ::    &
  u   !< wind components in the true geographical system

  REAL (KIND=wp), INTENT (IN)          ::    &
  v           !< wind components in the true geographical system

REAL (KIND=wp), INTENT (OUT)         ::    &
  f   !< wind speed 

  REAL (KIND=wp), INTENT (OUT)         ::    &
  d   !< wind direction

! Local variables

REAL (KIND=wp)                       ::    &
  zrpi18 = 57.2957795_wp,       & ! conversion from radians to degrees
  zsmall = 0.001_wp

!------------------------------------------------------------------------------
! Begin Subroutine uv2df
!------------------------------------------------------------------------------

  IF (ABS(u) > zsmall) THEN
    f  =  SQRT( u*u + v*v )
    d  =  v / u
    d  =  180.0_wp + SIGN( 90.0_wp , u ) - ATAN( d ) *zrpi18
  ELSEIF (ABS(v) > zsmall) THEN
    f  =  ABS( v )
    d  =  270.0_wp - SIGN( 90.0_wp , v )
  ELSE
    f  =    0.0_wp
    d  =    0.0_wp
  ENDIF

END SUBROUTINE uv2df

!==============================================================================
!==============================================================================

!------------------------------------------------------------------------------
!> This routine computes wind speed and wind direction from the wind components. This is the vectorized form. 
SUBROUTINE uv2df_vec (u, v, d, f, idim, jdim)

!------------------------------------------------------------------------------
!
! Description:
!   This routine computes wind speed and wind direction from the wind
!   components.  This is the vectorized form of the routine above,
!   i.e. the computation is for a whole 2D field.
!
! Method:
!   Straightforward.
!
!------------------------------------------------------------------------------
!
! Parameter list:
INTEGER (KIND=iintegers), INTENT(IN)     ::    &
  idim        !< dimensions of the field
INTEGER (KIND=iintegers), INTENT(IN)     ::    &
  jdim        !< dimensions of the field
  

REAL (KIND=wp), INTENT (IN)          ::    &
  u  (idim,jdim)  !< wind components in the true geographical system
REAL (KIND=wp), INTENT (IN)          ::    &
  v  (idim,jdim)  !< wind components in the true geographical system


REAL (KIND=wp), INTENT (OUT)         ::    &
  f  (idim,jdim)  !< wind speed
REAL (KIND=wp), INTENT (OUT)         ::    &
  d  (idim,jdim)    !< wind direction

! Local variables

INTEGER (KIND=iintegers)                 ::    i, j
REAL (KIND=wp)                       ::    &
  zrpi18 = 57.2957795_wp,       & ! conversion from radians to degrees
  zsmall = 0.001_wp

!------------------------------------------------------------------------------
! Begin Subroutine uv2df_vec
!------------------------------------------------------------------------------

  DO j = 1, jdim
    DO i = 1, idim

      IF (ABS(u(i,j)) > zsmall) THEN
        f (i,j)  =  SQRT( u(i,j)*u(i,j) + v(i,j)*v(i,j) )
        d (i,j)  =  180.0_wp + SIGN( 90.0_wp , u(i,j) )               &
                                 - ATAN( v(i,j) / u(i,j) ) *zrpi18
      ELSEIF (ABS(v(i,j)) > zsmall) THEN
        f (i,j)  =  ABS( v(i,j) )
        d (i,j)  =  270.0_wp - SIGN( 90.0_wp , v(i,j) )
      ELSE
        f (i,j)  =    0.0_wp
        d (i,j)  =    0.0_wp
      ENDIF

    ENDDO
  ENDDO

END SUBROUTINE uv2df_vec

!==============================================================================
!==============================================================================

END MODULE mo_utilities_extpar
