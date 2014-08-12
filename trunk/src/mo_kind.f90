!+ Module determines kinds for different precisions.
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================!>
!!  Module determines kinds for different precisions.
!! 
!!  Module determines kinds for different precisions
!!  Number model from which the SELECTED_*\\_KIND are requested: <br>
!!
!! @f{tabular}{{r@{\hspace*{3em}}c@{\hspace*{3em}}c}
!!                     &4 byte REAL     &8 byte REAL        \\\
!!        CRAY:        &-               &precision =   13   \\\
!!                     &                &exponent  = 2465   \\\
!!        IEEE:        &precision = 6   &precision =   15   \\\
!!                     &exponent  = 37  &exponent  =  307
!! @f}
!! \\medskip
!! 
!!  Most likely this are the only possible models.
!! 
!! @par Revision History
!!  Working precision and comments are added by Luis Kornblueh (2001)
!!  Modification by Luis Kornblueh (2010-02-16):
!!  Working precision selection is parameterized by values, which can be as 
!!  well used for selection of MPI data types associated with the respective 
!!  Fortran datatypes.
!! 
MODULE mo_kind

  IMPLICIT NONE

! !VERSION CONTROL:
  CHARACTER(len=*), PARAMETER, PRIVATE :: version = '$Id: mo_kind.f90,v 1.11 2013-04-16 11:09:20 for0adm Exp $'

! !DEFINED PARAMETERS:

!
! ! Floating point section 
!

  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37)  
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307)
  INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(12,307)
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307)
  !INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)
  ! The follwoing is for the quadruple precision:
  !INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(25,307)



  INTEGER, PARAMETER :: ishort = SELECTED_INT_KIND(4)

  INTEGER, PARAMETER :: i4 = SELECTED_INT_KIND(9)

  INTEGER, PARAMETER :: i8 = SELECTED_INT_KIND(14)

   ! this is from the COSMO code, data_parameters.f90
   INTEGER, PARAMETER ::      ireals    = SELECTED_REAL_KIND (12,200) !< KIND parameter for real variables
                     ! number of desired significant digits for
                     ! real variables
                     ! corresponds to 8 byte real variables
                       INTEGER, PARAMETER       ::      iintegers = KIND  (1)                                            !< KIND parameter for integer variables
                     ! kind-type parameter of the integer values
                     ! corresponds to the default integers

! 2. KIND-Parameters for the variables in the GRIB-library
! --------------------------------------------------------

   INTEGER, PARAMETER :: intgribf = KIND(1)                   !< KIND parameter for integer variables for GRIB library
                                          
!   intgribf  = 4,      &  ! (if using libgrib1 on the T3E)
       ! Kind type for Fortran integer variables used in the GRIB library
       ! this normally is the Standard integer with the exception of using
       ! "libgrib1" (former supplib) on a machine with 8 byte INTEGER default
       ! (like Cray-machines; then intgribf has be set to 32-bit INTEGER).

   INTEGER, PARAMETER :: intgribc = KIND(1)                    !< KIND parameter for C integer variables for GRIB library

       ! Kind type for C integer variables used in the GRIB library
       ! this always is the Standard integer

   INTEGER, PARAMETER :: irealgrib = KIND(1.0)                                               !< KIND parameter for real variables for GRIB library

       ! Kind type for Fortran real variables used in the GRIB library
       ! this is the Standard real of the machine


! 3. KIND-Parameters for the generic formulation of some utility routines:
! ------------------------------------------------------------------------

    ! The distinction between ireals (working precision) and irealgrib
    ! is not enough, because it could be possible that these KIND parameters
    ! are the same. Compilers could get in trouble then, because they could
    ! not decide, which routine to take then.
    ! Therefore we define the KIND parameters idouble (for double precision
    ! or 8 byte reals) and isinge (for single precision, or 4 byte reals)

   INTEGER, PARAMETER :: idouble = KIND (1.0D0)        !< KIND parameter for double precision real variables

   INTEGER, PARAMETER :: isingle = KIND (1.0)          !< KIND parameter for single precsion real variable






!EOP  
!--------------------------------------------------------------------  
!BOC  

END MODULE mo_kind

!EOC 
