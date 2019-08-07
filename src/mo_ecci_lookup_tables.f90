!+ Fortran Module with lookup-tables for the ecci data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_3         2011/04/19 Hermann Asensio
!  Initial release
! V1_6         2011/11/29 Jan-Peter Schulz
!  Let land use class no. 09 (open needleleaved deciduous or evergreen
!  forest) contribute to FOR_E instead of FOR_D. This appears to be more
!  realistic in high latitudes with evergreen forest. (Jan-Peter Schulz)
!  Calculate the urban fraction in the same way as for GLC2000, i.e.
!  set it to 1.0 instead of 0.8 for land use class no. 19 (artificial
!  surfaces). (Kristina Trusilova)
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran Module with lookup-tables for the ecci data
!> \author Hermann Asensio
!!
!! Description:
!! The ecci dataset contains the following land use classification scheme
! class value         description
! 01  11   'irrigated croplands
! 02  14   'rainfed croplands                             '
! 03  20   'mosaic cropland (50-70%) - vegetation (20-50%)'
! 04  30   'mosaic vegetation (50-70%) - cropland (20-50%)'
! 05  40   'closed broadleaved evergreen forest           '
! 06  50   'closed broadleaved deciduous forest           '
! 07  60   'open broadleaved deciduous forest             '
! 08  70   'closed needleleaved evergreen forest          '
! 09  90   'open needleleaved decid. or evergr. forest    '
! 10  100  'mixed broadleaved and needleleaved forest     '
! 11  110  'mosaic shrubland (50-70%) - grassland (20-50%)'
! 12  120  'mosaic grassland (50-70%) - shrubland (20-50%)'
! 13  130  'closed to open shrubland                      '
! 14  140  'closed to open herbaceous vegetation          '
! 15  150  'sparse vegetation                             '
! 16  160  'closed to open forest regulary flooded        '
! 17  170  'closed forest or shrubland permanently flooded'
! 18  180  'closed to open grassland regularly flooded    '
! 19  190  'artificial surfaces                           '
! 20  200  'bare areas                                    '
! 21  210  'water bodies                                  '
! 22  220  'permanent snow and ice                        '
! 23  230  'undefined                                     '
!! lookup-table compiled by Hermann Asensio, DWD
MODULE mo_ecci_lookup_tables

 !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar
  USE mo_io_units,          ONLY: filename_max

IMPLICIT NONE

PRIVATE

PUBLIC :: init_ecci_lookup_tables
PUBLIC :: get_name_ecci_lookup_tables
PUBLIC :: ecci_look_up
PUBLIC :: get_ecci_idx
PUBLIC :: ecci_legend
PUBLIC :: ecci_value

PUBLIC :: nclass_ecci
PUBLIC :: ilookup_table_ecci
PUBLIC :: i_extpar_lookup_table, i_extpar_test_lookup_table
PUBLIC :: name_lookup_table_ecci
PUBLIC :: z0_lt_ecci, lnz0_lt_ecci, plc_mn_lt_ecci, plc_mx_lt_ecci
PUBLIC :: lai_mn_lt_ecci, lai_mx_lt_ecci, rd_lt_ecci, skinc_lt_ecci
PUBLIC :: emiss_lt_ecci, rs_min_lt_ecci



INTEGER (KIND=i4), PARAMETER :: nclass_ecci = 23 !< ecci has 23 classes for the land use description

INTEGER (KIND=i4), PARAMETER :: i_extpar_lookup_table = 1 !< lookup_table for ecci land use classes (IGBP correspondence)
INTEGER (KIND=i4), PARAMETER :: i_extpar_test_lookup_table = 3 !< lookup_table for ecci land use classes
                                                               !< (IGBP correspondence) for experimental setting, analogue to
                                                               !< look-up tables of ECOCLIMAP (Masson 2003)

INTEGER (KIND=i4) :: ilookup_table_ecci !< integer switch to choose a lookup table
CHARACTER (LEN=filename_max) :: name_lookup_table_ecci !< name of lookup table




REAL (KIND=wp) :: z0_lt_ecci(nclass_ecci)      !< lookup table landuse class to roughness length [m]
REAL (KIND=wp) :: lnz0_lt_ecci(nclass_ecci)    !< corresponding natural logarithm of z0c_extpar_o
REAL (KIND=wp) :: plc_mn_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal plant cover
REAL (KIND=wp) :: plc_mx_lt_ecci(nclass_ecci)  !< lookup table landuse class to maximal plant cover
REAL (KIND=wp) :: lai_mn_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal leaf area index
REAL (KIND=wp) :: lai_mx_lt_ecci(nclass_ecci)  !< lookup table landuse class to maximal leaf area index
REAL (KIND=wp) :: rd_lt_ecci(nclass_ecci)      !< lookup table landuse class to root depth [m]
REAL (KIND=wp) :: skinc_lt_ecci(nclass_ecci)   !< lookup table landuse class to skin conductivity [W m-2 K-1]
REAL (KIND=wp) :: emiss_lt_ecci(nclass_ecci)   !< lookup table landuse class to surface thermal emissivity
REAL (KIND=wp) :: rs_min_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal stomata resistance

!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------
REAL (KIND=wp) :: z0c_extpar_o(nclass_ecci)  = (/ &       !< lookup table landuse class to roughness length [m]
 &  0.07 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.07 ,  & ! 'rainfed croplands                             ' ! 2.
 &  0.25 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.07 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  1.0  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  1.0  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.15 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  1.0  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  1.0  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  1.0  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.20 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.20 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.15 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.03 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.05 ,  & ! 'sparse vegetation                             ' ! 15.
 &  1.0 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  1.0 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.05 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  1.0,    & ! 'artificial surfaces                           ' ! 19.
 &  0.05,   & ! 'bare areas                                    ' ! 20.
 &  0.0002 ,& ! 'water bodies                                  ' ! 21.
 &  0.01,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.



!REAL (KIND=wp) :: lnz0c_extpar_o(nclass_ecci)    !< corresponding natural logarithm of z0c_extpar_o


REAL (KIND=wp) :: zplcmnc_extpar_o(nclass_ecci) = (/ &      !< lookup table landuse class to minimal plant cover
 &  0.5 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.5 ,  & ! 'rainfed croplands                             ' ! 2.
 &  0.65 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.5 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  0.8  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  0.75  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.7 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.8  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.75  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.75  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.70 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.70 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.70 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.75 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.5 ,  & ! 'sparse vegetation                             ' ! 15.
 &  0.8 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  0.8 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.5 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.02,    & ! 'artificial surfaces                           ' ! 19.
 &  0.0,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.



REAL (KIND=wp) :: zplcmxc_extpar_o(nclass_ecci) = (/ &     !< lookup table landuse class to maximal plant cover
 &  0.9 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.9 ,  & ! 'rainfed croplands                             ' ! 2.
 &  0.8 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.9 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  0.8  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  0.9  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.8 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.8  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.9  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.9  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.8 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.8 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.8 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.9 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.5 ,  & ! 'sparse vegetation                             ' ! 15.
 &  0.8 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  0.8 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.8 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.2,    & ! 'artificial surfaces                           ' ! 19.
 &  0.05,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.


REAL (KIND=wp) :: zlaimnc_extpar_o(nclass_ecci) = (/ &      !< lookup table landuse class to minimal leaf area index
 &  0.7 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.7 ,  & ! 'rainfed croplands                             ' ! 2.
 &  1.0 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.7 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  1.4  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  1.0  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  1.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  1.3  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  1.0  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  1.0  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.6 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.6 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.6 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  1.0 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.4 ,  & ! 'sparse vegetation                             ' ! 15.
 &  1.4 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  1.4 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  1.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.1,    & ! 'artificial surfaces                           ' ! 19.
 &  0.4,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.


REAL (KIND=wp) :: zlaimxc_extpar_o(nclass_ecci) = (/ &      !< lookup table landuse class to maximal leaf area index
 &  3.3 ,  & ! 'irrigated croplands                           ' ! 1.
 &  3.3 ,  & ! 'rainfed croplands                             ' ! 2.
 &  3.0 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  3.5 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  5.0 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  6.0  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  4.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  5.0  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  5.0  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  5.0  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  2.5 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  2.5 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  2.5 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  3.1 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.6 ,  & ! 'sparse vegetation                             ' ! 15.
 &  5.0 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  5.0 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  2.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  1.6,    & ! 'artificial surfaces                           ' ! 19.
 &  0.6,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.


REAL (KIND=wp) :: zrd_extpar_o(nclass_ecci)  = (/ &         !< lookup table landuse class to root depth [m]
 &  1.0 ,  & ! 'irrigated croplands                           ' ! 1.
 &  1.0 ,  & ! 'rainfed croplands                             ' ! 2.
 &  1.0 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  1.0 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  1.0 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  1.0 ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  2.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.6 ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.6 ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.8 ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  1.0 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  1.0 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  1.5 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.6 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.3 ,  & ! 'sparse vegetation                             ' ! 15.
 &  1.0 ,  & ! 'closed to open forest regulary flooded        ' ! 16.
 &  1.0 ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  1.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.6 ,  & ! 'artificial surfaces                           ' ! 19.
 &  0.3 ,  & ! 'bare areas                                    ' ! 20.
 &  0.0 ,  & ! 'water bodies                                  ' ! 21.
 &  0.0 ,  & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.

REAL (KIND=wp) :: zskinc_extpar_o(nclass_ecci) = (/ &       !< lookup table landuse class to skin conductivity
&   30.0 ,  & ! 'irrigated croplands                           ' ! 1.
&   30.0 ,  & ! 'rainfed croplands                             ' ! 2.
&   10.0 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
&   30.0 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
&   50.0 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
&   50.0 ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
&   30.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
&   50.0 ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
&   50.0 ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
&   50.0 ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
&   30.0 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
&   10.0 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
&   50.0 ,  & ! 'closed to open shrubland                      ' ! 13.
&   30.0 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
&   10.0 ,  & ! 'sparse vegetation                             ' ! 15.
&   50.0 ,  & ! 'closed to open forest regulary flooded        ' ! 16.
&   50.0 ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
&   30.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
&  200.0 ,  & ! 'artificial surfaces                           ' ! 19.
&  200.0 ,  & ! 'bare areas                                    ' ! 20.
&  200.0 ,  & ! 'water bodies                                  ' ! 21.
&  200.0 ,  & ! 'permanent snow and ice                        ' ! 22.
&  200.0 /)   ! 'undefined                                     ' ! 23.

REAL (KIND=wp) :: zemiss_extpar_o(nclass_ecci) = (/ &       !< lookup table landuse class to surface thermal emissivity
 &  0.990 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.990 ,  & ! 'rainfed croplands                             ' ! 2.
 &  0.990 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.990 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  0.996 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  0.990 ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.993 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.996 ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.990 ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.993 ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.985 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.989 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.990 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.993 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.950 ,  & ! 'sparse vegetation                             ' ! 15.
 &  0.996 ,  & ! 'closed to open forest regulary flooded        ' ! 16.
 &  0.996 ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.992 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.960 ,  & ! 'artificial surfaces                           ' ! 19.
 &  0.950 ,  & ! 'bare areas                                    ' ! 20.
 &  0.991 ,  & ! 'water bodies                                  ' ! 21.
 &  0.9999,  & ! 'permanent snow and ice                        ' ! 22.
 &  0.9999 /)     !'undefined                                      ' ! 23.


REAL (KIND=wp) :: zrs_min_extpar_o(nclass_ecci) = (/ &      !< lookup table landuse class to minimal stomata resistance
 &  120. ,  & ! 'irrigated croplands                           ' ! 1.
 &  120. ,  & ! 'rainfed croplands                             ' ! 2.
 &  120. ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  100. ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  250. ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  150. ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  150. ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  150. ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  150. ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  150. ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  150. ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  150. ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  120. ,  & ! 'closed to open shrubland                      ' ! 13.
 &   40. ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &   40. ,  & ! 'sparse vegetation                             ' ! 15.
 &  150. ,  & ! 'closed to open forest regulary flooded        ' ! 16.
 &  150. ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
 &   40. ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  120. ,  & ! 'artificial surfaces                           ' ! 19.
 &  120. ,  & ! 'bare areas                                    ' ! 20.
 &  120. ,  & ! 'water bodies                                  ' ! 21.
 &  120. ,  & ! 'permanent snow and ice                        ' ! 22.
 &  250. /)     !'undefined                                      ' ! 23.

!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------


REAL (KIND=wp) :: z0c_experimental(nclass_ecci)   = (/ &       !< lookup table landuse class to roughness length [m]
 &  0.07 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.07 ,  & ! 'rainfed croplands                               ! 2.
 &  0.25 ,  & !  'mosaic cropland (50-70%) - vegetation (20-50%)'! 3.
 &  0.07 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  1.0  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  1.0  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.15 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  1.0  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  1.0  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  1.0  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.20 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.20 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.15 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.03 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.05 ,  & ! 'sparse vegetation                             ' ! 15.
 &  1.0 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  1.0 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.05 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  1.0,    & ! 'artificial surfaces                           ' ! 19.
 &  0.05,   & ! 'bare areas                                    ' ! 20.
 &  0.0002 ,& ! 'water bodies                                  ' ! 21.
 &  0.01,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      '

!REAL (KIND=wp) :: lnz0c_experimental(nclass_ecci)    !< corresponding natural logarithm of z0c_extpar_o

REAL (KIND=wp) :: zplcmnc_experimental(nclass_ecci)  = (/ &      !< lookup table landuse class to minimal plant cover
 &  0.5 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.5 ,  & ! 'rainfed croplands                             ' ! 2.
 &  0.65 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.5 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  0.8  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  0.75  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.7 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.8  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.75  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.75  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.70 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.70 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.70 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.75 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.5 ,  & ! 'sparse vegetation                             ' ! 15.
 &  0.8 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  0.8 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.5 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.02,    & ! 'artificial surfaces                           ' ! 19.
 &  0.0,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.



REAL (KIND=wp) :: zplcmxc_experimental(nclass_ecci)  = (/ &     !< lookup table landuse class to maximal plant cover
 &  0.9 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.9 ,  & ! 'rainfed croplands                             ' ! 2.
 &  0.8 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.9 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  0.8  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  0.9  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.8 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.8  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.9  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.9  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.8 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.8 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.8 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.9 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.5 ,  & ! 'sparse vegetation                             ' ! 15.
 &  0.8 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  0.8 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.8 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.2,    & ! 'artificial surfaces                           ' ! 19.
 &  0.05,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.



 !< lookup table landuse class to maximal plant cover
REAL (KIND=wp) :: zlaimnc_experimental(nclass_ecci)   = (/ &      !< lookup table landuse class to minimal leaf area index
 &  0.7 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.7 ,  & ! 'rainfed croplands                             ' ! 2.
 &  1.0 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.7 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  1.4  ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  1.0  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  1.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  1.3  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  1.0  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  1.0  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.6 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.6 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.6 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  1.0 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.4 ,  & ! 'sparse vegetation                             ' ! 15.
 &  1.4 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  1.4 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  1.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.1,    & ! 'artificial surfaces                           ' ! 19.
 &  0.4,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                ' ! 23.


REAL (KIND=wp) :: zlaimxc_experimental(nclass_ecci)  = (/ &      !< lookup table landuse class to maximal leaf area index
 &  3.3 ,  & ! 'irrigated croplands                           ' ! 1.
 &  3.3 ,  & ! 'rainfed croplands                             ' ! 2.
 &  2.1 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  3.3 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  2.4 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  3.4  ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  2.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  3.8  ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  3.8  ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  3.4  ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  1.5 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  1.5 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  1.5 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  3.1 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.6 ,  & ! 'sparse vegetation                             ' ! 15.
 &  2.4 ,   & ! 'closed to open forest regulary flooded        ' ! 16.
 &  2.4 ,   & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  2.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  1.6,    & ! 'artificial surfaces                           ' ! 19.
 &  0.6,   & ! 'bare areas                                    ' ! 20.
 &  0.0 ,& ! 'water bodies                                  ' ! 21.
 &  0.0,   & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.



REAL (KIND=wp) :: zrd_experimental(nclass_ecci)   = (/ &         !< lookup table landuse class to root depth [m]
 &  1.0 ,  & ! 'irrigated croplands                           ' ! 1.
 &  1.0 ,  & ! 'rainfed croplands                             ' ! 2.
 &  1.0 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  1.0 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  1.0 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  1.0 ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  2.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.6 ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.6 ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.8 ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  1.0 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  1.0 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  1.5 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.6 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.3 ,  & ! 'sparse vegetation                             ' ! 15.
 &  1.0 ,  & ! 'closed to open forest regulary flooded        ' ! 16.
 &  1.0 ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  1.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.6 ,  & ! 'artificial surfaces                           ' ! 19.
 &  0.3 ,  & ! 'bare areas                                    ' ! 20.
 &  0.0 ,  & ! 'water bodies                                  ' ! 21.
 &  0.0 ,  & ! 'permanent snow and ice                        ' ! 22.
 &  0. /)     !'undefined                                      ' ! 23.

REAL (KIND=wp) :: zskinc_experimental(nclass_ecci) = (/ &    !< lookup table landuse class to skin conductivity
&   30.0 ,  & ! 'irrigated croplands                           ' ! 1.
&   30.0 ,  & ! 'rainfed croplands                             ' ! 2.
&   10.0 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
&   30.0 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
&   50.0 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
&   50.0 ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
&   30.0 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
&   50.0 ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
&   50.0 ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
&   50.0 ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
&   30.0 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
&   10.0 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
&   50.0 ,  & ! 'closed to open shrubland                      ' ! 13.
&   30.0 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
&   10.0 ,  & ! 'sparse vegetation                             ' ! 15.
&   50.0 ,  & ! 'closed to open forest regulary flooded        ' ! 16.
&   50.0 ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
&   30.0 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
&  200.0 ,  & ! 'artificial surfaces                           ' ! 19.
&  200.0 ,  & ! 'bare areas                                    ' ! 20.
&  200.0 ,  & ! 'water bodies                                  ' ! 21.
&  200.0 ,  & ! 'permanent snow and ice                        ' ! 22.
&  200.0 /)   ! 'undefined                                     ' ! 23.

REAL (KIND=wp) :: zemiss_experimental(nclass_ecci)  = (/ &   !< lookup table LU class to surface thermal emissivity
 &  0.990 ,  & ! 'irrigated croplands                           ' ! 1.
 &  0.990 ,  & ! 'rainfed croplands                             ' ! 2.
 &  0.990 ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  0.990 ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  0.996 ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  0.990 ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  0.993 ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  0.996 ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  0.990 ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  0.993 ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  0.985 ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  0.989 ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  0.990 ,  & ! 'closed to open shrubland                      ' ! 13.
 &  0.993 ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  0.950 ,  & ! 'sparse vegetation                             ' ! 15.
 &  0.996 ,  & ! 'closed to open forest regulary flooded        ' ! 16.
 &  0.996 ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  0.992 ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  0.960 ,  & ! 'artificial surfaces                           ' ! 19.
 &  0.950 ,  & ! 'bare areas                                    ' ! 20.
 &  0.991 ,  & ! 'water bodies                                  ' ! 21.
 &  0.9999,  & ! 'permanent snow and ice                        ' ! 22.
 &  0.9999 /)     !'undefined                                      ' ! 23.



REAL (KIND=wp) :: zrs_min_experimental(nclass_ecci) =(/ &
 &  180. ,  & ! 'irrigated croplands                           ' ! 1.
 &  180. ,  & ! 'rainfed croplands                             ' ! 2.
 &  200. ,  & ! 'mosaic cropland (50-70%) - vegetation (20-50%)' ! 3.
 &  150. ,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  175. ,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  240. ,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  240. ,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  500. ,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  500. ,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 &  350. ,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 &  300. ,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 &  300. ,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 &  225. ,  & ! 'closed to open shrubland                      ' ! 13.
 &  150. ,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 &  110. ,  & ! 'sparse vegetation                             ' ! 15.
 &  350. ,  & ! 'closed to open forest regulary flooded        ' ! 16.
 &  350. ,  & ! 'closed forest or shrubland permanently flooded' ! 17.
 &  110. ,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 &  150. ,  & ! 'artificial surfaces                           ' ! 19.
 &  150. ,  & ! 'bare areas                                    ' ! 20.
 &  250. ,  & ! 'water bodies                                  ' ! 21.
 &  250. ,  & ! 'permanent snow and ice                        ' ! 22.
 &  250. /)     !'undefined                                      ' ! 23.



!> legend of the ecci vegetation classes
CHARACTER(len=45) :: ecci_legend(nclass_ecci) = (/&    ! No.
 &  'irrigated croplands                           ' , &    ! 1.         
 &  'rainfed croplands                             ' , &    ! 2.        
 &  'mosaic cropland (50-70%) - vegetation (20-50%)' , &    ! 3.       
 &  'mosaic vegetation (50-70%) - cropland (20-50%)' , &    ! 4.      
 &  'closed broadleaved evergreen forest           ' , &    ! 5.     
 &  'closed broadleaved deciduous forest           ' , &    ! 6.    
 &  'open broadleaved deciduous forest             ' , &    ! 7.   
 &  'closed needleleaved evergreen forest          ' , &    ! 8.  
 &  'open needleleaved decid. or evergr. forest    ' , &    ! 9. 
 &  'mixed broadleaved and needleleaved forest     ' , &    ! 10.
 &  'mosaic shrubland (50-70%) - grassland (20-50%)' , &    ! 11.        
 &  'mosaic grassland (50-70%) - shrubland (20-50%)' , &    ! 12.       
 &  'closed to open shrubland                      ' , &    ! 13.      
 &  'closed to open herbaceous vegetation          ' , &    ! 14.
 &  'sparse vegetation                             ' , &    ! 15.        
 &  'closed to open forest regulary flooded        ' , &    ! 16.        
 &  'closed forest or shrubland permanently flooded' , &    ! 17.        
 &  'closed to open grassland regularly flooded    ' , &    ! 18.        
 &  'artificial surfaces                           ' , &    ! 19.        
 &  'bare areas                                    ' , &    ! 20.        
 &  'water bodies                                  ' , &    ! 21.        
 &  'permanent snow and ice                        ' , &    ! 22.        
 &  'undefined                                     ' /)     ! 23.

!> values of the ecci landuse classes
INTEGER :: ecci_value(nclass_ecci) =          (/&    ! No.
 &  11 , & ! 'irrigated croplands                           ' ! 1.
 &  14,  & ! 'rainfed croplands                               ! 2.
 &  20,  & !  'mosaic cropland (50-70%) - vegetation (20-50%)'! 3.
 &  30,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
 &  40,  & ! 'closed broadleaved evergreen forest           ' ! 5.
 &  50,  & ! 'closed broadleaved deciduous forest           ' ! 6.
 &  60,  & ! 'open broadleaved deciduous forest             ' ! 7.
 &  70,  & ! 'closed needleleaved evergreen forest          ' ! 8.
 &  90,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
 & 100,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
 & 110,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
 & 120,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
 & 130,  & ! 'closed to open shrubland                      ' ! 13.
 & 140,  & ! 'closed to open herbaceous vegetation          ' ! 14.
 & 150,  & ! 'sparse vegetation                             ' ! 15.
 & 160,  & ! 'closed to open forest regulary flooded        ' ! 16.
 & 170,  & ! 'closed forest or shrubland permanently flooded' ! 17.
 & 180,  & ! 'closed to open grassland regularly flooded    ' ! 18.
 & 190,  & ! 'artificial surfaces                           ' ! 19.
 & 200,  & ! 'bare areas                                    ' ! 20.
 & 210,  & ! 'water bodies                                  ' ! 21.
 & 220,  & ! 'permanent snow and ice                        ' ! 22.
 & 230 /)  !'undefined                                      ' ! 23.


CONTAINS

  !> define lookup table for ecci landuse classes
  SUBROUTINE init_ecci_lookup_tables(nclass_ecci, &
    &      ilookup_table_ecci, &
    &      z0_lt_ecci,           &
    &      lnz0_lt_ecci,       &
    &      plc_mn_lt_ecci,        &
    &      plc_mx_lt_ecci,        &
    &      lai_mn_lt_ecci,        &
    &      lai_mx_lt_ecci,        &
    &      rd_lt_ecci,          &
    &      skinc_lt_ecci,       &
    &      emiss_lt_ecci,       &
    &      rs_min_lt_ecci)
    INTEGER, INTENT(IN) :: nclass_ecci !< ecci has 23 classes for the land use description
    INTEGER, INTENT(IN) :: ilookup_table_ecci  !< integer switch to choose a lookup table
    REAL (KIND=wp), INTENT(OUT) :: z0_lt_ecci(nclass_ecci)      !< lookup table LU class to roughness length [m]
    REAL (KIND=wp), INTENT(OUT) :: lnz0_lt_ecci(nclass_ecci)    !< corresponding natural logarithm of z0c_extpar_o
    REAL (KIND=wp), INTENT(OUT) :: plc_mn_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal plant cover
    REAL (KIND=wp), INTENT(OUT) :: plc_mx_lt_ecci(nclass_ecci)  !< lookup table landuse class to maximal plant cover
    REAL (KIND=wp), INTENT(OUT) :: lai_mn_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal LAI
    REAL (KIND=wp), INTENT(OUT) :: lai_mx_lt_ecci(nclass_ecci)  !< lookup table landuse class to maximal LAI
    REAL (KIND=wp), INTENT(OUT) :: rd_lt_ecci(nclass_ecci)      !< lookup table LU class to root depth [m]
    REAL (KIND=wp), INTENT(OUT) :: skinc_lt_ecci(nclass_ecci)   !< lookup table landuse class to skin conductivity [W m-2 K-1]
    REAL (KIND=wp), INTENT(OUT) :: emiss_lt_ecci(nclass_ecci)   !< lookup table LU class to surface thermal emiss.
    REAL (KIND=wp), INTENT(OUT) :: rs_min_lt_ecci(nclass_ecci)  !< lookup table LU class to minimal stomata resis.

    ! local variable
    INTEGER :: i !< counter
    REAL :: arg

      SELECT CASE (ilookup_table_ecci)
        CASE(i_extpar_lookup_table)
           z0_lt_ecci = z0c_extpar_o
           plc_mn_lt_ecci = zplcmnc_extpar_o
           plc_mx_lt_ecci = zplcmxc_extpar_o
           lai_mn_lt_ecci = zlaimnc_extpar_o
           lai_mx_lt_ecci = zlaimxc_extpar_o
           rd_lt_ecci = zrd_extpar_o
           skinc_lt_ecci = zskinc_extpar_o
           emiss_lt_ecci = zemiss_extpar_o
           rs_min_lt_ecci = zrs_min_extpar_o
        CASE(i_extpar_test_lookup_table)
           z0_lt_ecci = z0c_experimental
           plc_mn_lt_ecci = zplcmnc_experimental
           plc_mx_lt_ecci = zplcmxc_experimental
           lai_mn_lt_ecci = zlaimnc_experimental
           lai_mx_lt_ecci = zlaimxc_experimental
           rd_lt_ecci = zrd_experimental
           skinc_lt_ecci = zskinc_experimental
           emiss_lt_ecci = zemiss_experimental
           rs_min_lt_ecci = zrs_min_experimental
        CASE DEFAULT
           z0_lt_ecci = z0c_extpar_o
           plc_mn_lt_ecci = zplcmnc_extpar_o
           plc_mx_lt_ecci = zplcmxc_extpar_o
           lai_mn_lt_ecci = zlaimnc_extpar_o
           lai_mx_lt_ecci = zlaimxc_extpar_o
           rd_lt_ecci = zrd_extpar_o
           skinc_lt_ecci = zskinc_extpar_o
           emiss_lt_ecci = zemiss_extpar_o
           rs_min_lt_ecci = zrs_min_extpar_o
      END SELECT

      lnz0_lt_ecci = 0.
      DO i=1,nclass_ecci
        IF (z0_lt_ecci(i) > 0.) THEN
          arg = z0_lt_ecci(i)
          lnz0_lt_ecci(i) = ALOG(arg)
        ENDIF
      ENDDO

  END  SUBROUTINE init_ecci_lookup_tables


  !> define  name of lookup table for ecci
  SUBROUTINE get_name_ecci_lookup_tables(ilookup_table_ecci, name_lookup_table_ecci)
    INTEGER, INTENT(IN) :: ilookup_table_ecci  !< integer switch to choose a lookup table
    CHARACTER (LEN=filename_max), INTENT(OUT) :: name_lookup_table_ecci !< name of lookup table
    ! local variable
      SELECT CASE (ilookup_table_ecci)
        CASE(i_extpar_lookup_table)
           name_lookup_table_ecci='Asensio_2011'
        CASE(i_extpar_test_lookup_table)
           name_lookup_table_ecci='Asensio_2010'
        CASE DEFAULT
           name_lookup_table_ecci='Asensio_2011'
      END SELECT

  END  SUBROUTINE get_name_ecci_lookup_tables



   !> assign the ecci land use classes to some characteristic (more or less) physical parameters
  SUBROUTINE ecci_look_up(lu, &
    &      nclass_ecci, &
    &      lnz0_lt_ecci,          &
    &      plc_mn_lt_ecci,        &
    &      plc_mx_lt_ecci,        &
    &      lai_mn_lt_ecci,        &
    &      lai_mx_lt_ecci,        &
    &      rd_lt_ecci,            &
    &      skinc_lt_ecci,         &
    &      emiss_lt_ecci,         &
    &      rs_min_lt_ecci,        &
    &      pland,          &
    &      pice,           &
    &      plnz0,          &
    &      proot,          &
    &      pmn,            &
    &      pmx,            &
    &      plaimn,         &
    &      plaimx,         &
    &      purb,           &
    &      pfor_d,         &
    &      pfor_e,         &
    &      pskinc,         &
    &      pemissivity,    &
    &      prs_min,        &
    &      k_error)

  INTEGER, INTENT(IN) :: lu             !< land use class
  INTEGER, INTENT(IN) :: nclass_ecci !< ecci has 23 classes for the land use description
  REAL (KIND=wp), INTENT(IN) :: lnz0_lt_ecci(nclass_ecci)    !< corresponding natural logarithm of z0c_extpar_o
  REAL (KIND=wp), INTENT(IN) :: plc_mn_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal plant cover
  REAL (KIND=wp), INTENT(IN) :: plc_mx_lt_ecci(nclass_ecci)  !< lookup table landuse class to maximal plant cover
  REAL (KIND=wp), INTENT(IN) :: lai_mn_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal LAI
  REAL (KIND=wp), INTENT(IN) :: lai_mx_lt_ecci(nclass_ecci)  !< lookup table landuse class to maximal LAI
  REAL (KIND=wp), INTENT(IN) :: rd_lt_ecci(nclass_ecci)      !< lookup table landuse class to root depth [m]
  REAL (KIND=wp), INTENT(IN) :: skinc_lt_ecci(nclass_ecci)   !< lookup table landuse class to skin conductivity [W m-2 K-1]
  REAL (KIND=wp), INTENT(IN) :: emiss_lt_ecci(nclass_ecci)   !< lookup table landuse class to surface thermal emiss.
  REAL (KIND=wp), INTENT(IN) :: rs_min_lt_ecci(nclass_ecci)  !< lookup table landuse class to minimal stomata resis.

  REAL (KIND=wp), INTENT(OUT) :: pland          !< land cover                      (-)
  REAL (KIND=wp), INTENT(OUT) :: pice           !< ice fraction                    (-)
  REAL (KIND=wp), INTENT(OUT) :: plnz0          !< logarithm of roughness length   (m)
  REAL (KIND=wp), INTENT(OUT) :: proot          !< root depth                      (m)
  REAL (KIND=wp), INTENT(OUT) :: pmn            !< minimal plant cover             (-)
  REAL (KIND=wp), INTENT(OUT) :: pmx            !< maximum plant cover             (-)
  REAL (KIND=wp), INTENT(OUT) :: plaimn         !< minimal leaf area index         (m**2/m**2)
  REAL (KIND=wp), INTENT(OUT) :: plaimx         !< maximum leaf area index         (m**2/m**2)
  REAL (KIND=wp), INTENT(OUT) :: purb           !< urbanisation                    (-)
  REAL (KIND=wp), INTENT(OUT) :: pfor_d         !< deciduous forest                (-)
  REAL (KIND=wp), INTENT(OUT) :: pfor_e         !< evergreen forest                (-)
  REAL (KIND=wp), INTENT(OUT) :: pskinc         !< skin conductivity               (W m-2 K-1)
  REAL (KIND=wp), INTENT(OUT) :: pemissivity    !< surface thermal emissivity      (-)
  REAL (KIND=wp), INTENT(OUT) :: prs_min        !< minimum stomata resistance      (s/m)

  INTEGER, INTENT(OUT)        :: k_error     !< error return code

  ! local variables
  INTEGER :: nclass !< position of landuse class in arrays

    CALL get_ecci_idx(lu,nclass)


       ! Test for true land points
          IF (nclass>=1 .AND. nclass<=22 .AND.nclass/=21) THEN
            k_error     = 0
            pland       = 1.0
            plnz0       = lnz0_lt_ecci(nclass)
            pmn         = plc_mn_lt_ecci(nclass)
            pmx         = plc_mx_lt_ecci(nclass)
            plaimn      = lai_mn_lt_ecci(nclass)
            plaimx      = lai_mx_lt_ecci(nclass)
            proot       = rd_lt_ecci(nclass)
            pskinc      = skinc_lt_ecci(nclass)
            prs_min     = rs_min_lt_ecci(nclass)
            pemissivity = emiss_lt_ecci(nclass)
            purb    = 0.0
            pfor_d  = 0.0
            pfor_e  = 0.0
            pice    = 0.0

            IF (lu==190             ) purb   = 1.0  ! artificial surfaces
            IF (lu== 50 .OR. lu== 60) pfor_d = 1.0  ! deciduous forest
            IF (lu== 40 .OR. lu== 70 .OR. lu== 90) pfor_e = 1.0  ! evergreen forest
            IF (lu== 100) THEN                      ! mixed forest
              pfor_d = 0.5
              pfor_e = 0.5
            END IF
            IF (lu==220            ) pice   = 1.0  ! ice or snow pixel
          ELSE IF (lu==210) THEN ! water
            k_error     = 0
            pland       = 0.0
            pskinc      = skinc_lt_ecci(nclass)
            pemissivity = emiss_lt_ecci(nclass)             ! emissivity is required everywhere
          ELSE
            k_error     = 1  ! not a valid land use class
            pland       = 0.0
            pskinc      = 200.0
          END IF

  END  SUBROUTINE ecci_look_up

  SUBROUTINE get_ecci_idx(lu,nclass)
    INTEGER, INTENT(IN) :: lu             !< land use class
    INTEGER, INTENT(OUT) :: nclass !< position of landuse class in arrays
!  INTEGER :: ecci_value(nlcass_ecci) =          (/&    ! No.
! &  11 , & ! 'irrigated croplands                           ' ! 1.
! &  14,  & ! 'rainfed croplands                               ! 2.
! &  20,  & !  'mosaic cropland (50-70%) - vegetation (20-50%)'! 3.
! &  30,  & ! 'mosaic vegetation (50-70%) - cropland (20-50%)' ! 4.
! &  40,  & ! 'closed broadleaved evergreen forest           ' ! 5.
! &  50,  & ! 'closed broadleaved deciduous forest           ' ! 6.
! &  60,  & ! 'open broadleaved deciduous forest             ' ! 7.
! &  70,  & ! 'closed needleleaved evergreen forest          ' ! 8.
! &  90,  & ! 'open needleleaved decid. or evergr. forest    ' ! 9.
! & 100,  & ! 'mixed broadleaved and needleleaved forest     ' ! 10.
! & 110,  & ! 'mosaic shrubland (50-70%) - grassland (20-50%)' ! 11.
! & 120,  & ! 'mosaic grassland (50-70%) - shrubland (20-50%)' ! 12.
! & 130,  & ! 'closed to open shrubland                      ' ! 13.
! & 140,  & ! 'closed to open herbaceous vegetation          ' ! 14.
! & 150,  & ! 'sparse vegetation                             ' ! 15.
! & 160,  & ! 'closed to open forest regulary flooded        ' ! 16.
! & 170,  & ! 'closed forest or shrubland permanently flooded' ! 17.
! & 180,  & ! 'closed to open grassland regularly flooded    ' ! 18.
! & 190,  & ! 'artificial surfaces                           ' ! 19.
! & 200,  & ! 'bare areas                                    ' ! 20.
! & 210,  & ! 'water bodies                                  ' ! 21.
! & 220,  & ! 'permanent snow and ice                        ' ! 22.
! & 230 /)  !'undefined                                      ' ! 23.




  SELECT CASE(lu)
     CASE (11) ! ecci_value(1)
       nclass = 1
     CASE (14) !ecci_value(2)
       nclass = 2
     CASE (20) ! ecci_value(3)
       nclass = 3
     CASE (30) ! ecci_value(4)
       nclass = 4
     CASE (40) ! ecci_value(5)
       nclass = 5
     CASE (50) ! ecci_value(6)
       nclass = 6
     CASE (60) ! ecci_value(7)
       nclass = 7
     CASE (70) ! ecci_value(8)
       nclass = 8
     CASE (90) ! ecci_value(9)
       nclass = 9
     CASE (100) !  ecci_value(10)
       nclass = 10
       CASE (110) !  ecci_value(11)
       nclass = 11
     CASE (120) !  ecci_value(12)
       nclass = 12
     CASE (130) ! ecci_value(13)
       nclass = 13
     CASE (140) ! ecci_value(14)
       nclass = 14
     CASE (150) ! ecci_value(15)
       nclass = 15
     CASE (160) ! ecci_value(16)
       nclass = 16
     CASE (170) ! ecci_value(17)
       nclass = 17
     CASE (180) ! ecci_value(18)
       nclass = 18
     CASE (190) ! ecci_value(19)
       nclass = 19
     CASE (200) ! ecci_value(20)
       nclass = 20
     CASE (210) ! ecci_value(21)
       nclass = 21
     CASE (220) ! ecci_value(22)
       nclass = 22

     CASE DEFAULT
       nclass = 23 ! undefined

  END SELECT


  END SUBROUTINE get_ecci_idx


END MODULE mo_ecci_lookup_tables

