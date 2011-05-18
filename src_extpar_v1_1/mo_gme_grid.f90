!+ Fortran module with routines for the GME target grid 
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
!
! Code Description:
! Language: Fortran 95.
!=======================================================================
!> Fortran module with routines for the GME target grid 
!> definition of coordinates, input/ouptut routines
!> \author Hermann Asensio
!> Note: this program is used to generate external parameters for the GME. 
!>       It originates from old code for the GME grid from
!>       B. Ritter, A. Mueller, D. Majewski and other DWD staff members.

MODULE mo_gme_grid

!> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  USE mo_grid_structures, ONLY: gme_triangular_grid
  USE mo_grid_structures, ONLY: target_grid_def
  USE mo_grid_structures, ONLY: igrid_gme

  USE mo_utilities_extpar, ONLY: abort_extpar
  
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: nd,ni,ni2,ni3,ig1s,ig1e,ig2s,ig2e ! this was the common block /param_gme/
  
  PUBLIC :: xn, rlon_gme, rlat_gme,        &  ! this was the common block /horgrid_gme/ 
    &       ispoke, ispokes, i1mrp, i2mrp, &
    &       dxmin, dhmin
 ! PUBLIC :: xns
  PUBLIC :: gme_real_field, gme_int_field, gme_i4_field
  PUBLIC :: init_gme_grid
  PUBLIC :: get_gme_grid_info
  PUBLIC :: gme_grid

  PUBLIC :: factorni
  PUBLIC :: spoke, same, distance

  PUBLIC :: sync_diamond_edge
  PUBLIC :: cp_buf2gme, cp_gme2buf
  
  INTEGER (KIND=i4) :: nd !< number of diamonds 
  INTEGER (KIND=i4) :: ni !< number of intervals on a main triangle side; ni=3**ni3*2**ni2 with ni3 0 or 1 and ni2 > 1
  INTEGER (KIND=i4) :: ni2 !< ni2 > 1
  INTEGER (KIND=i4) :: ni3 !< ni3 0 or 1
  INTEGER (KIND=i4) :: ig1s !< first dimension of arrays, start index  (ig1s >= 0)
  INTEGER (KIND=i4) :: ig1e !< first dimension of arrays, end index    (ig1e <= ni)
  INTEGER (KIND=i4) :: ig2s !< second dimension of arrays, start index (ig2s >= 1) 
  INTEGER (KIND=i4) :: ig2e !< second dimension of arrays, end index   (ig2e <= ni+1)

  REAL (KIND=wp), ALLOCATABLE  :: rlon_gme(:, :, :) !< geographical longitude of the gridpoints; phys. dim. (radians)
  REAL (KIND=wp), ALLOCATABLE  :: rlat_gme(:, :, :) !< geographical latitude of the gridpoints; phys. dim. (radians)
  REAL (KIND=wp), ALLOCATABLE  :: gme_real_field(:,:,:) !< GME real field
  INTEGER (KIND=i8), ALLOCATABLE :: gme_int_field(:,:,:) !< GME integer field
  INTEGER (KIND=i4), ALLOCATABLE :: gme_i4_field(:,:,:) !< GME integer field


  REAL (KIND=wp), ALLOCATABLE  :: xn(:,:,:,:) !< xn(ig1s:ig1e, ig2s:ig2e, 3, nd):  Cartesian (x,y,z) coordinates
                                              ! of the location vector of the gridpoints (nodes)
                                              ! on the unit-sphere; phys. dim. ( - )
 !REAL (KIND=wp), ALLOCATABLE  :: xns(:,:,:,:) !< xns(ig2s:ig2e,ig2s:ig2e, 3, nd):  Cartesian (x,y,z) coordinates
                                              ! of the location vector of the gridpoints (nodes)
                                              ! on the unit-sphere; phys. dim. ( - )

  REAL (KIND=wp) :: dxmin !< Minimum mesh size
  REAL (KIND=wp) :: dhmin !< Minimum triangle height

  INTEGER (KIND=i4) :: ispoke (12) !< offsets of the 6 (5) neighbouring gridpoints relative
                                   !! to the central node for 2-dimensional array addressing; in 
                                   !! i1-direction use ispoke(m), m=1,6 (5), in i2-direction use
                                   !! ispoke(m+6), m=1,6 (5); phys. dim. ( - )
                                   !! The four (mirrored) points at the corners of the array which
                                   !! has been extended by 1 row/column around, need other spokes,
                                   !! namely "ispokes".
  INTEGER (KIND=i4) :: ispokes(12,4) !< offsets of the 6 neighbouring gridpoints relative offsets of the 6 neighbouring gridpoints relative
  INTEGER (KIND=i4) :: i1mrp(8) !< i1-index of the four mirrored points of the extended array
  INTEGER (KIND=i4) :: i2mrp(8) !< i2-index of the four mirrored points of the extended array

  TYPE(gme_triangular_grid) ::  gme_grid  !< structure which contains the definition of the GME grid

  
  INTERFACE sync_diamond_edge
     MODULE PROCEDURE sync_diamond_edge_contr_real
     MODULE PROCEDURE sync_diamond_edge_contr_int
     MODULE PROCEDURE sync_diamond_edge_contr_int4
  END INTERFACE sync_diamond_edge

  INTERFACE cp_buf2gme
     MODULE PROCEDURE cp_buf2gme_r
     MODULE PROCEDURE cp_buf2gme_i
     MODULE PROCEDURE cp_buf2gme_i4
  END INTERFACE cp_buf2gme
  
  INTERFACE cp_gme2buf
     MODULE PROCEDURE cp_gme2buf_r
     MODULE PROCEDURE cp_gme2buf_i
     MODULE PROCEDURE cp_gme2buf_i4
  END INTERFACE cp_gme2buf






  CONTAINS

    !> get resolution information for GME grid for namelist
    SUBROUTINE get_gme_grid_info(input_namelist_file,tg,gme_grid)

     USE mo_io_units, ONLY: filename_max
     USE mo_utilities_extpar, ONLY: free_un

     CHARACTER (len=filename_max), INTENT(IN) :: input_namelist_file !< file with input namelist with GME grid definition
     TYPE(target_grid_def), INTENT(OUT)      :: tg               !< structure with target grid description
     TYPE(gme_triangular_grid), INTENT(OUT) :: gme_grid !< structure which contains the definition of the GME grid

     ! local variables
     INTEGER  :: nuin !< unit number
     INTEGER  :: ierr !< error flag


     NAMELIST /gme_grid_setup/ ni

     nuin = free_un()  ! functioin free_un returns free Fortran unit number
     PRINT *,'input_namelist_file: ', TRIM(input_namelist_file)
     OPEN(nuin,FILE=TRIM(input_namelist_file), IOSTAT=ierr)
     READ(nuin, NML=gme_grid_setup, IOSTAT=ierr)
     CLOSE(nuin)
     PRINT *,'ni: ',ni
     nd   = 10
     ig1s = 0
     ig1e = ni
     ig2s = 1
     ig2e = ni+1

     gme_grid%ni = ni
     gme_grid%ig1s = ig1s
     gme_grid%ig1e = ig1e
     gme_grid%ig2s = ig2s
     gme_grid%ig2e = ig2e
     gme_grid%nd = nd
     gme_grid%nip1 = ni+1


     tg%igrid_type = igrid_gme
     tg%ie = ig1e - ig1s + 1 ! ni +1
     tg%je = ig2e - ig2s + 1 ! ni +1
     tg%ke = nd              ! nd


     END  SUBROUTINE get_gme_grid_info





   
    !> allocate the variables for the GME grid and calcualate the coordinates rlon_gme, rlat_gme 
    !! and xn (cartesian coordinates) for each grid element
    SUBROUTINE init_gme_grid(gme_grid) 
      TYPE(gme_triangular_grid), INTENT(IN) :: gme_grid !< structure which contains the definition of the GME grid
      INTEGER :: errorcode
      LOGICAL :: ldebug=.FALSE.

      ALLOCATE ( xn(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,1:3,1:gme_grid%nd), STAT=errorcode )
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array xn')
      xn = 0.0

 !     ALLOCATE ( xns(gme_grid%ig2s:gme_grid%ig2e,gme_grid%ig2s:gme_grid%ig2e,1:3, 1:gme_grid%nd), STAT=errorcode )
  !      IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array xn')
   !   xn = 0.0



      ALLOCATE ( rlon_gme(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd), STAT=errorcode )
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array rlon_gme')
      rlon_gme = 0.0
      ALLOCATE ( rlat_gme(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd), STAT=errorcode )
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array rlat_gme')
      rlat_gme=0.0


       ALLOCATE ( gme_real_field(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd), STAT=errorcode )
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array gme_real_field')
      gme_real_field=0.0
       ALLOCATE ( gme_int_field(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd), STAT=errorcode )
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array gme_int_field')
      gme_int_field=0

      ALLOCATE ( gme_i4_field(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd), STAT=errorcode )
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array gme_i4_field')
      gme_i4_field=0



      !=======================================================================
      !
      !     PART I  Calculate the horizontal grid
      !
      !     1. Factorize ni into 3**ni3 * 2**ni2 with ni3 0 or 1 and ni2 > 1,
      !        if possible, otherwise abort the program
      CALL factorni (ni, ldebug, ni2, ni3, errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('factorize ni failed')
      !=======================================================================
      !
      !     2. Calculate the arrays xn, rlon_gme, rlat_gme, and corio_gme for
      !        all 10 diamonds
      !
      CALL glo_coor (xn    , rlon_gme  , rlat_gme  ,   &
       &              ig1s  ,  ig1e  ,  ig2s  ,  ig2e  , nd ,   &
       &              ni     , ni2    ,ni3     , ldebug , errorcode)

        IF(errorcode.NE.0) CALL abort_extpar('No coordinates for GME grid! STOP!')

 !       xns(gme_grid%ig2s:gme_grid%ig2e,gme_grid%ig2s:gme_grid%ig2e,1:3, 1:gme_grid%nd) = &
  !      &  xn(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,1:3,1:gme_grid%nd)  ! shift index to (1:nip1,1:nip1,1:2,1:nd)

 
    END SUBROUTINE init_gme_grid

    !> calculates the global arrays which define the triangular grid.
    SUBROUTINE glo_coor (pxn    , prlon  , prlat  ,                 &
                         kig1s  , kig1e  , kig2s  , kig2e  , knd,   &
                         kni    , kni2   , kni3   , ldebug , kierr)
    ! Description:
    !
    !
    !
    ! Note: this program is used to generate external parameters for the GME. 
    !       It originates from old code from
    !       B. Ritter, A. Mueller, J. Helmert and other DWD staff members.
    !       In a future version all code related to external parameters should be 
    !       ported into a new version using Fortran Modules.
    ! Current Code Owner: DWD, Hermann Asensio
    !  phone:  +49  69  8062 2709
    !  fax:    +49  69  8062 3721
    !  email:  Hermann.Asensio@dwd.de 
    !
    !
    ! ---------- ---------- ----
    !     -      May 1996   D. Majewski based on J. Baumgardner
    !  Initial release
    ! 1.9        1998/03/18 D. Majewski / D. Liermann
    !  MPP Version (R. Johanni/CRAY)
    !  New header - installation of template
    !  Set zxn to 0 if it is less than 2.5 e-14 to avoid round-off errors
    ! 1.10       1998/05/18 R. Johanni
    !  Make zxn global : xnglob  
    ! 1.12       1998/12/11 D. Majewski
    !  Insert subroutine *gcpt* into *glo_coor*
    !  Make xnglob a dummy argument; remove INCLUDE of horgrid.h and
    !  globallimits.h
    ! 1.13       1999/05/05 B.Ritter strip routine of all code not needed,
    !                       when only basic grid properties (xn,rlon,rlat)
    !                       are required
    ! Declarations:
    !
    !     Input/Output
    !  
    !     prlon  (kig1s  :kig1e  , kig2s  :kig2e  ,    knd):  geographical
    !            longitude of the gridpoints; phys. dim. (radians)
    !     prlat  (kig1s  :kig1e  , kig2s  :kig2e  ,    knd):  geographical
    !            latitude of the gridpoints; phys. dim. (radians)
    !     pxn    (0      :kni    , 1      :kni+1  , 3, knd) REAL:  Cartesian
    !            (x,y,z) coordinates of the location vector of the grid
    !            points (nodes) on the unit-sphere; phys. dim. ( - )
    !            Full domain of the diamonds
    !  
    !     kig1s   INTEGER  first dimension of arrays, start index (kig1s = 0)
    !     kig1e   INTEGER  first dimension of arrays, end index   (kig1e = ni)
    !     kig2s   INTEGER  sec. dimension of arrays, start index  (kig2s = 1)
    !     kig2e   INTEGER  sec. dimension of arrays, end index    (kig2e = ni+1)
    !     knd     INTEGER  number of diamonds                     (knd = 10)
    !     kni     INTEGER  number of intervals on main triangle edge
    !     kni2    INTEGER  exponent of factor "2" in the factorization of ni,
    !                      i.e. the number of bisections to perform
    !     kni3    INTEGER  exponent of factor "3" in the factorization of ni,
    !                      i.e. the number of trisections to perform (0 or1)
    !     ldebug  LOGICAL  debug flag, if .true. print debug information
    !
    !=======================================================================
    !
    !     Output
    !     kierr   INTEGER  error flag, set to 0, if no error occured
    !
    !=======================================================================
    !
          IMPLICIT NONE
    !
    !
    !     Dummy arguments
    INTEGER  kig1s, kig1e, kig2s, kig2e, knd , kierr,kni2 , kni3 , kni
    !
    REAL (KIND=wp) :: pxn    (kig1s  :kig1e  , kig2s  :kig2e  , 3, knd), &
     &                prlon  (kig1s  :kig1e  , kig2s  :kig2e  ,    knd), &
     &                prlat  (kig1s  :kig1e  , kig2s  :kig2e  ,    knd) 
    !
    LOGICAL  ldebug
    !
    !=======================================================================
    !
    !     Local arrays and variables
        
    REAL PI,Pid5     ! Pi and Pi/5.

    REAL zw   ,  & ! the spherical angle in an icosahedron
                   ! subtended by two vertices.
         zcosw,  & ! cosine(zw)
         zsinw,  & ! sine  (zw)
         zsgn ,  & ! zsgn is a hemisphere factor.
                   ! zsgn =  1.0 is north  (diamonds 1- 5)
                   ! zsgn = -1.0 is south  (diamonds 6-10)
         zrlon,  & ! longitude of diamond vertices
         zgamma, & ! fraction of great circle angle
         zchord, & ! Cartesian distance between two points
         ztheta, & ! Great circle angle between two points
         zalpha, & ! Weighting factor
         zbeta     ! Weighting factor
    !
    INTEGER  mcosv(knd)  ! meridian angle locations of the 10
    !                            non-polar vertices in units of Pi/5
    INTEGER  ml,   &     ! recursive index interval
             ml2,  &     ! recursive bisected index interval
             ml3,  &     ! trisected index interval
             mi1,  &     ! recursive row index of new node
             mi2,  &     ! recursive column index of new node
             mm ,  &     ! recursive number of subdivisions
             mni         ! number of subdivisions;
    !                      mni=2**kni2 * 3**kni3
    INTEGER  j1, j2, jd, jb  ! Loop indices
    !
    CHARACTER  yfld*10
    !
    !
    Pi   = 4.*ATAN(1.)   
    Pid5 = Pi/5.

    ! ======================================================================
    !
    !     1. Calculate the Cartesian coordinates of the gridpoints of the
    !        icosahedral grid on the unit sphere.  The grid resolution
    !        corresponds to a subdivision of the edges of the original
    !        icosahedral triangles into mni equal parts.
    !
    !     Compute angles associated with the icosahedron.
    zw      = 2.0*acos(1./(2.*sin(Pid5)))
    zcosw   = cos(zw)
    zsinw   = sin(zw)
    mni     = 2**kni2 * 3**kni3
    !
    !     Compute the local array mcosv, i.e. the meridian angle locations
    DO jd = 1,knd
    IF (MOD(jd,2) .eq. 1) THEN
      mcosv((jd+1)/2) = -1 + (jd - 1) - knd*((jd - 1)/7)
    ELSE   
      mcosv(jd/2+5)   = -1 + (jd - 1) - knd*((jd - 1)/7)
    ENDIF    
    ENDDO 
    !
    ! ======================================================================
    !
    !     Loop over the ten diamonds computing diamond vertices (x,y,z)
    !     coordinates and then iteratively bisecting them kni2 times.
    !     First a trisection is performed, if required (kni3=1).
    DO jd = 1,knd     ! Loop over the diamonds
    !
    !     Toggle the hemisphere
      IF (jd .GE. 6) THEN
        zsgn = -1.    ! southern
      ELSE
        zsgn =  1.    ! northern
      ENDIF 
    !
    !     Compute the meridian angle for each diamond "home" vertex.
      zrlon = mcosv(jd)*Pid5
    !
    !     Every diamond has one vertex at a pole (N or S).
    !     Label this point (0,1,,) in each diamond, and
    !     initialize it to have the (x,y,z) coordinates of
    !     the pole point on the unit sphere.
      pxn    (  0,    1, 1, jd) =  0.
      pxn    (  0,    1, 2, jd) =  0.
      pxn    (  0,    1, 3, jd) =  zsgn
    !
    !     Now initialize the (x,y,z) coordinates of the "home" vertex,
    !     which defines which diamond we are talking about, and label
    !     this point (mni,1,,).
      pxn    (mni,    1, 1, jd) =  zsinw*cos(zrlon)
      pxn    (mni,    1, 2, jd) =  zsinw*sin(zrlon)
      pxn    (mni,    1, 3, jd) =  zcosw*zsgn
    !
    !     Next initialize the (x,y,z) coordinates for the corner of the
    !     diamond on the same latitude as the (mni,1,,) vertex, which
    !     is (0,mni+1,,)
      pxn    (  0,mni+1, 1, jd) =  zsinw*cos(zrlon + 2*Pid5)
      pxn    (  0,mni+1, 2, jd) =  zsinw*sin(zrlon + 2*Pid5)
      pxn    (  0,mni+1, 3, jd) =  zcosw*zsgn
    !
    !     Initialize the last diamond vertex, which is located
    !     in the opposite hemisphere as (mni,mni+1,,)
      pxn    (mni,mni+1, 1, jd) =  zsinw*cos(zrlon + Pid5)
      pxn    (mni,mni+1, 2, jd) =  zsinw*sin(zrlon + Pid5)
      pxn    (mni,mni+1, 3, jd) = -zcosw*zsgn
    !
    ! ======================================================================
    !
    !     First a trisection is performed, if required (kni3=1).
      IF (kni3 .EQ. 1) THEN
        ml3 = mni/3
    !
    !     Trisect the rows of the diamond.
        DO j1 = 1,2
          DO j2 = 1,2
          mi1    = (j1-1)*mni
          mi2    = j2*ml3 + 1
          zgamma = REAL(j2)/3.
    !
    !     Calculate "zchord", the Cartesian distance between x1 and x2
          zchord =                                                     &
          SQRT ( (pxn    (mi1,mni+1,1,jd) - pxn    (mi1,1,1,jd))**2 +  &
                 (pxn    (mi1,mni+1,2,jd) - pxn    (mi1,1,2,jd))**2 +  &
                 (pxn    (mi1,mni+1,3,jd) - pxn    (mi1,1,3,jd))**2 )
    !
    !     Calculate "ztheta", the great circle angle between x1 and x2 
          ztheta = 2.*ASIN (0.5*zchord)
    !
    !     Calculate the weighting factors which follow from the condition
    !     that x is a point on the unit-sphere, too.
          zbeta  = SIN (zgamma*ztheta)/SIN (ztheta)
          zalpha = SIN ((1.-zgamma)*ztheta)/SIN (ztheta)
    !
    !     Store the (x,y,z) coordinates of the point x
          pxn (mi1,mi2,1,jd)=zalpha*pxn(mi1,1 ,1,jd)+zbeta*pxn(mi1,mni+1,1,jd)
          pxn (mi1,mi2,2,jd)=zalpha*pxn(mi1,1 ,2,jd)+zbeta*pxn(mi1,mni+1,2,jd)
          pxn (mi1,mi2,3,jd)=zalpha*pxn(mi1,1 ,3,jd)+zbeta*pxn(mi1,mni+1,3,jd)
    !
          ENDDO
        ENDDO
    !
    !     Trisect the columns of the diamond.
        DO j1 = 1,2
          DO j2 = 1,2
          mi1    = j2*ml3
          mi2    = (j1-1)*mni + 1
          zgamma = REAL(j2)/3.
    !
    !     Calculate "zchord", the Cartesian distance between x1 and x2
          zchord =                                                  &
          SQRT ( (pxn (mni,mi2,1,jd) - pxn (0,mi2,1,jd))**2 + &     
                 (pxn (mni,mi2,2,jd) - pxn (0,mi2,2,jd))**2 + &
                 (pxn (mni,mi2,3,jd) - pxn (0,mi2,3,jd))**2 )
    !
    !     Calculate "ztheta", the great circle angle between x1 and x2 
          ztheta = 2.*ASIN (0.5*zchord)
    !
    !     Calculate the weighting factors which follow from the condition
    !     that x is a point on the unit-sphere, too.
          zbeta  = SIN (zgamma*ztheta)/SIN (ztheta)
          zalpha = SIN ((1.-zgamma)*ztheta)/SIN (ztheta)
    !
    !     Store the (x,y,z) coordinates of the point x
          pxn(mi1,mi2,1,jd)=zalpha*pxn(0  ,mi2,1,jd)+zbeta*pxn(mni,mi2,1,jd)
          pxn(mi1,mi2,2,jd)=zalpha*pxn(0  ,mi2,2,jd)+zbeta*pxn(mni,mi2,2,jd)
          pxn(mi1,mi2,3,jd)=zalpha*pxn(0  ,mi2,3,jd)+zbeta*pxn(mni,mi2,3,jd)
    !
          ENDDO
        ENDDO
    !
    !     Trisect the diagonal of the diamond.
        DO j2 = 1,2
        mi1 = mni - j2*ml3
        mi2 =   1 + j2*ml3
        zgamma = REAL(j2)/3.
    !
    !     Calculate "zchord", the Cartesian distance between x1 and x2
        zchord =                                           &
        SQRT ( (pxn(0,mni+1,1,jd) - pxn(mni,1,1,jd))**2 +  &   
               (pxn(0,mni+1,2,jd) - pxn(mni,1,2,jd))**2 +  &
               (pxn(0,mni+1,3,jd) - pxn(mni,1,3,jd))**2 )
    !
    !     Calculate "ztheta", the great circle angle between x1 and x2 
        ztheta = 2.*ASIN (0.5*zchord)
    !
    !     Calculate the weighting factors which follow from the condition
    !     that x is a point on the unit-sphere, too.
        zbeta  = SIN (zgamma*ztheta)/SIN (ztheta)
        zalpha = SIN ((1.-zgamma)*ztheta)/SIN (ztheta)
    !
    !     Store the (x,y,z) coordinates of the point x
        pxn(mi1,mi2,1,jd)=zalpha*pxn(mni,1  ,1,jd)+zbeta *pxn(0  ,mni+1,1,jd)
        pxn(mi1,mi2,2,jd)=zalpha*pxn(mni,1  ,2,jd)+zbeta *pxn(0  ,mni+1,2,jd)
        pxn(mi1,mi2,3,jd)=zalpha*pxn(mni,1  ,3,jd)+zbeta *pxn(0  ,mni+1,3,jd)
    !
        ENDDO
    !
    !     Compute coordinates of icosahedral triangle centers.
        CALL tricntr (pxn    , 0, kni, 1, kni+1, knd, jd , mni)
    !
      ENDIF     ! End of trisection
    !
    ! ======================================================================
    !
    !     Find the coordinates of the triangle nodes by iteratively
    !     bisecting the diamond intervals.
      DO jb = 0, kni2-1
      mm  = (3**kni3)*(2**jb)
      ml  = mni/mm
      ml2 = ml/2
    !
    !     Compute the rows of the diamond.
        DO j1 = 1,mm+1
          DO j2 = 1,mm
          mi1 = (j1-1)*ml
          mi2 = (j2-1)*ml + ml2 + 1
          zgamma = 0.5
    !
    !     Calculate "zchord", the Cartesian distance between x1 and x2
          zchord=SQRT( (pxn(mi1,mi2+ml2,1,jd)-pxn(mi1,mi2-ml2,1,jd))**2 + &
                       (pxn(mi1,mi2+ml2,2,jd)-pxn(mi1,mi2-ml2,2,jd))**2 + &
                       (pxn(mi1,mi2+ml2,3,jd)-pxn(mi1,mi2-ml2,3,jd))**2 )
    !
    !     Calculate "ztheta", the great circle angle between x1 and x2
          ztheta = 2.*ASIN (0.5*zchord)
    !
    !     Calculate the weighting factors which follow from the condition
    !     that x is a point on the unit-sphere, too.
          zbeta  = SIN (zgamma*ztheta)/SIN (ztheta)
          zalpha = SIN ((1.-zgamma)*ztheta)/SIN (ztheta)
    !
    !     Store the (x,y,z) coordinates of the point x
          pxn(mi1,mi2,1,jd)=zalpha*pxn(mi1,mi2-ml2,1,jd)+zbeta *pxn(mi1,mi2+ml2,1,jd)
          pxn(mi1,mi2,2,jd)=zalpha*pxn(mi1,mi2-ml2,2,jd)+zbeta *pxn(mi1,mi2+ml2,2,jd)
          pxn(mi1,mi2,3,jd)=zalpha*pxn(mi1,mi2-ml2,3,jd)+zbeta *pxn(mi1,mi2+ml2,3,jd)
    !
          ENDDO
        ENDDO
    !
    !     Compute the columns of diamond.
        DO j1 = 1,mm+1
          DO j2 = 1,mm
          mi1 = (j2-1)*ml + ml2 
          mi2 = (j1-1)*ml + 1
          zgamma = 0.5
    !
    !     Calculate "zchord", the Cartesian distance between x1 and x2
          zchord=SQRT( (pxn(mi1+ml2,mi2,1,jd)-pxn(mi1-ml2,mi2,1,jd))**2 + &
                       (pxn(mi1+ml2,mi2,2,jd)-pxn(mi1-ml2,mi2,2,jd))**2 + &
                       (pxn(mi1+ml2,mi2,3,jd)-pxn(mi1-ml2,mi2,3,jd))**2 )
    !
    !     Calculate "ztheta", the great circle angle between x1 and x2
          ztheta = 2.*ASIN (0.5*zchord)
    !
    !     Calculate the weighting factors which follow from the condition
    !     that x is a point on the unit-sphere, too.
          zbeta  = SIN (zgamma*ztheta)/SIN (ztheta)
          zalpha = SIN ((1.-zgamma)*ztheta)/SIN (ztheta)
    !
    !     Store the (x,y,z) coordinates of the point x
          pxn(mi1,mi2,1,jd)=zalpha*pxn(mi1-ml2,mi2,1,jd)+zbeta *pxn(mi1+ml2,mi2,1,jd)
          pxn(mi1,mi2,2,jd)=zalpha*pxn(mi1-ml2,mi2,2,jd)+zbeta *pxn(mi1+ml2,mi2,2,jd)
          pxn(mi1,mi2,3,jd)=zalpha*pxn(mi1-ml2,mi2,3,jd)+zbeta *pxn(mi1+ml2,mi2,3,jd)
    !
          ENDDO
        ENDDO
    !
    !     Compute the diagonals of the diamond.
        DO j1 = 1,mm
          DO j2 = 1,mm
          mi1 = (j1-1)*ml + ml2 
          mi2 = (j2-1)*ml + ml2 + 1
          zgamma = 0.5
    !
    !     Calculate "zchord", the Cartesian distance between x1 and x2
          zchord = SQRT (                                            &
         (pxn(mi1+ml2,mi2-ml2,1,jd)-pxn(mi1-ml2,mi2+ml2,1,jd))**2 +  &
         (pxn(mi1+ml2,mi2-ml2,2,jd)-pxn(mi1-ml2,mi2+ml2,2,jd))**2 +  &
         (pxn(mi1+ml2,mi2-ml2,3,jd)-pxn(mi1-ml2,mi2+ml2,3,jd))**2 )
    !
    !     Calculate "ztheta", the great circle angle between x1 and x2
          ztheta = 2.*ASIN (0.5*zchord)
    !
    !     Calculate the weighting factors which follow from the condition
    !     that x is a point on the unit-sphere, too.
          zbeta  = SIN (zgamma*ztheta)/SIN (ztheta)
          zalpha = SIN ((1.-zgamma)*ztheta)/SIN (ztheta)
    !
    !     Store the (x,y,z) coordinates of the point x
          pxn(mi1,mi2,1,jd)=zalpha*pxn(mi1-ml2,mi2+ml2,1,jd) + &
                            zbeta *pxn(mi1+ml2,mi2-ml2,1,jd)
          pxn(mi1,mi2,2,jd)=zalpha*pxn(mi1-ml2,mi2+ml2,2,jd) + &
                            zbeta *pxn(mi1+ml2,mi2-ml2,2,jd)
          pxn(mi1,mi2,3,jd)=zalpha*pxn(mi1-ml2,mi2+ml2,3,jd) + &
                            zbeta *pxn(mi1+ml2,mi2-ml2,3,jd)
    !
          ENDDO
        ENDDO
    !
      ENDDO       ! end loop over bisections
    !
    !=======================================================================
    !
    !     Set pxn     to 0 if it is less than 2.5 e-14 to avoid round-off
    !     errors
    !
      DO j2 = 1, kni+1
        DO j1 = 0,kni
        IF (ABS(pxn(j1,j2,1,jd)) .LT. 2.5e-14) pxn(j1,j2,1,jd) = 0.
        IF (ABS(pxn(j1,j2,2,jd)) .LT. 2.5e-14) pxn(j1,j2,2,jd) = 0.
        IF (ABS(pxn(j1,j2,3,jd)) .LT. 2.5e-14) pxn(j1,j2,3,jd) = 0.
        ENDDO
      ENDDO
    !
    !=======================================================================
    !
    !
    ENDDO         ! end loop over diamonds
    !
    !=======================================================================
    !
    !     Calculate the longitude "prlon" and the latitude "prlat";
    !     only for the core of the diamonds, not the extended ones.
    DO jd = 1,knd     ! Loop over the diamonds
      DO j2 = kig2s, kig2e
        DO j1 = kig1s, kig1e
        prlon (j1,j2,jd) = ATAN2(pxn(j1,j2,2,jd),pxn(j1,j2,1,jd)+1.e-20)
        prlat (j1,j2,jd) = ASIN (pxn(j1,j2,3,jd))
        ENDDO
      ENDDO
    ENDDO         ! end loop over diamonds
    !
    !=======================================================================
    !
    RETURN
    END SUBROUTINE glo_coor

    !> finds the coordinates of the node at the center of the two icosahedral triangles comprising diamond kjda
    !! Description:
    !!=======================================================================
    !!
    !!     *tricntr* finds the coordinates of the node at the center
    !!     of the two icosahedral triangles comprising diamond kjd. These
    !!     nodes are present when ni contains the factor 3, i.e. if ni3 = 1.
    SUBROUTINE tricntr (pxn, kig1s, kig1e, kig2s, kig2e, knd, kjd, kni)
      !
      ! Note: this program is used to generate external parameters for the GME. 
      !       It originates from old code from
      !       B. Ritter, A. Mueller, J. Helmert and other DWD staff members.
      !       In a future version all code related to external parameters should be 
      !       ported into a new version using Fortran Modules.
      ! Current Code Owner: DWD, Hermann Asensio
      !  phone:  +49  69  8062 2709
      !  fax:    +49  69  8062 3721
      !  email:  Hermann.Asensio@dwd.de 
      !=======================================================================
      !
      !     Input 
      !     pxn     REAL      pxn(kig1s:kig1e, kig2s:kig2e, 3, knd): Carte-
      !                       sian (x,y,z) coordinates of the location vector
      !                       of the gridpoints (nodes) on the unit-sphere;
      !                       phys. dim. ( - )
      !     kig1s   INTEGER   first  dimension of array, start index
      !     kig1e   INTEGER   first  dimension of array, end   index
      !     kig2s   INTEGER   second dimension of array, start index
      !     kig2e   INTEGER   second dimension of array, end   index
      !     knd     INTEGER   number of diamonds (knd = 10)
      !     kjd     INTEGER   index of actual diamond (kjd = 1 to 10)
      !     kni     INTEGER   number of intervals on a main triangle side
      !
      !=======================================================================
      !
      !     Output 
      !     pxn     REAL      see above
      !
      !
      !     
      !     Dummy arrays and variables
      INTEGER  kig1s, kig1e, kig2s, kig2e, knd, kjd, kni
      REAL (KIND=wp) :: pxn(kig1s:kig1e, kig2s:kig2e, 3, knd)

      !=======================================================================
      !
      !     Local variables
      REAL zxnorm            ! norm temporary
       
      INTEGER j,      &      ! loop index
        &      mi1,    &      ! index of center point
        &      mi2            ! index of top or bottom diamond corner
      !
      !
      !=======================================================================
      !
        DO j = 1,2   ! Loop over the two triangles
          mi1  = j*kni/3
          mi2  = 1 + (j - 1)*kni
          pxn(mi1,mi1+1,1,kjd) = pxn(mi2-1,mi2  ,1,kjd) + & 
                                 pxn(kni  ,1    ,1,kjd) + &
                                 pxn(0    ,kni+1,1,kjd)
          pxn(mi1,mi1+1,2,kjd) = pxn(mi2-1,mi2  ,2,kjd) + & 
                                 pxn(kni  ,1    ,2,kjd) + &
                                 pxn(0    ,kni+1,2,kjd)
          pxn(mi1,mi1+1,3,kjd) = pxn(mi2-1,mi2  ,3,kjd) + & 
                                 pxn(kni  ,1    ,3,kjd) + &
                                 pxn(0    ,kni+1,3,kjd)
          !     Normalize to unit-sphere
          zxnorm = 1./SQRT (pxn(mi1,mi1+1,1,kjd)**2 + &
                            pxn(mi1,mi1+1,2,kjd)**2 + &
                            pxn(mi1,mi1+1,3,kjd)**2) 
          pxn(mi1,mi1+1,1,kjd) = zxnorm*pxn(mi1,mi1+1,1,kjd)
          pxn(mi1,mi1+1,2,kjd) = zxnorm*pxn(mi1,mi1+1,2,kjd)
          pxn(mi1,mi1+1,3,kjd) = zxnorm*pxn(mi1,mi1+1,3,kjd)
        ENDDO    ! End of loop over triangles

        !=======================================================================
    END SUBROUTINE tricntr

    !> *factorni* computes the factors of the integer input kni
    SUBROUTINE factorni (kni, ldebug, kni2, kni3, kierr)
    !
    ! Description:
    !     *factorni* computes the factors of the integer input kni, assuming
    !     that kni decomposes into kni3 factors (kni3 either 0 or 1) of "3"
    !     and kni2 (kni2 > 0) factors of "2". The subroutine returns the
    !     number of factors of "3", kni3, number of factors of "2", kni2,
    !     and sets the error flag kierr=0 if kni can be expressed this way.
    !     If kni cannot be expressed in this way, the error flag kierr is 
    !     set to -1. 
    !     If the debug flag (ldebug) is set to .true., all input and output
    !     variables are printed.
    !
    !
    ! Note: this program is used to generate external parameters for the GME. 
    !       It originates from old code from
    !       B. Ritter, A. Mueller, J. Helmert and other DWD staff members.
    !       In a future version all code related to external parameters should be 
    !       ported into a new version using Fortran Modules.
    ! Current Code Owner: DWD, Hermann Asensio
    !  phone:  +49  69  8062 2709
    !  fax:    +49  69  8062 3721
    !  email:  Hermann.Asensio@dwd.de 
    !
    !
    !     Input
    !     kni      INTEGER   number to be factorized
    !     ldebug   LOGICAL   debug flag; if .true. print information
    !
    !=======================================================================
    !     
    !     Output
    !     kni3     INTEGER   exponent of "3", either 0 or 1
    !     kni2     INTEGER   exponent of "2", kni2 > 0
    !     kierr    INTEGER   error flag (0: no error, -1: error)
    !
    !
    IMPLICIT NONE
    !     Dummy arguments
    LOGICAL  ldebug
    INTEGER  kni, kni2, kni3, kierr
    !
    !=======================================================================
    !
    !     Local variables
    INTEGER  mx   ! auxiliary variable, set to kni initially
    !
    !=======================================================================
      !
      !     Start of the factorization of kni
      mx    = kni
      kierr = 0
      kni2  = 0
      kni3  = 0
      IF (ldebug) THEN
        PRINT *,'  SUBROUTINE *factorni*, Input:  kni= ', kni
      ENDIF
      !
      DO WHILE (mx.GT.1) 
        IF (MOD(mx,2).EQ.0) THEN
          kni2  = kni2 + 1
          mx    = mx/2
        ELSE IF (MOD(mx,3).EQ.0) THEN
          kni3  = kni3 + 1
          mx    = mx/3
        ELSE
          kierr = -1
          RETURN
        ENDIF
      ENDDO
      !
      !     kni3 must not be greater than 1
      IF (kni3.GT.1) kierr = -1
      !
      IF (ldebug) THEN
        PRINT *,'  SUBROUTINE *factorni*, Output: kni3= ', kni3, &
                '  kni2= ', kni2, '  kierr= ', kierr
      ENDIF
      !=======================================================================
    END SUBROUTINE factorni 

    !> determines identical nodes for points on edge of a diamond
    SUBROUTINE same(k1,k2,kd,kni,kid,k1id,k2id,kdid)
    ! Note: this program is used to generate external parameters for the GME. 
    !       It originates from old code from
    !       B. Ritter, A. Mueller, J. Helmert and other DWD staff members.
    !       In a future version all code related to external parameters should be 
    !       ported into a new version using Fortran Modules.
    !     arguments:
    !     ---------
    !     k1  : 'j1'-index of edge point              INPUT
    !     k2  : 'j2'-index of edge point              INPUT
    !     kd  : diamond index of edge point           INPUT
    !     kni : model reosultion parameter            INPUT

    !     kid : no. of idential nodes                 OUTPUT
    !     k1id: 'j1' index of all identical nodes     OUTPUT
    !     k2id: 'j2' index of all identical nodes     OUTPUT
    !     kdid: diamond index of all identical nodes  OUTPUT

    INTEGER, INTENT(IN) :: k1 !< k1  : 'j1'-index of edge point 
    INTEGER, INTENT(IN) :: k2 !< k2  : 'j2'-index of edge point    
    INTEGER, INTENT(IN) :: kd !< kd  : diamond index of edge point
    INTEGER, INTENT(IN) :: kni !< kni : model reosultion parameter 

    INTEGER, INTENT(OUT) :: kid !<  kid : no. of idential nodes   
    INTEGER, INTENT(OUT) :: kdid(5) !< k1id: 'j1' index of all identical nodes
    INTEGER, INTENT(OUT) :: k1id(5) !< k2id: 'j2' index of all identical nodes  
    INTEGER, INTENT(OUT) :: k2id(5) !< kdid: diamond index of all identical nodes 

    ! local variables
    INTEGER mns  ,   & ! N/S-hemishere discriminator
      &      mpe  ,   & ! poleward eastern-neighbour
      &      mpw  ,   & ! poleward western-neighbour
      &      mae  ,   & ! anti-poleward eastern-neighbour
      &      maw  ,   & ! anti-poleward western-neighbour
      &      mpp  ,   & ! across pole point 'neighbour'
      &      mppm1      ! across pole point 'neighbour - 1'
   INTEGER :: jsp !< counter
    !     input node is always first point in result group
    kid = 1           ! preset no. of idential nodes
    k1id (1) = k1
    k2id (1) = k2     
    kdid (1) = kd
    !     adjacent diamonds
    !     -----------------
    mns   = (kd-1)/5
    mpe   =  kd + 1 - (kd/(5*(1+mns)))*5
    mpw   =  kd - 1 + ((mns*10+6-kd)/(5*(1+mns)))*5
    mae   =  kd + 5 - 9*mns-5*(kd/10)
    maw   =  kd + 4 + ((6-kd)/5)*5-9*mns
    mpp   =  kd + 3 - ((kd+2)/5)*5+5*mns
    mppm1 =  kd + 2 - ((kd+1)/5)*5+5*mns
    IF (k1.EQ.0.AND.k2.EQ.1) THEN   ! pole point
      kid = 5
      kdid(  2) = mpe
      kdid(  3) = mppm1
      kdid(  4) = mpp
      kdid(  5) = mpw
      DO jsp=1,kid
        k1id(jsp) = 0                    
        k2id(jsp) = 1
      END DO
    ELSE IF (k1.EQ.0.AND.k2.EQ.kni+1) THEN ! eastern corner
      kid = 3
      kdid (2) = mpe
      k1id (2) = kni 
      k2id (2) = 1
      kdid (3) = mae
      k1id (3) = kni
      k2id (3) = kni+1 
    ELSE IF (k1.EQ.kni.AND.k2.EQ.kni+1) THEN ! antipole corner
      kid = 3
      kdid (2) = maw
      k1id (2) =  0
      k2id (2) = kni+1 
      kdid (3) = mae
      k1id (3) = kni
      k2id (3) =  1 
    ELSE IF (k1.EQ.kni.AND.k2.EQ.1) THEN ! western corner 
      kid = 3
      kdid (2) = mpw
      k1id (2) =  0
      k2id (2) = kni+1 
      kdid (3) = maw 
      k1id (3) = kni
      k2id (3) = kni+1
    ELSE IF (k1.eq.0) THEN   ! poleward eastern edge, but no corner
      kid = 2
      kdid (2) = mpe
      k1id (2) =  k2-1
      k2id (2) =  1
    ELSE IF (k2.eq.1) THEN   ! poleward western edge, but no corner
      kid = 2
      kdid (2) = mpw
      k1id (2) = 0
      k2id (2) = k1 + 1
    ELSE IF (k1.eq.kni) THEN   ! anti-poleward western edge, but no corner
      kid = 2
      kdid (2) = maw
      k1id (2) = kni - k2 + 1
      k2id (2) = kni + 1
    ELSE IF (k2.eq.kni+1) THEN   ! anti-poleward eastern edge, but no corner
      kid = 2
      kdid (2) = mae 
      k1id (2) = kni
      k2id (2) = kni + 1 - k1
    END IF
  END SUBROUTINE same

  !> Find the surrounding points in the GME stencil
  SUBROUTINE spoke(k1,k2,kd,kni,knb,ks1,ks2,ksd)
  !
  !=======================================================================
  !
  ! For a given GME point (k1,k2,kd) the indices of the surrounding knb
  ! (5 or 6) points are given in the arrays ks1,ks2,ksd.
  !
  !=======================================================================
  !
  ! Code Description:
  ! Language: Fortran 90.
  !
  !=======================================================================
  !
  ! Declarations:
      IMPLICIT NONE

      INTEGER k1, k2, kd ! indices of GME point

      INTEGER kni        ! GME horizontal resolution

      INTEGER ksd (6)    ! diamonds of grid point neighbours
      INTEGER ks1 (6)    ! first indices of grid point neighbours
      INTEGER ks2 (6)    ! first indices of grid point neighbours

      INTEGER knb        ! number of neighbors (5 or 6)

      INTEGER mns  ,  & ! N/S-hemishere discriminator
     &        mpe  ,  & ! poleward eastern-neighbour
     &        mpw  ,  & ! poleward western-neighbour
     &        mae  ,  & ! anti-poleward eastern-neighbour
     &        maw  ,  & ! anti-poleward western-neighbour
     &        mpp  ,  & ! across pole point 'neighbour'
     &        mppm1     ! across pole point 'neighbour - 1'

      INTEGER jsp       ! loop variable for spoke

      INTEGER mspoke1(6)  ! offset of internal neighbours from point in 1.index
      INTEGER mspoke2(6)  ! offset of internal neighbours from point in 2.index
      DATA mspoke1 / 1, 0,-1,-1, 0, 1/
      DATA mspoke2 / 0, 1, 1, 0,-1,-1/

     !  Determination of appropriate neighbouring diamonds
        mns   = (kd-1)/5
        mpe   =  kd + 1 - (kd/(5*(1+mns)))*5
        mpw   =  kd - 1 + ((mns*10+6-kd)/(5*(1+mns)))*5
        mae   =  kd + 5 - 9*mns-5*(kd/10)
        maw   =  kd + 4 + ((6-kd)/5)*5-9*mns
        mpp   =  kd + 3 - ((kd+2)/5)*5+5*mns
        mppm1 =  kd + 2 - ((kd+1)/5)*5+5*mns
     IF     (k1.EQ.0   .OR. k2.EQ.1                          &
     &   .OR. k1.EQ.kni .or. k2.EQ.kni+1) THEN   ! edge point
       IF (k1.EQ.0 .AND. k2.EQ.1) THEN   ! pole point
       knb = 5
       DO jsp=1,knb
         ksd(jsp) = jsp + mns*5
         ks1(jsp) = 1                    
         ks2(jsp) = 1
       END DO
       ELSE IF (k1.eq.0 .and. k2.eq.kni+1) THEN ! eastern corner
        knb = 5
        ksd (1) = kd
        ks1 (1) =  1
        ks2 (1) = kni + 1
        ksd (2) = mpe
        ks1 (2) = kni
        ks2 (2) = 2
        ksd (3) = mpe
        ks1 (3) = kni - 1
        ks2 (3) = 2
        ksd (4) = kd
        ks1 (4) =  0
        ks2 (4) = kni 
        ksd (5) = kd
        ks1 (5) =  1
        ks2 (5) = kni 
       ELSE IF (k1.eq.kni .and. k2.eq.kni+1) THEN ! antipole corner
        knb = 5
        ksd (1) = maw
        ks1 (1) =  1
        ks2 (1) = kni 
        ksd (2) = maw
        ks1 (2) = 0
        ks2 (2) = kni
        ksd (3) = mae
        ks1 (3) = kni - 1
        ks2 (3) = 2
        ksd (4) = kd
        ks1 (4) = kni-1
        ks2 (4) = kni+1 
        ksd (5) = kd
        ks1 (5) = kni  
        ks2 (5) = kni 
       ELSE IF (k1.eq.kni .and. k2.eq.1) THEN ! western corner 
        knb = 5
        ksd (1) = mpw
        ks1 (1) =  1
        ks2 (1) = kni+1 
        ksd (2) = kd  
        ks1 (2) = kni
        ks2 (2) = 2
        ksd (3) = kd  
        ks1 (3) = kni - 1
        ks2 (3) = 2
        ksd (4) = kd
        ks1 (4) = kni-1
        ks2 (4) = 1 
        ksd (5) = mpw
        ks1 (5) = 1  
        ks2 (5) = kni 
       ELSE IF (k1.eq.0) THEN   ! poleward eastern edge, but no corner
        knb = 6
        ksd (1) = kd
        ks1 (1) =  1
        ks2 (1) =  k2
        ksd (2) = kd  
        ks1 (2) =  0 
        ks2 (2) =  k2+1
        ksd (3) = mpe 
        ks1 (3) = k2 - 1
        ks2 (3) = 2
        ksd (4) = mpe
        ks1 (4) = k2 - 2
        ks2 (4) = 2 
        ksd (5) = kd  
        ks1 (5) = 0  
        ks2 (5) = k2 - 1
        ksd (6) = kd  
        ks1 (6) = 1  
        ks2 (6) = k2 - 1
        ELSE IF (k2.eq.1) THEN   ! poleward western edge, but no corner
        knb = 6
        ksd (1) = mpw
        ks1 (1) = 1
        ks2 (1) = k1 + 1
        ksd (2) = kd  
        ks1 (2) = k1 + 1
        ks2 (2) = 1
        ksd (3) = kd  
        ks1 (3) = k1
        ks2 (3) = 2
        ksd (4) = kd
        ks1 (4) = k1 - 1
        ks2 (4) = 2 
        ksd (5) = kd  
        ks1 (5) = k1 - 1
        ks2 (5) = 1  
        ksd (6) = mpw 
        ks1 (6) = 1  
        ks2 (6) = k1
        ELSE IF (k1.eq.kni) THEN   ! anti-poleward western edge, but no corner
        knb = 6
        ksd (1) = maw
        ks1 (1) = kni - k2 + 1
        ks2 (1) = kni
        ksd (2) = kd  
        ks1 (2) = kni 
        ks2 (2) = k2 + 1
        ksd (3) = kd  
        ks1 (3) = kni - 1
        ks2 (3) = k2 + 1
        ksd (4) = kd
        ks1 (4) = kni - 1
        ks2 (4) = k2 
        ksd (5) = kd  
        ks1 (5) = kni    
        ks2 (5) = k2 - 1
        ksd (6) = maw 
        ks1 (6) = kni - k2 + 2
        ks2 (6) = kni
        ELSE IF (k2.eq.kni+1) THEN   ! anti-poleward eastern edge, but no corner
        knb = 6
        ksd (1) = kd  
        ks1 (1) = k1 + 1
        ks2 (1) = kni + 1
        ksd (2) = mae 
        ks1 (2) = kni - 1 
        ks2 (2) = kni - k1 + 1
        ksd (3) = mae 
        ks1 (3) = kni - 1 
        ks2 (3) = kni - k1 + 2
        ksd (4) = kd  
        ks1 (4) = k1 - 1
        ks2 (4) = kni + 1
        ksd (5) = kd
        ks1 (5) = k1
        ks2 (5) = kni
        ksd (6) = kd  
        ks1 (6) = k1 + 1 
        ks2 (6) = kni    
        END IF
      ELSE                                ! inner point
      knb = 6
      DO jsp=1,knb
       ksd(jsp) = kd
       ks1(jsp) = k1 + mspoke1(jsp)
       ks2(jsp) = k2 + mspoke2(jsp)
      END DO
      END IF
      END SUBROUTINE spoke

    !> Initialize the point search in the GME grid
          SUBROUTINE distance(xn,ni,spd,spd_t)   
    ! Modifications: 22.1.2008: Search for largest distance between grid point and 'half' distance to neighbours
    !                           instead of using simply the distance of the pole point to its neighbours
    !                           Use offset of 0.237 (=1.-1/cos(36)) in determination of position on grid point boundary
    !                           to account for difference in distance between corner points and points on intersection line
    !                           to pentagon center
    ! Modifications: 18.5.2007: Renormalize half way scalar product to put the point back on the unit sphere
    !
    !
    ! Note: this program is used to generate external parameters for the GME. 
    !       It originates from old code from
    !       B. Ritter, A. Mueller, J. Helmert and other DWD staff members.
    !       In a future version all code related to external parameters should be 
    !       ported into a new version using Fortran Modules.
    ! Current Code Owner: DWD, Hermann Asensio
    !  phone:  +49  69  8062 2709
    !  fax:    +49  69  8062 3721
    !  email:  Hermann.Asensio@dwd.de 
    !
    !
          IMPLICIT NONE
          INTEGER                   :: ni
          REAL (KIND=wp)            :: xh, yh, zh, spd, spd_t,spd_min,spd_tmin
          REAL (KIND=wp)            :: xn(0:ni,1:ni+1,3,10)
          LOGICAL                   :: ldebug
          INTEGER                   :: inb,i1(6),i2(6),id(6)
          INTEGER                   :: j1,j2,jd,jj1,jj2,jjd,jnb 
    ldebug=.false.
    ! if (ldebug) print *,'distance in debug mode ni=',ni
    spd_min =1.0
    spd_tmin=1.0
    DO jd=1,1
    ! if (ldebug) print *,'diamond ',jd              
     DO j2=1,ni+1
    ! if (ldebug) print *,'j2      ',j2              
      DO j1=0,ni
    ! if (ldebug) print *,'j1      ',j1              
      CALL spoke (j1,j2,jd,ni,inb,i1,i2,id)
    ! if (ldebug) print *,'after spoke inb:',inb       
    ! if (ldebug) print *,'after spoke i1:',i1       
    ! if (ldebug) print *,'after spoke i2:',i2       
    ! if (ldebug) print *,'after spoke id:',id       
       DO jnb=1,inb
       jj1=i1(jnb)
       jj2=i2(jnb)
       jjd=id(jnb)
    !  xh    = 0.5*(xn(j1,j2,1,jd) + xn (jj1,jj2,1,jjd))
    !  yh    = 0.5*(xn(j1,j2,2,jd) + xn (jj1,jj2,2,jjd))
    !  zh    = 0.5*(xn(j1,j2,3,jd) + xn (jj1,jj2,3,jjd))
       xh    = xn(j1,j2,1,jd) + 0.5*1.237*(xn(jj1,jj2,1,jjd) - xn (j1,j2,1,jd))
       yh    = xn(j1,j2,2,jd) + 0.5*1.237*(xn(jj1,jj2,2,jjd) - xn (j1,j2,2,jd))
       zh    = xn(j1,j2,3,jd) + 0.5*1.237*(xn(jj1,jj2,3,jjd) - xn (j1,j2,3,jd))
       spd = xn(j1,j2,1,jd) * xn (jj1,jj2,1,jjd) +xn(j1,j2,2,jd) * xn (jj1,jj2,2,jjd) +xn(j1,j2,3,jd) * xn (jj1,jj2,3,jjd)
       spd_t = xn(j1,j2,1,jd) * xh +xn(j1,j2,2,jd) * yh +xn(j1,j2,3,jd) * zh
       spd_t = spd_t/SQRT(xh**2+yh**2+zh**2)                         ! renormalize, since |(xh,yh,zh)| < 1.0  
       spd_min =min(spd_min ,spd)
       spd_tmin=min(spd_tmin,spd_t)
       ENDDO
      ENDDO
     ENDDO
    ENDDO

    !
    !=======================================================================
    !
    spd  =spd_min
    spd_t=spd_tmin
    END SUBROUTINE distance

    !> Synchronisation of diamond corner and edge nodes of GME grid for the
    !! contributions of summation fields (REAL (KIND=wp))
    SUBROUTINE sync_diamond_edge_contr_real(gme_grid,gme_sum_field)
    TYPE(gme_triangular_grid) ::  gme_grid  !< GME grid information
    REAL (KIND=wp) :: gme_sum_field(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field

    ! local variables
    INTEGER :: n_edge
    INTEGER :: jh
    INTEGER :: jd, jda, jde
    INTEGER :: j1e, j2e, jep
    INTEGER :: jid
    INTEGER :: nid !<  kid : no. of idential nodes   
    INTEGER :: j1id(5) !< j1id: 'j1' index of all identical nodes
    INTEGER :: j2id(5) !< j2id: 'j2' index of all identical nodes  
    INTEGER :: jdid(5) !< jdid: diamond index of all identical nodes 
    INTEGER :: jj1,jj2,jjd

    REAL (KIND=wp) :: zw_sum

      !============================
      ! diamond edge synchronisation
      !============================
                               !                                            P
                               !                                           / \
                               !                                          /   \
                               ! no. of points along a 'meridional'      W  1  E A
      n_edge = 2*gme_grid%ni  ! edge of a diamond, excluding the         \   / /
                               ! 'anti-poleward' corner                    \ / /
                               !                                            A W  6
                               !                                               \
                               !                                                \
                               !                                                 P
      DO jh=1,2              ! loop over hemispheres       
        jda = (jh-1)*5 + 1     ! first diamond index in hemisphere
        jde = (jh-1)*5 + 5     ! last diamond index in hemispher
        DO jd =jda,jde         ! loop over 5 diamonds in hemisphere
          DO jep=1,n_edge        ! loop over edge nodes            
            j1e  = MIN(jep,gme_grid%ni+1) - 1 ! j1-index on edge
            j2e  = MAX(jep-gme_grid%ni,1)     ! j2-index on edge
            IF (jep.GT.1 .OR. jd.EQ.jda) THEN ! pole node is handled only 
                                              ! once per hemisphere
              CALL same (j1e,j2e,jd,gme_grid%ni,nid,j1id,j2id,jdid) ! find identical nodes 
              !     initialize summation variables
              zw_sum    = 0.0                              
              DO jid=1,nid               ! collect individual contributions
                jj1 = j1id(jid)
                jj2 = j2id(jid)
                jjd = jdid(jid)
                zw_sum    = zw_sum +  gme_sum_field(jj1,jj2,jjd)
              END DO
              DO jid=1,nid               ! redistribute results for identical nodes
                jj1 = j1id(jid)
                jj2 = j2id(jid)
                jjd = jdid(jid)
                gme_sum_field(jj1,jj2,jjd) = zw_sum
              END DO
            END IF                     ! pole node or other edge node
          END DO      ! edge nodes
        END DO      ! diamonds
      END DO      ! hemisphere

    END SUBROUTINE sync_diamond_edge_contr_real

    !> Synchronisation of diamond corner and edge nodes of GME grid for the
    !! contributions of summation fields (INTEGER (KIND=i8))
    SUBROUTINE sync_diamond_edge_contr_int(gme_grid,gme_sum_field)
    ! This code is base on Bodo Ritters code for the generation of external parameters for GME
    TYPE(gme_triangular_grid) ::  gme_grid  !< GME grid information
    INTEGER (KIND=i8) :: gme_sum_field(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field

    ! local variables
    INTEGER :: n_edge
    INTEGER :: jh
    INTEGER :: jd, jda, jde
    INTEGER :: j1e, j2e, jep
    INTEGER :: jid
    INTEGER :: nid !<  kid : no. of idential nodes   
    INTEGER :: j1id(5) !< j1id: 'j1' index of all identical nodes
    INTEGER :: j2id(5) !< j2id: 'j2' index of all identical nodes  
    INTEGER :: jdid(5) !< jdid: diamond index of all identical nodes 
    INTEGER :: jj1,jj2,jjd

    INTEGER (KIND=i8) :: zw_sum

      !============================
      ! diamond edge synchronisation
      !============================
                               !                                            P
                               !                                           / \
                               !                                          /   \
                               ! no. of points along a 'meridional'      W  1  E A
      n_edge = 2*gme_grid%ni  ! edge of a diamond, excluding the         \   / /
                               ! 'anti-poleward' corner                    \ / /
                               !                                            A W  6
                               !                                               \
                               !                                                \
                               !                                                 P
      DO jh=1,2              ! loop over hemispheres       
        jda = (jh-1)*5 + 1     ! first diamond index in hemisphere
        jde = (jh-1)*5 + 5     ! last diamond index in hemispher
        DO jd =jda,jde         ! loop over 5 diamonds in hemisphere
          DO jep=1,n_edge        ! loop over edge nodes            
            j1e  = MIN(jep,gme_grid%ni+1) - 1 ! j1-index on edge
            j2e  = MAX(jep-gme_grid%ni,1)     ! j2-index on edge
            IF (jep.GT.1 .OR. jd.EQ.jda) THEN ! pole node is handled only 
                                              ! once per hemisphere
              CALL same (j1e,j2e,jd,gme_grid%ni,nid,j1id,j2id,jdid) ! find identical nodes 
              !     initialize summation variables
              zw_sum    = 0                             
              DO jid=1,nid               ! collect individual contributions
                jj1 = j1id(jid)
                jj2 = j2id(jid)
                jjd = jdid(jid)
                zw_sum    = zw_sum +  gme_sum_field(jj1,jj2,jjd)
              END DO
              DO jid=1,nid               ! redistribute results for identical nodes
                jj1 = j1id(jid)
                jj2 = j2id(jid)
                jjd = jdid(jid)
                gme_sum_field(jj1,jj2,jjd) = zw_sum
              END DO
            END IF                     ! pole node or other edge node
          END DO      ! edge nodes
        END DO      ! diamonds
      END DO      ! hemisphere

    END SUBROUTINE sync_diamond_edge_contr_int

    
    !> Synchronisation of diamond corner and edge nodes of GME grid for the
    !! contributions of summation fields (INTEGER (KIND=i4))
    SUBROUTINE sync_diamond_edge_contr_int4(gme_grid,gme_sum_field)
    ! This code is base on Bodo Ritters code for the generation of external parameters for GME
    TYPE(gme_triangular_grid) ::  gme_grid  !< GME grid information
    INTEGER (KIND=i4) :: gme_sum_field(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field

    ! local variables
    INTEGER :: n_edge
    INTEGER :: jh
    INTEGER :: jd, jda, jde
    INTEGER :: j1e, j2e, jep
    INTEGER :: jid
    INTEGER :: nid !<  kid : no. of idential nodes   
    INTEGER :: j1id(5) !< j1id: 'j1' index of all identical nodes
    INTEGER :: j2id(5) !< j2id: 'j2' index of all identical nodes  
    INTEGER :: jdid(5) !< jdid: diamond index of all identical nodes 
    INTEGER :: jj1,jj2,jjd

    INTEGER (KIND=i4) :: zw_sum

      !============================
      ! diamond edge synchronisation
      !============================
                               !                                            P
                               !                                           / \
                               !                                          /   \
                               ! no. of points along a 'meridional'      W  1  E A
      n_edge = 2*gme_grid%ni  ! edge of a diamond, excluding the         \   / /
                               ! 'anti-poleward' corner                    \ / /
                               !                                            A W  6
                               !                                               \
                               !                                                \
                               !                                                 P
      DO jh=1,2              ! loop over hemispheres       
        jda = (jh-1)*5 + 1     ! first diamond index in hemisphere
        jde = (jh-1)*5 + 5     ! last diamond index in hemispher
        DO jd =jda,jde         ! loop over 5 diamonds in hemisphere
          DO jep=1,n_edge        ! loop over edge nodes            
            j1e  = MIN(jep,gme_grid%ni+1) - 1 ! j1-index on edge
            j2e  = MAX(jep-gme_grid%ni,1)     ! j2-index on edge
            IF (jep.GT.1 .OR. jd.EQ.jda) THEN ! pole node is handled only 
                                              ! once per hemisphere
              CALL same (j1e,j2e,jd,gme_grid%ni,nid,j1id,j2id,jdid) ! find identical nodes 
              !     initialize summation variables
              zw_sum    = 0                             
              DO jid=1,nid               ! collect individual contributions
                jj1 = j1id(jid)
                jj2 = j2id(jid)
                jjd = jdid(jid)
                zw_sum    = zw_sum +  gme_sum_field(jj1,jj2,jjd)
              END DO
              DO jid=1,nid               ! redistribute results for identical nodes
                jj1 = j1id(jid)
                jj2 = j2id(jid)
                jjd = jdid(jid)
                gme_sum_field(jj1,jj2,jjd) = zw_sum
              END DO
            END IF                     ! pole node or other edge node
          END DO      ! edge nodes
        END DO      ! diamonds
      END DO      ! hemisphere

    END SUBROUTINE sync_diamond_edge_contr_int4


    !> copy buffer to gme field
    SUBROUTINE cp_buf2gme_r(tg,gme_grid,buffer_r,gme_field_r)
    TYPE(target_grid_def), INTENT(IN) ::  tg  !< buffer grid information
    TYPE(gme_triangular_grid), INTENT(IN) ::  gme_grid  !< GME grid information
    REAL (KIND=wp), INTENT(IN)  :: buffer_r(1:tg%ie,1:tg%je,1:tg%ke) !< real buffer
    REAL (KIND=wp), INTENT(OUT) :: gme_field_r(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field
       gme_field_r(gme_grid%ig1s:gme_grid%ig1e, gme_grid%ig2s:gme_grid%ig2e,1:gme_grid%nd) = &  
     & buffer_r(1:tg%ie,1:tg%je,1:tg%ke)
    END SUBROUTINE cp_buf2gme_r
    
    !> copy buffer to gme field
    SUBROUTINE cp_buf2gme_i(tg,gme_grid,buffer_i,gme_field_i)
    TYPE(target_grid_def), INTENT(IN) ::  tg  !< buffer grid information
    TYPE(gme_triangular_grid), INTENT(IN) ::  gme_grid  !< GME grid information
    INTEGER (KIND=i8), INTENT(IN)  :: buffer_i(1:tg%ie,1:tg%je,1:tg%ke) !< integer buffer
    INTEGER (KIND=i8), INTENT(OUT) :: gme_field_i(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field
       gme_field_i(gme_grid%ig1s:gme_grid%ig1e, gme_grid%ig2s:gme_grid%ig2e,1:gme_grid%nd) = &  
     & buffer_i(1:tg%ie,1:tg%je,1:tg%ke)
    END SUBROUTINE cp_buf2gme_i

     !> copy buffer to gme field
    SUBROUTINE cp_buf2gme_i4(tg,gme_grid,buffer_i,gme_field_i)
    TYPE(target_grid_def), INTENT(IN) ::  tg  !< buffer grid information
    TYPE(gme_triangular_grid), INTENT(IN) ::  gme_grid  !< GME grid information
    INTEGER (KIND=i4), INTENT(IN)  :: buffer_i(1:tg%ie,1:tg%je,1:tg%ke) !< integer buffer
    INTEGER (KIND=i4), INTENT(OUT) :: gme_field_i(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field
       gme_field_i(gme_grid%ig1s:gme_grid%ig1e, gme_grid%ig2s:gme_grid%ig2e,1:gme_grid%nd) = &  
     & buffer_i(1:tg%ie,1:tg%je,1:tg%ke)
    END SUBROUTINE cp_buf2gme_i4

    
    !> copy gme to buffer field
    SUBROUTINE cp_gme2buf_r(tg,gme_grid,gme_field_r,buffer_r)
    TYPE(target_grid_def), INTENT(IN) ::  tg  !< buffer grid information
    TYPE(gme_triangular_grid), INTENT(IN) ::  gme_grid  !< GME grid information
    REAL (KIND=wp), INTENT(IN) :: gme_field_r(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field
    REAL (KIND=wp), INTENT(OUT)  :: buffer_r(1:tg%ie,1:tg%je,1:tg%ke) !< real buffer
      buffer_r(1:tg%ie,1:tg%je,1:tg%ke) = &
     &  gme_field_r(gme_grid%ig1s:gme_grid%ig1e, gme_grid%ig2s:gme_grid%ig2e,1:gme_grid%nd) 
    END SUBROUTINE cp_gme2buf_r

    !> copy buffer to gme field
    SUBROUTINE cp_gme2buf_i(tg,gme_grid,gme_field_i,buffer_i)
    TYPE(target_grid_def), INTENT(IN) ::  tg  !< buffer grid information
    TYPE(gme_triangular_grid), INTENT(IN) ::  gme_grid  !< GME grid information
    INTEGER (KIND=i8), INTENT(IN) :: gme_field_i(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field
    INTEGER (KIND=i8), INTENT(OUT)  :: buffer_i(1:tg%ie,1:tg%je,1:tg%ke) !< integer buffer
       buffer_i(1:tg%ie,1:tg%je,1:tg%ke) = &
      &   gme_field_i(gme_grid%ig1s:gme_grid%ig1e, gme_grid%ig2s:gme_grid%ig2e,1:gme_grid%nd) 
    END SUBROUTINE cp_gme2buf_i

    !> copy buffer to gme field
    SUBROUTINE cp_gme2buf_i4(tg,gme_grid,gme_field_i,buffer_i)
    TYPE(target_grid_def), INTENT(IN) ::  tg  !< buffer grid information
    TYPE(gme_triangular_grid), INTENT(IN) ::  gme_grid  !< GME grid information
    INTEGER (KIND=i4), INTENT(IN) :: gme_field_i(gme_grid%ig1s:gme_grid%ig1e,gme_grid%ig2s:gme_grid%ig2e,gme_grid%nd) !< GME field
    INTEGER (KIND=i4), INTENT(OUT)  :: buffer_i(1:tg%ie,1:tg%je,1:tg%ke) !< integer buffer
       buffer_i(1:tg%ie,1:tg%je,1:tg%ke) = &
      &   gme_field_i(gme_grid%ig1s:gme_grid%ig1e, gme_grid%ig2s:gme_grid%ig2e,1:gme_grid%nd) 
    END SUBROUTINE cp_gme2buf_i4

END MODULE mo_gme_grid


