!+ Fortran module with basic geometric data types and routines.
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
!> Fortran module with basic geometric data types and routines.
!!
!!  Contains the definition of basic used defined data types.
!!
!!  Contains the definition of basic used defined data types
!!  to store the gridpoint coordinates either in cartesian or
!!  spherical coordinates. It also contains a number of (mostly
!!  ELEMENTAL) functions used to compute vectors and geometric quantities
!!  needed by the grid generator.
!!
!! @par Revision History
!!  Initial version  by Luis Kornblueh (2004)
!!  Modified to include tangent vectors and Protex headers
!!  by Luca Bonaventura (2005)
!! @par
!!  Guenther Zaengl, DWD, 2008-10-10:
!!  Add subroutine gvec2cvec (copied from mo_math_utilities)
!!
MODULE mo_base_geometry
 
  USE mo_kind, ONLY: wp
  USE mo_math_constants, ONLY: pi, pi_2, dbl_eps

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: cartesian_coordinates
  PUBLIC :: coordinates_reference
  PUBLIC :: geographical_coordinates
  PUBLIC :: tangent_vectors

  PUBLIC :: cc2gc, gc2cc, cc2tv, gvec2cvec
  PUBLIC :: triangle_area, circum_center, arc_length, cos_arc_length, bary_center
  PUBLIC :: normal_vector, inter_section, inter_section2, vector_product
  PUBLIC :: rotate_z,rotate_x,rotate_y,x_rot_angle,y_rot_angle,z_rot_angle

  TYPE cartesian_coordinates
    REAL(wp) :: x(3)
  END TYPE cartesian_coordinates

  TYPE coordinates_reference
    TYPE(cartesian_coordinates), POINTER :: px
  END TYPE coordinates_reference

  TYPE geographical_coordinates
    REAL(wp) :: lon
    REAL(wp) :: lat
  END TYPE geographical_coordinates

  TYPE tangent_vectors
    REAL(wp) :: v1
    REAL(wp) :: v2
  END TYPE tangent_vectors

  REAL(wp) :: x_rot_angle,y_rot_angle,z_rot_angle

CONTAINS

!EOP  
!--------------------------------------------------------------------  
!BOC
!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  cc2gc
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION cc2gc(x) RESULT (position)
! !DESCRIPTION:
! Converts cartesian coordinates to geographical.
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
! Completely new version by Thomas Heinze (2006-07-20)

! !INPUT PARAMETERS:  
    TYPE(cartesian_coordinates), INTENT(IN) :: x

! !RETURN VALUE:  
    TYPE(geographical_coordinates)          :: position

! !LOCAL VARIABLES:  
    REAL(wp)                                :: z_x, z_y, z_z, z_r

!EOP  
!-----------------------------------------------------------------------  
!BOC

    z_x = x%x(1)
    z_y = x%x(2)
    z_z = x%x(3)

    z_r = z_x * z_x + z_y * z_y
    z_r = SQRT(z_r)
    
    IF (ABS(z_r) < dbl_eps) THEN    ! one of the poles

      IF (z_z > 0.0_wp) THEN
        position%lat = pi_2
      ELSE
        position%lat = -1._wp * pi_2
      END IF
      position%lon = 0._wp

    ELSE  

      position%lat = ATAN2 ( z_z, z_r) 

      IF (ABS(z_x) < dbl_eps) THEN    ! z_x == 0 ?

        IF (z_y >= 0.0_wp) THEN
          position%lon = pi_2
        ELSE
          position%lon = -1._wp * pi_2
        END IF

      ELSE
       position%lon = ATAN2( z_y, z_x)
      END IF

    END IF

  END FUNCTION  cc2gc

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  gc2cc
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION gc2cc(position) RESULT(x)
! !DESCRIPTION:
! Converts longitude and latitude to cartesian coordinates.
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
!EOP  
!-----------------------------------------------------------------------  
!BOC
 
    TYPE(geographical_coordinates), INTENT(IN) :: position
    TYPE(cartesian_coordinates) :: x

    REAL (wp) :: z_cln, z_sln, z_clt, z_slt

    z_sln = SIN(position%lon)
    z_cln = COS(position%lon)
    z_slt = SIN(position%lat)
    z_clt = COS(position%lat)

    x%x(1) = z_cln*z_clt
    x%x(2) = z_sln*z_clt
    x%x(3) = z_slt

  END FUNCTION gc2cc

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  cc2tv
!  
! !SUBROUTINE INTERFACE: 

  FUNCTION cc2tv(xx,position) RESULT (tt)
! !DESCRIPTION:
! Converts  vectors (in)  cartesian coordinate representation
! to tangent vectors.
!
! !REVISION HISTORY:  
! Developed  by Luca Bonaventura  (2005).
!EOP  
!-----------------------------------------------------------------------  
!BOC
    

    TYPE(cartesian_coordinates), INTENT(IN) :: xx
    TYPE(geographical_coordinates),INTENT(IN)  :: position
    TYPE(tangent_vectors):: tt

    REAL(wp) :: z_sinlo,z_coslo,z_sinlacoslo,z_sinlasinlo

       z_sinlo=SIN(position%lon)
       z_coslo=COS(position%lon)
       z_sinlacoslo=SIN(position%lat)*z_coslo
       z_sinlasinlo=SIN(position%lat)*z_sinlo
       tt%v1= -z_sinlo*xx%x(1)+z_coslo*xx%x(2)
       tt%v2= -z_sinlacoslo*xx%x(1)-z_sinlasinlo*xx%x(2) + COS(position%lat)*xx%x(3)

  END FUNCTION cc2tv
 

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  triangle_area
!  
! !SUBROUTINE INTERFACE:  

  ELEMENTAL FUNCTION triangle_area (x0, x1, x2) RESULT(area)

! !DESCRIPTION:
! Computes area of triangular cell.
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC


    TYPE(cartesian_coordinates), INTENT(IN) :: x0, x1, x2

    REAL(wp) :: area 
    REAL(wp) :: z_s12, z_s23, z_s31, z_ca1, z_ca2, z_ca3, z_a1, z_a2, z_a3


    ! This variant to calculate the area of a spherical triangle
    ! is more precise.

    TYPE(cartesian_coordinates) :: u12, u23, u31

   

    !  Compute cross products Uij = Vi X Vj.
    
    u12 = vector_product (x0, x1)
    u23 = vector_product (x1, x2)
    u31 = vector_product (x2, x0)

    !  Normalize Uij to unit vectors.
    
    z_s12 = DOT_PRODUCT ( u12%x(1:3), u12%x(1:3) )
    z_s23 = DOT_PRODUCT ( u23%x(1:3), u23%x(1:3) )
    z_s31 = DOT_PRODUCT ( u31%x(1:3), u31%x(1:3) )
    
    !  Test for a degenerate triangle associated with collinear vertices.
    
    IF ( z_s12 == 0.0_wp .OR. z_s23 == 0.0_wp  .OR. z_s31 == 0.0_wp ) THEN
      area = 0.0_wp
      RETURN
    END IF

    z_s12 = SQRT(z_s12)
    z_s23 = SQRT(z_s23)
    z_s31 = SQRT(z_s31)

    u12%x(1:3) = u12%x(1:3)/z_s12
    u23%x(1:3) = u23%x(1:3)/z_s23
    u31%x(1:3) = u31%x(1:3)/z_s31

    !  Compute interior angles Ai as the dihedral angles between planes:
    !  CA1 = cos(A1) = -<U12,U31>
    !  CA2 = cos(A2) = -<U23,U12>
    !  CA3 = cos(A3) = -<U31,U23>

    z_ca1 = -u12%x(1)*u31%x(1)-u12%x(2)*u31%x(2)-u12%x(3)*u31%x(3)
    z_ca2 = -u23%x(1)*u12%x(1)-u23%x(2)*u12%x(2)-u23%x(3)*u12%x(3)
    z_ca3 = -u31%x(1)*u23%x(1)-u31%x(2)*u23%x(2)-u31%x(3)*u23%x(3)

    IF (z_ca1 < -1.0_wp) z_ca1 = -1.0_wp
    IF (z_ca1 >  1.0_wp) z_ca1 =  1.0_wp
    IF (z_ca2 < -1.0_wp) z_ca2 = -1.0_wp
    IF (z_ca2 >  1.0_wp) z_ca2 =  1.0_wp
    IF (z_ca3 < -1.0_wp) z_ca3 = -1.0_wp
    IF (z_ca3 >  1.0_wp) z_ca3 =  1.0_wp
    
    z_a1 = ACOS(z_ca1)
    z_a2 = ACOS(z_ca2)
    z_a3 = ACOS(z_ca3)

    !  Compute areas = z_a1 + z_a2 + z_a3 - pi.
    
    area = z_a1+z_a2+z_a3-pi

    IF ( area < 0.0_wp ) area = 0.0_wp

  END FUNCTION triangle_area

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  vector_product
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION vector_product (x0, x1) RESULT(x2)

! !DESCRIPTION:
! Computes vector product of x0,x1
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC
    TYPE(cartesian_coordinates), INTENT(IN) :: x0, x1
    TYPE(cartesian_coordinates) :: x2

    x2%x(1) = x0%x(2)*x1%x(3) - x0%x(3)*x1%x(2)
    x2%x(2) = x0%x(3)*x1%x(1) - x0%x(1)*x1%x(3)
    x2%x(3) = x0%x(1)*x1%x(2) - x0%x(2)*x1%x(1)

  END FUNCTION vector_product



ELEMENTAL FUNCTION  vecpro(geog1,geog2) RESULT (xx)

!
! Computes vector product of 2 unit vectors
! 
   
TYPE(geographical_coordinates), INTENT(IN) :: geog2, geog1

TYPE(cartesian_coordinates)  :: xx
REAL (wp)  :: long1, lat1,long2,lat2
REAL (wp) :: sn1,sn2,sn3,sn4,cs1,cs2

long1=geog1%lon
lat1=geog1%lat
long2=geog2%lon
lat2=geog2%lat
   
sn1=SIN(lat1-lat2)
sn2=SIN(lat1+lat2)
sn3=SIN(0.5_wp*(long1-long2))
sn4=SIN(0.5_wp*(long1+long2))
cs1=COS(0.5_wp*(long1-long2))
cs2=COS(0.5_wp*(long1+long2))
   
xx%x(1)=sn1*sn4*cs1-sn2*cs2*sn3
xx%x(2)=sn1*cs2*cs1+sn2*sn4*sn3
xx%x(3)=COS(lat1)*COS(lat2)*SIN(long1-long2)
    
END FUNCTION vecpro

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  rotate_z
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION rotate_z (p_phirot,x1) RESULT(x2)

! !DESCRIPTION:
! Rotate counterclockwise (seen from positive axis direction)
! vector x1 around axis z for phirot radians
!
! !REVISION HISTORY:  
! Developed  by Luca Bonaventura  (2005).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC
    REAL(wp), INTENT(IN) :: p_phirot
    TYPE(cartesian_coordinates), INTENT(IN) :: x1
    TYPE(cartesian_coordinates) :: x2

    x2%x(1) = cos(p_phirot)*x1%x(1) - sin(p_phirot)*x1%x(2)
    x2%x(2) = sin(p_phirot)*x1%x(1) + cos(p_phirot)*x1%x(2)
    x2%x(3) = x1%x(3)

  END FUNCTION rotate_z


!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  rotate_y
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION rotate_y (p_phirot,x1) RESULT(x2)

! !DESCRIPTION:
! Rotate counterclockwise (seen from positive axis direction)
! vector x1 around axis y for phirot radians
!
! !REVISION HISTORY:  
! Developed  by Luca Bonaventura  (2005).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC
    REAL(wp), INTENT(IN) :: p_phirot
    TYPE(cartesian_coordinates), INTENT(IN) :: x1
    TYPE(cartesian_coordinates) :: x2

    x2%x(1) =  cos(p_phirot)*x1%x(1) + sin(p_phirot)*x1%x(3)
    x2%x(3) = -sin(p_phirot)*x1%x(1) + cos(p_phirot)*x1%x(3)
    x2%x(2) =  x1%x(2)

  END FUNCTION rotate_y


!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  rotate_x
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION rotate_x (p_phirot,x1) RESULT(x2)

! !DESCRIPTION:
! Rotate counterclockwise (seen from positive axis direction)
! vector x1 around axis x for phirot radians
!
! !REVISION HISTORY:  
! Developed  by Luca Bonaventura  (2005).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC
    REAL(wp), INTENT(IN) :: p_phirot
    TYPE(cartesian_coordinates), INTENT(IN) :: x1
    TYPE(cartesian_coordinates) :: x2

    x2%x(2) =  cos(p_phirot)*x1%x(2) + sin(p_phirot)*x1%x(3)
    x2%x(3) = -sin(p_phirot)*x1%x(2) + cos(p_phirot)*x1%x(3)
    x2%x(1) =  x1%x(1)

  END FUNCTION rotate_x


!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  normal_vector
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION normal_vector (x0, x1) RESULT(x2)

! !DESCRIPTION:
! Computes normal vector to plane determined by x0,x1
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC

    TYPE(cartesian_coordinates), INTENT(IN) :: x0, x1
    TYPE(cartesian_coordinates) :: x2
    REAL(wp) :: z_xnorm

    x2%x(1) = x0%x(2)*x1%x(3) - x0%x(3)*x1%x(2)
    x2%x(2) = x0%x(3)*x1%x(1) - x0%x(1)*x1%x(3)
    x2%x(3) = x0%x(1)*x1%x(2) - x0%x(2)*x1%x(1)

    z_xnorm = SQRT(DOT_PRODUCT(x2%x, x2%x))
    x2%x = x2%x/z_xnorm

  END FUNCTION normal_vector

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  bary_center
!  
! !SUBROUTINE INTERFACE: 
  ELEMENTAL FUNCTION bary_center (v0, v1, v2) RESULT(center) 
!
! !DESCRIPTION:
! Determines the bary\_center of triangle with vertices v0,v1,v2
!
! !REVISION HISTORY:  
! original version by Thomas Heinze (2006-08-17).
!
! !REMARKS:
! currently not used in the code, just for testing purposes  

! ! the coordinates of the three triangle vertices (unit vectors) 
! ! in counter clockwise order.
!
    TYPE(cartesian_coordinates), INTENT(IN) :: v0, v1, v2

!
!    ! the coordinates of the circumcenter unless
!
    TYPE(cartesian_coordinates) :: center

    !  Local variables:
    !
    TYPE(cartesian_coordinates) :: bc ! barycenter of the underlying planar 
                                      ! triangle:
    REAL(wp) :: z_cnorm               ! norm of e1

!EOP  
!-----------------------------------------------------------------------  
!BOC

! get barycenter of planar triangle

    bc%x = v0%x + v1%x + v2%x
   
    bc%x = bc%x/3._wp
    
! norm of barycenter

    z_cnorm = SQRT(DOT_PRODUCT(bc%x, bc%x))

! new center

    center%x = bc%x/z_cnorm

  END FUNCTION bary_center

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  circum_center
!  
! !SUBROUTINE INTERFACE: 
  ELEMENTAL FUNCTION circum_center (v0, v1, v2) RESULT(center) 
!
! !DESCRIPTION:
! Determines the circum\_center of triangle with vertices v0,v1,v2
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
!
! ! the coordinates of the three triangle vertices (unit vectors) 
! ! in counter clockwise order.
!
    TYPE(cartesian_coordinates), INTENT(IN) :: v0, v1, v2

!
!    ! the coordinates of the circumcenter unless
!
    TYPE(cartesian_coordinates) :: center

    !  Local variables:
    !
    TYPE(cartesian_coordinates) :: e1, e2 ! edges of the underlying planar triangle:
                                          ! v1-v0 ands v2-v0, respectively          
    TYPE(cartesian_coordinates) :: cu     ! dot product of center:  e1 x e2
    REAL(wp) :: z_cnorm                     ! norm of cu

!EOP  
!-----------------------------------------------------------------------  
!BOC

    e1%x = v1%x - v0%x
    e2%x = v2%x - v0%x
   
    ! compute cu = e1 x e2 and cnorm**2.

    cu = vector_product (e1, e2)

    IF (DOT_PRODUCT(cu%x, v0%x) < 0.0_wp) THEN
        cu%x = -cu%x
    END IF

    z_cnorm = SQRT(DOT_PRODUCT(cu%x, cu%x))

    center%x = cu%x/z_cnorm
    
  END FUNCTION circum_center

!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  arc_length
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION arc_length (x0, x1) RESULT(arc)

! !DESCRIPTION:
! Computes length of geodesic arc with endpoints x0,x1.
!
! !REVISION HISTORY:  
! Developed by Th.Heinze (2006-09-19).
! Previous version by Luis Kornblueh (2004) discarded.
!
!EOP  
!-----------------------------------------------------------------------  
!BOC

    TYPE(cartesian_coordinates), INTENT(IN) :: x0, x1
    REAL(wp) :: arc
    REAL(wp) :: z_d0,  z_d1,  z_cc

    z_d0 = SQRT(DOT_PRODUCT(x0%x,x0%x))
    z_d1 = SQRT(DOT_PRODUCT(x1%x,x1%x))

    z_cc = DOT_PRODUCT(x0%x, x1%x)/(z_d0*z_d1)

! in case we get numerically incorrect solutions 
    IF (z_cc > 1._wp )  z_cc =  1._wp
    IF (z_cc < -1._wp ) z_cc = -1._wp

    arc = ACOS(z_cc)
 
  END FUNCTION arc_length


!EOC  
!-----------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  cos_arc_length
!  
! !SUBROUTINE INTERFACE: 

  ELEMENTAL FUNCTION cos_arc_length (x0, x1) RESULT(carc)

! !DESCRIPTION:
! Computes the cosine of the length of geodesic arc with endpoints x0,x1.
!
! !REVISION HISTORY:  
! Developed by Almut Gassmann (2007-03-13).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC

    TYPE(cartesian_coordinates), INTENT(IN) :: x0, x1
    REAL(wp) :: carc
    REAL(wp) :: z_d0,  z_d1,  z_cc

    z_d0 = SQRT(DOT_PRODUCT(x0%x,x0%x))
    z_d1 = SQRT(DOT_PRODUCT(x1%x,x1%x))

    z_cc = DOT_PRODUCT(x0%x, x1%x)/(z_d0*z_d1)

! in case we get numerically incorrect solutions 
    IF (z_cc > 1._wp )  z_cc =  1._wp
    IF (z_cc < -1._wp ) z_cc = -1._wp

    carc = z_cc
 
  END FUNCTION cos_arc_length



!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  inter_section
!  
! !SUBROUTINE INTERFACE: 

  

 ELEMENTAL FUNCTION inter_section (p0, p1, v0, v1) RESULT(p)
! !DESCRIPTION:
! Finds the intersection of two great circles.
! Cannot be made elemental, as long as the 
! subroutine calls to message and finish are maintained.
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC
    

    TYPE(cartesian_coordinates), INTENT(IN) :: p0, p1
    TYPE(cartesian_coordinates), INTENT(IN) :: v0, v1
    TYPE(cartesian_coordinates) :: p

    TYPE(cartesian_coordinates) :: en, cn
    
    REAL(wp) :: z_pn

    ! normal to plane where edge lies

    en = vector_product (v0, v1)

    ! normal tp plane where triangle centers lies

    cn = vector_product (p0, p1)
 
    p = vector_product (en, cn)

    ! rescale on sphere surface

    z_pn = SQRT(DOT_PRODUCT(p%x, p%x))

    p%x = p%x/z_pn

    IF (DOT_PRODUCT(p0%x, p%x) < 0.0_wp) THEN
      p%x = -p%x
    END IF
    
  END FUNCTION inter_section




!EOC  
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  inter_section2
!  
! !SUBROUTINE INTERFACE: 

  

 ELEMENTAL FUNCTION inter_section2 (p0, p1, v0, v1) RESULT(p)
! !DESCRIPTION:
! Finds the intersection of two great circles.
! Cannot be made elemental, as long as the 
! subroutine calls to message and finish are maintained.
!
! !REVISION HISTORY:  
! Developed  by Luis Kornblueh  (2004).
!
!EOP  
!-----------------------------------------------------------------------  
!BOC
    

    TYPE(cartesian_coordinates), INTENT(IN) :: p0, p1
    TYPE(cartesian_coordinates), INTENT(IN) :: v0, v1

    TYPE(geographical_coordinates) :: g1,g2

    TYPE(cartesian_coordinates) :: p

    TYPE(cartesian_coordinates) :: en, cn
    
!!$    REAL(wp), PARAMETER :: voronoi_tolerance = 1.0e-9_wp

    REAL(wp) :: z_pn

    ! normal to plane where edge lies

  !  en = vector_product (v0, v1)
    
      g1=cc2gc(v0)
      g2=cc2gc(v1)

      en = vecpro (g1, g2)

    ! normal tp plane where triangle centers lies

   ! cn = vector_product (p0, p1)
 

      g1=cc2gc(p0)
      g2=cc2gc(p1)

      cn = vecpro (g1, g2)

    ! check Voronoi/Delaunay properties  

!!$    IF (ABS(DOT_PRODUCT(en%x, cn%x)) > voronoi_tolerance) THEN
!!$      WRITE (message_text,'(a,e12.6)') &
!!$           'Orthogonality = ',                &
!!$           ABS(DOT_PRODUCT(en%x, cn%x))
!!$      CALL message ('inter_sect', TRIM(message_text))
!!$      CALL finish ('inter_sect','Voronoi orthogonality violated.')
!!$    END IF

      g1=cc2gc(en)
      g2=cc2gc(cn)

  !  p = vector_product (en, cn)
  
    p = vecpro (g1, g2)

    ! rescale on sphere surface

    z_pn = SQRT(DOT_PRODUCT(p%x, p%x))

    p%x = p%x/z_pn

    IF (DOT_PRODUCT(p0%x, p%x) < 0.0_wp) THEN
      p%x = -p%x
    END IF
    
  END FUNCTION inter_section2

!EOP
!-------------------------------------------------------------------------  
!BOP
!
! !IROUTINE:  gvec2cvec
!  
! !SUBROUTINE INTERFACE: 
  SUBROUTINE gvec2cvec (p_gu, p_gv, p_long, p_lat, p_cu, p_cv, p_cw)
!
! !DESCRIPTION:
! Converts zonal $p\_gu$ and meridional vector components $p\_gv$ into cartesian
! ones $(p\_cu, p\_cv, p\_cw)$ using the conversion
! \begin{align*}
! \begin{pmatrix} p\_cu \\ p\_cv \\ p\_cw \end{pmatrix} =
! \begin{pmatrix}
! -\sin p\_long & -\sin p\_lat \cdot \cos p\_long \\
! \cos p\_long & -\sin p\_lat \cdot \sin p\_long \\
! 0 & -\cos p\_lat 
! \end{pmatrix} \cdot
! \begin{pmatrix} p\_gu \\ p\_gv \end{pmatrix}
! \end{align*}
!
! !REVISION HISTORY:  
! Original version by Tobias Ruppert and Thomas Heinze, DWD (2006-11-14)  
!  
! !INPUT PARAMETERS:  
  REAL(wp), INTENT(in)  :: p_gu, p_gv     ! zonal and meridional vec. component
  REAL(wp), INTENT(in)  :: p_long, p_lat  ! geo. coord. of data point

! !OUTPUT PARAMETERS:  
  REAL(wp), INTENT(out) :: p_cu, p_cv, p_cw            ! Cart. vector 

! !LOCAL VARIABLES:  
  REAL(wp)              :: z_cln, z_sln, z_clt, z_slt  ! sin and cos of 
                                                       ! p_long and p_lat
    
!EOP  
!-------------------------------------------------------------------------  
!BOC  

  z_sln = SIN(p_long)
  z_cln = COS(p_long)
  z_slt = SIN(p_lat)
  z_clt = COS(p_lat)

  p_cu = z_sln * p_gu + z_slt * z_cln * p_gv
  p_cu = -1._wp * p_cu
  p_cv = z_cln * p_gu - z_slt * z_sln * p_gv
  p_cw = z_clt * p_gv

  END SUBROUTINE gvec2cvec

END MODULE mo_base_geometry

!---------------------------------------------
!EOC









