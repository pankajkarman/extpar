SUBROUTINE lanczos_window(dim_1, dim_2, lat_h, h, hh)
      

  IMPLICIT NONE
  INTEGER, INTENT(IN)                               :: dim_1
  INTEGER, INTENT(IN)                               :: dim_2
  REAL, INTENT(IN), DIMENSION(0:dim_1-1)            :: lat_h
  REAL, INTENT(IN), DIMENSION(0:dim_2-1,0:dim_1-1)  :: h
  REAL, INTENT(OUT), DIMENSION(0:dim_2-1,0:dim_1-1) :: hh

  REAL, PARAMETER                :: dlat = 1.0/120.0
  REAL, PARAMETER                :: dlon = dlat
  REAL, PARAMETER                :: pi = 3.14159265358979
  REAL, PARAMETER                :: rad = pi/180.0
  REAL, PARAMETER                :: radius_earth = 6371.0             !radius WGS84
  REAL, PARAMETER                :: wave_l = 3.0                      ! km
  REAL, PARAMETER                :: dist_lat = 2.0*pi*radius_earth/360.0
  INTEGER, PARAMETER             :: number_lat_pix = NINT((1.0/(dist_lat/wave_l))/dlat)
  INTEGER                        :: number_max_lon_pix
  REAL, PARAMETER                :: alpha = 0.75
  REAL, PARAMETER                :: alpha_rez = 1.0/alpha
  INTEGER, PARAMETER             :: nboarder = 40
  
  REAL, DIMENSION(0:number_lat_pix-1) :: wi
  REAL, ALLOCATABLE              :: wj(:)
  REAL                           :: hh1
  REAL                           :: weight
  REAL                           :: diff1, diff2
  REAL                           :: dist_lon
  REAL                           :: no_lon_pix_real
  INTEGER                        :: number_lon_pix, no_lon_pix_m1,no_lon_pix_p1 
  INTEGER                        :: i,j,n,m,k,u,v,error,x,y
!  REAL                           :: x,y


  number_max_lon_pix = NINT((1.0/((2.0*pi*radius_earth*cos(MINVAL(lat_h)*pi/180.0)/360.0)/wave_l))/dlat)+1

  ALLOCATE(wj(0:number_max_lon_pix-1), STAT = error)
  IF (error .ne. 0 ) print*, 'cant allocate wj'

  wj = 0.0
  hh = h
 

  print*, number_lat_pix
  x = (number_lat_pix-1)/2
  do i = -x,x
    IF (i .eq. 0) THEN
      wi(i+x) = 1.0
    ELSE
      wi(i+x) = (x * SIN(REAL(i)) * SIN(REAL(i)/REAL(x)))/&
           &    REAL(i*i)
    ENDIF
  end do

  print*, 'wi: ', wi

  do m = 2,dim_1-3
    if (mod(m,120) .eq. 0) then
      print*,lat_h(m)
    end if
    
    dist_lon = 2.0*pi*radius_earth*cos(lat_h(m)*pi/180.0)/360.0
    number_lon_pix = NINT((1.0/(dist_lon/wave_l))/dlon)
    no_lon_pix_real = (1.0/(dist_lon/wave_l))/dlon
    
    do n = nboarder-1,dim_2-nboarder
      if (mod(number_lon_pix,2) .eq. 0) then
        no_lon_pix_m1 = number_lon_pix - 1
        no_lon_pix_p1 = number_lon_pix + 1
        diff1 = abs(no_lon_pix_real - no_lon_pix_m1)
        diff2 = abs(no_lon_pix_real - no_lon_pix_p1)
        if (diff1.le.diff2) then
          number_lon_pix = no_lon_pix_m1
        else 
          number_lon_pix = no_lon_pix_p1
        end if
      end if
      
      k = (number_lon_pix - 1)/2
      hh1 = 0.0
      weight = 0.0

      do j = -k,k
        IF (j .eq. 0) THEN
          wj(j+k) = 1.0
        ELSE
          wj(j+k) = (k * SIN(REAL(j)) * SIN(REAL(j)/REAL(k)))/&
               &    REAL(j*j)
     
        ENDIF
      end do

   !   print*, 'wj: ', wj

      
      do u = 0,number_lat_pix-1
        do v = 0,number_lon_pix-1
          
          hh1 = hh1 + wi(u)*wj(v)*h(n+v-k,m+u-1)
          weight = weight +wi(u)*wj(v)
        end do
      end do
     
      hh(n,m) = hh1/weight
      
    end do
  end do
 
  print*, "done"

END SUBROUTINE lanczos_window
