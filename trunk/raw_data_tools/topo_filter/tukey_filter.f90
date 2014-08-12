SUBROUTINE tukey_filter(dim_1, dim_2, lat_h, h, hh)
      

  IMPLICIT NONE
  INTEGER, INTENT(IN)                               :: dim_1
  INTEGER, INTENT(IN)                               :: dim_2
  REAL, INTENT(IN), DIMENSION(0:dim_1-1)            :: lat_h
  REAL, INTENT(IN), DIMENSION(0:dim_2-1,0:dim_1-1)  :: h
  REAL, INTENT(OUT), DIMENSION(0:dim_2-1,0:dim_1-1) :: hh

  REAL, PARAMETER                :: dlat = 1.0/120.0
  REAL, PARAMETER                :: dlon = dlat
  REAL, PARAMETER                :: pi = 3.14159265358979
  REAL, PARAMETER                :: radius_earth = 6371.0             !radius WGS84
  REAL, PARAMETER                :: wave_l = 3.0
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
  INTEGER                        :: i,j,n,m,k,u,v,error

  number_max_lon_pix = NINT((1.0/((2.0*pi*radius_earth*cos(MINVAL(lat_h)*pi/180.0)/360.0)/wave_l))/dlat)+1

  ALLOCATE(wj(0:number_max_lon_pix-1), STAT = error)
  IF (error .ne. 0 ) print*, 'cant allocate wj'

  wj = 0.0
  hh = h
  print*, number_lat_pix

  do i = 0,number_lat_pix-1
    if(real(i) .lt. alpha*real(number_lat_pix-1)*0.5) then
      wi(i) = 0.5*(1.0+cos(pi*(2.0*real(i)/(alpha*real(number_lat_pix-1))-1.0)))
    else if (real(i) .ge. alpha*real(number_lat_pix-1)*0.5 &
         & .and. real(i) .lt. real(number_lat_pix-1)*(1.0-alpha*0.5)) then
      wi(i) = 1.0
    else
      wi(i) = 0.5*(1+cos(pi*(4.0*real(i)/real(number_lat_pix-1)-2.0*alpha_rez-1.0)))
    end if
  end do


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

      do j = 0,number_lon_pix-1
        if (real(j) .lt. alpha*real(number_lon_pix-1)*0.5) then
          wj(j) = 0.5*(1.0+cos(pi*(2.0*real(j)/(alpha*real(number_lon_pix-1))-1.0)))
        else if (real(j) .ge. alpha*real(number_lon_pix-1)*0.5 &
        & .and. real(j) .lt. real(number_lon_pix-1)*(1.0-alpha*0.5)) then
          wj(j) = 1.0
        else
          wj(j) = 0.5*(1+cos(pi*(4.0*real(j)/real(number_lon_pix-1)-2.0*alpha_rez-1.0)))
        end if
      end do

      
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

END SUBROUTINE tukey_filter
