!-----------------------------------------------------------------------------------!
! Read in topography from NetCDF file and calculate maximum gradient of the         !
! topography. Required as input field for the runoff calculations of terra_ml.      !
! Input data is the "altitude" parameter from the GLOBE dataset. The resolution     !
! of the data set is 30''.                                                          !
!                                                                                   !
! compilation:                                                                      !
! cscs:                                                                             !
! ftn -o topo_grad topo_grad.f90 `/opt/cray/netcdf/4.3.3.1/bin/nf-config --fflags --flibs`        !
! iac:                                                                              !
! module load pgi/14.10                                                             !
! module load netcdf/4.3.2-pgf90                                                    !
! pgf90 -O3 -o topo_grad topo_grad.f90 `/usr/local/netcdf-4.3.2-4.2-pgf90/bin/nf-config --fflags --flibs` !
! usage:                                                                            !
!                                                                                   !
! ./topo_grad infile.nc outfile.nc                                                  !
!                                                                                   !
! Linda Schlemmer, IAC ETH Zurich, July 2016                                        !
!-----------------------------------------------------------------------------------!

Program topo_grad

  use netcdf

  implicit none     

  integer nx, ny, nt, nxp2, nyp2
  integer, parameter :: dp = selected_real_kind(15, 307)


  character*80 infile,outfile

  ! input fields
  real, allocatable :: &

       hsurf  (:,:), hsurf_inner(:,:)

  ! output fields
  real, allocatable :: &

       s_oro  (:,:)

  ! grid
  real(dp), allocatable :: &

       lat   (:),    & !
       lon   (:)

 !* netCDF id
  integer  ncid, ncido, status
  !* dimension ids
  integer londim, latdim
  !* variable ids
  integer lonid, latid
  integer varid, outid

  integer i, j, nargs

  character(len=100) name, char

  real dx, dy, oolenx, ooleny, grad(9), mdv
  real zlats0, zlats1, zlats2, crlat0, crlat1, crlat2, dx0, dx2
  real len0, len2, oolen0, oolen2

  real(dp), parameter :: r_earth  =  6371.229E3 ! mean radius of the earth
  real(dp), parameter :: pi       =  4.0 * ATAN (1.0)
  real(dp), parameter :: degrad   =  pi / 180.0
  real(dp), parameter :: dlat     =  30./3600. ! resolution
  real(dp), parameter :: dlon     =  30./3600. ! resolution
  real, parameter :: eps      =  1.E-9

  ! Read user input
  
  nargs = command_argument_count()
  if(nargs .ne. 2) then
     print*,'provide - input output - as arguments (see source code), STOPPING'
     stop
  else
     call get_command_argument(1,infile)
     call get_command_argument(2,outfile)
     print*,'in: ', infile, 'out: ',outfile
  end if

  ! Open file
  
  status = nf90_open(infile, nf90_nowrite, ncid)
  IF (STATUS .NE. NF90_NOERR) THEN
     PRINT *, NF90_STRERROR(STATUS)
     STOP
  ENDIF

  ! inquire dimensions

  status=nf90_inq_dimid(ncid,"lon",lonid)
  status=nf90_inquire_dimension(ncid,lonid,len=nxp2)
  status=nf90_inq_dimid(ncid,"lat",latid)
  status=nf90_inquire_dimension(ncid,latid,len=nyp2)

  ! allocate fields
  nx = nxp2 - 2
  ny = nyp2 - 2

  allocate(hsurf(0:nx+1,0:ny+1))
  allocate(s_oro(nx,ny),hsurf_inner(nx,ny))

  allocate(lon(0:nx+1))
  allocate(lat(0:ny+1))

  hsurf(:,:)    = 0.0
  s_oro(:,:)    = 0.0

  lat(:)       = 0.0
  lon(:)       = 0.0

  ! Read in variables
  
  !print*,'READING VARIABLES'

  status = nf90_inq_varid(ncid,"lon", lonid)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_get_var(ncid,lonid,lon)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  status = nf90_inq_varid(ncid,"lat", latid)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_get_var(ncid,latid,lat)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  status = nf90_inq_varid(ncid,"altitude", varid)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_get_var(ncid,varid,hsurf)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_get_att(ncid, varid,'_FillValue',mdv)
  call check_err(status)
  
  ! Calculations
  hsurf_inner(:,:) = hsurf(1:nx,1:ny)
  where (abs(hsurf-mdv) <= eps) hsurf=0.0

  dy = r_earth * dlat * degrad
  ooleny = 1./dy
  DO   j = 1, ny
     zlats0 = lat(j-1)
     zlats1 = lat(j)
     zlats2 = lat(j+1)
     crlat0  = COS ( zlats0  * degrad )
     crlat1  = COS ( zlats1  * degrad )
     crlat2  = COS ( zlats2  * degrad )
     dx0     = dlon * r_earth * degrad * crlat0
     dx      = dlon * r_earth * degrad * crlat1
     dx2     = dlon * r_earth * degrad * crlat2
     len0    = sqrt(dx0**2+dy**2)
     len2    = sqrt(dx2**2+dy**2)
     oolen0  = 1./len0
     oolen2  = 1./len2
     oolenx  = 1./dx
     DO i = 1, nx
       IF (abs(hsurf_inner(i,j)-mdv).gt.eps) THEN
         grad(1)      = oolenx * (hsurf(i,j)-hsurf(i-1,j  ))
         grad(2)      = oolenx * (hsurf(i,j)-hsurf(i+1,j  ))
         grad(3)      = ooleny * (hsurf(i,j)-hsurf(i  ,j-1))
         grad(4)      = ooleny * (hsurf(i,j)-hsurf(i  ,j+1))
         grad(5)      = oolen0 * (hsurf(i,j)-hsurf(i-1,j-1))
         grad(6)      = oolen2 * (hsurf(i,j)-hsurf(i-1,j+1))
         grad(7)      = oolen0 * (hsurf(i,j)-hsurf(i+1,j-1))
         grad(8)      = oolen2 * (hsurf(i,j)-hsurf(i+1,j+1))
         grad(9)      = 0.0
         s_oro(i,j)   = maxval(grad)
       ELSE
         s_oro(i,j)   = mdv
       ENDIF
     END DO

  END DO
 
!  print*,'CREATE NEW NetCDF FILE'

  !* enter define mode
  status = nf90_create (outfile,  NF90_CLOBBER, ncido)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)


  !* define dimensions
  status = nf90_def_dim(ncido, 'lon', nx, londim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_def_dim(ncido, 'lat', ny, latdim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  !* define variables

  status = nf90_def_var(ncido, 'lon', NF90_DOUBLE,(/londim/), lonid)
  call check_err(status)

  status = nf90_def_var(ncido, 'lat', NF90_DOUBLE,(/latdim/), latid)
  call check_err(status)

  status = nf90_def_var(ncido, 'S_ORO', NF90_FLOAT,(/londim,latdim/),outid)
  call check_err(status)

!  print*,'ATTRIBUTES'
  status = nf90_inq_varid(ncid,"lat", varid)
  status = nf90_get_att(ncid, varid,'long_name',name)
  call check_err(status)
  status = nf90_put_att(ncido, latid,'long_name',trim(name))
  call check_err(status)
  status = nf90_get_att(ncid, varid,'units',name)
  call check_err(status)
  status = nf90_put_att(ncido, latid,'units',trim(name))
  call check_err(status)

  status = nf90_inq_varid(ncid,"lon", varid)
  status = nf90_get_att(ncid, varid,'long_name',name)
  call check_err(status)
  status = nf90_put_att(ncido, lonid,'long_name',trim(name))
  call check_err(status)
  status = nf90_get_att(ncid, varid,'units',name)
  call check_err(status)
  status = nf90_put_att(ncido, lonid,'units',trim(name))
  call check_err(status)

  status = nf90_put_att(ncido, outid,'standard_name','topography gradient')
  call check_err(status)
  status = nf90_put_att(ncido, outid,'long_name','maximum local gradient of surface height')
  call check_err(status)
  status = nf90_put_att(ncido, outid,'units','')
  call check_err(status)

  status = nf90_inq_varid(ncid,"altitude", varid)
  status = nf90_get_att(ncid, varid,'grid_mapping',name)
  call check_err(status)
  status = nf90_put_att(ncido, outid,'grid_mapping',trim(name))
  call check_err(status)
  status = nf90_put_att(ncido, outid,'_FillValue',mdv)
  call check_err(status)
  status = nf90_get_att(ncid, varid,'comment',name)
  call check_err(status)
  status = nf90_put_att(ncido, outid,'comment',trim(name))
  call check_err(status)

!* leave define mode
  STATUS = NF90_ENDDEF(ncido)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

!* store variables
  STATUS = NF90_PUT_VAR(ncido, lonid, lon(1:nx))
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  STATUS = NF90_PUT_VAR(ncido, latid, lat(1:ny))
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  STATUS = NF90_PUT_VAR(ncido, outid,s_oro)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  deallocate(hsurf)
  deallocate(hsurf_inner)
  deallocate(s_oro)

  deallocate(lon)
  deallocate(lat)

contains
  subroutine check_err(iret)

    use netcdf
    implicit none
    integer iret
    if (iret .ne. NF90_NOERR) then
       print *, nf90_strerror(iret)
       stop
    endif
  
  end subroutine check_err

end Program topo_grad
