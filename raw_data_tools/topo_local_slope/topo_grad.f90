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

  integer nx, ny, nt

  character*80 infile,outfile

  ! input fields
  real, allocatable :: &

       hsurf  (:,:)

  ! output fields
  real, allocatable :: &

       s_oro  (:,:)

  ! grid
  real, allocatable :: &

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

  real dx, dy, len, oolen, oolenx, ooleny, grad(9), zlats, crlat, mdv

  real, parameter :: r_earth  =  6371.229E3 ! mean radius of the earth
  real, parameter :: pi       =  4.0 * ATAN (1.0)
  real, parameter :: degrad   =   pi / 180.0
  real, parameter :: dlat     =  30./3600. ! resolution
  real, parameter :: dlon     =  30./3600. ! resolution
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
  status=nf90_inquire_dimension(ncid,lonid,len=nx)
  status=nf90_inq_dimid(ncid,"lat",latid)
  status=nf90_inquire_dimension(ncid,latid,len=ny)

  ! allocate fields

  allocate(hsurf(nx,ny))
  allocate(s_oro(nx,ny))

  allocate(lon(nx))
  allocate(lat(ny))

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

  dy = r_earth * dlat * degrad
  ooleny = 1./dy
  DO   j = 2, ny-1
     zlats  = lat(j)
     crlat  = COS ( zlats  * degrad )
     dx     = dlon * r_earth * degrad * crlat
     len    = sqrt(dx**2+dy**2)
     oolen  = 1./len
     oolenx = 1./dx
     DO i = 2, nx-1
        IF (abs(hsurf(i,j)-mdv).gt.eps) THEN
           IF (abs(hsurf(i-1,j)-mdv).gt.eps) THEN
              grad(1)      = oolenx * (hsurf(i,j)-hsurf(i-1,j  ))
           ELSE
              grad(1)      = 0.
           END IF
           IF (abs(hsurf(i+1,j)-mdv).gt.eps) THEN
              grad(2)      = oolenx * (hsurf(i,j)-hsurf(i+1,j  ))
           ELSE
              grad(2)      = 0.
           END IF
           IF (abs(hsurf(i,j-1)-mdv).gt.eps) THEN
              grad(3)      = ooleny * (hsurf(i,j)-hsurf(i  ,j-1))
           ELSE
              grad(3)      = 0.
           END IF
           IF (abs(hsurf(i,j+1)-mdv).gt.eps) THEN
              grad(4)      = ooleny * (hsurf(i,j)-hsurf(i  ,j+1))
           ELSE
              grad(4)      = 0.
           END IF
           IF (abs(hsurf(i-1,j-1)-mdv).gt.eps) THEN
              grad(5)      = oolen  * (hsurf(i,j)-hsurf(i-1,j-1))
           ELSE
              grad(5)      = 0.
           END IF
           IF (abs(hsurf(i-1,j+1)-mdv).gt.eps) THEN
              grad(6)      = oolen  * (hsurf(i,j)-hsurf(i-1,j+1))
           ELSE
              grad(6)      = 0.
           END IF
           IF (abs(hsurf(i+1,j-1)-mdv).gt.eps) THEN
              grad(7)      = oolen  * (hsurf(i,j)-hsurf(i+1,j-1))
           ELSE
              grad(7)      = 0.
           END IF
           IF (abs(hsurf(i+1,j+1)-mdv).gt.eps) THEN
              grad(8)      = oolen  * (hsurf(i,j)-hsurf(i+1,j+1))
           ELSE
              grad(8)      = 0.
           END IF
           grad(9)      = 0.0
           s_oro(i,j)   = maxval(grad)
        ELSE
           s_oro(i,j)   = mdv
        ENDIF
     END DO

     ! boundary values
     ! i=1
     IF (abs(hsurf(1,j)-mdv).gt.eps) THEN
        grad(1)      = 0.0
        grad(2)      = oolenx * (hsurf(1,j)-hsurf(2,j  ))
        grad(3)      = ooleny * (hsurf(1,j)-hsurf(1  ,j-1))
        grad(4)      = ooleny * (hsurf(1,j)-hsurf(1  ,j+1))
        grad(5)      = 0.0
        grad(6)      = 0.0
        grad(7)      = oolen  * (hsurf(1,j)-hsurf(2,j-1))
        grad(8)      = oolen  * (hsurf(1,j)-hsurf(2,j+1))
        grad(9)      = 0.0
        s_oro(1,j)   = maxval(grad)
     ELSE
        s_oro(1,j)   = mdv
     ENDIF
        
     ! i=nx
     IF (abs(hsurf(nx,j)-mdv).gt.eps) THEN
        grad(1)      = oolenx * (hsurf(nx,j)-hsurf(nx-1,j  ))
        grad(2)      = 0.0
        grad(3)      = ooleny * (hsurf(nx,j)-hsurf(nx  ,j-1))
        grad(4)      = ooleny * (hsurf(nx,j)-hsurf(nx  ,j+1))
        grad(5)      = oolen  * (hsurf(i,j)-hsurf(nx-1,j-1))
        grad(6)      = oolen  * (hsurf(i,j)-hsurf(nx-1,j+1))
        grad(7)      = 0.0
        grad(8)      = 0.0
        grad(9)      = 0.0
        s_oro(nx,j)   = maxval(grad)
     ELSE
        s_oro(nx,j)   = mdv
     ENDIF

  END DO
  ! boundary values
  ! j=1
  zlats  = lat(1)
  crlat  = COS ( zlats  * degrad )
  dx     = dlon * r_earth * degrad * crlat
  len    = sqrt(dx**2+dy**2)
  oolen  = 1./len
  oolenx = 1./dx
  DO i = 2, nx-1
     IF (abs(hsurf(i,1)-mdv).gt.eps) THEN

        grad(1)      = oolenx * (hsurf(i,1)-hsurf(i-1,1  ))
        grad(2)      = oolenx * (hsurf(i,1)-hsurf(i+1,1  ))
        grad(3)      = 0.0
        grad(4)      = ooleny * (hsurf(i,1)-hsurf(i  ,2))
        grad(5)      = 0.0
        grad(6)      = oolen  * (hsurf(i,1)-hsurf(i-1,2))
        grad(7)      = 0.0
        grad(8)      = oolen  * (hsurf(i,1)-hsurf(i+1,2))
        grad(9)      = 0.0
        s_oro(i,1)   = maxval(grad)
     ELSE
        s_oro(i,1)   = mdv
     ENDIF

     !ENDIF
  END DO

  ! j=ny
  zlats  = lat(ny)
  crlat  = COS ( zlats  * degrad )
  dx     = dlon * r_earth * degrad * crlat
  len    = sqrt(dx**2+dy**2)
  oolen  = 1./len
  oolenx = 1./dx
  DO i = 2, nx-1
        IF (abs(hsurf(i,ny)-mdv).gt.eps) THEN
     !IF(llandmask(i,j)) THEN   ! for land-points only

           grad(1)      = oolenx * (hsurf(i,ny)-hsurf(i-1,ny  ))
           grad(2)      = oolenx * (hsurf(i,ny)-hsurf(i+1,ny  ))
           grad(3)      = ooleny * (hsurf(i,ny)-hsurf(i  ,ny-1))
           grad(4)      = 0.0
           grad(5)      = oolen  * (hsurf(i,ny)-hsurf(i-1,ny-1))
           grad(6)      = 0.0
           grad(7)      = oolen  * (hsurf(i,ny)-hsurf(i+1,ny-1))
           grad(8)      = 0.0
           grad(9)      = 0.0
           s_oro(i,ny)  = maxval(grad)
        ELSE
           s_oro(i,ny)   = mdv
        ENDIF

     END DO
  ! corner values
  ! i=1, j=1
  zlats  = lat(1)
  crlat  = COS ( zlats  * degrad )
  dx     = dlon * r_earth * degrad * crlat
  len    = sqrt(dx**2+dy**2)
  oolen  = 1./len
  oolenx = 1./dx
  IF (abs(hsurf(1,1)-mdv).gt.eps) THEN

     grad(1)      = 0.0
     grad(2)      = oolenx * (hsurf(1,1)-hsurf(2,1))
     grad(3)      = 0.0
     grad(4)      = ooleny * (hsurf(1,1)-hsurf(1,2))
     grad(5)      = 0.0
     grad(6)      = 0.0
     grad(7)      = 0.0
     grad(8)      = oolen  * (hsurf(1,1)-hsurf(2,2))
     grad(9)      = 0.0
     s_oro(1,1)   = maxval(grad)
  ELSE
     s_oro(1,1)   = mdv
  ENDIF
  
  ! i=nx, j=1
  zlats  = lat(1)
  crlat  = COS ( zlats  * degrad )
  dx     = dlon * r_earth * degrad * crlat
  len    = sqrt(dx**2+dy**2)
  oolen  = 1./len
  oolenx = 1./dx
  IF (abs(hsurf(nx,1)-mdv).gt.eps) THEN

     grad(1)      = oolenx * (hsurf(nx,1)-hsurf(nx-1,1))
     grad(2)      = 0.0
     grad(3)      = 0.0
     grad(4)      = ooleny * (hsurf(nx,1)-hsurf(nx  ,2))
     grad(5)      = 0.0
     grad(6)      = oolen  * (hsurf(nx,1)-hsurf(nx-1,2))
     grad(7)      = 0.0
     grad(8)      = 0.0
     grad(9)      = 0.0
     s_oro(nx,1)  = maxval(grad)
  ELSE
     s_oro(nx,1)   = mdv
  ENDIF
  
  ! i=1,j=ny
  zlats  = lat(ny)
  crlat  = COS ( zlats  * degrad )
  dx     = dlon * r_earth * degrad * crlat
  len    = sqrt(dx**2+dy**2)
  oolen  = 1./len
  oolenx = 1./dx
  IF (abs(hsurf(1,ny)-mdv).gt.eps) THEN

     grad(1)      = 0.0
     grad(2)      = oolenx * (hsurf(1,ny)-hsurf(2,ny  ))
     grad(3)      = ooleny * (hsurf(1,ny)-hsurf(1  ,ny-1))
     grad(4)      = 0.0
     grad(5)      = 0.0
     grad(6)      = 0.0
     grad(7)      = oolen  * (hsurf(i,ny)-hsurf(2,ny-1))
     grad(8)      = 0.0
     grad(9)      = 0.0
     s_oro(1,ny)  = maxval(grad)
  ELSE
     s_oro(1,ny)   = mdv
  ENDIF

  ! i=nx,j=ny

  IF (abs(hsurf(nx,ny)-mdv).gt.eps) THEN
     grad(1)      = oolenx * (hsurf(nx,ny)-hsurf(nx-1,ny  ))
     grad(2)      = 0.0
     grad(3)      = ooleny * (hsurf(nx,ny)-hsurf(nx  ,ny-1))
     grad(4)      = 0.0
     grad(5)      = oolen  * (hsurf(nx,ny)-hsurf(nx-1,ny-1))
     grad(6)      = 0.0
     grad(7)      = 0.0
     grad(8)      = 0.0
     grad(9)      = 0.0
     s_oro(nx,ny) = maxval(grad)
  ELSE
     s_oro(nx,ny)   = mdv
  ENDIF

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

  status = nf90_def_var(ncido, 'lon', NF90_FLOAT,(/londim/), lonid)
  call check_err(status)

  status = nf90_def_var(ncido, 'lat', NF90_FLOAT,(/latdim/), latid)
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
  STATUS = NF90_PUT_VAR(ncido, lonid, lon)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  STATUS = NF90_PUT_VAR(ncido, latid, lat)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  STATUS = NF90_PUT_VAR(ncido, outid,s_oro)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  deallocate(hsurf)
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
