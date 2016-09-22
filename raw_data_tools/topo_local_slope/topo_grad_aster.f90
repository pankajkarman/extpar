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
! module load netcdf/4.3.2
! gfortran -O3 -o topo_grad_aster topo_grad_aster.f90 `nf-config --fflags --flibs` !
! usage:                                                                            !
!                                                                                   !
! ./topo_grad infile.nc outfile.nc                                                  !
!                                                                                   !
! based on code from Linda Schlemmer, IAC ETH Zurich, July 2016                 
                       !
! Daniel Luethi, IAC ETH Zurich, September 2016                                      
!-----------------------------------------------------------------------------------!

Program topo_grad

  use netcdf

  implicit none     

  integer nx, ny, nt, nxp2, nyp2

  character*80 infile,outfile
  CHARACTER*4  tilename
  INTEGER, PARAMETER :: short = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: double = SELECTED_REAL_KIND(15)

  ! input fields
  real, allocatable :: &

       hsurf  (:,:), hsurf_inner(:,:)

  ! output fields
  INTEGER(short), ALLOCATABLE :: &

       s_oro  (:,:)

  ! grid
  REAL(double), ALLOCATABLE :: &

       lat   (:),    & !
       lon   (:)

  !* netCDF id
  integer  ncid, ncido, status
  !* dimension ids
  integer londim, latdim
  !* variable ids
  integer lonid, latid
  integer varid, outid, mapid

  integer i, j, nargs

  CHARACTER(LEN=100) name, char

  REAL(double) dx, dy, len, oolen, oolenx, ooleny, grad(9), zlats, crlat 
  INTEGER(short) :: mdv

  REAL(double), PARAMETER :: r_earth  =  6371.229E3 ! radius of the earth
  REAL(double), PARAMETER :: pi       =  4.0 * ATAN (1.0)
  REAL(double), PARAMETER :: degrad   =   pi / 180.0
  REAL(double), PARAMETER :: dlat     =  1./3600. ! resolution
  REAL(double), PARAMETER :: dlon     =  1./3600. ! resolution
  REAL(double), PARAMETER :: eps      =  1.E-9
  REAL(double), PARAMETER :: add_offset = 0.
  REAL(double), PARAMETER :: scale_factor = 0.001
  REAL(double), PARAMETER :: r_scfct = 1. / scale_factor

  ! Read user input
  
  nargs = command_argument_count()
  IF(nargs .NE. 3) THEN
     PRINT*,'provide - input output tilename - as arguments (see source code), STOPPING'
     STOP
  ELSE
     CALL get_command_argument(1,infile)
     CALL get_command_argument(2,outfile)
     CALL get_command_argument(3,tilename)
     PRINT*,'in: ', TRIM(infile), 'out: ',TRIM(outfile),' tile: ',tilename
  end if

  ! Open file
  
  status = nf90_open(infile, nf90_nowrite, ncid)
  IF (STATUS .NE. NF90_NOERR) THEN
     PRINT *, NF90_STRERROR(STATUS)
     STOP
  ENDIF
  PRINT *,'file infile opened'
  ! inquire dimensions

  status=nf90_inq_dimid(ncid,"lon",lonid)
  status=nf90_inquire_dimension(ncid,lonid,len=nxp2)
  status=nf90_inq_dimid(ncid,"lat",latid)
  status=nf90_inquire_dimension(ncid,latid,len=nyp2)
  PRINT *,'dimensions read:', nxp2, nyp2

  ! allocate fields
  nx = nxp2 - 2
  ny = nyp2 - 2

  ALLOCATE(hsurf(0:nx+1,0:ny+1))
  ALLOCATE(s_oro(nx,ny),hsurf_inner(nx,ny))
  PRINT *,'2d fields allocated'

  ALLOCATE(lon(0:nx+1))
  ALLOCATE(lat(0:ny+1))
  PRINT *,'1d fields allocated'

!  s_oro(1:nx,1:ny)    = 0.0
!  print *,'s_oro initialized'
!  hsurf(0:nx+1,0:ny+1)    = 0.0
!  print *,'hsurf initialized'

  lat(:)       = 0.0
  lon(:)       = 0.0
  PRINT *,'arrays initialized'

  ! Read in variables
  
  !print*,'READING VARIABLES'

  status = nf90_inq_varid(ncid,"lon", lonid)
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(status)
  status = nf90_get_var(ncid,lonid,lon)
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(status)
  PRINT *,'lon read'
 
  status = nf90_inq_varid(ncid,"lat", latid)
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(status)
  status = nf90_get_var(ncid,latid,lat)
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(status)
  PRINT *,'lat read'
  
  status = nf90_inq_varid(ncid,"Z", varid)
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(status)
  status = nf90_get_var(ncid,varid,hsurf)
  PRINT *,'hsurf read'
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(status)
  status = nf90_get_att(ncid, varid,'_FillValue',mdv)
  CALL check_err(status)

  PRINT *,'mdv = ',mdv
  
  ! Calculations
  hsurf_inner(:,:) = hsurf(1:nx,1:ny)
  WHERE (ABS(hsurf-mdv) <= eps) hsurf=0.0

  dy = r_earth * dlat * degrad
  ooleny = 1./dy
  DO   j = 1, ny
     zlats  = lat(j)
     crlat  = COS ( zlats  * degrad )
     dx     = dlon * r_earth * degrad * crlat
     len    = sqrt(dx**2+dy**2)
     oolen  = 1./len
     oolenx = 1./dx
     DO i = 1, nx
       IF (abs(hsurf_inner(i,j)-mdv).gt.eps) THEN
         grad(1)      = oolenx * (hsurf(i,j)-hsurf(i-1,j  ))
         grad(2)      = oolenx * (hsurf(i,j)-hsurf(i+1,j  ))
         grad(3)      = ooleny * (hsurf(i,j)-hsurf(i  ,j-1))
         grad(4)      = ooleny * (hsurf(i,j)-hsurf(i  ,j+1))
         grad(5)      = oolen  * (hsurf(i,j)-hsurf(i-1,j-1))
         grad(6)      = oolen  * (hsurf(i,j)-hsurf(i-1,j+1))
         grad(7)      = oolen  * (hsurf(i,j)-hsurf(i+1,j-1))
         grad(8)      = oolen  * (hsurf(i,j)-hsurf(i+1,j+1))
         grad(9)      = 0.0
         s_oro(i,j)   = NINT((MAXVAL(grad)-add_offset) * r_scfct)
       ELSE
         s_oro(i,j)   = mdv
       ENDIF
     END DO

  END DO
 
!  print*,'CREATE NEW NetCDF FILE'

  !* enter define mode
  status = nf90_create (outfile,  NF90_NETCDF4, ncido)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)


  !* define dimensions
  status = nf90_def_dim(ncido, 'lon', nx, londim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_def_dim(ncido, 'lat', ny, latdim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  !* define variables

  status = nf90_def_var(ncido, 'lon', NF90_DOUBLE,(/londim/), lonid)
  CALL check_err(status)

  status = nf90_def_var(ncido, 'lat', NF90_DOUBLE,(/latdim/), latid)
  CALL check_err(status)

  status = nf90_def_var(ncido, 'S_ORO', NF90_SHORT,(/londim,latdim/),outid)
  CALL check_err(status)

  status = nf90_def_var(ncido, 'regular_grid', NF90_CHAR,mapid)
  CALL check_err(status)

!  print*,'ATTRIBUTES'
  status = nf90_inq_varid(ncid,"lat", varid)
  status = nf90_get_att(ncid, varid,'long_name',name)
  CALL check_err(status)
  status = nf90_put_att(ncido, latid,'long_name',TRIM(name))
  CALL check_err(status)
  status = nf90_get_att(ncid, varid,'units',name)
  CALL check_err(status)
  status = nf90_put_att(ncido, latid,'units',TRIM(name))
  CALL check_err(status)

  status = nf90_inq_varid(ncid,"lon", varid)
  status = nf90_get_att(ncid, varid,'long_name',name)
  CALL check_err(status)
  status = nf90_put_att(ncido, lonid,'long_name',TRIM(name))
  CALL check_err(status)
  status = nf90_get_att(ncid, varid,'units',name)
  CALL check_err(status)
  status = nf90_put_att(ncido, lonid,'units',TRIM(name))
  CALL check_err(status)

  status = nf90_put_att(ncido, outid,'standard_name','topography gradient')
  CALL check_err(status)
  status = nf90_put_att(ncido, outid,'long_name','maximum local gradient of surface height')
  CALL check_err(status)
  status = nf90_put_att(ncido, outid,'scale_factor',scale_factor)
  CALL check_err(status)
  status = nf90_put_att(ncido, outid,'add_offset',add_offset)
  CALL check_err(status)
  status = nf90_put_att(ncido, outid,'units','')
  CALL check_err(status)

  status = nf90_put_att(ncido, outid,'grid_mapping','regular_grid')
  CALL check_err(status)
  status = nf90_put_att(ncido, outid,'_FillValue',mdv)
  CALL check_err(status)
  status = nf90_put_att(ncido, outid,'comment','ASTER tile: '//tilename)
  CALL check_err(status)

  status = nf90_put_att(ncido, mapid,'grid_mapping_name','latitude_longitude')
  CALL check_err(status)

!* leave define mode
  status = NF90_ENDDEF(ncido)
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(status)

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
