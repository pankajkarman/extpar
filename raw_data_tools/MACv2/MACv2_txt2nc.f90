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

PROGRAM macv2_txt2nc

  USE netcdf

  IMPLICIT none     

  INTEGER nx, ny, nt, nspb, nbds

  CHARACTER*80 infile,outfile

  ! output fields
  REAL, ALLOCATABLE :: MAC_aot (:,:,:,:), &
                       MAC_ssa (:,:,:,:), &
                       MAC_asy (:,:,:,:)  

  ! grid
  REAL, ALLOCATABLE :: lat   (:),    & 
                       lon   (:),    &
                       spectr(:),    &
                       sp_bds(:,:),  &
                       timev (:)

  REAL              :: mdv, timintv(11)

  !* netCDF id
  INTEGER  ncid, ncido, status, nuin
  !* dimension ids
  INTEGER londim, latdim, specdim, timdim, bdsdim
  !* variable ids
  INTEGER lonid, latid, timid, sbdsid, specid
  INTEGER varid, aotid, ssaid, asyid, rgrid

  INTEGER i, j, im, ispb, nargs, ilon, ilat

  CHARACTER(len=100) name, char

! set dimension length
  nx = 360
  ny = 180
  nspb = 9
  nt = 12
  nbds = 2
!  nuin = 33
  mdv = -999.99
  
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

  ALLOCATE (lon(nx), lat(ny))
  ALLOCATE (MAC_aot(nx,ny,nspb,nt), MAC_ssa(nx,ny,nspb,nt))
  ALLOCATE (MAC_asy(nx,ny,nspb,nt))
  ALLOCATE (spectr(nspb),sp_bds(nbds,nspb),timev(nt))

  timintv = (/31,28,31,30,31,30,31,31,30,31,30/)
  spectr=(/1.47,0.87,0.55,30.,15.,10.42,9.75,7.0,0.55/)
  sp_bds = reshape ((/1.53,4.64,0.7,1.53,0.25,0.7,20.,104.5,12.5,20.0,8.33, &
           12.5,9.01,10.31,4.64,8.33,0.25,0.7/), shape(sp_bds))

  timev(1)=15.0
  DO i=1,11
    timev(i+1)=timev(i)+timintv(i)
  ENDDO
  PRINT *,'1: timev = ',timev
  ! Open file
  
  OPEN (nuin, FILE=infile)
  DO i = 1,360
    DO j = 1,180
      DO im = 1,12
        READ(nuin,101) (MAC_aot(i,j,ispb,im), ispb = 1,nspb) !read file
      ENDDO
    ENDDO
  ENDDO
  PRINT *,'read in AOT data'
  DO i = 1,360
    DO j = 1,180
      DO im = 1,12
        READ(nuin,101) (MAC_ssa(i,j,ispb,im), ispb = 1,9) !read file
      ENDDO
    ENDDO
  ENDDO
  PRINT *,'read in SSA data'
  DO i = 1,360
    DO j = 1,180
      DO im = 1,12
        READ(nuin,101) (MAC_asy(i,j,ispb,im), ispb = 1,9) !read file
      ENDDO
    ENDDO
  ENDDO
  PRINT *,'read in ASY data'
  CLOSE(nuin)
101 FORMAT (9(f5.3,1x))
  ! allocate fields
  DO i = 1, nx
    lon(i) = -179.5 + (i-1)*1.0
  ENDDO
  DO j = 1, ny
    lat(j) = 89.5 - (j-1)*1.0
  ENDDO
  PRINT *,'defined longitude latitude', nx, ny

  !* enter define mode
  status = nf90_create (outfile,  NF90_CLOBBER, ncido)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)


  !* define dimensions
  status = nf90_def_dim(ncido, 'lon', nx, londim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_def_dim(ncido, 'lat', ny, latdim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_def_dim(ncido, 'spectr', nspb, specdim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_def_dim(ncido, 'bds', nbds, bdsdim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  status = nf90_def_dim(ncido, 'time', nt, timdim)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  !* define variables

  status = nf90_def_var(ncido, 'lon', NF90_FLOAT,(/londim/), lonid)
  call check_err(status)

  status = nf90_def_var(ncido, 'lat', NF90_FLOAT,(/latdim/), latid)
  call check_err(status)

  status = nf90_def_var(ncido, 'spectval', NF90_FLOAT,(/specdim/), specid)
  call check_err(status)

  status = nf90_def_var(ncido, 'sp_bds', NF90_FLOAT,(/bdsdim,specdim/),sbdsid)
  call check_err(status)

  status = nf90_def_var(ncido, 'time', NF90_FLOAT,(/timdim/), timid)
  call check_err(status)  

  status = nf90_def_var(ncido, 'AOT', NF90_FLOAT,(/londim,latdim,specdim,timdim/),aotid)
  call check_err(status)
  status = nf90_def_var(ncido, 'SSA', NF90_FLOAT,(/londim,latdim,specdim,timdim/),ssaid)
  call check_err(status)
  status = nf90_def_var(ncido, 'ASY', NF90_FLOAT,(/londim,latdim,specdim,timdim/),asyid)
  call check_err(status)
  status = nf90_def_var(ncido, 'regular_grid', NF90_CHAR,rgrid)
  call check_err(status)

!  print*,'ATTRIBUTES'
  status = nf90_put_att(ncido, latid,'long_name','latitude')
  call check_err(status)
  status = nf90_put_att(ncido, latid,'units','degrees_north')
  call check_err(status)

  status = nf90_put_att(ncido, lonid,'long_name','longitude')
  call check_err(status)
  status = nf90_put_att(ncido, lonid,'units','degrees_east')
  call check_err(status)

  status = nf90_put_att(ncido, timid,'units','days since 2005-01-01 00:00:00')
  call check_err(status)

  status = nf90_put_att(ncido, specid,'bounds','sp_bds')
  call check_err(status)

  status = nf90_put_att(ncido, aotid,'standard_name','-')
  call check_err(status)
  status = nf90_put_att(ncido, aotid,'long_name','atmospheric optical thickness')
  call check_err(status)
  status = nf90_put_att(ncido, aotid,'units','-')
  call check_err(status)
  status = nf90_put_att(ncido, aotid,'grid_mapping','regular_grid')
  call check_err(status)

  status = nf90_put_att(ncido, ssaid,'standard_name','-')
  call check_err(status)
  status = nf90_put_att(ncido, ssaid,'long_name','single scattering albedo')
  call check_err(status)
  status = nf90_put_att(ncido, ssaid,'units','-')
  call check_err(status)
  status = nf90_put_att(ncido, ssaid,'grid_mapping','regular_grid')
  call check_err(status)

  status = nf90_put_att(ncido, asyid,'standard_name','-')
  call check_err(status)
  status = nf90_put_att(ncido, asyid,'long_name','asymmetry')
  call check_err(status)
  status = nf90_put_att(ncido, asyid,'units','-')
  call check_err(status)
  status = nf90_put_att(ncido, asyid,'grid_mapping','regular_grid')
  call check_err(status)

  status = nf90_put_att(ncido, aotid,'grid_mapping','regular_grid')
  call check_err(status)
  status = nf90_put_att(ncido, aotid,'_FillValue',mdv)
  call check_err(status)

  status = nf90_put_att(ncido, ssaid,'grid_mapping','regular_grid')
  call check_err(status)
  status = nf90_put_att(ncido, ssaid,'_FillValue',mdv)
  call check_err(status)

  status = nf90_put_att(ncido, asyid,'grid_mapping','regular_grid')
  call check_err(status)
  status = nf90_put_att(ncido, asyid,'_FillValue',mdv)
  call check_err(status)
  status = nf90_put_att(ncido, rgrid,'grid_mapping','latitude_longitude')
  call check_err(status)

!* leave define mode
  status = NF90_ENDDEF(ncido)
  IF (status .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

!* store variables
  STATUS = NF90_PUT_VAR(ncido, lonid, lon)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  STATUS = NF90_PUT_VAR(ncido, latid, lat)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  STATUS = NF90_PUT_VAR(ncido, timid, timev)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  STATUS = NF90_PUT_VAR(ncido, specid, spectr)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  STATUS = NF90_PUT_VAR(ncido, sbdsid, sp_bds)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)
  
  STATUS = NF90_PUT_VAR(ncido, aotid, MAC_aot)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  STATUS = NF90_PUT_VAR(ncido, ssaid, MAC_ssa)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  STATUS = NF90_PUT_VAR(ncido, asyid, MAC_asy)
  IF (STATUS .NE. NF90_NOERR) PRINT *, NF90_STRERROR(STATUS)

  deallocate(MAC_aot)
  deallocate(MAC_ssa)
  deallocate(MAC_asy)

  DEALLOCATE(lon)
  DEALLOCATE(lat)
  DEALLOCATE(timev)
  DEALLOCATE(spectr)
  DEALLOCATE(sp_bds)

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

end Program  macv2_txt2nc
