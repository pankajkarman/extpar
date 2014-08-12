!+  Fortran module with data fields for Aerosol optical thickness data
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
!> Fortran module with data fields for Aerosol optical thickness data
!> \author Hermann Asensio
!
MODULE mo_aot_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

USE netcdf,      ONLY :   &
  nf90_open,              &
  nf90_close,             &
  nf90_inquire,           &
  nf90_inquire_dimension, &
  nf90_inquire_variable,  &
  nf90_inq_attname,       &
  nf90_inquire_attribute, &
  nf90_get_att,           &
  nf90_inquire_dimension, &
  nf90_inq_varid,          &
  nf90_get_var,            &
  nf90_noerr,              &
  nf90_strerror

USE netcdf,      ONLY:     &
  nf90_create,             &
  nf90_def_dim,            &
  nf90_def_var,            &
  nf90_enddef,             &
  nf90_redef,              &
  nf90_put_att,            &
  nf90_put_var

 
USE netcdf,      ONLY :   &
  NF90_CHAR,               &
  NF90_DOUBLE,             &
  NF90_FLOAT,              &
  NF90_INT,                &
  NF90_BYTE,               &
  NF90_SHORT


USE netcdf,      ONLY :   &
  NF90_GLOBAL,             &
  NF90_UNLIMITED,          &
  NF90_CLOBBER,            &
  NF90_NOWRITE



!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_io_utilities, ONLY: check_netcdf

USE mo_GRID_structures, ONLY: reg_lonlat_grid

USE mo_io_units,          ONLY: filename_max
                           
IMPLICIT NONE
PRIVATE

PUBLIC :: aot_grid
PUBLIC :: allocate_aot_data, &
          deallocate_aot_data, &
          read_namelists_extpar_aerosol, &
          read_aot_data_input_namelist, &
          get_dimension_aot_data, &
          get_aot_grid_and_data, &
          lon_aot, &
          lat_aot, &
          aot_data, &
          ntype_aot, &
          aot_varname, &
          aot_longname, &
          aot_shortname
PUBLIC :: ntime_aot
PUBLIC :: iaot_type




TYPE(reg_lonlat_grid) :: aot_grid !< structure with defenition of the raw data grid for the AOT dataset

REAL (KIND=wp), ALLOCATABLE :: lon_aot(:) !< longitude of aot grid
REAL (KIND=wp), ALLOCATABLE :: lat_aot(:) !< latitude of aot grid

REAL (KIND=wp), ALLOCATABLE :: aot_data(:,:,:,:) !< aerosol optical thickness, aot(ntype,ncolumns,nrows,ntime) 

INTEGER (KIND=i8), PARAMETER :: ntype_aot = 5 !< number of types of aerosols
INTEGER (KIND=i8), PARAMETER :: ntime_aot = 12 !< 12 monthly mean data of aeorsol optical thickness

CHARACTER (len=32) :: aot_varname(ntype_aot) = &    !< variable name for aerosolt type
                    & (/ 'bc   ', 'dust ', 'org  ', 'so4  ', 'ssalt' /)
CHARACTER (len=80) :: aot_longname(ntype_aot) = &   !< long name for aereosol type
                    &(/ 'aerosol optical thickness of black carbon  ', &
                    &   'aerosol optical thickness of dust          ', &
                    &   'aerosol optical thickness of organic matter', &
                    &   'aerosol optical thickness of sulfate       ', &
                    &   'aerosol optical thickness of sea salt      ' /)
CHARACTER (len=20) :: aot_shortname(ntype_aot)= &   !< short name for aereosol type
                    &(/ 'AER_BC              ', &
                    &   'AER_DUST            ', &
                    &   'AER_ORG             ', &
                    &   'AER_SO4             ', &
                    &   'AER_SS              ' /)

INTEGER (KIND=i4)            :: iaot_type = 1






CONTAINS


!---------------------------------------------------------------------------
!> subroutine to read namelist for aerosol optical thickness data settings for EXTPAR 
SUBROUTINE read_namelists_extpar_aerosol(namelist_file, &
                                         iaot_type,    &
                                         raw_data_aot_path, &
                                         raw_data_aot_filename, &
                                         aot_buffer_file, &
                                         aot_output_file)

  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number

  
  CHARACTER (len=filename_max), INTENT(IN) :: namelist_file !< filename with namelists for for EXTPAR settings


! aerosol optical thickness

CHARACTER (len=filename_max) :: raw_data_aot_path        !< path to raw data
CHARACTER (len=filename_max) :: raw_data_aot_filename !< filename temperature climatology raw data
INTEGER (KIND=i4)            :: iaot_type  !< ID of dataset used

CHARACTER (len=filename_max) :: aot_buffer_file !< name for aerosol buffer file
CHARACTER (len=filename_max) :: aot_output_file !< name for aerosol output file

!> namelist with filenames for aerosol optical thickness data input
NAMELIST /aerosol_raw_data/ raw_data_aot_path, raw_data_aot_filename, iaot_type

!> namelist with filenames for aerosol optical thickness data output
NAMELIST /aerosol_io_extpar/ aot_buffer_file, aot_output_file


   INTEGER           :: nuin !< unit number
   INTEGER (KIND=i4) :: ierr !< error flag


   nuin = free_un()  ! functioin free_un returns free Fortran unit number
   OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)

   READ(nuin, NML=aerosol_raw_data, IOSTAT=ierr)
   READ(nuin, NML=aerosol_io_extpar, IOSTAT=ierr)

   CLOSE(nuin)


END SUBROUTINE read_namelists_extpar_aerosol
!---------------------------------------------------------------------------


!> subroutine to allocate aot data fields
  SUBROUTINE allocate_aot_data(nrows,ncolumns,ntime,ntype)
  IMPLICIT NONE
  INTEGER (KIND=i8), INTENT(IN) :: ntype !< number of types of aerosols
  INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
  INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns
  INTEGER (KIND=i8), INTENT(IN) :: ntime !< number of times

  INTEGER :: errorcode !< error status variable


    ALLOCATE (lon_aot(1:ncolumns), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_aot')
    lon_aot = 0.0

     ALLOCATE (lat_aot(1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_aot')
    lat_aot = 0.0

    ALLOCATE (aot_data(1:ncolumns,1:nrows,1:ntime,1:ntype),STAT=errorcode)
      IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array aot')
      aot_data = 0.0





  END SUBROUTINE allocate_aot_data


  !> subroutine to read namelist with settings for aerosol data
  !! \author Hermann Asensio
  SUBROUTINE read_aot_data_input_namelist(input_namelist_file, aot_filename)
  
         USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number
         USE mo_io_units,          ONLY: filename_max


           CHARACTER (LEN=filename_max), INTENT(IN)  :: input_namelist_file !< file with input namelist 
           CHARACTER (LEN=filename_max), INTENT(OUT) :: aot_filename  !< filename aot raw data

           !>Define the namelist group
           NAMELIST /AOT_file_info/ aot_filename

           INTEGER (KIND=i4) :: ierr !< error flag
           INTEGER           :: nuin !< unit number

              nuin = free_un()  ! functioin free_un returns free Fortran unit number
              open(nuin,FILE=TRIM(input_namelist_file), IOSTAT=ierr)
              read(nuin, NML=AOT_file_info, IOSTAT=ierr)

              close(nuin)

   END SUBROUTINE read_aot_data_input_namelist

   !> get dimension information of aot data from netcdf file
   SUBROUTINE get_dimension_aot_data(aot_filename, &
                                     nrows,        &
                                     ncolumns,     &
                                     ntime,        &
                                     ntype)
   IMPLICIT NONE
   CHARACTER (LEN=filename_max), INTENT(IN)  ::  aot_filename  !< filename aot raw data

   INTEGER (KIND=i8), INTENT(OUT) :: ntype !< number of types of aerosols
   INTEGER (KIND=i8), INTENT(OUT) :: nrows !< number of rows
   INTEGER (KIND=i8), INTENT(OUT) :: ncolumns !< number of columns
   INTEGER (KIND=i8), INTENT(OUT) :: ntime !< number of times

     !local variables
        INTEGER :: ncid                             !< netcdf unit file number
        INTEGER :: ndimension                       !< number of dimensions in netcdf file
        INTEGER :: nVars                            !< number of variables in netcdf file
        INTEGER :: nGlobalAtts                      !< number of gloabal Attributes in netcdf file
        INTEGER :: unlimdimid                       !< id of unlimited dimension (e.g. time) in netcdf file

        INTEGER :: dimid                            !< id of dimension
        CHARACTER (len=80) :: dimname               !< name of dimensiona
        INTEGER :: length                           !< length of dimension



    ! open netcdf file 
    call check_netcdf( nf90_open(TRIM(aot_filename),NF90_NOWRITE, ncid))
    
    ! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible unlimited dimension (probably time)
     call check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))

       !; the dimid in netcdf-files is counted from 1 to ndimension
       !; look for the name and length of the dimension with f90_inquire_dimension
       !; nf90_inquire_dimension input: ncid, dimid; nf90_inquire_dimension output: name, length
       do dimid=1,ndimension
                    !print *,'dimension loop dimid ',dimid
         call check_netcdf( nf90_inquire_dimension(ncid,dimid, dimname, length) )
                     !print*, 'ncid,dimid, dimname, length',ncid,dimid, trim(dimname), length
         if ( trim(dimname) == 'lon') ncolumns=length          ! here I know that the name of zonal dimension is 'lon'
         if ( trim(dimname) == 'lat') nrows=length             ! here I know that the name of meridional dimension is 'lat'
         if ( trim(dimname) == 'time') ntime=length            ! here I know that the name of time dimension is "time"
       enddo

    ! close netcdf file 
     call check_netcdf( nf90_close( ncid))

     ntype = ntype_aot ! here I know that i have 5 different aerosol types in the netcdf file
                       ! i.e. black_carbon, dust, organic, sulfate, sea_salt





   END SUBROUTINE get_dimension_aot_data 


   !> get all aot data and coordinates and grid description
   SUBROUTINE get_aot_grid_and_data(aot_filename, &
                                     nrows,        &
                                     ncolumns,     &
                                     ntime,        &
                                     ntype,        &
                                     aot_grid,     &
                                     lon_aot,      &
                                     lat_aot,      &
                                     aot_data)
   IMPLICIT NONE
   CHARACTER (LEN=filename_max), INTENT(IN)  ::  aot_filename  !< filename aot raw data
   INTEGER (KIND=i8), INTENT(IN) :: ntype !< number of types of aerosols
   INTEGER (KIND=i8), INTENT(IN) :: nrows !< number of rows
   INTEGER (KIND=i8), INTENT(IN) :: ncolumns !< number of columns
   INTEGER (KIND=i8), INTENT(IN) :: ntime !< number of times
   
   TYPE(reg_lonlat_grid), INTENT(INOUT) :: aot_grid !< structure with defenition of the raw data grid for the AOT dataset

   REAL (KIND=wp), INTENT(INOUT) :: lon_aot(1:ncolumns) !< longitude coordinates of aot grid
   REAL (KIND=wp), INTENT(INOUT) :: lat_aot(1:nrows) !< latitude coordinates of aot grid
   REAL (KIND=wp), INTENT(INOUT) :: aot_data(1:ncolumns,1:nrows,1:ntime,1:ntype) !< aerosol optical thickness, aot(ntype,ncolumns,nrows,ntime) 


    !local variables
        REAL :: aot_data_stype(ncolumns,nrows,ntime)
        INTEGER :: ncid                             !< netcdf unit file number

        CHARACTER (LEN=80) :: varname(ntype)  !< name of variable
        INTEGER :: varid(ntype)               !< id of variable

        CHARACTER (LEN=80) :: cooname(2) !< name of coordinates
        INTEGER :: coovarid(2)           !< varid of coordinats

        INTEGER :: n !< counter

        cooname(1) = 'lon'
        cooname(2) = 'lat'


        ! I know the names of tha variables already
        varname(1) = 'black_carbon'
        varname(2) = 'dust'
        varname(3) = 'organic'
        varname(4) = 'sulfate'
        varname(5) = 'sea_salt'

  
    ! open netcdf file 
    call check_netcdf( nf90_open(TRIM(aot_filename),NF90_NOWRITE, ncid))

    DO n=1,2
     call check_netcdf( nf90_inq_varid(ncid, TRIM(cooname(n)), coovarid(n)))
    ENDDO

    CALL check_netcdf(nf90_get_var(ncid, coovarid(1),  lon_aot))
    CALL check_netcdf(nf90_get_var(ncid, coovarid(2),  lat_aot))



    DO n=1,ntype
    call check_netcdf( nf90_inq_varid(ncid, TRIM(varname(n)), varid(n)))

    CALL check_netcdf(nf90_get_var(ncid, varid(n),  aot_data_stype))


     aot_data(:,:,:,n) = aot_data_stype(:,:,:)

     ENDDO
     ! close netcdf file 
     call check_netcdf( nf90_close( ncid))

     ! set aot_grid values
     aot_grid%start_lon_reg = lon_aot(1)
     !aot_grid%end_lon_reg = lon_aot(ncolumns)
     aot_grid%start_lat_reg = lat_aot(1)
     !aot_grid%end_lat_reg = lat_aot(nrows)
     aot_grid%dlon_reg = (lon_aot(ncolumns) -  lon_aot(1) ) / (ncolumns - 1)
     aot_grid%dlat_reg = (lat_aot(nrows) - lat_aot(1) ) / (nrows -1) 
     aot_grid%nlon_reg = ncolumns
     aot_grid%nlat_reg = nrows

     aot_grid%end_lon_reg = lon_aot(ncolumns)
     aot_grid%end_lat_reg = lat_aot(nrows)





   END SUBROUTINE get_aot_grid_and_data


  SUBROUTINE deallocate_aot_data()

    USE mo_aot_target_fields, ONLY: aot_tg

    IMPLICIT NONE
   
    INTEGER :: errorcode !< error status variable


    DEALLOCATE (lon_aot, STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant deallocate the array lon_aot')
    DEALLOCATE (lat_aot, STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant deallocate the array lat_aot')
    DEALLOCATE (aot_data, STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant deallocate the array aot_data')
    DEALLOCATE (aot_tg, STAT=errorcode)
    IF(errorcode.NE.0) CALL abort_extpar('Cant deallocate the array aot_tg')
    
  END SUBROUTINE deallocate_aot_data

END MODULE mo_aot_data
