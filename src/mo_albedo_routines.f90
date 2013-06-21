!+ Fortran module with Albedo data handling routines
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_8         2013-03-12 Frank Brenner
!  introduced MODIS albedo dataset(s) as new external parameter(s)
! V1_11        2013/04/16 Juergen Helmert
!  Adaptions for using external land-sea mask  
! V1_12        2013-04-24 Frank Brenner
!  bug fix regarding old file paths         
! V1_13        2013-05-29 Frank Brenner
!  missing values fixed         
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with Albedo data handling routines
!> \author Hermann Asensio, Frank Brenner
!>
MODULE mo_albedo_routines

USE mo_kind, ONLY: wp, &
                   i8, &
                   i4

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


USE mo_io_utilities,           ONLY: check_netcdf
USE mo_io_units,          ONLY: filename_max


USE mo_GRID_structures,        ONLY: reg_lonlat_grid



IMPLICIT NONE

PRIVATE


PUBLIC    open_netcdf_ALB_data, &
          close_netcdf_ALB_data, &
          read_namelists_extpar_alb, &
          read_alb_data_input_namelist, &
          get_dimension_ALB_data, &
          get_ALB_data_coordinates, &
          get_one_row_ALB_data, &
          get_block_ALB_data, &
          get_pixel_ALB_data, &
          const_check_interpol_alb

CONTAINS

!---------------------------------------------------------------------------
!> subroutine to read namelist including albedo data settings for EXTPAR 
SUBROUTINE read_namelists_extpar_alb(namelist_file, &
    &                                  raw_data_alb_path, &
    &                                  raw_data_alb_filename, &
    &                                  raw_data_alnid_filename, &
    &                                  raw_data_aluvd_filename, &
    &                                  alb_buffer_file, &
    &                                  alb_output_file, &
    &                                  alb_source, &
    &                                  alnid_source, &
    &                                  aluvd_source)


  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number

  
  CHARACTER (len=filename_max), INTENT(IN) :: namelist_file !< filename with namelists for for EXTPAR settings

! NDVI
CHARACTER (len=filename_max) :: raw_data_alb_path        !< path to raw data
CHARACTER (len=filename_max) :: raw_data_alb_filename !< filenames albedo raw data
CHARACTER (len=filename_max) :: raw_data_alnid_filename
CHARACTER (len=filename_max) :: raw_data_aluvd_filename

CHARACTER (len=filename_max) :: alb_buffer_file !< name for albedo buffer file
CHARACTER (len=filename_max) :: alb_output_file !< name for albedo output file

CHARACTER (len=filename_max) :: alb_source
CHARACTER (len=filename_max) :: alnid_source
CHARACTER (len=filename_max) :: aluvd_source

!> namelist with filenames for albedo data input
NAMELIST /alb_raw_data/ raw_data_alb_path, raw_data_alb_filename
NAMELIST /alnid_raw_data/ raw_data_alb_path, raw_data_alnid_filename
NAMELIST /aluvd_raw_data/ raw_data_alb_path, raw_data_aluvd_filename

!> namelist with filenames for albedo data output
NAMELIST /alb_io_extpar/ alb_buffer_file, alb_output_file

NAMELIST /alb_source_file/ alb_source, alnid_source, aluvd_source

   INTEGER           :: nuin !< unit number
   INTEGER (KIND=i4) :: ierr !< error flag


   nuin = free_un()  ! functioin free_un returns free Fortran unit number

   OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)

   READ(nuin, NML=alb_raw_data, IOSTAT=ierr)
   READ(nuin, NML=alnid_raw_data, IOSTAT=ierr)
   READ(nuin, NML=aluvd_raw_data, IOSTAT=ierr)
   READ(nuin, NML=alb_io_extpar, IOSTAT=ierr)
   READ(nuin, NML=alb_source_file, IOSTAT=ierr)
   
   CLOSE(nuin)


END SUBROUTINE read_namelists_extpar_alb
!---------------------------------------------------------------------------

    !> open netcdf-file and get netcdf unit file number
       SUBROUTINE open_netcdf_ALB_data(path_alb_file, &
                                        ncid)
         CHARACTER (len=*), INTENT(in) :: path_alb_file         !< filename with path for albedo raw data
         INTEGER, INTENT(out) :: ncid                             !< netcdf unit file number
        !! open netcdf file 
            call check_netcdf( nf90_open(TRIM(path_alb_file),NF90_NOWRITE, ncid))

       END SUBROUTINE open_netcdf_ALB_data

        !> close netcdf-file 
       SUBROUTINE close_netcdf_ALB_data(ncid)
         INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number

        !! close netcdf file 
            call check_netcdf( nf90_close( ncid))

       END SUBROUTINE close_netcdf_ALB_data


        !> read namelist with settings for COSMO target grid
        !> \author Hermann Asensio
       SUBROUTINE read_alb_data_input_namelist(input_namelist_file,        &
                                                 raw_data_path,             &
                                                 raw_data_alb_filename,    &
                                                 raw_data_alnid_filename, &
                                                 raw_data_aluvd_filename, &
                                                 outputgrid_alb_filename)

         USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number

         USE mo_io_units,          ONLY: filename_max

           CHARACTER (len=*), INTENT(in) :: input_namelist_file !< file with input namelist 
           CHARACTER (len=filename_max), INTENT(out) :: raw_data_path        !< path to raw data
           CHARACTER (len=filename_max), INTENT(out) :: raw_data_alb_filename !< filename albedo raw data
           CHARACTER (len=filename_max), INTENT(out) :: raw_data_alnid_filename
           CHARACTER (len=filename_max), INTENT(out) :: raw_data_aluvd_filename
           CHARACTER (len=filename_max), INTENT(out) :: outputgrid_alb_filename !< output filename

           !>Define the namelist group
           NAMELIST /alb_data_input/ raw_data_path, raw_data_alb_filename, &
 & raw_data_alnid_filename, raw_data_aluvd_filename, outputgrid_alb_filename

           
           INTEGER (KIND=i4) :: ierr !< error flag
           INTEGER                  :: nuin !< unit number

              nuin = free_un()  ! functioin free_un returns free Fortran unit number
              open(nuin,FILE=input_namelist_file, IOSTAT=ierr)
              !print *, ierr
              read(nuin, NML=alb_data_input, IOSTAT=ierr)
              !print *, ierr

              close(nuin)

       END SUBROUTINE read_alb_data_input_namelist


            !> inquire dimension information for albedo raw data 
       SUBROUTINE get_dimension_ALB_data(ncid, &
                                          nlon_alb, &
                                          nlat_alb, &
                                          ntime_alb)

        INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number

        INTEGER (KIND=i4), INTENT(out) :: nlon_alb !< number of grid elements in zonal direction for albedo data
        INTEGER (KIND=i4), INTENT(out) :: nlat_alb !< number of grid elements in meridional direction for albedo data
        INTEGER (KIND=i4), INTENT(out) :: ntime_alb !< number of dates with albedo data

        !local variables
        INTEGER :: ndimension                       !< number of dimensions in netcdf file
        INTEGER :: nVars                            !< number of variables in netcdf file
        INTEGER :: nGlobalAtts                      !< number of gloabal Attributes in netcdf file
        INTEGER :: unlimdimid                       !< id of unlimited dimension (e.g. time) in netcdf file

        INTEGER :: dimid                            !< id of dimension
        CHARACTER (len=80) :: dimname               !< name of dimensiona
        INTEGER :: length                           !< length of dimension


    !! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible
!  unlimited dimension (probably time)
    !; nf90_inquire input: ncid; nf90_inquire output: ndimension, nVars, nGlobalAtts,unlimdimid
                call check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))
    !print *,'ncid,ndimension, nVars, nGlobalAtts,unlimdimid',ncid,ndimension, nVars, nGlobalAtts,unlimdimid


                !; the dimid in netcdf-files is counted from 1 to ndimension
                !; look for the name and length of the dimension with f90_inquire_dimension
                !; nf90_inquire_dimension input: ncid, dimid; nf90_inquire_dimension output: name, length
                do dimid=1,ndimension
                    !print *,'dimension loop dimid ',dimid
                    call check_netcdf( nf90_inquire_dimension(ncid,dimid, dimname, length) )
                            !print*, 'ncid,dimid, dimname, length',ncid,dimid, trim(dimname), length
                             if ( trim(dimname) == 'lon') nlon_alb=length          
! here I know that the name of zonal dimension is 'lon'
                             if ( trim(dimname) == 'lat') nlat_alb=length          
! here I know that the name of meridional dimension is 'lat'
                             if ( trim(dimname) == 'time') ntime_alb=length        
! here I know that the name of time dimension is 'time'
                enddo


       END SUBROUTINE get_dimension_ALB_data


       
        !> read coordinates for NDVI raw data from netcdf file
       SUBROUTINE get_ALB_data_coordinates(ncid,          &
                                            nlon_alb,     &
                                            nlat_alb,     &  
                                            startlon_alb, &
                                            startlat_alb, &
                                            dlon_alb,     &
                                            dlat_alb,     &
                                            lon,           &
                                            lat)

        INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number
        INTEGER (KIND=i4), INTENT(in) :: nlon_alb !< number of grid elements in zonal direction for albedo data
        INTEGER (KIND=i4), INTENT(in) :: nlat_alb !< number of grid elements in meridional direction for albedo data

        REAL (KIND=wp), INTENT(out) :: startlon_alb !< longitude of lower left grid element for albedo data 
        REAL (KIND=wp), INTENT(out) :: startlat_alb !< latitude of lower left grid element for albed data

        REAL (KIND=wp), INTENT(out) :: dlon_alb !< grid point distance in zonal direction (in degrees) for albedo data
        REAL (KIND=wp), INTENT(out) :: dlat_alb !< grid point distance in meridional direction (in degrees) for albedo data

        REAL (KIND=8), INTENT(out) :: lon(1:nlon_alb)      !< longitude of albedo raw data in geographical system
        REAL (KIND=8), INTENT(out) :: lat(1:nlat_alb)      !< latitude of albedo raw date in geographical system
        

        !local variables
        INTEGER :: ndimension                       !< number of dimensions in netcdf file
        INTEGER :: nVars                            !< number of variables in netcdf file
        INTEGER :: nGlobalAtts                      !< number of gloabal Attributes in netcdf file
        INTEGER :: unlimdimid                       !< id of unlimited dimension (e.g. time) in netcdf file

        INTEGER :: dimid                            !< id of dimension
        CHARACTER (len=80) :: dimname               !< name of dimension
        INTEGER :: length                           !< length of dimension

        INTEGER :: varid                            !< id of variable
        CHARACTER (len=80) :: varname               !< name of variable
        INTEGER :: xtype                            !< netcdf type of variable/attribute
        INTEGER :: ndim                             !< number of dimensions of variable
        INTEGER, ALLOCATABLE :: var_dimids(:)       !< id of variable dimensions, vector, maximal dimension ndimension
        INTEGER :: nAtts                            !< number of attributes for a netcdf variable
        
        INTEGER :: errorcode                        !< error status variable
        
        INTEGER :: attnum                           !< counter for attribute number
        CHARACTER (len=80) :: attname               !< name of attribute
        CHARACTER (len=80) :: attributetext         !< attribute text
        

        



   !! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible
!  unlimited dimension (probably time)
                !; nf90_inquire input: ncid; nf90_inquire output: ndimension, nVars, nGlobalAtts,unlimdimid
                CALL check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))
   !print *,'ncid,ndimension, nVars, nGlobalAtts,unlimdimid',ncid,ndimension, nVars, nGlobalAtts,unlimdimid
                        ALLOCATE (var_dimids(ndimension), STAT=errorcode)
                        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array var_dimids')
                        var_dimids = 0

      !; the varid in netcdf files is counted from 1 to nVars
      !; look for variable names, number of dimension, var_dimids etc with nf90_inquire_variable
      !; nf90_inquire_variable input: ncid, varid; nf90_inquire_variable output name xtype, ndim, var_dimids, nAtts
                !print *,'nVars ',nVars
                variables: DO varid=1,nVars
                    !print *,'variable loop, varid ',varid
                    CALL check_netcdf(nf90_inquire_variable(ncid,varid,varname,xtype, ndim, var_dimids, nAtts))

                    IF (nAtts.gt.0) THEN
                    ! get Attribute name
                        DO attnum=1,nAtts
                        ! input nf90_inq_attname: ncid, varid, attnum, output nf90_inq_attname name
                            CALL check_netcdf ( nf90_inq_attname(ncid, varid, attnum, attname ))
                            ! print *,'ncid,varid,attnumm, attname',ncid,varid,attnum,trim(attname)
   ! input nf90_inquire_attribute: ncid, varid, name; nf90_inquire_attribute output: xtype, length, attnum
                            CALL check_netcdf(nf90_inquire_attribute(ncid, varid, attname, xtype, length, attnum))
!print *, 'ncid, varid, attname, xtype, length, attnum',ncid, varid, trim(attname),' ', xtype, length, attnum
                            ! nf90_get_att input: ncid, varid, name; nf90_get_att ou put: attributetext
                            ! note, attributetext should be the right type of variable
                            getattribute: SELECT CASE (xtype)
                                 CASE (NF90_CHAR) ! for character attributes
                                 CALL check_netcdf(nf90_get_att(ncid, varid, attname, attributetext))
! print *,'get attribute: ncid, varid, attname, attributetext',ncid, varid, trim(attname),' ', trim(attributetext)
                                 END SELECT getattribute
                        END DO ! done with attributes
                    ENDIF

                IF (trim(varname) == 'lon') THEN                           
! here I know that the variable with longitude coordinates is called 'lon'

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(1), dimname, len=length) )
                     if (length /= nlon_alb)  CALL abort_extpar('nlon_alb is not equal data dimension')
                    ! read from netcdf file into lon(:)
                    CALL check_netcdf(nf90_get_var(ncid,varid,lon,start=(/1/),count=(/length/))) 
                ENDIF

                IF (trim(varname) == 'lat') THEN                           
! here I know that the variable with latitude coordinates is called 'lat'

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(1), dimname, len=length) )
                    if (length /= nlat_alb)  CALL abort_extpar('nlat_alb is not equal data dimension')
                    !print *,'dimt ',dimt
                    CALL check_netcdf(nf90_get_var(ncid,varid,lat,start=(/1/),count=(/length/))) 
! read from netcdf file into lat(:)
                ENDIF

            ENDDO variables

             startlon_alb = lon(1) ! longitude of the upper left grid element
             startlat_alb = lat(1) ! latitude of the upper left grid element

             dlon_alb = 360./float(nlon_alb) ! dlon_alb in degrees
             dlat_alb = 180./float(nlat_alb) ! dlat_alb in degrees




       END SUBROUTINE get_ALB_data_coordinates

       
   !> get one row of albedo raw data from netcdf file (along zonal direction)
       SUBROUTINE get_one_row_ALB_data(ncid,                &
                                        nlon_alb,           & 
                                        nlat_alb,           &
                                        ntime_alb,          &
                                        row_index,           &
                                        time_index,          &
                                        alb_raw_data_lonrow, &
                                        alb_source)

        INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number
        INTEGER (KIND=i4), INTENT(in) :: nlon_alb    !< number of grid elements in zonal direction for albedo data
        INTEGER (KIND=i4), INTENT(in) :: nlat_alb    !< number of grid elements in meridional direction for albedo data
        INTEGER (KIND=i4), INTENT(in) :: ntime_alb   !< number of dates with albedo data

        INTEGER (KIND=i4), INTENT(in) :: row_index    !< the index of the data row to read in
        INTEGER (KIND=i4), INTENT(in) :: time_index   !< the index of the time (month) to read in

        REAL (KIND=wp), INTENT(out) :: alb_raw_data_lonrow(1:nlon_alb)  
!< longitude of albedo raw data in geographical system
        CHARACTER (len=filename_max) :: alb_source


        !local variables
        INTEGER :: ndimension                       !< number of dimensions in netcdf file
        INTEGER :: nVars                            !< number of variables in netcdf file
        INTEGER :: nGlobalAtts                      !< number of gloabal Attributes in netcdf file
        INTEGER :: unlimdimid                       !< id of unlimited dimension (e.g. time) in netcdf file

        INTEGER :: dimid                            !< id of dimension
        CHARACTER (len=80) :: dimname               !< name of dimension
        INTEGER :: length                           !< length of dimension
        INTEGER :: dim_lon                           !< length of dimension lon
        INTEGER :: dim_lat                           !< length of dimension lat
        INTEGER :: dim_time                           !< length of dimension time

        INTEGER :: varid                            !< id of variable
        CHARACTER (len=80) :: varname               !< name of variable
        INTEGER :: xtype                            !< netcdf type of variable/attribute
        INTEGER :: ndim                             !< number of dimensions of variable
        INTEGER, ALLOCATABLE :: var_dimids(:)       !< id of variable dimensions, vector, maximal dimension ndimension
        INTEGER :: nAtts                            !< number of attributes for a netcdf variable
        
        INTEGER :: errorcode                        !< error status variable
        
        INTEGER :: attnum                           !< counter for attribute number
        CHARACTER (len=80) :: attname               !< name of attribute
        CHARACTER (len=80) :: attributetext         !< attribute text
        

        



!! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible
!  unlimited dimension (probably time)
                !; nf90_inquire input: ncid; nf90_inquire output: ndimension, nVars, nGlobalAtts,unlimdimid
                CALL check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))
                !print *,'ncid,ndimension, nVars, nGlobalAtts,unlimdimid',ncid,ndimension, nVars, nGlobalAtts,unlimdimid
                        ALLOCATE (var_dimids(ndimension), STAT=errorcode)
                        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array var_dimids')
                        var_dimids = 0


           !; the varid in netcdf files is counted from 1 to nVars
           !; look for variable names, number of dimension, var_dimids etc with nf90_inquire_variable
           !; nf90_inquire_variable input: ncid, varid; nf90_inquire_variable output name xtype, ndim, var_dimids, nAtts
                !print *,'nVars ',nVars
                variables: DO varid=1,nVars
                    !print *,'variable loop, varid ',varid
                    CALL check_netcdf(nf90_inquire_variable(ncid,varid,varname,xtype, ndim, var_dimids, nAtts))
                    !print *,'------------------------------------'
                    !print *,'ncid,varid,vrname',ncid,varid,trim(varname)
                    !print *, 'xtype',xtype
                    !print *, 'ndim', ndim
                    !print *, 'var_dimids', var_dimids
                    !print *, 'nAtts', nAtts
                    !print *,'------------------------------------'


                    IF (nAtts.gt.0) THEN
                    ! get Attribute name
                        DO attnum=1,nAtts
                        ! input nf90_inq_attname: ncid, varid, attnum, output nf90_inq_attname name
                            CALL check_netcdf ( nf90_inq_attname(ncid, varid, attnum, attname ))
                            ! print *,'ncid,varid,attnumm, attname',ncid,varid,attnum,trim(attname)
! input nf90_inquire_attribute: ncid, varid, name; nf90_inquire_attribute output: xtype, length, attnum
                            CALL check_netcdf(nf90_inquire_attribute(ncid, varid, attname, xtype, length, attnum))
!print *, 'ncid, varid, attname, xtype, length, attnum',ncid, varid, trim(attname),' ', xtype, length, attnum
                            ! nf90_get_att input: ncid, varid, name; nf90_get_att ou put: attributetext
                            ! note, attributetext should be the right type of variable
                            getattribute: SELECT CASE (xtype)
                                 CASE (NF90_CHAR) ! for character attributes
                                 CALL check_netcdf(nf90_get_att(ncid, varid, attname, attributetext))
! print *,'get attribute: ncid, varid, attname, attributetext',ncid, varid, trim(attname),' ', trim(attributetext)
                                 END SELECT getattribute
                        END DO ! done with attributes
                    ENDIF


                IF (trim(varname) == alb_source) THEN                          

                    !print *,'------------------------------------'
                    !print *,'ncid,varid,vrname',ncid,varid,trim(varname)
                    !print *, 'xtype',xtype
                    !print *, 'ndim', ndim
                    !print *, 'var_dimids', var_dimids
                    !print *, 'nAtts', nAtts
                    !print *,'------------------------------------'

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(1), dimname, len=dim_lon) )
                       ! print *, 'var_dimids(1), dimname, dim_lon: ', var_dimids(1), TRIM(dimname), dim_lon
                        if (dim_lon /= nlon_alb)   CALL abort_extpar('nlon_alb is not equal data dimension')
                            
                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(2), dimname, len=dim_lat) )
                       ! print *, 'var_dimids(2), dimname, dim_lat: ', var_dimids(2), TRIM(dimname), dim_lat
                        if (dim_lat /= nlat_alb)   CALL abort_extpar('nlat_alb is not equal data dimension')

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(3), dimname, len=dim_time) )
                       ! print *, 'var_dimids(3), dimname, dim_time: ', var_dimids(3), TRIM(dimname), dim_time
                        if (dim_time /= ntime_alb)  CALL abort_extpar('nlon_alb is not equal data dimension')


                    !print *,'dimt ',dimt
                    CALL check_netcdf(nf90_get_var(ncid,varid,alb_raw_data_lonrow,                          &
                    start=(/1,row_index,time_index/),count=(/nlon_alb,1,1/))) 
! read from netcdf file into alb_raw_data_lonrow(:)
                ENDIF

            ENDDO variables



       END SUBROUTINE get_one_row_ALB_data

       
   !> get a block of albedo raw data from netcdf file (given startrow, endrow, startcolumn, endcolumn)
       SUBROUTINE get_block_ALB_data(ncid,                &
                                      nlon_alb,           & 
                                      nlat_alb,           &
                                      ntime_alb,          &
                                      startcolumn_index,   &
                                      endcolumn_index,     &
                                      startrow_index,      &
                                      endrow_index,        &
                                      ncolumns,            &
                                      nrows,               &
                                      time_index,          &
                                      alb_data_block,      &
                                      alb_source)

        INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number
        INTEGER (KIND=i4), INTENT(in) :: nlon_alb       !< number of grid elements in zonal direction for albedo data
        INTEGER (KIND=i4), INTENT(in) :: nlat_alb       !< number of grid elements in meridional direction for albedo data
        INTEGER (KIND=i4), INTENT(in) :: ntime_alb      !< number of dates with albedo data
        INTEGER (KIND=i4), INTENT(in) :: startcolumn_index    !< the index of the startcolumn of data to read in
        INTEGER (KIND=i4), INTENT(in) :: endcolumn_index      !< the index of the endcolumn of data to read in
        INTEGER (KIND=i4), INTENT(in) :: startrow_index       !< the index of the startrow of data to read in
        INTEGER (KIND=i4), INTENT(in) :: endrow_index         !< the index of the endrow of data to read in
        INTEGER (KIND=i4), INTENT(in) ::   ncolumns           !< number of columns of data block
        INTEGER (KIND=i4), INTENT(in) ::   nrows              !< number of rows of data block
        
        INTEGER (KIND=i4), INTENT(in) :: time_index      !< the index of the time (month) to read in

        REAL (KIND=wp), INTENT(out) :: alb_data_block(1:ncolumns,1:nrows)      
!< longitude of albedo raw data in geographical system

        CHARACTER (len=filename_max) :: alb_source       !< name of albedo variable inside input file

        !local variables
        INTEGER :: ndimension                       !< number of dimensions in netcdf file
        INTEGER :: nVars                            !< number of variables in netcdf file
        INTEGER :: nGlobalAtts                      !< number of gloabal Attributes in netcdf file
        INTEGER :: unlimdimid                       !< id of unlimited dimension (e.g. time) in netcdf file

        INTEGER :: dimid                            !< id of dimension
        CHARACTER (len=80) :: dimname               !< name of dimension
        INTEGER :: length                           !< length of dimension
        INTEGER :: dim_lon                           !< length of dimension lon
        INTEGER :: dim_lat                           !< length of dimension lat
        INTEGER :: dim_time                           !< length of dimension time

        INTEGER :: varid                            !< id of variable
        CHARACTER (len=80) :: varname               !< name of variable
        INTEGER :: xtype                            !< netcdf type of variable/attribute
        INTEGER :: ndim                             !< number of dimensions of variable
        INTEGER, ALLOCATABLE :: var_dimids(:)       !< id of variable dimensions, vector, maximal dimension ndimension
        INTEGER :: nAtts                            !< number of attributes for a netcdf variable
        
        INTEGER :: errorcode                        !< error status variable
        
        INTEGER :: attnum                           !< counter for attribute number
        CHARACTER (len=80) :: attname               !< name of attribute
        CHARACTER (len=80) :: attributetext         !< attribute text

        
                IF ( (startcolumn_index < 1) .or. (startcolumn_index > nlon_alb)) then
                    CALL abort_extpar('startcolumn_index out of range')
                ENDIF
                 IF ( (endcolumn_index < 1) .or. (endcolumn_index > nlon_alb)) then
                    CALL abort_extpar('endcolumn_index out of range')
                ENDIF
                 IF ( (startrow_index < 1) .or. (startrow_index > nlat_alb)) then
                    CALL abort_extpar('startrow_index out of range')
                ENDIF
        
                IF ( (endrow_index < 1) .or. (endrow_index > nlat_alb)) then
                    CALL abort_extpar('endrow_index out of range')
                ENDIF
        
                !ncolumns = endcolumn_index - startcolumn_index + 1
                !nrows    = endrow_index - startrow_index + 1

        
!! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible
! unlimited dimension (probably time)
                !; nf90_inquire input: ncid; nf90_inquire output: ndimension, nVars, nGlobalAtts,unlimdimid
                CALL check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))
                !print *,'ncid,ndimension, nVars, nGlobalAtts,unlimdimid',ncid,ndimension, nVars, nGlobalAtts,unlimdimid
                        ALLOCATE (var_dimids(ndimension), STAT=errorcode)
                        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array var_dimids')
                        var_dimids = 0


           !; the varid in netcdf files is counted from 1 to nVars
           !; look for variable names, number of dimension, var_dimids etc with nf90_inquire_variable
           !; nf90_inquire_variable input: ncid, varid; nf90_inquire_variable output name xtype, ndim, var_dimids, nAtts
                !print *,'nVars ',nVars
                variables: DO varid=1,nVars
                    !print *,'variable loop, varid ',varid
                    CALL check_netcdf(nf90_inquire_variable(ncid,varid,varname,xtype, ndim, var_dimids, nAtts))
                    !print *,'------------------------------------'
                    !print *,'ncid,varid,vrname',ncid,varid,trim(varname)
                    !print *, 'xtype',xtype
                    !print *, 'ndim', ndim
                    !print *, 'var_dimids', var_dimids
                    !print *, 'nAtts', nAtts
                    !print *,'------------------------------------'



                    IF (nAtts.gt.0) THEN
                    ! get Attribute name
                        DO attnum=1,nAtts
                        ! input nf90_inq_attname: ncid, varid, attnum, output nf90_inq_attname name
                            CALL check_netcdf ( nf90_inq_attname(ncid, varid, attnum, attname ))
                            ! print *,'ncid,varid,attnumm, attname',ncid,varid,attnum,trim(attname)
! input nf90_inquire_attribute: ncid, varid, name; nf90_inquire_attribute output: xtype, length, attnum
                            CALL check_netcdf(nf90_inquire_attribute(ncid, varid, attname, xtype, length, attnum))
!print *, 'ncid, varid, attname, xtype, length, attnum',ncid, varid, trim(attname),' ', xtype, length, attnum
                            ! nf90_get_att input: ncid, varid, name; nf90_get_att ou put: attributetext
                            ! note, attributetext should be the right type of variable
                            getattribute: SELECT CASE (xtype)
                                 CASE (NF90_CHAR) ! for character attributes
                                 CALL check_netcdf(nf90_get_att(ncid, varid, attname, attributetext))      
! print *,'get attribute: ncid, varid, attname, attributetext',ncid, varid, trim(attname),' ', trim(attributetext)
                                 END SELECT getattribute
                        END DO ! done with attributes
                    ENDIF


                IF (trim(varname) == alb_source) THEN               
                    !print *,'------------------------------------'
                    !print *,'ncid,varid,vrname',ncid,varid,trim(varname)
                    !print *, 'xtype',xtype
                    !print *, 'ndim', ndim
                    !print *, 'var_dimids', var_dimids
                    !print *, 'nAtts', nAtts
                    !print *,'------------------------------------'

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(1), dimname, len=dim_lon) )
                       ! print *, 'var_dimids(1), dimname, dim_lon: ', var_dimids(1), TRIM(dimname), dim_lon
                        if (dim_lon /= nlon_alb)   CALL abort_extpar('nlon_alb is not equal data dimension')
                            
                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(2), dimname, len=dim_lat) )
                       ! print *, 'var_dimids(2), dimname, dim_lat: ', var_dimids(2), TRIM(dimname), dim_lat
                        if (dim_lat /= nlat_alb)   CALL abort_extpar('nlat_alb is not equal data dimension')

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(3), dimname, len=dim_time) )
                       ! print *, 'var_dimids(3), dimname, dim_time: ', var_dimids(3), TRIM(dimname), dim_time
                        if (dim_time /= ntime_alb)  CALL abort_extpar('nlon_alb is not equal data dimension')


                    !print *,'dimt ',dimt
                    CALL check_netcdf(nf90_get_var(ncid,varid,alb_data_block,                          &
                    start=(/startcolumn_index,startrow_index,time_index/),count=(/ncolumns,nrows,1/))) 
! read from netcdf file into alb_raw_data_lonrow(:)
                ENDIF


            ENDDO variables



       END SUBROUTINE get_block_ALB_data

       !> get a pixel of albedo raw data from netcdf file (given grid index)
       SUBROUTINE get_pixel_ALB_data(ncid,                &
                                      nlon_alb,           & 
                                      nlat_alb,           &
                                      ntime_alb,          &
                                      column_index,        &
                                      row_index,           &
                                      time_index,          &
                                      alb_pixel_data,      &
                                      alb_source)

        INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number
        INTEGER (KIND=i4), INTENT(in) :: nlon_alb       !< number of grid elements in zonal direction for albedo data
        INTEGER (KIND=i4), INTENT(in) :: nlat_alb       !< number of grid elements in meridional direction for albedo data
        INTEGER (KIND=i4), INTENT(in) :: ntime_alb      !< number of dates with albedo data
        INTEGER (KIND=i4), INTENT(in) :: column_index    !< the index of the column of data to read in
        INTEGER (KIND=i4), INTENT(in) :: row_index       !< the index of the trow of data to read in
        INTEGER (KIND=i4), INTENT(in) :: time_index      !< the index of the time (month) to read in

        REAL (KIND=wp), INTENT(out) :: alb_pixel_data      !< value of albedo raw data pixel
        CHARACTER (len=filename_max) :: alb_source
        

        !local variables
        INTEGER :: ndimension                       !< number of dimensions in netcdf file
        INTEGER :: nVars                            !< number of variables in netcdf file
        INTEGER :: nGlobalAtts                      !< number of gloabal Attributes in netcdf file
        INTEGER :: unlimdimid                       !< id of unlimited dimension (e.g. time) in netcdf file

        INTEGER :: dimid                            !< id of dimension
        CHARACTER (len=80) :: dimname               !< name of dimension
        INTEGER :: length                           !< length of dimension
        INTEGER :: dim_lon                          !< length of dimension lon
        INTEGER :: dim_lat                          !< length of dimension lat
        INTEGER :: dim_time                         !< length of dimension time

        INTEGER :: varid                            !< id of variable
        CHARACTER (len=80) :: varname               !< name of variable
        INTEGER :: xtype                            !< netcdf type of variable/attribute
        INTEGER :: ndim                             !< number of dimensions of variable
        INTEGER, ALLOCATABLE :: var_dimids(:)       !< id of variable dimensions, vector, maximal dimension ndimension
        INTEGER :: nAtts                            !< number of attributes for a netcdf variable
        
        INTEGER :: errorcode                        !< error status variable
        
        INTEGER :: attnum                           !< counter for attribute number
        CHARACTER (len=80) :: attname               !< name of attribute
        CHARACTER (len=80) :: attributetext         !< attribute text

        
                IF ( (column_index < 1) .or. (column_index > nlon_alb)) then
                    CALL abort_extpar('column_index out of range')
                ENDIF
                 IF ( (row_index < 1) .or. (row_index > nlat_alb)) then
                    CALL abort_extpar('row_index out of range')
                ENDIF
        

        
           !! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible
! unlimited dimension (probably time)
                !; nf90_inquire input: ncid; nf90_inquire output: ndimension, nVars, nGlobalAtts,unlimdimid
                CALL check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))
           !print *,'ncid,ndimension, nVars, nGlobalAtts,unlimdimid',ncid,ndimension, nVars, nGlobalAtts,unlimdimid
                        ALLOCATE (var_dimids(ndimension), STAT=errorcode)
                        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array var_dimids')
                        var_dimids = 0


           !; the varid in netcdf files is counted from 1 to nVars
           !; look for variable names, number of dimension, var_dimids etc with nf90_inquire_variable
           !; nf90_inquire_variable input: ncid, varid; nf90_inquire_variable output name xtype, ndim, var_dimids, nAtts
                !print *,'nVars ',nVars
                variables: DO varid=1,nVars
                    !print *,'variable loop, varid ',varid
                    CALL check_netcdf(nf90_inquire_variable(ncid,varid,varname,xtype, ndim, var_dimids, nAtts))
                    !print *,'------------------------------------'
                    !print *,'ncid,varid,vrname',ncid,varid,trim(varname)
                    !print *, 'xtype',xtype
                    !print *, 'ndim', ndim
                    !print *, 'var_dimids', var_dimids
                    !print *, 'nAtts', nAtts
                    !print *,'------------------------------------'

                IF (trim(varname) == alb_source) THEN                        

                    !print *,'------------------------------------'
                    !print *,'ncid,varid,vrname',ncid,varid,trim(varname)
                    !print *, 'xtype',xtype
                    !print *, 'ndim', ndim
                    !print *, 'var_dimids', var_dimids
                    !print *, 'nAtts', nAtts
                    !print *,'------------------------------------'

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(1), dimname, len=dim_lon) )
                       ! print *, 'var_dimids(1), dimname, dim_lon: ', var_dimids(1), TRIM(dimname), dim_lon
                        if (dim_lon /= nlon_alb)   CALL abort_extpar('nlon_alb is not equal data dimension')
                            
                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(2), dimname, len=dim_lat) )
                       ! print *, 'var_dimids(2), dimname, dim_lat: ', var_dimids(2), TRIM(dimname), dim_lat
                        if (dim_lat /= nlat_alb)   CALL abort_extpar('nlat_alb is not equal data dimension')

                    CALL check_netcdf( nf90_inquire_dimension(ncid,var_dimids(3), dimname, len=dim_time) )
                       ! print *, 'var_dimids(3), dimname, dim_time: ', var_dimids(3), TRIM(dimname), dim_time
                        if (dim_time /= ntime_alb)  CALL abort_extpar('nlon_alb is not equal data dimension')


                    !print *,'dimt ',dimt
                    CALL check_netcdf(nf90_get_var(ncid,varid,alb_pixel_data,                      &
                    start=(/column_index,row_index,time_index/))) ! read from netcdf file into alb_pixel_data
                ENDIF


            ENDDO variables



       END SUBROUTINE get_pixel_ALB_data



 SUBROUTINE const_check_interpol_alb(alb_field_mom_d,fr_land_lu)

  USE mo_albedo_tg_fields, ONLY: alb_interpol
  USE mo_soil_tg_fields, ONLY:  soiltype_fao
  USE mo_target_grid_data, ONLY: tg
!  USE mo_lu_tg_fields, ONLY: fr_land_lu
  USE mo_target_grid_data, ONLY: lon_geo,lat_geo
  USE mo_bilinterpol, ONLY: get_4_surrounding_raw_data_indices, &
   &                       calc_weight_bilinear_interpol, &
   &                       calc_value_bilinear_interpol

  USE mo_albedo_data, ONLY: lon_alb, &
                          lat_alb, &
                          zalso
  USE  mo_icon_grid_data, ONLY: icon_grid
  USE  mo_icon_grid_data, ONLY: icon_grid_region
  USE  mo_icon_grid_data, ONLY: nvertex_per_cell

                     
  REAL(KIND=wp), INTENT(INOUT) :: alb_field_mom_d(:,:,:,:)
  REAL(KIND=wp), INTENT(IN) :: fr_land_lu(:,:,:)
  INTEGER (KIND=i4), PARAMETER :: mpy=12     !< month per year
  INTEGER (KIND=i4) :: t,k,j,i,i_miss,i2,j2
  INTEGER (KIND=i4) :: count1,count2,count3,count4,count5,count6
  INTEGER (KIND=i4) :: igrid_type 
  REAL (KIND=wp) :: lon_geo_w,lon_geo_e,lat_geo_n,lat_geo_s
  REAL (KIND=wp)   :: alb_sw,alb_nw,alb_se,alb_ne
  REAL (KIND=wp) :: bwlon,bwlat
  REAL (KIND=wp) :: point_lon_geo, point_lon       !< longitude coordinate in geographical system of input point 
  REAL (KIND=wp) :: point_lat_geo, point_lat       !< latitude coordinate in geographical system of input point    
  INTEGER (KIND=i4) :: dummy,nvert   
  INTEGER, ALLOCATABLE  :: noOfVertices(:)       ! no of cell vertices = no of edges = no of neighboring cells
  INTEGER, ALLOCATABLE  :: neighbor_index(:,:)   ! neighboring cells indeces, from 1 to noOfVertices 
  INTEGER, ALLOCATABLE :: alb_index (:)
  INTEGER (KIND=i8) :: western_column     !< the index of the western_column of raw data 
  INTEGER (KIND=i8) :: eastern_column     !< the index of the eastern_column of raw data 
  INTEGER (KIND=i8) :: northern_row       !< the index of the northern_row of raw data 
  INTEGER (KIND=i8) :: southern_row       !< the index of the southern_row of raw data
  INTEGER :: nnb !< number of neighbor grid elements with common edge
  INTEGER :: nv  !< number of vertices
  INTEGER, ALLOCATABLE :: n_index(:) !< help variable



  igrid_type = tg%igrid_type

     count1 = 0
     count2 = 0
     count3 = 0
     count4 = 0
     count5 = 0
     count6 = 0

     !preparing albedo values for interpolation, original data is copied to
     !alb_interpol to easier remove the procedure if necessary.
     !at the end alb_interpol is written in alb_field_mom

     DO t=1, mpy
      DO k=1,tg%ke
       DO j=1,tg%je
        DO i=1,tg%ie
                 alb_interpol(i,j,k,t) = alb_field_mom_d(i,j,k,t)
        ENDDO
       ENDDO
      ENDDO
     ENDDO
     print *,'alb_interpol filled with albedo values'

      DO t=1, mpy
       DO k=1,tg%ke
        DO j=1,tg%je
         DO i=1,tg%ie
  
            IF (igrid_type.eq.2) THEN !COSMO interpolation

            !albedo < 0.07 and no water point
              IF ((alb_interpol(i,j,k,t).lt.0.07).AND.(fr_land_lu(i,j,k).ge.0.5)) THEN 

                      
                !avoiding 'out of range' errors at the border
                j2 = j
                i2 = i
                IF (j.eq.(tg%je)) THEN
                  j2 = j-1
                ENDIF
                IF (j.eq.1) THEN
                  j2 = j+1
                ENDIF
                IF (i.eq.(tg%ie)) THEN
                  i2 = i-1
                ENDIF
                IF (i.eq.1) THEN
                  i2 = i+1
                ENDIF

                !coordinates and albedo values for neighbor points

                lon_geo_w = lon_geo(i2-1,j2,k)
                lon_geo_e = lon_geo(i2+1,j2,k)
                lat_geo_n = lat_geo(i2,j2+1,k)
                lat_geo_s = lat_geo(i2,j2-1,k)

                alb_sw = alb_interpol(i2-1,j2-1,k,t)
                alb_se = alb_interpol(i2+1,j2-1,k,t)
                alb_nw = alb_interpol(i2-1,j2+1,k,t)
                alb_ne = alb_interpol(i2+1,j2+1,k,t)
               

               !calculate weights for interpolation
               !these weights might give wrong results, if there are no 4 valid
               !points surrounding the interpolated point(i,j)
                CALL calc_weight_bilinear_interpol(lon_geo(i,j,k), &
                                                  lat_geo(i,j,k), &
                                                  lon_geo_w,      &
                                                  lon_geo_e,      &
                                                  lat_geo_n,     &
                                                  lat_geo_s,     &
                                                  bwlon,         &
                                                  bwlat)

               
                i_miss = 0
!               using only landpoints for albedo interpolation
!               if an interpolation point is not surrounded by 4 landpoints, an
!               additional factor (4/(4-i_miss)) is multiplied to the weightings
!               from calc_weight_bilinear_interpol
!               IF (igrid_type.eq.2) THEN !COSMO
                IF (fr_land_lu(i2-1,j2-1,k).lt.0.5) THEN
                   alb_sw = 0.
                   i_miss = i_miss + 1
                ENDIF
                IF (fr_land_lu(i2+1,j2-1,k).lt.0.5) THEN
                   alb_se = 0.
                   i_miss = i_miss + 1
                ENDIF
                IF (fr_land_lu(i2-1,j2+1,k).lt.0.5) THEN
                   alb_nw = 0.
                   i_miss = i_miss + 1
                ENDIF
                IF (fr_land_lu(i2+1,j2+1,k).lt.0.5) THEN
                   alb_ne = 0.
                   i_miss = i_miss + 1
                ENDIF


                IF (i_miss.eq.4) THEN
!                  print *,'albedo point is surrounded by water only'
!                  if there are no valid interpolation values, the next step is skipped  
                  i_miss = 5
                  GOTO 100
                ENDIF
               
                alb_interpol(i,j,k,t) = calc_value_bilinear_interpol(bwlon, bwlat, &
   &                    alb_sw, alb_se, alb_ne, alb_nw)*(4/(4-i_miss))

                !printing albedo values that are still too small, only COSMO!!
  100           IF (alb_interpol(i,j,k,t).lt.0.07.and. soiltype_fao(i,j,k).le.9 .and. soiltype_fao(i,j,k).ne.0) THEN
                   count3 = count3 + 1
                !values that are still too small, will receive a soiltype dependent albedo
                   alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)*fr_land_lu(i,j,k) + &
                                          0.07*(1.-fr_land_lu(i,j,k))
                 ELSEIF (alb_interpol(i,j,k,t).lt.0.07 .and. (soiltype_fao(i,j,k).gt.9 &
                      & .or. soiltype_fao(i,j,k).eq.0)) THEN
                   count3 = count3 + 1
                   alb_interpol(i,j,k,t) = 0.07
                 ENDIF
!override wrong values due to bad interpolation (occurs only at some borderpoints)
                IF (alb_interpol(i,j,k,t).gt.0.7) THEN
!                   print *,'exception at: ',i,j,k,t,alb_interpol(i,j,k,t),fr_land_lu(i,j,k)
                   alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)*fr_land_lu(i,j,k) + &
                                          0.07*(1.-fr_land_lu(i,j,k))
                ENDIF

                 count1 = count1 + 1
              !water points:
              ELSEIF ((alb_interpol(i,j,k,t).lt.0.07).AND.(fr_land_lu(i,j,k).lt.0.5)) THEN
!                  count2 = count2 + 1
                alb_interpol(i,j,k,t) = 0.07
              ELSEIF ((alb_interpol(i,j,k,t).ge.0.07).AND.(fr_land_lu(i,j,k).lt.0.5)) THEN
                count2 = count2 + 1
                alb_interpol(i,j,k,t) = 0.07
              ENDIF

            ELSEIF (igrid_type.eq.1) THEN !ICON interpolation 

             IF (fr_land_lu(i,j,k).lt.0.5) THEN
               !water point
               alb_interpol(i,j,k,t) = 0.07
             ELSEIF ((alb_interpol(i,j,k,t).le.0).AND.(fr_land_lu(i,j,k).ge.0.5)) THEN
               !land point with albedo = 0
 
              count1 = count1+1

              alb_se = alb_interpol(icon_grid_region%cells%neighbor_index(i,1),j,k,t)
              alb_nw = alb_interpol(icon_grid_region%cells%neighbor_index(i,2),j,k,t)
              alb_ne = alb_interpol(icon_grid_region%cells%neighbor_index(i,3),j,k,t)

              IF (alb_se.gt.0.AND.alb_nw.gt.0.AND.alb_ne.gt.0) THEN
                count2 = count2+1
                alb_interpol(i,j,k,t) = (alb_se+alb_nw+alb_ne)/3
              ELSE IF (alb_se.gt.0.AND.alb_nw.gt.0) THEN
                count3 = count3+1
                alb_interpol(i,j,k,t) = (alb_se+alb_nw)/2
              ELSE IF (alb_nw.gt.0.AND.alb_ne.gt.0) THEN
                alb_interpol(i,j,k,t) = (alb_nw+alb_ne)/2
                count4 = count4+1
              ELSE IF (alb_nw.gt.0.AND.alb_se.gt.0) THEN
                alb_interpol(i,j,k,t) = (alb_nw+alb_se)/2
              ELSE
                count5 = count5+1
                IF (soiltype_fao(i,j,k).eq.1) THEN
                  count6 = count6+1
                ENDIF
                alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)*fr_land_lu(i,j,k) + &
                                          0.07*(1.-fr_land_lu(i,j,k))
              ENDIF

             ENDIF     
            ENDIF  !ICON interpolation

            !Gletscher
  101       IF ((soiltype_fao(i,j,k).eq.1).AND.(fr_land_lu(i,j,k).ge.0.5)) THEN
               alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)
            ENDIF
 
           ENDDO !i
          ENDDO !j
         ENDDO !k
!roa this level of verbose is not ok!
!!$         IF (igrid_type.eq.2) THEN !COSMO counting
!!$           print *,'t: ',t,'alb < 0.07, fr_land >= 0.5 ',count1,'after interp.: ', &
!!$       &           count3,' alb > 0.07, fr_land < 0.5: ',count2
!!$         ELSE IF (igrid_type.eq.1) THEN !ICON counting
!!$           print *,'t: ',t,'missing values over land: ',count1,', without valid neighbours: ',count5-count6
!!$         ENDIF
!!$         print *,'         albedo max value: ',maxval(alb_interpol(:,:,:,t)),'  albedo min value: ' &
!!$              ,minval(alb_interpol(:,:,:,t))
         count1 = 0
         count2 = 0
         count3 = 0
         count4 = 0
         count5 = 0
         count6 = 0
      ENDDO !t           


     DO t=1, mpy
      DO k=1,tg%ke
       DO j=1,tg%je
        DO i=1,tg%ie
!                IF (alb_interpol(i,j,k,t).lt.0.07) THEN
!                   print *,'albedo too small',i,j,k,t
!                ENDIF
                alb_field_mom_d(i,j,k,t) = 100*alb_interpol(i,j,k,t)
        ENDDO
       ENDDO
      ENDDO
     ENDDO
          
   
 END SUBROUTINE const_check_interpol_alb


END MODULE mo_albedo_routines
