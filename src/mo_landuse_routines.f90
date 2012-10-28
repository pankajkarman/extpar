!+ Fortran module with routines for landuse data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_3         2011/04/19 Hermann Asensio
! introduce Globcover 2009 land use data set for external parameters
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with routines for landuse data 
!> \author Hermann Asensio
!>
MODULE mo_landuse_routines

!> kind parameters are defined in MODULE data_parameters
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


USE mo_grid_structures,        ONLY: reg_lonlat_grid
USE mo_base_geometry,          ONLY: geographical_coordinates

IMPLICIT NONE

PRIVATE

PUBLIC :: read_namelists_extpar_land_use,  &
 &         get_dimension_glc2000_data,        &
 &         get_lonlat_glc2000_data

PUBLIC :: get_dimension_glcc_data, &
  &       get_lonlat_glcc_data

PUBLIC :: get_dimension_globcover_data, &
  &       get_lonlat_globcover_data
CONTAINS

!---------------------------------------------------------------------------
!> subroutine to read namelist for orography data settings for EXTPAR 
SUBROUTINE read_namelists_extpar_land_use(namelist_file, &
                                         i_landuse_data, &
                                         raw_data_lu_path, &
                                         raw_data_lu_filename, &
                                         ilookup_table_lu, &
                                         lu_buffer_file, &
                                         lu_output_file, &
                                         raw_data_glcc_path_opt, &
                                         raw_data_glcc_filename_opt, &
                                         ilookup_table_glcc_opt,         &
                                         glcc_buffer_file_opt, &
                                         glcc_output_file_opt)


  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number

  
  CHARACTER (len=filename_max), INTENT(IN) :: namelist_file !< filename with namelists for for EXTPAR settings
! land use
  INTEGER, INTENT(OUT) :: i_landuse_data !< integer switch to choose a land use raw data set
                                         !! 1 Globcover2009, 2 GLC2000, 3 GLCC
CHARACTER (len=filename_max), INTENT(OUT) :: raw_data_lu_path        !< path to raw data
CHARACTER (len=filename_max), INTENT(OUT) :: raw_data_lu_filename !< filename lu raw data
INTEGER, INTENT(OUT) :: ilookup_table_lu !< integer switch to choose a lookup table

CHARACTER (len=filename_max), INTENT(OUT) :: lu_buffer_file !< name for landuse buffer file
CHARACTER (len=filename_max), INTENT(OUT) :: lu_output_file !< name for landuse output file

!--
CHARACTER (len=filename_max), INTENT(OUT), OPTIONAL :: raw_data_glcc_path_opt        !< path to raw data
CHARACTER (len=filename_max), INTENT(OUT), OPTIONAL :: raw_data_glcc_filename_opt !< filename glc2000 raw data
INTEGER, INTENT(OUT), OPTIONAL :: ilookup_table_glcc_opt  !< integer switch to choose a lookup table

CHARACTER (len=filename_max), INTENT(OUT), OPTIONAL :: glcc_buffer_file_opt    !< name for glcc buffer file
CHARACTER (len=filename_max), INTENT(OUT), OPTIONAL :: glcc_output_file_opt    !< name for glcc output file
!--
CHARACTER (len=filename_max) :: raw_data_glcc_path        !< path to raw data
CHARACTER (len=filename_max) :: raw_data_glcc_filename !< filename glc2000 raw data
INTEGER :: ilookup_table_glcc  !< integer switch to choose a lookup table


CHARACTER (len=filename_max) :: glcc_buffer_file    !< name for glcc buffer file
CHARACTER (len=filename_max) :: glcc_output_file    !< name for glcc output file

!> namelist with land use data input
NAMELIST /lu_raw_data/ raw_data_lu_path, raw_data_lu_filename, i_landuse_data, ilookup_table_lu
!> namelist with filenames for land use data output
NAMELIST /lu_io_extpar/ lu_buffer_file, lu_output_file

!> namelist with land use data input, glcc
NAMELIST /glcc_raw_data/ raw_data_glcc_path, raw_data_glcc_filename, ilookup_table_glcc

!> namelist with filenames for land use data output. glcc data
NAMELIST /glcc_io_extpar/  glcc_buffer_file, glcc_output_file


   INTEGER           :: nuin !< unit number
   INTEGER (KIND=i4) :: ierr !< error flag


   nuin = free_un()  ! functioin free_un returns free Fortran unit number
   OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)

   READ(nuin, NML=lu_raw_data, IOSTAT=ierr)
   READ(nuin, NML=lu_io_extpar, IOSTAT=ierr)
   
   ! If optional argument is present for output, copy the value from the local variable to the output argument variable
   IF (PRESENT(raw_data_glcc_path_opt)) THEN
     READ(nuin, NML=glcc_raw_data, IOSTAT=ierr)
     READ(nuin, NML=glcc_io_extpar, IOSTAT=ierr)
   ENDIF

   CLOSE(nuin)

     ! If optional argument is present for output, copy the value from the local variable to the output argument variable
  IF (PRESENT(raw_data_glcc_path_opt)) raw_data_glcc_path_opt = TRIM(raw_data_glcc_path)


  ! If optional argument is present for output, copy the value from the local variable to the output argument variable
  IF (PRESENT(raw_data_glcc_filename_opt)) raw_data_glcc_filename_opt = TRIM(raw_data_glcc_filename)

  ! If optional argument is present for output, copy the value from the local variable to the output argument variable
  IF (PRESENT(ilookup_table_glcc_opt)) ilookup_table_glcc = ilookup_table_glcc_opt 


    ! If optional argument is present for output, copy the value from the local variable to the output argument variable
  IF (PRESENT(glcc_buffer_file_opt)) glcc_buffer_file_opt = TRIM(glcc_buffer_file)

   ! If optional argument is present for output, copy the value from the local variable to the output argument variable
  IF (PRESENT(glcc_output_file_opt)) glcc_output_file_opt = TRIM(glcc_output_file)






END SUBROUTINE read_namelists_extpar_land_use
!---------------------------------------------------------------------------
        !> inquire dimension information for glc2000 raw data 
        SUBROUTINE get_dimension_glc2000_data(path_glc2000_file, &
                                          nlon_glc2000, &
                                          nlat_glc2000)


        CHARACTER (len=*), INTENT(in) :: path_glc2000_file         !< filename with path for glc2000 raw data
        INTEGER (KIND=i8), INTENT(out) :: nlon_glc2000 !< number of grid elements in zonal direction for glc2000 data
        INTEGER (KIND=i8), INTENT(out) :: nlat_glc2000 !< number of grid elements in meridional direction for glc2000 data

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
        CALL check_netcdf( nf90_open(TRIM(path_glc2000_file),NF90_NOWRITE, ncid))


       ! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible unlimited dimension (probably time)
       !; nf90_inquire input: ncid; nf90_inquire output: ndimension, nVars, nGlobalAtts,unlimdimid
       CALL check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))
       !print *,'ncid,ndimension, nVars, nGlobalAtts,unlimdimid',ncid,ndimension, nVars, nGlobalAtts,unlimdimid


       !; the dimid in netcdf-files is counted from 1 to ndimension
       !; look for the name and length of the dimension with f90_inquire_dimension
       !; nf90_inquire_dimension input: ncid, dimid; nf90_inquire_dimension output: name, length
       DO dimid=1,ndimension
                    !print *,'dimension loop dimid ',dimid
         CALL check_netcdf( nf90_inquire_dimension(ncid,dimid, dimname, length) )
                     !print*, 'ncid,dimid, dimname, length',ncid,dimid, trim(dimname), length
         IF ( trim(dimname) == 'lon') nlon_glc2000=length          ! here I know that the name of zonal dimension is 'lon'
         IF ( trim(dimname) == 'lat') nlat_glc2000=length          ! here I know that the name of meridional dimension is 'lat'
       ENDDO


       ! close netcdf file 
       CALL check_netcdf( nf90_close( ncid))

       END SUBROUTINE get_dimension_glc2000_data

!----------------------------------------------------------------------------------------------------------------  
        !> get coordinates for glc2000 raw data 
        SUBROUTINE get_lonlat_glc2000_data(path_glc2000_file, &
                                          nlon_glc2000, &
                                          nlat_glc2000, &
                                          lon_glc2000,  &
                                          lat_glc2000,  &
                                          glc2000_grid)

       USE mo_grid_structures, ONLY: reg_lonlat_grid

        CHARACTER (len=*), INTENT(in) :: path_glc2000_file         !< filename with path for glc2000 raw data
        INTEGER (KIND=i8), INTENT(in) :: nlon_glc2000 !< number of grid elements in zonal direction for glc2000 data
        INTEGER (KIND=i8), INTENT(in) :: nlat_glc2000 !< number of grid elements in meridional direction for glc2000 data
        REAL (KIND=wp), INTENT(out)    :: lon_glc2000(1:nlon_glc2000) !< longitude of glc2000 raw data
        REAL (KIND=wp), INTENT(out)    :: lat_glc2000(1:nlat_glc2000) !< latitude of glc2000 raw data
        TYPE(reg_lonlat_grid), INTENT(OUT) :: glc2000_grid !< structure with defenition of the raw data grid for the whole GLOBE dataset
        
        !local variables
        INTEGER :: ncid                             !< netcdf unit file number

        CHARACTER (LEN=80) :: varname  !< name of variable
        INTEGER :: varid               !< id of variable

        ! open netcdf file 
        CALL check_netcdf( nf90_open(TRIM(path_glc2000_file),NF90_NOWRITE, ncid))

        varname = 'lon' ! I know that the longitude coordinates for the GLC2000 data are stored in a variable called 'lon'

        CALL check_netcdf( nf90_inq_varid(ncid, TRIM(varname), varid))

        CALL check_netcdf(nf90_get_var(ncid, varid,  lon_glc2000))

        varname = 'lat' ! I know that the longitude coordinates for the GLC2000 data are stored in a variable called 'lon'

        CALL check_netcdf( nf90_inq_varid(ncid, TRIM(varname), varid))

        CALL check_netcdf(nf90_get_var(ncid, varid,  lat_glc2000))

        ! close netcdf file 
        CALL check_netcdf( nf90_close( ncid))

        ! define the values for the structure glc2000_grid
 
        glc2000_grid%start_lon_reg = lon_glc2000(1)
        glc2000_grid%end_lon_reg   = lon_glc2000(nlon_glc2000)
        glc2000_grid%start_lat_reg = lat_glc2000(1)
        glc2000_grid%end_lat_reg   = lat_glc2000(nlat_glc2000)
        glc2000_grid%dlon_reg      = (lon_glc2000(nlon_glc2000) - lon_glc2000(1)) / (nlon_glc2000 - 1)
        glc2000_grid%dlat_reg      = (lat_glc2000(nlat_glc2000) - lat_glc2000(1)) / (nlat_glc2000 - 1)
        glc2000_grid%nlon_reg      = nlon_glc2000
        glc2000_grid%nlat_reg      = nlat_glc2000

       END SUBROUTINE get_lonlat_glc2000_data

        !----------------------------------------------------------------------------------------------------------------
        !----------------------------------------------------------------------------------------------------------------  
        !> get one row of glc2000 raw data 
        SUBROUTINE get_row_glc2000_data(path_glc2000_file, &
                                          nlon_glc2000, &
                                          data_row, &
                                          glc2000_data_row)

       USE mo_grid_structures, ONLY: reg_lonlat_grid

        CHARACTER (len=*), INTENT(in) :: path_glc2000_file         !< filename with path for glc2000 raw data
        INTEGER , INTENT(in) :: nlon_glc2000 !< number of grid elements in zonal direction for glc2000 data
        INTEGER , INTENT(in) :: data_row !< number or row for data to read in

        INTEGER , INTENT(OUT) :: glc2000_data_row(1:nlon_glc2000)


        !local variables
        INTEGER :: ncid                             !< netcdf unit file number

        CHARACTER (LEN=80) :: varname  !< name of variable
        INTEGER :: varid               !< id of variable

       ! open netcdf file 
        CALL check_netcdf( nf90_open(TRIM(path_glc2000_file),NF90_NOWRITE, ncid))

        varname = 'glc2000byte' ! I know that the longitude coordinates for the GLC2000 data are stored in a variable called 'glc2000byte'

         CALL check_netcdf( nf90_inq_varid(ncid, TRIM(varname), varid))

         CALL check_netcdf(nf90_get_var(ncid, varid,  glc2000_data_row,  &
                       start=(/1,data_row/),count=(/nlon_glc2000,1/)))

       ! close netcdf file 
       CALL check_netcdf( nf90_close( ncid))

       END SUBROUTINE get_row_glc2000_data

      !----------------------------------------------------------------------------------------------------------------
      !----------------------------------------------------------------------------------------------------------------
      !> open netcdf-file and get netcdf unit file number
      SUBROUTINE open_netcdf_glc2000(path_glc2000_file, &
                                        ncid)
        CHARACTER (len=*), INTENT(in) :: path_glc2000_file         !< filename with path to GLOBE tile
        INTEGER, INTENT(out) :: ncid                             !< netcdf unit file number

        ! open netcdf file 
         CALL check_netcdf( nf90_open(TRIM(path_glc2000_file),NF90_NOWRITE, ncid))

      END SUBROUTINE open_netcdf_glc2000

      !> close netcdf-file 
      SUBROUTINE close_netcdf_glc2000(ncid)
         INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number

        ! close netcdf file 
        CALL check_netcdf( nf90_close( ncid))

       END SUBROUTINE close_netcdf_glc2000

       !----------------------------------------------------------------------------------------------------------------
       !----------------------------------------------------------------------------------------------------------------

       !----------------------------------------------------------------------------------------------------------------
       !----------------------------------------------------------------------------------------------------------------


       !> inquire dimension information for glcc raw data 
       SUBROUTINE get_dimension_glcc_data(path_glcc_file, &
                                          nlon_glcc, &
                                          nlat_glcc)


        CHARACTER (len=*), INTENT(in) :: path_glcc_file         !< filename with path for glcc raw data
        INTEGER (KIND=i8), INTENT(out) :: nlon_glcc !< number of grid elements in zonal direction for glcc data
        INTEGER (KIND=i8), INTENT(out) :: nlat_glcc !< number of grid elements in meridional direction for glcc data

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
        CALL check_netcdf( nf90_open(TRIM(path_glcc_file),NF90_NOWRITE, ncid))


         ! look for numbers of dimensions, Variable, Attributes, and the dimid for the one possible unlimited dimension (probably time)
         !; nf90_inquire input: ncid; nf90_inquire output: ndimension, nVars, nGlobalAtts,unlimdimid
         CALL check_netcdf (nf90_inquire(ncid,ndimension, nVars, nGlobalAtts,unlimdimid))
         !print *,'ncid,ndimension, nVars, nGlobalAtts,unlimdimid',ncid,ndimension, nVars, nGlobalAtts,unlimdimid


         !; the dimid in netcdf-files is counted from 1 to ndimension
         !; look for the name and length of the dimension with f90_inquire_dimension
         !; nf90_inquire_dimension input: ncid, dimid; nf90_inquire_dimension output: name, length
         DO dimid=1,ndimension
                      !print *,'dimension loop dimid ',dimid
           CALL check_netcdf( nf90_inquire_dimension(ncid,dimid, dimname, length) )
                       !print*, 'ncid,dimid, dimname, length',ncid,dimid, trim(dimname), length
           IF ( TRIM(dimname) == 'lon') nlon_glcc=length          ! here I know that the name of zonal dimension is 'lon'
           IF ( TRIM(dimname) == 'lat') nlat_glcc=length          ! here I know that the name of meridional dimension is 'lat'
         ENDDO


         ! close netcdf file 
         CALL check_netcdf( nf90_close( ncid))

       END SUBROUTINE get_dimension_glcc_data

      !> get coordinates for glcc raw data 
      SUBROUTINE get_lonlat_glcc_data(path_glcc_file, &
                                          nlon_glcc, &
                                          nlat_glcc, &
                                          lon_glcc,  &
                                          lat_glcc,  &
                                          glcc_grid)

      USE mo_grid_structures, ONLY: reg_lonlat_grid

        CHARACTER (len=*), INTENT(in) :: path_glcc_file         !< filename with path for glcc raw data
        INTEGER (KIND=i8), INTENT(in) :: nlon_glcc !< number of grid elements in zonal direction for glcc data
        INTEGER (KIND=i8), INTENT(in) :: nlat_glcc !< number of grid elements in meridional direction for glcc data
        REAL (KIND=wp), INTENT(out)    :: lon_glcc(1:nlon_glcc) !< longitude of glcc raw data
        REAL (KIND=wp), INTENT(out)    :: lat_glcc(1:nlat_glcc) !< latitude of glcc raw data
        TYPE(reg_lonlat_grid), INTENT(OUT) :: glcc_grid !< structure with defenition of the raw data grid for the whole GLOBE dataset

        !local variables
        INTEGER :: ncid                             !< netcdf unit file number

        CHARACTER (LEN=80) :: varname  !< name of variable
        INTEGER :: varid               !< id of variable

        ! open netcdf file 
        CALL check_netcdf( nf90_open(TRIM(path_glcc_file),NF90_NOWRITE, ncid))

        varname = 'lon' ! I know that the longitude coordinates for the GLC2000 data are stored in a variable called 'lon'

        CALL check_netcdf( nf90_inq_varid(ncid, TRIM(varname), varid))

        CALL check_netcdf(nf90_get_var(ncid, varid,  lon_glcc))

        varname = 'lat' ! I know that the longitude coordinates for the GLC2000 data are stored in a variable called 'lon'

        CALL check_netcdf( nf90_inq_varid(ncid, TRIM(varname), varid))

        CALL check_netcdf(nf90_get_var(ncid, varid,  lat_glcc))

        ! close netcdf file 
        CALL check_netcdf( nf90_close( ncid))

        ! define the values for the structure glcc_grid

        glcc_grid%start_lon_reg = lon_glcc(1)
        glcc_grid%end_lon_reg   = lon_glcc(nlon_glcc)
        glcc_grid%start_lat_reg = lat_glcc(1)
        glcc_grid%end_lat_reg   = lat_glcc(nlat_glcc)
        glcc_grid%dlon_reg      = (lon_glcc(nlon_glcc) - lon_glcc(1)) / (nlon_glcc - 1)
        glcc_grid%dlat_reg      = (lat_glcc(nlat_glcc) - lat_glcc(1)) / (nlat_glcc - 1)
        glcc_grid%nlon_reg      = nlon_glcc
        glcc_grid%nlat_reg      = nlat_glcc

      END SUBROUTINE get_lonlat_glcc_data

      !----------------------------------------------------------------------------------------------------------------
      !----------------------------------------------------------------------------------------------------------------  
      !> get one row of glcc raw data 
      SUBROUTINE get_row_glcc_data(path_glcc_file, &
                                          nlon_glcc, &
                                          data_row, &
                                          glcc_data_row)

      USE mo_grid_structures, ONLY: reg_lonlat_grid

        CHARACTER (len=*), INTENT(in) :: path_glcc_file         !< filename with path for glcc raw data
        INTEGER , INTENT(in) :: nlon_glcc !< number of grid elements in zonal direction for glcc data
        INTEGER , INTENT(in) :: data_row !< number or row for data to read in

        INTEGER , INTENT(OUT) :: glcc_data_row(1:nlon_glcc)

        !local variables
        INTEGER :: ncid                             !< netcdf unit file number

        CHARACTER (LEN=80) :: varname  !< name of variable
        INTEGER :: varid               !< id of variable

        ! open netcdf file 
        CALL check_netcdf( nf90_open(TRIM(path_glcc_file),NF90_NOWRITE, ncid))

        varname = 'glccbyte' ! I know that the longitude coordinates for the GLC2000 data are stored in a variable called 'glccbyte'

        CALL check_netcdf( nf90_inq_varid(ncid, TRIM(varname), varid))

        CALL check_netcdf(nf90_get_var(ncid, varid,  glcc_data_row,  &
          &             start=(/1,data_row/),count=(/nlon_glcc,1/)))

        ! close netcdf file 
        CALL check_netcdf( nf90_close( ncid))

      END SUBROUTINE get_row_glcc_data

      !----------------------------------------------------------------------------------------------------------------
      !----------------------------------------------------------------------------------------------------------------
      !> open netcdf-file and get netcdf unit file number
      SUBROUTINE open_netcdf_glcc(path_glcc_file, &
                                        ncid)
        CHARACTER (len=*), INTENT(in) :: path_glcc_file         !< filename with path to GLOBE tile
        INTEGER, INTENT(out) :: ncid                             !< netcdf unit file number

        ! open netcdf file 
        CALL check_netcdf( nf90_open(TRIM(path_glcc_file),NF90_NOWRITE, ncid))

       END SUBROUTINE open_netcdf_glcc

       !> close netcdf-file 
       SUBROUTINE close_netcdf_glcc(ncid)
         INTEGER, INTENT(in) :: ncid                             !< netcdf unit file number
         ! close netcdf file 
         CALL check_netcdf( nf90_close( ncid))
       END SUBROUTINE close_netcdf_glcc

               !> inquire dimension information for globcover raw data
        SUBROUTINE get_dimension_globcover_data(nlon_globcover, &
                                          nlat_globcover)

        INTEGER (KIND=i8), INTENT(OUT) :: nlon_globcover !< number of grid elements in zonal direction for globcover data
        INTEGER (KIND=i8), INTENT(OUT) :: nlat_globcover !< number of grid elements in meridional direction for globcover data

        !local variables
        INTEGER, PARAMETER :: nx=129600
        INTEGER, PARAMETER :: ny=55800

        nlon_globcover = nx
        nlat_globcover = ny

       END SUBROUTINE get_dimension_globcover_data

!----------------------------------------------------------------------------------------------------------------
        !> get coordinates for globcover raw data
        SUBROUTINE get_lonlat_globcover_data(nlon_globcover, &
           &                               nlat_globcover, &
           &                               lon_globcover,  &
           &                               lat_globcover,  &
           &                               globcover_grid)

       USE mo_grid_structures, ONLY: reg_lonlat_grid

        INTEGER (KIND=i8), INTENT(IN) :: nlon_globcover !< number of grid elements in zonal direction for globcover data
        INTEGER (KIND=i8), INTENT(IN) :: nlat_globcover !< number of grid elements in meridional direction for globcover data
        REAL (KIND=wp), INTENT(OUT)    :: lon_globcover(1:nlon_globcover) !< longitude of globcover raw data
        REAL (KIND=wp), INTENT(OUT)    :: lat_globcover(1:nlat_globcover) !< latitude of globcover raw data
        TYPE(reg_lonlat_grid), INTENT(OUT) :: globcover_grid !< structure with defenition of the raw data grid for the whole GLOBE dataset

        !local variables
        REAL, PARAMETER ::  xmin_glc = -180.001388888889 ! area of glcover data: western longitude
        REAL, PARAMETER ::  xmax_glc  =  179.998611111111 ! area of glcover data: eastern longitude
        REAL, PARAMETER ::  ymax_glc  =   90.001388888888! area of glcover data: northern latitude
        REAL, PARAMETER ::  ymin_glc = -64.9986111111111! area of glcover data: southern latitude

        REAL, PARAMETER :: dx_glc =    0.0027777777  ! grid element size of glcover data pixel in zonal direction
        REAL, PARAMETER :: dy_glc =   -0.0027777777  ! grid element size of glcover data pixel in meridional directionon

        INTEGER (KIND=i8) :: jx,jy
           DO jx=1,nlon_globcover
              lon_globcover(jx)  = xmin_glc + (jx-1)*dx_glc
           ENDDO
           DO jy=1,nlat_globcover
            lat_globcover(jy) = ymax_glc + (jy-1)*dy_glc ! note negative increment!
           ENDDO

        ! define the values for the structure globcover_grid

        globcover_grid%start_lon_reg = lon_globcover(1)
        globcover_grid%end_lon_reg   = lon_globcover(nlon_globcover)
        globcover_grid%start_lat_reg = lat_globcover(1)
        globcover_grid%end_lat_reg   = lat_globcover(nlat_globcover)
        globcover_grid%dlon_reg      = dx_glc ! (lon_globcover(nlon_globcover) - lon_globcover(1)) / (nlon_globcover - 1)
        globcover_grid%dlat_reg      = dy_glc ! (lat_globcover(nlat_globcover) - lat_globcover(1)) / (nlat_globcover - 1)
        globcover_grid%nlon_reg      = nlon_globcover
        globcover_grid%nlat_reg      = nlat_globcover

       END SUBROUTINE get_lonlat_globcover_data

        !----------------------------------------------------------------------------------------------------------------
        !----------------------------------------------------------------------------------------------------------------
        !> get one row of globcover raw data
        SUBROUTINE get_row_globcover_data(path_globcover_file, &
                                          nlon_globcover, &
                                          data_row, &
                                          globcover_data_row)

       USE mo_grid_structures, ONLY: reg_lonlat_grid

        CHARACTER (LEN=filename_max), INTENT(IN) :: path_globcover_file         !< filename with path for globcover raw data
        INTEGER , INTENT(IN) :: nlon_globcover !< number of grid elements in zonal direction for globcover data
        INTEGER , INTENT(IN) :: data_row !< number or row for data to read in

        INTEGER , INTENT(OUT) :: globcover_data_row(1:nlon_globcover)

        !local variables
        INTEGER :: ncid                             !< netcdf unit file number

        CHARACTER (LEN=80) :: varname  !< name of variable
        INTEGER :: varid               !< id of variable

       ! open netcdf file
        CALL check_netcdf( nf90_open(TRIM(path_globcover_file),NF90_NOWRITE, ncid))

        varname = 'Band1' ! I know that the globcover data are stored in a variable called 'Band1'

         CALL check_netcdf( nf90_inq_varid(ncid, TRIM(varname), varid))

         CALL check_netcdf(nf90_get_var(ncid, varid,  globcover_data_row,  &
                       start=(/1,data_row/),count=(/nlon_globcover,1/)))

       ! close netcdf file
       CALL check_netcdf( nf90_close( ncid))

       END SUBROUTINE get_row_globcover_data

      !----------------------------------------------------------------------------------------------------------------


END MODULE mo_landuse_routines

