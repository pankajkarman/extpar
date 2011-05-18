!+ Fortran module for input/ouptut utilities
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
!> Fortran module for input/ouptut utilities
!! some useful functions and data structures for netcdf i/o
MODULE mo_io_utilities

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  USE mo_utilities_extpar, ONLY: abort_extpar


  IMPLICIT NONE

  PUBLIC :: dim_meta_info
  PUBLIC :: var_meta_info
  
  PUBLIC :: netcdf_attributes
  PUBLIC :: netcdf_char_attributes
  PUBLIC :: netcdf_real_attributes
  PUBLIC :: netcdf_int_attributes

  PUBLIC :: netcdf_grid_mapping
  
  PUBLIC :: check_netcdf

  PUBLIC :: netcdf_def_var
  PUBLIC :: netcdf_def_var_real
  PUBLIC :: netcdf_def_var_int

  PUBLIC ::  netcdf_def_var_int_1d
  PUBLIC ::  netcdf_def_var_int_2d
  PUBLIC ::  netcdf_def_var_int_3d
  PUBLIC ::  netcdf_def_var_int_4d
  PUBLIC ::  netcdf_def_var_int_5d
  PUBLIC ::  netcdf_def_var_real_1d
  PUBLIC ::  netcdf_def_var_real_2d
  PUBLIC ::  netcdf_def_var_real_3d
  PUBLIC ::  netcdf_def_var_real_4d
  PUBLIC ::  netcdf_def_var_real_5d

  PUBLIC :: netcdf_def_grid_mapping

  PUBLIC :: netcdf_write_varlist

  PUBLIC :: netcdf_put_var
  PUBLIC :: netcdf_put_int_i4_1d
  PUBLIC :: netcdf_put_int_i4_2d
  PUBLIC :: netcdf_put_int_i4_3d
  PUBLIC :: netcdf_put_int_i4_4d
  PUBLIC :: netcdf_put_int_i4_5d
  PUBLIC :: netcdf_put_int_i8_1d
  PUBLIC :: netcdf_put_int_i8_2d
  PUBLIC :: netcdf_put_int_i8_3d
  PUBLIC :: netcdf_put_int_i8_4d
  PUBLIC :: netcdf_put_int_i8_5d
  PUBLIC :: netcdf_put_real_1d
  PUBLIC :: netcdf_put_real_2d
  PUBLIC :: netcdf_put_real_3d
  PUBLIC :: netcdf_put_real_4d
  PUBLIC :: netcdf_put_real_5d
  PUBLIC :: open_new_netcdf_file
  PUBLIC :: close_netcdf_file

  PUBLIC :: netcdf_get_var
  PUBLIC :: netcdf_get_var_real_3d, netcdf_get_var_real_4d, netcdf_get_var_real_5d
  PUBLIC :: netcdf_get_var_int_3d_i4 ,netcdf_get_var_int_3d_i8, netcdf_get_var_int_4d

  PUBLIC :: vartype_int
  PUBLIC :: vartype_real
  PUBLIC :: vartype_char

  PUBLIC :: struct_real
  PUBLIC :: struct_real_1d
  PUBLIC :: struct_real_2d
  PUBLIC :: struct_real_3d
  PUBLIC :: struct_real_4d
  PUBLIC :: struct_real_5d

  PUBLIC :: struct_int
  PUBLIC :: struct_int_1d
  PUBLIC :: struct_int_2d
  PUBLIC :: struct_int_3d
  PUBLIC :: struct_int_4d
  PUBLIC :: struct_int_5d

  PUBLIC :: get_date_const_field
  PUBLIC :: set_date_mm_extpar_field

 INTEGER, PARAMETER :: keylen_max = 80 !< maximum length for the length of keys of type character



        
  !> structure to save dimension information
  TYPE dim_meta_info
    CHARACTER (len=12) :: dimname !< name of dimension
    INTEGER            :: dimsize !< size of dimension
    INTEGER            :: dimid   !< netcdf id for dimension
  END TYPE dim_meta_info

  !> structure to save meta information for target variables (name, units, etc)
  TYPE var_meta_info               
    CHARACTER (len=20):: varname    !< name of variable
    INTEGER  :: n_dim      !< number of dimensions
    TYPE(dim_meta_info), POINTER :: diminfo(:)     !< pointer to dimensions of variable
    INTEGER  :: vartype    !< type of variable, 1 for INTEGER, 2 for REAL, 3 for CHARACTER
    !INTEGER :: varid  !< netcdf varid of variable
    CHARACTER (len=keylen_max):: standard_name !< netcdf attribute for standard name
    CHARACTER (len=keylen_max):: long_name  !< netcdf attribute for long name
    CHARACTER (len=20):: units      !< netcdf attribute for units
    CHARACTER (len=keylen_max):: grid_mapping !< netcdf attribute grid mapping
    CHARACTER (len=keylen_max):: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=keylen_max):: shortName    !< GRIB API shortName key 

  END TYPE var_meta_info

  !> Data structure for real scalar variables with meta information
  TYPE struct_real
    REAL (KIND=wp) :: var_real !< real variable
    REAL (KIND=wp):: fill_value_r !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_real

  !> Data structure for real 1D variables with meta information
  TYPE struct_real_1d
    REAL (KIND=wp) , ALLOCATABLE :: var_real_1d(:) !< 1D array variable
    REAL (KIND=wp):: fill_value_r !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_real_1d 

  !> Data structure for real 2d variables with meta information
  TYPE struct_real_2d
    REAL (KIND=wp) , ALLOCATABLE :: var_real_2d(:,:) !< 2D array variable
    REAL(KIND=wp) :: fill_value_r !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_real_2d 

  !> Data structure for real 3d variables with meta information
  TYPE struct_real_3d
    REAL (KIND=wp) , ALLOCATABLE :: var_real_3d(:,:,:) !< 3D array variable
    REAL(KIND=wp) :: fill_value_r !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_real_3d 

  !> Data structure for real 4d variables with meta information
  TYPE struct_real_4d
    REAL (KIND=wp) , ALLOCATABLE :: var_real_4d(:,:,:,:) !< 4D array variable
    REAL(KIND=wp) :: fill_value_r !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_real_4d 

  !> Data structure for real 5d variables with meta information
  TYPE struct_real_5d
    REAL (KIND=wp) , ALLOCATABLE :: var_real_5d(:,:,:,:,:) !< 5D array variable
    REAL(KIND=wp) :: fill_value_r !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_real_5d 




  !> Data structure for integer scalar variables with meta information
  TYPE struct_int
    INTEGER (KIND=i8) :: var_int !< int variable
    INTEGER :: fill_value_i !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_int

  !> Data structure for integer 1D variables with meta information
  TYPE struct_int_1d
    INTEGER (KIND=i8) , ALLOCATABLE :: var_int_1d(:) !< 1D array variable
    INTEGER :: fill_value_i !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_int_1d 

  !> Data structure for integer 2d variables with meta information
  TYPE struct_int_2d
    INTEGER (KIND=i8) , ALLOCATABLE :: var_int_2d(:,:) !< 2D array variable
    INTEGER :: fill_value_i !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_int_2d 

  !> Data structure for integer 3d variables with meta information
  TYPE struct_int_3d
    INTEGER (KIND=i8) , ALLOCATABLE :: var_int_3d(:,:,:) !< 3D array variable
    INTEGER :: fill_value_i !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_int_3d 

  !> Data structure for integer 4d variables with meta information
  TYPE struct_int_4d
    INTEGER (KIND=i8) , ALLOCATABLE :: var_int_4d(:,:,:,:) !< 4D array variable
    INTEGER :: fill_value_i !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_int_4d 

  !> Data structure for integer 5d variables with meta information
  TYPE struct_int_5d
    INTEGER (KIND=i8) , ALLOCATABLE :: var_int_5d(:,:,:,:,:) !< 5D array variable
    INTEGER :: fill_value_i !< value for undefined data
    TYPE (var_meta_info) :: meta   !< addtional information for buffer field
  END TYPE struct_int_5d 




 
  INTEGER, PARAMETER :: vartype_int = 1
  INTEGER, PARAMETER :: vartype_real = 2
  INTEGER, PARAMETER :: vartype_char = 3

  

  !> structure to save global netcdf attributes
  TYPE netcdf_attributes
     CHARACTER (len=80)  :: attname
     CHARACTER (len=255) :: attributetext
  END TYPE netcdf_attributes

    !> structure to store character type netcdf attributes
  TYPE netcdf_char_attributes
     CHARACTER (len=80)  :: attname
     character (len=255) :: attributetext
  END TYPE netcdf_char_attributes

  !> structure to store real type netcdf attributes
  TYPE netcdf_real_attributes
     CHARACTER (len=80)  :: attname
     REAL :: att_value_r
  END TYPE netcdf_real_attributes

  !> structure to store integer type netcdf attributes
  TYPE netcdf_int_attributes
     CHARACTER (len=80)  :: attname
     INTEGER :: att_value_int
  END TYPE netcdf_int_attributes

  !> structure to store netcdf grid mapping information according to cf conventions
  TYPE netcdf_grid_mapping
     CHARACTER (len=80) :: grid_mapping_varname !< name for variable in netcdf file with grid_mapping data
     TYPE(netcdf_char_attributes) :: grid_mapping_name  !< netcdf attribute with grid mapping name according to cf, see e.g. http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/apf.html
     INTEGER :: n_r_att !< number of real attributes
     TYPE(netcdf_real_attributes), ALLOCATABLE :: map_param(:) !< store mapping parameters for the netcdf output 
  END TYPE netcdf_grid_mapping


!  INTERFACE netcdf_def_var
!     MODULE PROCEDURE netcdf_def_var_real
!     MODULE PROCEDURE netcdf_def_var_int
!  END INTERFACE netcdf_def_var

  INTERFACE netcdf_put_att
     MODULE PROCEDURE netcdf_put_att_real
     MODULE PROCEDURE netcdf_put_att_int
  END INTERFACE netcdf_put_att

  INTERFACE netcdf_put_var
     MODULE PROCEDURE netcdf_put_int_scalar
     MODULE PROCEDURE netcdf_put_int_i4_1d
     MODULE PROCEDURE netcdf_put_int_i4_2d
     MODULE PROCEDURE netcdf_put_int_i4_3d
     MODULE PROCEDURE netcdf_put_int_i4_4d
     MODULE PROCEDURE netcdf_put_int_i4_5d
     MODULE PROCEDURE netcdf_put_int_i8_1d
     MODULE PROCEDURE netcdf_put_int_i8_2d
     MODULE PROCEDURE netcdf_put_int_i8_3d
     MODULE PROCEDURE netcdf_put_int_i8_4d
     MODULE PROCEDURE netcdf_put_int_i8_5d
     MODULE PROCEDURE netcdf_put_real_scalar
     MODULE PROCEDURE netcdf_put_real_1d
     MODULE PROCEDURE netcdf_put_real_2d
     MODULE PROCEDURE netcdf_put_real_3d
     MODULE PROCEDURE netcdf_put_real_4d
     MODULE PROCEDURE netcdf_put_real_5d
  END INTERFACE netcdf_put_var


  INTERFACE netcdf_def_var
     MODULE PROCEDURE netcdf_def_var_real
     MODULE PROCEDURE netcdf_def_var_int
     MODULE PROCEDURE netcdf_def_var_int_1d
     MODULE PROCEDURE netcdf_def_var_int_2d
     MODULE PROCEDURE netcdf_def_var_int_3d
     MODULE PROCEDURE netcdf_def_var_int_4d
     MODULE PROCEDURE netcdf_def_var_int_5d
     MODULE PROCEDURE netcdf_def_var_real_1d
     MODULE PROCEDURE netcdf_def_var_real_2d
     MODULE PROCEDURE netcdf_def_var_real_3d
     MODULE PROCEDURE netcdf_def_var_real_4d
     MODULE PROCEDURE netcdf_def_var_real_5d
  END INTERFACE netcdf_def_var

  INTERFACE netcdf_get_var
     MODULE PROCEDURE netcdf_get_var_int_3d_i8
     MODULE PROCEDURE netcdf_get_var_int_3d_i4
     MODULE PROCEDURE netcdf_get_var_int_4d
     MODULE PROCEDURE netcdf_get_var_real_3d
     MODULE PROCEDURE netcdf_get_var_real_4d
     MODULE PROCEDURE netcdf_get_var_real_5d
  END INTERFACE netcdf_get_var



  CONTAINS

  !> subroutine to check error status of netcdf function error codes
  SUBROUTINE check_netcdf(status)

      USE netcdf, ONLY : nf90_noerr, nf90_strerror
      USE mo_utilities_extpar, ONLY: abort_extpar
      INTEGER, INTENT(in) :: status
     
      IF(status /= nf90_noerr) then
         CALL abort_extpar(TRIM(nf90_strerror(status)))
      END IF

  END SUBROUTINE check_netcdf

  !> specific subroutine to define netcdf real variable (inclusive attributes)
  SUBROUTINE netcdf_def_var_real(ncid, varinfo, fill_value_r, varid)

   USE netcdf, ONLY: nf90_def_var, nf90_put_att
   USE netcdf, ONLY: NF90_FLOAT

   INTEGER, INTENT(IN)             :: ncid        !< id for netcdf file
   TYPE(var_meta_info), INTENT(IN) :: varinfo     !< structure with information on the meta data
   REAL, INTENT(IN)                :: fill_value_r  !< undefined value

   INTEGER, INTENT(OUT)            :: varid       !< variable id for netcdf variable

      call check_netcdf( nf90_def_var(ncid, &
                                      TRIM(varinfo%varname), &
                                      NF90_FLOAT, &
                                      varinfo%diminfo(:)%dimid ,& ! the shape of the variables is defined here by the number of dimensions
                                      varid))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('standard_name'), &
                                      TRIM(varinfo%standard_name)))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('long_name'), &
                                      TRIM(varinfo%long_name)))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('units'), &
                                      TRIM(varinfo%units)))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('_FillValue'), &
                                      fill_value_r))




  END SUBROUTINE netcdf_def_var_real



!> specific subroutine to define netcdf integer variable (inclusive attributes)
  SUBROUTINE netcdf_def_var_int(ncid, varinfo, fill_value_i, varid)

   USE netcdf, ONLY: nf90_def_var, nf90_put_att
   USE netcdf, ONLY: NF90_INT

   INTEGER, INTENT(IN)             :: ncid        !< id for netcdf file
   TYPE(var_meta_info), INTENT(IN) :: varinfo     !< structure with information on the meta data
   INTEGER, INTENT(IN)             :: fill_value_i  !< undefined value

   INTEGER, INTENT(OUT)            :: varid       !< variable id for netcdf variable

      call check_netcdf( nf90_def_var(ncid, &
                                      TRIM(varinfo%varname), &
                                      NF90_INT, &
                                      varinfo%diminfo(:)%dimid ,&
                                      varid))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('standard_name'), &
                                      TRIM(varinfo%standard_name)))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('long_name'), &
                                      TRIM(varinfo%long_name)))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('units'), &
                                      TRIM(varinfo%units)))

      call check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('_FillValue'), &
                                      fill_value_i))





  END SUBROUTINE netcdf_def_var_int

  
!> specific subroutine to put some standard attributes to an integer type netcdf variable
  SUBROUTINE netcdf_put_att_int(ncid, varid, varinfo, fill_value_i)

    USE netcdf, ONLY: nf90_put_att
    INTEGER, INTENT(IN)             :: ncid        !< id for netcdf file
    INTEGER, INTENT(IN)             :: varid       !< variable id for netcdf variable
    TYPE(var_meta_info), INTENT(IN) :: varinfo     !< structure with information on the meta data
    INTEGER, INTENT(IN)             :: fill_value_i  !< undefined value

    ! local variables
    INTEGER :: fill_value

    
      CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('standard_name'), &
                         &             TRIM(varinfo%standard_name)))

      CALL check_netcdf( nf90_put_att(ncid, &
                                      varid, &
                                      TRIM('long_name'), &
                                      TRIM(varinfo%long_name)))


      IF (TRIM(varinfo%units) /= "-") THEN
      CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('units'), &
                         &             TRIM(varinfo%units)))
      ENDIF

      IF (TRIM(varinfo%grid_mapping) /= "-") THEN
        CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('grid_mapping'), &
                         &             TRIM(varinfo%grid_mapping)))


      ENDIF


     IF (TRIM(varinfo%coordinates) /= "-") THEN
        CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('coordinates'), &
                         &             TRIM(varinfo%coordinates)))


     ENDIF



       fill_value = fill_value_i
       CALL check_netcdf( nf90_put_att(ncid, &
                           &             varid, &
                           &             TRIM('_FillValue'), &
                           &             fill_value))


  END SUBROUTINE netcdf_put_att_int

  !> specific subroutine to put some standard attributes to an real type netcdf variable
  SUBROUTINE netcdf_put_att_real(ncid, varid, varinfo, fill_value_r)
     USE netcdf, ONLY: nf90_put_att
    INTEGER, INTENT(IN)             :: ncid        !< id for netcdf file
    INTEGER, INTENT(IN)             :: varid       !< variable id for netcdf variable
    TYPE(var_meta_info), INTENT(IN) :: varinfo     !< structure with information on the meta data
    REAL(KIND=wp), INTENT(IN)                :: fill_value_r  !< undefined value

    ! local variable
    REAL :: fill_value

    
      CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('standard_name'), &
                         &             TRIM(varinfo%standard_name)))

      CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('long_name'), &
                         &             TRIM(varinfo%long_name)))

      IF (TRIM(varinfo%units) /= "-") THEN
      CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('units'), &
                         &             TRIM(varinfo%units)))
      ENDIF

      IF (TRIM(varinfo%grid_mapping) /= "-") THEN
        CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('grid_mapping'), &
                         &             TRIM(varinfo%grid_mapping)))


      ENDIF


     IF (TRIM(varinfo%coordinates) /= "-") THEN
        CALL check_netcdf( nf90_put_att(ncid, &
                         &             varid, &
                         &             TRIM('coordinates'), &
                         &             TRIM(varinfo%coordinates)))


     ENDIF


     fill_value = fill_value_r !type conversion
      CALL check_netcdf( nf90_put_att(ncid, &
                        &             varid, &
                        &             TRIM('_FillValue'), &
                        &             fill_value))


  END SUBROUTINE netcdf_put_att_real


  !> specific subroutine to define 1D integer variable for netcdf
  SUBROUTINE netcdf_def_var_int_1d(ncid, n_1d_int, list_int_1d, varids_1d_int)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_INT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_1d_int !< number of 1D integer variables
  TYPE(struct_int_1d), INTENT(IN) :: list_int_1d(1:n_1d_int) !< array with list of integer 1D variables
  INTEGER, INTENT(OUT) :: varids_1d_int(1:n_1d_int) !< list of varids 1D integer

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_1d !< dimension id for 1d variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname !< name of dimension
  CHARACTER (len=20) :: varname !< name of variable

  DO n=1, n_1d_int
    ! get dimid
    dimname=TRIM(list_int_1d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_1d))
    ! get varname
    varname = TRIM(list_int_1d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_1d,           &
                    varid))
    varids_1d_int(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_int_1d(n)%meta, list_int_1d(n)%fill_value_i)

  ENDDO

  END SUBROUTINE netcdf_def_var_int_1d

  !> specific subroutine to define 2D integer variable for netcdf
  SUBROUTINE netcdf_def_var_int_2d(ncid, n_2d_int, list_int_2d, varids_2d_int)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_INT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_2d_int !< number of 2D integer variables
  TYPE(struct_int_2d), INTENT(IN) :: list_int_2d(1:n_2d_int) !< array with list of integer 2D variables
  INTEGER, INTENT(OUT) :: varids_2d_int(1:n_2d_int) !< list of varids 2D integer

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_2d(2)  !< dimension ids for 2d variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_2d_int
    ! get dimid
    dimname=TRIM(list_int_2d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_2d(1)))
    dimname=TRIM(list_int_2d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_2d(2)))


    ! get varname
    varname = TRIM(list_int_2d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_2d,           &
                    varid))
    varids_2d_int(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_int_2d(n)%meta, list_int_2d(n)%fill_value_i)

  ENDDO

  END SUBROUTINE netcdf_def_var_int_2d

   !> specific subroutine to define 3D integer variable for netcdf
  SUBROUTINE netcdf_def_var_int_3d(ncid, n_3d_int, list_int_3d, varids_3d_int)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_INT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_3d_int !< number of 3D integer variables
  TYPE(struct_int_3d), INTENT(IN) :: list_int_3d(1:n_3d_int) !< array with list of integer 3D variables
  INTEGER, INTENT(OUT) :: varids_3d_int(1:n_3d_int) !< list of varids 3D integer

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_3d(3)  !< dimension ids for 3D variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_3d_int
    ! get dimid
    dimname=TRIM(list_int_3d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_3d(1)))
    dimname=TRIM(list_int_3d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_3d(2)))
    dimname=TRIM(list_int_3d(n)%meta%diminfo(3)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_3d(3)))

    ! get varname
    varname = TRIM(list_int_3d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_3d,           &
                    varid))
    varids_3d_int(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_int_3d(n)%meta, list_int_3d(n)%fill_value_i)

  ENDDO

  END SUBROUTINE netcdf_def_var_int_3d

  
  !> specific subroutine to define 4D real variable for netcdf
  SUBROUTINE netcdf_def_var_int_4d(ncid, n_4d_int, list_int_4d, varids_4d_int)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_INT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_4d_int !< number of 4D real variables
  TYPE(struct_int_4d), INTENT(IN) :: list_int_4d(1:n_4d_int) !< array with list of real 4D variables
  INTEGER, INTENT(OUT) :: varids_4d_int(1:n_4d_int) !< list of varids 4D real

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_4d(4)  !< dimension ids for 4D variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_4d_int
    ! get dimid
    dimname=TRIM(list_int_4d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(1)))
    dimname=TRIM(list_int_4d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(2)))
    dimname=TRIM(list_int_4d(n)%meta%diminfo(3)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(3)))
    dimname=TRIM(list_int_4d(n)%meta%diminfo(4)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(4)))

    ! get varname
    varname = TRIM(list_int_4d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_4d,           &
                    varid))
    varids_4d_int(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_int_4d(n)%meta, list_int_4d(n)%fill_value_i)

  ENDDO

  END SUBROUTINE netcdf_def_var_int_4d

  !> specific subroutine to define 5D real variable for netcdf
  SUBROUTINE netcdf_def_var_int_5d(ncid, n_5d_int, list_int_5d, varids_5d_int)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_INT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_5d_int !< number of 5D real variables
  TYPE(struct_int_5d), INTENT(IN) :: list_int_5d(1:n_5d_int) !< array with list of real 5D variables
  INTEGER, INTENT(OUT) :: varids_5d_int(1:n_5d_int) !< list of varids 5D real

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_5d(5)  !< dimension ids for 5D variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_5d_int
    ! get dimid
    dimname=TRIM(list_int_5d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(1)))
    dimname=TRIM(list_int_5d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(2)))
    dimname=TRIM(list_int_5d(n)%meta%diminfo(3)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(3)))
    dimname=TRIM(list_int_5d(n)%meta%diminfo(4)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(4)))
    dimname=TRIM(list_int_5d(n)%meta%diminfo(5)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(5)))

    ! get varname
    varname = TRIM(list_int_5d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_5d,           &
                    varid))
    varids_5d_int(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_int_5d(n)%meta, list_int_5d(n)%fill_value_i)

  ENDDO

  END SUBROUTINE netcdf_def_var_int_5d




   !> specific subroutine to define 1D real variable for netcdf
  SUBROUTINE netcdf_def_var_real_1d(ncid, n_1d_real, list_real_1d, varids_1d_real)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_FLOAT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_1d_real !< number of 1D real variables
  TYPE(struct_real_1d), INTENT(IN) :: list_real_1d(1:n_1d_real) !< array with list of real 1D variables
  INTEGER, INTENT(OUT) :: varids_1d_real(1:n_1d_real) !< list of varids 1D real

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_1d !< dimension id for 1d variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname !< name of dimension
  CHARACTER (len=20) :: varname !< name of variable

  DO n=1, n_1d_real
    ! get dimid
    dimname=TRIM(list_real_1d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_1d))
    ! get varname
    varname = TRIM(list_real_1d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_1d,           &
                    varid))
    varids_1d_real(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_real_1d(n)%meta, list_real_1d(n)%fill_value_r)

  ENDDO

  END SUBROUTINE netcdf_def_var_real_1d

  !> specific subroutine to define 2D real variable for netcdf
  SUBROUTINE netcdf_def_var_real_2d(ncid, n_2d_real, list_real_2d, varids_2d_real)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_FLOAT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_2d_real !< number of 2D real variables
  TYPE(struct_real_2d), INTENT(IN) :: list_real_2d(1:n_2d_real) !< array with list of real 2D variables
  INTEGER, INTENT(OUT) :: varids_2d_real(1:n_2d_real) !< list of varids 2D real

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_2d(2)  !< dimension ids for 2d variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_2d_real
    ! get dimid
    dimname=TRIM(list_real_2d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_2d(1)))
    dimname=TRIM(list_real_2d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_2d(2)))

    ! get varname
    varname = TRIM(list_real_2d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_2d,           &
                    varid))
    varids_2d_real(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_real_2d(n)%meta, list_real_2d(n)%fill_value_r)

  ENDDO

  END SUBROUTINE netcdf_def_var_real_2d

   !> specific subroutine to define 3D real variable for netcdf
  SUBROUTINE netcdf_def_var_real_3d(ncid, n_3d_real, list_real_3d, varids_3d_real)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_FLOAT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_3d_real !< number of 3D real variables
  TYPE(struct_real_3d), INTENT(IN) :: list_real_3d(1:n_3d_real) !< array with list of real 3D variables
  INTEGER, INTENT(OUT) :: varids_3d_real(1:n_3d_real) !< list of varids 3D real

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_3d(3)  !< dimension ids for 3D variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_3d_real
    ! get dimid
    dimname=TRIM(list_real_3d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_3d(1)))
    dimname=TRIM(list_real_3d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_3d(2)))
    dimname=TRIM(list_real_3d(n)%meta%diminfo(3)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_3d(3)))

    ! get varname
    varname = TRIM(list_real_3d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_3d,           &
                    varid))
    varids_3d_real(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_real_3d(n)%meta, list_real_3d(n)%fill_value_r)

  ENDDO

  END SUBROUTINE netcdf_def_var_real_3d

  
  !> specific subroutine to define 4D real variable for netcdf
  SUBROUTINE netcdf_def_var_real_4d(ncid, n_4d_real, list_real_4d, varids_4d_real)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_FLOAT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_4d_real !< number of 4D real variables
  TYPE(struct_real_4d), INTENT(IN) :: list_real_4d(1:n_4d_real) !< array with list of real 4D variables
  INTEGER, INTENT(OUT) :: varids_4d_real(1:n_4d_real) !< list of varids 4D real

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_4d(4)  !< dimension ids for 4D variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_4d_real
    ! get dimid
    dimname=TRIM(list_real_4d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(1)))
    dimname=TRIM(list_real_4d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(2)))
    dimname=TRIM(list_real_4d(n)%meta%diminfo(3)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(3)))
    dimname=TRIM(list_real_4d(n)%meta%diminfo(4)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_4d(4)))

    ! get varname
    varname = TRIM(list_real_4d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_4d,           &
                    varid))
    varids_4d_real(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_real_4d(n)%meta, list_real_4d(n)%fill_value_r)

  ENDDO

  END SUBROUTINE netcdf_def_var_real_4d



!> specific subroutine to define 5D real variable for netcdf
  SUBROUTINE netcdf_def_var_real_5d(ncid, n_5d_real, list_real_5d, varids_5d_real)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: NF90_FLOAT

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER, INTENT(IN) :: n_5d_real !< number of 5D real variables
  TYPE(struct_real_5d), INTENT(IN) :: list_real_5d(1:n_5d_real) !< array with list of real 5D variables
  INTEGER, INTENT(OUT) :: varids_5d_real(1:n_5d_real) !< list of varids 5D real

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_5d(5)  !< dimension ids for 5D variable
  INTEGER :: varid  !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

  DO n=1, n_5d_real
    ! get dimid
    dimname=TRIM(list_real_5d(n)%meta%diminfo(1)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(1)))
    dimname=TRIM(list_real_5d(n)%meta%diminfo(2)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(2)))
    dimname=TRIM(list_real_5d(n)%meta%diminfo(3)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(3)))
    dimname=TRIM(list_real_5d(n)%meta%diminfo(4)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(4)))
    dimname=TRIM(list_real_5d(n)%meta%diminfo(5)%dimname)
    CALL check_netcdf(nf90_inq_dimid(ncid,dimname,dimid_5d(5)))

    ! get varname
    varname = TRIM(list_real_5d(n)%meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,         &
                    & dimid_5d,           &
                    & varid))
    varids_5d_real(n) = varid
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, list_real_5d(n)%meta, list_real_5d(n)%fill_value_r)

  ENDDO

  END SUBROUTINE netcdf_def_var_real_5d
  !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------

  !> specific subroutine to define scalar real variable for netcdf
  SUBROUTINE netcdf_put_real_scalar(ncid, var_real, meta, fill_value_r)

  USE netcdf, ONLY: nf90_def_var

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_FLOAT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  REAL (KIND=wp), INTENT(IN) ::  var_real
  TYPE (var_meta_info), INTENT(IN) :: meta   !< addtional information for array
  REAL(KIND=wp), INTENT(IN) :: fill_value_r

  !local variables
  INTEGER :: varid  !< netcdf varid of variable
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get varname
    varname = TRIM(meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & varid))
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, meta, fill_value_r)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_real))

  END SUBROUTINE netcdf_put_real_scalar
  !----------------------------------------------------------------------------------------

  
  !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 1d real variable for netcdf
  SUBROUTINE netcdf_put_real_1d(ncid, var_real_1d, meta_1d, fill_value_r)

  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_dim



  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_FLOAT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  REAL (KIND=wp), INTENT(IN) ::  var_real_1d(:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_1d   !< addtional information for array
  REAL(KIND=wp), INTENT(IN) :: fill_value_r

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_1d(1)  !< dimension ids for 1d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize

  REAL :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_1d%diminfo(1)%dimname), &
      &                        dimid_1d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_1d%diminfo(1)%dimname),&
        &                             meta_1d%diminfo(1)%dimsize,      &
        &                             dimid_1d(1)))
    ENDIF

    ! get varname
    varname = TRIM(meta_1d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_1d,           &
                    & varid))

    fill_value = fill_value_r
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, meta_1d, fill_value_r)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_real_1d))

  END SUBROUTINE netcdf_put_real_1d
  !----------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 2d real variable for netcdf
  SUBROUTINE netcdf_put_real_2d(ncid, var_real_2d, meta_2d, fill_value_r)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_FLOAT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  REAL (KIND=wp), INTENT(IN) ::  var_real_2d(:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_2d   !< addtional information for array
  REAL(KIND=wp), INTENT(IN) :: fill_value_r

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_2d(2)  !< dimension ids for 2d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize

  REAL :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_2d%diminfo(1)%dimname), &
      &                        dimid_2d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_2d%diminfo(1)%dimname),&
        &                             meta_2d%diminfo(1)%dimsize,      &
        &                             dimid_2d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_2d%diminfo(2)%dimname), &
      &                        dimid_2d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_2d%diminfo(2)%dimname),&
        &                             meta_2d%diminfo(2)%dimsize,      &
        &                             dimid_2d(2)))
    ENDIF

    ! get varname
    varname = TRIM(meta_2d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_2d,           &
                    varid))
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, meta_2d, fill_value_r)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_real_2d))

  END SUBROUTINE netcdf_put_real_2d
  !----------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 3d real variable for netcdf
  SUBROUTINE netcdf_put_real_3d(ncid, var_real_3d, meta_3d, fill_value_r)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_FLOAT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  REAL (KIND=wp), INTENT(IN) ::  var_real_3d(:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_3d   !< addtional information for array
  REAL(KIND=wp), INTENT(IN) :: fill_value_r

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_3d(3)  !< dimension ids for 3d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  REAL :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(1)%dimname), &
      &                        dimid_3d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(1)%dimname),&
        &                             meta_3d%diminfo(1)%dimsize,      &
        &                             dimid_3d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(2)%dimname), &
      &                        dimid_3d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(2)%dimname),&
        &                             meta_3d%diminfo(2)%dimsize,      &
        &                             dimid_3d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(3)%dimname), &
      &                        dimid_3d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(3)%dimname),&
        &                             meta_3d%diminfo(3)%dimsize,      &
        &                             dimid_3d(3)))
    ENDIF

    ! get varname
    varname = TRIM(meta_3d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_3d,           &
                    varid))
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, meta_3d, fill_value_r)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_real_3d))

  END SUBROUTINE netcdf_put_real_3d
  !----------------------------------------------------------------------------------------


  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 4d real variable for netcdf
  SUBROUTINE netcdf_put_real_4d(ncid, var_real_4d, meta_4d, fill_value_r)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_FLOAT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  REAL (KIND=wp), INTENT(IN) ::  var_real_4d(:,:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_4d   !< addtional information for array
  REAL(KIND=wp), INTENT(IN) :: fill_value_r

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_4d(4)  !< dimension ids for 4D variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  REAL :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(1)%dimname), &
      &                        dimid_4d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(1)%dimname),&
        &                             meta_4d%diminfo(1)%dimsize,      &
        &                             dimid_4d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(2)%dimname), &
      &                        dimid_4d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(2)%dimname),&
        &                             meta_4d%diminfo(2)%dimsize,      &
        &                             dimid_4d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(3)%dimname), &
      &                        dimid_4d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(3)%dimname),&
        &                             meta_4d%diminfo(3)%dimsize,      &
        &                             dimid_4d(3)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(4)%dimname), &
      &                        dimid_4d(4))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(4)%dimname),&
        &                             meta_4d%diminfo(4)%dimsize,      &
        &                             dimid_4d(4)))
    ENDIF

    ! get varname
    varname = TRIM(meta_4d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_4d,           &
                    varid))
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, meta_4d, fill_value_r)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_real_4d))

  END SUBROUTINE netcdf_put_real_4d
  !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 5d real variable for netcdf
  SUBROUTINE netcdf_put_real_5d(ncid, var_real_5d, meta_5d, fill_value_r)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_FLOAT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  REAL (KIND=wp), INTENT(IN) ::  var_real_5d(:,:,:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_5d   !< addtional information for array
  REAL(KIND=wp), INTENT(IN) :: fill_value_r

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_5d(5)  !< dimension ids for 5d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  REAL :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(1)%dimname), &
      &                        dimid_5d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(1)%dimname),&
        &                             meta_5d%diminfo(1)%dimsize,      &
        &                             dimid_5d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(2)%dimname), &
      &                        dimid_5d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(2)%dimname),&
        &                             meta_5d%diminfo(2)%dimsize,      &
        &                             dimid_5d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(3)%dimname), &
      &                        dimid_5d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(3)%dimname),&
        &                             meta_5d%diminfo(3)%dimsize,      &
        &                             dimid_5d(3)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(4)%dimname), &
      &                        dimid_5d(4))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(4)%dimname),&
        &                             meta_5d%diminfo(4)%dimsize,      &
        &                             dimid_5d(4)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(5)%dimname), &
      &                        dimid_5d(5))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(5)%dimname),&
        &                             meta_5d%diminfo(5)%dimsize,      &
        &                             dimid_5d(5)))
    ENDIF


    ! get varname
    varname = TRIM(meta_5d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_FLOAT,           &
                    & dimid_5d,           &
                    varid))
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, meta_5d, fill_value_r)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_real_5d))

  END SUBROUTINE netcdf_put_real_5d
  !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------
  !> specific subroutine to define scalar int variable for netcdf
  SUBROUTINE netcdf_put_int_scalar(ncid, var_int, meta, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i8) ::  var_int
  TYPE (var_meta_info), INTENT(IN) :: meta   !< addtional information for array
  INTEGER, INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: varid  !< netcdf varid of variable
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get varname
    varname = TRIM(meta%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    varid))
    ! put standard attributes to variable
    CALL netcdf_put_att(ncid, varid, meta, fill_value_i)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int))

  END SUBROUTINE netcdf_put_int_scalar
  !----------------------------------------------------------------------------------------
 !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 1d int variable for netcdf
  SUBROUTINE netcdf_put_int_i8_1d(ncid, var_int_1d, meta_1d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i8), INTENT(IN) ::  var_int_1d(:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_1d   !< addtional information for array
  INTEGER (KIND=i8), INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_1d(1)  !< dimension ids for 1d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize

  INTEGER :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_1d%diminfo(1)%dimname), &
      &                        dimid_1d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_1d%diminfo(1)%dimname),&
        &                             meta_1d%diminfo(1)%dimsize,      &
        &                             dimid_1d(1)))
    ENDIF

    ! get varname
    varname = TRIM(meta_1d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_1d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_1d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_1d))

  END SUBROUTINE netcdf_put_int_i8_1d
  !----------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 2d int variable for netcdf
  SUBROUTINE netcdf_put_int_i8_2d(ncid, var_int_2d, meta_2d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i8), INTENT(IN) ::  var_int_2d(:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_2d   !< addtional information for array
  INTEGER (KIND=i8), INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_2d(2)  !< dimension ids for 2d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize

  INTEGER :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_2d%diminfo(1)%dimname), &
      &                        dimid_2d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_2d%diminfo(1)%dimname),&
        &                             meta_2d%diminfo(1)%dimsize,      &
        &                             dimid_2d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_2d%diminfo(2)%dimname), &
      &                        dimid_2d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_2d%diminfo(2)%dimname),&
        &                             meta_2d%diminfo(2)%dimsize,      &
        &                             dimid_2d(2)))
    ENDIF

    ! get varname
    varname = TRIM(meta_2d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_2d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_2d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_2d))

  END SUBROUTINE netcdf_put_int_i8_2d
  !----------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 3d int variable for netcdf
  SUBROUTINE netcdf_put_int_i8_3d(ncid, var_int_3d, meta_3d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i8), INTENT(IN) ::  var_int_3d(:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_3d   !< addtional information for array
  INTEGER (KIND=i8), INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_3d(3)  !< dimension ids for 3d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  INTEGER :: fill_value


  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(1)%dimname), &
      &                        dimid_3d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(1)%dimname),&
        &                             meta_3d%diminfo(1)%dimsize,      &
        &                             dimid_3d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(2)%dimname), &
      &                        dimid_3d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(2)%dimname),&
        &                             meta_3d%diminfo(2)%dimsize,      &
        &                             dimid_3d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(3)%dimname), &
      &                        dimid_3d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(3)%dimname),&
        &                             meta_3d%diminfo(3)%dimsize,      &
        &                             dimid_3d(3)))
    ENDIF

    ! get varname
    varname = TRIM(meta_3d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_3d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_3d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_3d))

  END SUBROUTINE netcdf_put_int_i8_3d
  !----------------------------------------------------------------------------------------


  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 4d int variable for netcdf
  SUBROUTINE netcdf_put_int_i8_4d(ncid, var_int_4d, meta_4d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i8), INTENT(IN) ::  var_int_4d(:,:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_4d   !< addtional information for array
  INTEGER (KIND=i8), INTENT(IN) :: fill_value_i

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_4d(4)  !< dimension ids for 4D variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  INTEGER :: fill_value


  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(1)%dimname), &
      &                        dimid_4d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(1)%dimname),&
        &                             meta_4d%diminfo(1)%dimsize,      &
        &                             dimid_4d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(2)%dimname), &
      &                        dimid_4d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(2)%dimname),&
        &                             meta_4d%diminfo(2)%dimsize,      &
        &                             dimid_4d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(3)%dimname), &
      &                        dimid_4d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(3)%dimname),&
        &                             meta_4d%diminfo(3)%dimsize,      &
        &                             dimid_4d(3)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(4)%dimname), &
      &                        dimid_4d(4))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(4)%dimname),&
        &                             meta_4d%diminfo(4)%dimsize,      &
        &                             dimid_4d(4)))
    ENDIF

    ! get varname
    varname = TRIM(meta_4d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_4d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_4d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_4d))

  END SUBROUTINE netcdf_put_int_i8_4d
  !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 5d int variable for netcdf
  SUBROUTINE netcdf_put_int_i8_5d(ncid, var_int_5d, meta_5d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i8), INTENT(IN) ::  var_int_5d(:,:,:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_5d   !< addtional information for array
  INTEGER (KIND=i8), INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_5d(5)  !< dimension ids for 5d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  INTEGER :: fill_value


  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(1)%dimname), &
      &                        dimid_5d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(1)%dimname),&
        &                             meta_5d%diminfo(1)%dimsize,      &
        &                             dimid_5d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(2)%dimname), &
      &                        dimid_5d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(2)%dimname),&
        &                             meta_5d%diminfo(2)%dimsize,      &
        &                             dimid_5d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(3)%dimname), &
      &                        dimid_5d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(3)%dimname),&
        &                             meta_5d%diminfo(3)%dimsize,      &
        &                             dimid_5d(3)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(4)%dimname), &
      &                        dimid_5d(4))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(4)%dimname),&
        &                             meta_5d%diminfo(4)%dimsize,      &
        &                             dimid_5d(4)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(5)%dimname), &
      &                        dimid_5d(5))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(5)%dimname),&
        &                             meta_5d%diminfo(5)%dimsize,      &
        &                             dimid_5d(5)))
    ENDIF


    ! get varname
    varname = TRIM(meta_5d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_5d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_5d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_5d))

  END SUBROUTINE netcdf_put_int_i8_5d
  !----------------------------------------------------------------------------------------

 !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 1d int variable for netcdf
  SUBROUTINE netcdf_put_int_i4_1d(ncid, var_int_1d, meta_1d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i4), INTENT(IN) ::  var_int_1d(:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_1d   !< addtional information for array
  INTEGER, INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_1d(1)  !< dimension ids for 1d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize

  INTEGER :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_1d%diminfo(1)%dimname), &
      &                        dimid_1d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_1d%diminfo(1)%dimname),&
        &                             meta_1d%diminfo(1)%dimsize,      &
        &                             dimid_1d(1)))
    ENDIF

    ! get varname
    varname = TRIM(meta_1d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_1d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_1d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_1d))

  END SUBROUTINE netcdf_put_int_i4_1d
  !----------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 2d int variable for netcdf
  SUBROUTINE netcdf_put_int_i4_2d(ncid, var_int_2d, meta_2d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i4), INTENT(IN) ::  var_int_2d(:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_2d   !< addtional information for array
  INTEGER (KIND=i4), INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_2d(2)  !< dimension ids for 2d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize

  INTEGER :: fill_value

  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_2d%diminfo(1)%dimname), &
      &                        dimid_2d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_2d%diminfo(1)%dimname),&
        &                             meta_2d%diminfo(1)%dimsize,      &
        &                             dimid_2d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_2d%diminfo(2)%dimname), &
      &                        dimid_2d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_2d%diminfo(2)%dimname),&
        &                             meta_2d%diminfo(2)%dimsize,      &
        &                             dimid_2d(2)))
    ENDIF

    ! get varname
    varname = TRIM(meta_2d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_2d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_2d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_2d))

  END SUBROUTINE netcdf_put_int_i4_2d
  !----------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 3d int variable for netcdf
  SUBROUTINE netcdf_put_int_i4_3d(ncid, var_int_3d, meta_3d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i4), INTENT(IN) ::  var_int_3d(:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_3d   !< addtional information for array
  INTEGER (KIND=i4), INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_3d(3)  !< dimension ids for 3d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  INTEGER :: fill_value


  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(1)%dimname), &
      &                        dimid_3d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(1)%dimname),&
        &                             meta_3d%diminfo(1)%dimsize,      &
        &                             dimid_3d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(2)%dimname), &
      &                        dimid_3d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(2)%dimname),&
        &                             meta_3d%diminfo(2)%dimsize,      &
        &                             dimid_3d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_3d%diminfo(3)%dimname), &
      &                        dimid_3d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_3d%diminfo(3)%dimname),&
        &                             meta_3d%diminfo(3)%dimsize,      &
        &                             dimid_3d(3)))
    ENDIF

    ! get varname
    varname = TRIM(meta_3d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_3d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_3d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_3d))

  END SUBROUTINE netcdf_put_int_i4_3d
  !----------------------------------------------------------------------------------------


  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 4d int variable for netcdf
  SUBROUTINE netcdf_put_int_i4_4d(ncid, var_int_4d, meta_4d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i4), INTENT(IN) ::  var_int_4d(:,:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_4d   !< addtional information for array
  INTEGER (KIND=i4), INTENT(IN) :: fill_value_i

  !local variables

  INTEGER :: n !< counter
  INTEGER :: dimid_4d(4)  !< dimension ids for 4D variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  INTEGER :: fill_value


  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable

    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(1)%dimname), &
      &                        dimid_4d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(1)%dimname),&
        &                             meta_4d%diminfo(1)%dimsize,      &
        &                             dimid_4d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(2)%dimname), &
      &                        dimid_4d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(2)%dimname),&
        &                             meta_4d%diminfo(2)%dimsize,      &
        &                             dimid_4d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(3)%dimname), &
      &                        dimid_4d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(3)%dimname),&
        &                             meta_4d%diminfo(3)%dimsize,      &
        &                             dimid_4d(3)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_4d%diminfo(4)%dimname), &
      &                        dimid_4d(4))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_4d%diminfo(4)%dimname),&
        &                             meta_4d%diminfo(4)%dimsize,      &
        &                             dimid_4d(4)))
    ENDIF

    ! get varname
    varname = TRIM(meta_4d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_4d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_4d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_4d))

  END SUBROUTINE netcdf_put_int_i4_4d
  !----------------------------------------------------------------------------------------
  !----------------------------------------------------------------------------------------
  !> specific subroutine to define 5d int variable for netcdf
  SUBROUTINE netcdf_put_int_i4_5d(ncid, var_int_5d, meta_5d, fill_value_i)

  USE netcdf, ONLY: nf90_inq_dimid
  USE netcdf, ONLY: nf90_def_var
  USE netcdf, ONLY: nf90_def_dim

  USE netcdf, ONLY: nf90_redef
  USE netcdf, ONLY: nf90_enddef
  USE netcdf, ONLY: nf90_put_var

  USE netcdf, ONLY: NF90_INT
  USE netcdf, ONLY : nf90_noerr, nf90_strerror

  INTEGER, INTENT(IN) :: ncid !< id for netcdf file
  INTEGER (KIND=i4), INTENT(IN) ::  var_int_5d(:,:,:,:,:) 
  TYPE (var_meta_info), INTENT(IN) :: meta_5d   !< addtional information for array
  INTEGER (KIND=i4), INTENT(IN) :: fill_value_i

  !local variables
  INTEGER :: n !< counter
  INTEGER :: dimid_5d(5)  !< dimension ids for 5d variable
  INTEGER :: varid  !< netcdf varid of variable
  INTEGER :: errorcode
  INTEGER :: dimsize
  INTEGER :: fill_value


  CHARACTER (len=12) :: dimname  !< name of dimension
  CHARACTER (len=20) :: varname    !< name of variable
    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! get dimid
    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(1)%dimname), &
      &                        dimid_5d(1))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(1)%dimname),&
        &                             meta_5d%diminfo(1)%dimsize,      &
        &                             dimid_5d(1)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(2)%dimname), &
      &                        dimid_5d(2))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(2)%dimname),&
        &                             meta_5d%diminfo(2)%dimsize,      &
        &                             dimid_5d(2)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(3)%dimname), &
      &                        dimid_5d(3))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(3)%dimname),&
        &                             meta_5d%diminfo(3)%dimsize,      &
        &                             dimid_5d(3)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(4)%dimname), &
      &                        dimid_5d(4))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(4)%dimname),&
        &                             meta_5d%diminfo(4)%dimsize,      &
        &                             dimid_5d(4)))
    ENDIF

    errorcode = nf90_inq_dimid(ncid, &
      &                        TRIM(meta_5d%diminfo(5)%dimname), &
      &                        dimid_5d(5))
    IF (errorcode /= nf90_noerr) THEN
      CALL check_netcdf( nf90_def_dim(ncid,                            &
        &                             TRIM(meta_5d%diminfo(5)%dimname),&
        &                             meta_5d%diminfo(5)%dimsize,      &
        &                             dimid_5d(5)))
    ENDIF


    ! get varname
    varname = TRIM(meta_5d%varname)
    ! define netcdf variable
    CALL check_netcdf( nf90_def_var(ncid, &
                    & varname,            &
                    & NF90_INT,           &
                    & dimid_5d,           &
                    varid))
    ! put standard attributes to variable
    fill_value = fill_value_i
    CALL netcdf_put_att(ncid, varid, meta_5d, fill_value)

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))

    ! put variable to netcdf file

    CALL check_netcdf(nf90_put_var(ncid,varid,var_int_5d))

  END SUBROUTINE netcdf_put_int_i4_5d
  !----------------------------------------------------------------------------------------




  !> define variable with grid defintion for netcdf cf standard
  !! following the cf conventions
  !! see  see e.g. http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/
  SUBROUTINE netcdf_def_grid_mapping(ncid, nc_grid_def, varid)
    USE netcdf, ONLY: nf90_def_var
    USE netcdf, ONLY: nf90_redef
    USE netcdf, ONLY: nf90_enddef
    USE netcdf, ONLY: nf90_put_att
    USE netcdf, ONLY: NF90_CHAR
    INTEGER, INTENT(IN) :: ncid !< id for netcdf file
    TYPE(netcdf_grid_mapping), INTENT(IN) :: nc_grid_def !< mapping parameters for netcdf
    INTEGER, INTENT(OUT) :: varid  !< netcdf varid of variable
    ! local variables
    INTEGER :: n !< counter

    ! redef netcdf file
     CALL check_netcdf(nf90_redef(ncid))

    ! define "dummy" variable with grid defintion attributes for netcdf, following the cf conventions
    ! see  see e.g. http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/
    CALL check_netcdf( nf90_def_var(ncid,TRIM(nc_grid_def%grid_mapping_varname), &
      &                             NF90_CHAR, varid))

    CALL check_netcdf( nf90_put_att(ncid,varid, TRIM(nc_grid_def%grid_mapping_name%attname), &
      &                             TRIM(nc_grid_def%grid_mapping_name%attributetext)))


    DO n=1, nc_grid_def%n_r_att
    CALL check_netcdf( nf90_put_att(ncid,varid, &
      &                  TRIM(nc_grid_def%map_param(n)%attname), &
      &                  nc_grid_def%map_param(n)%att_value_r))

    ENDDO

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))


  END SUBROUTINE netcdf_def_grid_mapping

  !> write netcdf file with external parameters from variable lists
  SUBROUTINE netcdf_write_varlist(netcdf_filename,          &
    &                            dim_list,                 &
    &                            global_attributes,        &
    &                            nc_grid_def,              &
    &                            list_real_1d,             &
    &                            list_real_2d,             &
    &                            list_real_3d,             &
    &                            list_real_4d,             &
    &                            list_real_5d,             &
    &                            list_int_1d,              &
    &                            list_int_2d,              &
    &                            list_int_3d,              &
    &                            list_int_4d,              &
    &                            list_int_5d)

   USE mo_utilities_extpar, ONLY: abort_extpar

   USE netcdf,      ONLY :     &
     & nf90_open,              &
     & nf90_close,             &
     & nf90_inquire,           &
     & nf90_inquire_dimension, &
     & nf90_inquire_variable,  &
     & nf90_inq_attname,       &
     & nf90_inquire_attribute, &
     & nf90_get_att,           &
     & nf90_inquire_dimension, &
     & nf90_inq_dimid,         &
     & nf90_inq_varid,         &
     & nf90_get_var,           &
     & nf90_noerr,             &
     & nf90_strerror

   USE netcdf,      ONLY:     &
     & nf90_create,           &
     & nf90_def_dim,          &
     & nf90_def_var,          &
     & nf90_enddef,           &
     & nf90_redef,            &
     & nf90_put_att,          &
     & nf90_put_var

 
  USE netcdf,      ONLY :     &
    & NF90_CHAR,              &
    & NF90_DOUBLE,            &
    & NF90_FLOAT,             &
    & NF90_INT,               &
    & NF90_BYTE,              &
    & NF90_SHORT


  USE netcdf,      ONLY :     &
    & NF90_GLOBAL,            &
    & NF90_UNLIMITED,         &
    & NF90_CLOBBER,           &
    & NF90_NOWRITE,           &
    & NF90_WRITE

  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename     !< filename for the netcdf file
  TYPE(dim_meta_info), INTENT(IN)    :: dim_list(:)         !< dimensions for netcdf file
  TYPE(netcdf_attributes), INTENT(IN), OPTIONAL :: global_attributes(:)  !< structure with global attributes
  TYPE(netcdf_grid_mapping), INTENT(IN), OPTIONAL :: nc_grid_def !< mapping parameters for netcdf


  TYPE(struct_real_1d), INTENT(IN), OPTIONAL :: list_real_1d(:) !< array with structures of real 1d variables
  TYPE(struct_real_2d), INTENT(IN), OPTIONAL :: list_real_2d(:) !< array with structures of real 2d variables
  TYPE(struct_real_3d), INTENT(IN), OPTIONAL :: list_real_3d(:) !< array with structures of real 3d variables
  TYPE(struct_real_4d), INTENT(IN), OPTIONAL :: list_real_4d(:) !< array with structures of real 4d variables
  TYPE(struct_real_5d), INTENT(IN), OPTIONAL :: list_real_5d(:) !< array with structures of real 5d variables



  TYPE(struct_int_1d), INTENT(IN), OPTIONAL :: list_int_1d(:) !< array with structures of integer 1d variables
  TYPE(struct_int_2d), INTENT(IN), OPTIONAL :: list_int_2d(:) !< array with structures of integer 2d variables
  TYPE(struct_int_3d), INTENT(IN), OPTIONAL :: list_int_3d(:) !< array with structures of integer 3d variables
  TYPE(struct_int_4d), INTENT(IN), OPTIONAL :: list_int_4d(:) !< array with structures of integer 4d variables
  TYPE(struct_int_5d), INTENT(IN), OPTIONAL :: list_int_5d(:) !< array with structures of integer 5d variables



  ! local variables
  INTEGER :: ncid !< netcdf unit file number
  INTEGER :: varid!< id of variable

  INTEGER :: ndims !< number of dimension
  INTEGER :: ng_att!< number of global attributes

  INTEGER :: n_1d_real = 0 !< number of 1D real variables
  INTEGER :: n_2d_real = 0 !< number of 2D real variables
  INTEGER :: n_3d_real = 0 !< number of 3D real variables
  INTEGER :: n_4d_real = 0 !< number of 3D real variables
  INTEGER :: n_5d_real = 0 !< number of 3D real variables

  INTEGER :: n_1d_int = 0 !< number of 1D integer variables
  INTEGER :: n_2d_int = 0 !< number of 2D integer variables
  INTEGER :: n_3d_int = 0 !< number of 3D integer variables
  INTEGER :: n_4d_int = 0 !< number of 3D integer variables
  INTEGER :: n_5d_int = 0 !< number of 3D integer variables


  INTEGER, ALLOCATABLE :: dimids(:) !< list of netcdf dim ids

  INTEGER, ALLOCATABLE :: varids_1d_real(:) !< list of varids 1d real
  INTEGER, ALLOCATABLE :: varids_2d_real(:) !< list of varids 2d real
  INTEGER, ALLOCATABLE :: varids_3d_real(:) !< list of varids 3d real
  INTEGER, ALLOCATABLE :: varids_4d_real(:) !< list of varids 4d real
  INTEGER, ALLOCATABLE :: varids_5d_real(:) !< list of varids 5d real
  INTEGER, ALLOCATABLE :: varids_1d_int(:) !< list of varids 1D integer
  INTEGER, ALLOCATABLE :: varids_2d_int(:) !< list of varids 2d integer
  INTEGER, ALLOCATABLE :: varids_3d_int(:) !< list of varids 3d integer
  INTEGER, ALLOCATABLE :: varids_4d_int(:) !< list of varids 4d integer
  INTEGER, ALLOCATABLE :: varids_5d_int(:) !< list of varids 5d integer

  INTEGER :: n ! counter
  INTEGER :: errorcode !< error status variable
  
  PRINT*,'Enter netcdf_write_varlist'

  ! create netcdf file
  PRINT*,'create ',TRIM(netcdf_filename)

  CALL check_netcdf( nf90_create(TRIM(netcdf_filename),NF90_CLOBBER,ncid))
  ndims = SIZE(dim_list)
  ALLOCATE(dimids(1:ndims), STAT=errorcode)
  IF(errorcode /= 0) CALL abort_extpar('Cant allocate dimids')

  ! define dimensions
  PRINT *,'define dimensions'
  DO n=1, ndims
    CALL check_netcdf( nf90_def_dim(ncid,                      &
      &                             TRIM(dim_list(n)%dimname), &
      &                             dim_list(n)%dimsize,       &
      &                             dimids(n)))
  ENDDO

  ! put global attributes
  IF (PRESENT(global_attributes)) THEN
  PRINT *,'put global attributes'
    ng_att=SIZE(global_attributes)
    PRINT *,'ng_att: ',ng_att
    DO n=1, ng_att
      CALL check_netcdf( nf90_put_att(ncid,NF90_GLOBAL, &
        &                TRIM(global_attributes(n)%attname), &
        &                TRIM(global_attributes(n)%attributetext)))
    ENDDO
  ENDIF

  IF (PRESENT(list_real_1d)) THEN
  PRINT *,'list_real_1d'
    n_1d_real = SIZE(list_real_1d)
    ALLOCATE(varids_1d_real(1:n_1d_real), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_1d_real')
    CALL netcdf_def_var(ncid, n_1d_real, list_real_1d, varids_1d_real)
  ENDIF

  IF (PRESENT(list_real_2d)) THEN
  PRINT *,'list_real_2d'
    n_2d_real = SIZE(list_real_2d)
    ALLOCATE(varids_2d_real(1:n_2d_real), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_2d_real')
    CALL netcdf_def_var(ncid, n_2d_real, list_real_2d, varids_2d_real)
  ENDIF

  IF (PRESENT(list_real_3d)) THEN
  PRINT *,'list_real_3d'
    n_3d_real = SIZE(list_real_3d)
    ALLOCATE(varids_3d_real(1:n_3d_real), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_3d_real')
    CALL netcdf_def_var(ncid, n_3d_real, list_real_3d, varids_3d_real)
  ENDIF

  IF (PRESENT(list_real_4d)) THEN
  PRINT *,'list_real_4d'
    n_4d_real = SIZE(list_real_4d)
    ALLOCATE(varids_4d_real(1:n_4d_real), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_4d_real')
    CALL netcdf_def_var(ncid, n_4d_real, list_real_4d, varids_4d_real)
  ENDIF

  IF (PRESENT(list_real_5d)) THEN
  PRINT *,'list_real_5d'
    n_5d_real = SIZE(list_real_5d)
    ALLOCATE(varids_5d_real(1:n_5d_real), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_5d_real')
    CALL netcdf_def_var(ncid, n_5d_real, list_real_5d, varids_5d_real)
  ENDIF


  IF (PRESENT(list_int_1d)) THEN
  PRINT *,'list_int_1d'
    n_1d_int = SIZE(list_int_1d)
    ALLOCATE(varids_1d_int(1:n_1d_int), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_1d_int')
    CALL netcdf_def_var(ncid, n_1d_int, list_int_1d, varids_1d_int)
  ENDIF

  IF (PRESENT(list_int_2d)) THEN
  PRINT *,'list_int_2d'
    n_2d_int = SIZE(list_int_2d)
    ALLOCATE(varids_2d_int(1:n_2d_int), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_2d_int')
    CALL netcdf_def_var(ncid, n_2d_int, list_int_2d, varids_2d_int)
  ENDIF
  
  IF (PRESENT(list_int_3d)) THEN
  PRINT *,'list_int_3d'
    n_3d_int = SIZE(list_int_3d)
    ALLOCATE(varids_3d_int(1:n_3d_int), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_3d_int')
    CALL netcdf_def_var(ncid, n_3d_int, list_int_3d, varids_3d_int)
  ENDIF

  IF (PRESENT(list_int_4d)) THEN
  PRINT *,'list_int_4d'
    n_4d_int = SIZE(list_int_4d)
    ALLOCATE(varids_4d_int(1:n_4d_int), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_4d_int')
    CALL netcdf_def_var(ncid, n_4d_int, list_int_4d, varids_4d_int)
  ENDIF
  
  IF (PRESENT(list_int_5d)) THEN
  PRINT *,'list_int_5d'
    n_5d_int = SIZE(list_int_5d)
    ALLOCATE(varids_5d_int(1:n_5d_int), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate the structure varids_5d_int')
    CALL netcdf_def_var(ncid, n_5d_int, list_int_5d, varids_5d_int)
  ENDIF


  IF (PRESENT(nc_grid_def)) THEN
  PRINT *,'nc_grid_def'
    CALL netcdf_def_grid_mapping(ncid, nc_grid_def, varid)
  ENDIF

  ! end of definition
  PRINT *,'end of definition'
  CALL check_netcdf(nf90_enddef(ncid))

  ! write variable values
  PRINT *,'write variable values'
  IF (PRESENT(list_real_1d)) THEN
    DO n=1, n_1d_real
      CALL check_netcdf(nf90_put_var(ncid,varids_1d_real(n),list_real_1d(n)%var_real_1d))
    ENDDO

  ENDIF

  IF (PRESENT(list_real_2d)) THEN
    DO n=1, n_2d_real
      CALL check_netcdf(nf90_put_var(ncid,varids_2d_real(n),list_real_2d(n)%var_real_2d))
    ENDDO

  ENDIF

  IF (PRESENT(list_real_3d)) THEN
    DO n=1, n_3d_real
      CALL check_netcdf(nf90_put_var(ncid,varids_3d_real(n),list_real_3d(n)%var_real_3d))
    ENDDO
  ENDIF

  IF (PRESENT(list_real_4d)) THEN
    DO n=1, n_4d_real
      CALL check_netcdf(nf90_put_var(ncid,varids_4d_real(n),list_real_4d(n)%var_real_4d))
    ENDDO
  ENDIF

    
  IF (PRESENT(list_real_5d)) THEN
    DO n=1, n_5d_real
      CALL check_netcdf(nf90_put_var(ncid,varids_5d_real(n),list_real_5d(n)%var_real_5d))
    ENDDO
  ENDIF



  IF (PRESENT(list_int_1d)) THEN
    DO n=1, n_1d_int
      CALL check_netcdf(nf90_put_var(ncid,varids_1d_int(n),list_int_1d(n)%var_int_1d))
    ENDDO
  ENDIF

  IF (PRESENT(list_int_2d)) THEN
    DO n=1, n_2d_int
      CALL check_netcdf(nf90_put_var(ncid,varids_2d_int(n),list_int_2d(n)%var_int_2d))
    ENDDO
  ENDIF
  
  IF (PRESENT(list_int_3d)) THEN
    DO n=1, n_3d_int
      CALL check_netcdf(nf90_put_var(ncid,varids_3d_int(n),list_int_3d(n)%var_int_3d))
    ENDDO
  ENDIF

   IF (PRESENT(list_int_4d)) THEN
    DO n=1, n_4d_int
      CALL check_netcdf(nf90_put_var(ncid,varids_4d_int(n),list_int_4d(n)%var_int_4d))
    ENDDO
  ENDIF

  IF (PRESENT(list_int_5d)) THEN
    DO n=1, n_5d_int
      CALL check_netcdf(nf90_put_var(ncid,varids_5d_int(n),list_int_5d(n)%var_int_5d))
    ENDDO
  ENDIF

  ! close netcdf file 
  CALL check_netcdf( nf90_close( ncid))

  END SUBROUTINE netcdf_write_varlist

  !> specific subroutine to read 3D real variable from netcdf file
  SUBROUTINE netcdf_get_var_real_3d(path_netcdf_file,var_real_3d_meta,var_real_3d)

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension
  USE netcdf, ONLY: nf90_inq_varid
  USE netcdf, ONLY: nf90_get_var
  USE netcdf, ONLY: nf90_open, nf90_close 
  USE netcdf, ONLY: NF90_FLOAT, NF90_NOWRITE



   CHARACTER (len=*), INTENT(IN) :: path_netcdf_file
   TYPE(var_meta_info), INTENT(IN) :: var_real_3d_meta !< meta information for variable

   REAL (KIND=wp), INTENT(OUT) :: var_real_3d(1:var_real_3d_meta%diminfo(1)%dimsize, &
    &                               1:var_real_3d_meta%diminfo(2)%dimsize, &
    &                               1:var_real_3d_meta%diminfo(3)%dimsize) !< 3D real variable


  !local variables
  INTEGER :: ncid !< id for netcdf file
  INTEGER :: n !< counter

  CHARACTER (len=20) :: varname !< name of variable
  INTEGER :: varid !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  INTEGER :: dimid  !< id of dimension

  INTEGER :: ndim  !< number of dimensions of variable
  INTEGER :: length!< length of dimension


  ! open netcdf file 
  CALL check_netcdf(nf90_open(TRIM(path_netcdf_file),NF90_NOWRITE, ncid))

  ! first get information for variable
  varname = TRIM(var_real_3d_meta%varname)
  CALL check_netcdf(nf90_inq_varid(ncid, TRIM(varname), varid))
  ! second  check for dimension size
  ndim = var_real_3d_meta%n_dim
  DO n=1,ndim
    dimname = TRIM(var_real_3d_meta%diminfo(n)%dimname)
     CALL check_netcdf(nf90_inq_dimid(ncid,TRIM(dimname), dimid))
     CALL check_netcdf(nf90_inquire_dimension(ncid,dimid,len=length))
     IF (length /= var_real_3d_meta%diminfo(n)%dimsize) THEN
       CALL abort_extpar('Dimension size of input file in variable does not match')
     ENDIF
  ENDDO
  ! third get variable
  CALL check_netcdf(nf90_get_var(ncid,varid,var_real_3d) )

  ! close netcdf file 
  CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE netcdf_get_var_real_3d

  !> specific subroutine to read 4D real variable from netcdf file
  SUBROUTINE netcdf_get_var_real_4d(path_netcdf_file,var_real_4d_meta,var_real_4d)

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension
  USE netcdf, ONLY: nf90_inq_varid
  USE netcdf, ONLY: nf90_get_var
  USE netcdf, ONLY: nf90_open, nf90_close 
  USE netcdf, ONLY: NF90_FLOAT, NF90_NOWRITE

   CHARACTER (len=*), INTENT(IN) :: path_netcdf_file
   TYPE(var_meta_info), INTENT(IN) :: var_real_4d_meta !< meta information for variable

   REAL (KIND=wp), INTENT(OUT) :: var_real_4d(1:var_real_4d_meta%diminfo(1)%dimsize, &
    &                               1:var_real_4d_meta%diminfo(2)%dimsize, &
    &                               1:var_real_4d_meta%diminfo(3)%dimsize, &
    &                               1:var_real_4d_meta%diminfo(4)%dimsize) !< 4D real variable


  !local variables
  INTEGER :: ncid !< id for netcdf file
  INTEGER :: n !< counter

  CHARACTER (len=20) :: varname !< name of variable
  INTEGER :: varid !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  INTEGER :: dimid  !< id of dimension

  INTEGER :: ndim  !< number of dimensions of variable
  INTEGER :: length!< length of dimension


  ! open netcdf file 
  CALL check_netcdf(nf90_open(TRIM(path_netcdf_file),NF90_NOWRITE, ncid))

  ! first get information for variable
  varname = TRIM(var_real_4d_meta%varname)
  CALL check_netcdf(nf90_inq_varid(ncid, TRIM(varname), varid))
  ! second  check for dimension size
  ndim = var_real_4d_meta%n_dim
  DO n=1,ndim
    dimname = TRIM(var_real_4d_meta%diminfo(n)%dimname)
     CALL check_netcdf(nf90_inq_dimid(ncid,TRIM(dimname), dimid))
     CALL check_netcdf(nf90_inquire_dimension(ncid,dimid,len=length))
     IF (length /= var_real_4d_meta%diminfo(n)%dimsize) THEN
       CALL abort_extpar('Dimension size of input file in variable does not match')
     ENDIF
  ENDDO
  ! third get variable
  CALL check_netcdf(nf90_get_var(ncid,varid,var_real_4d) )

  ! close netcdf file 
  CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE netcdf_get_var_real_4d

  !> specific subroutine to read 4D real variable from netcdf file
  SUBROUTINE netcdf_get_var_real_5d(path_netcdf_file,var_real_5d_meta,var_real_5d)

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension
  USE netcdf, ONLY: nf90_inq_varid
  USE netcdf, ONLY: nf90_get_var
  USE netcdf, ONLY: nf90_open, nf90_close 
  USE netcdf, ONLY: NF90_FLOAT, NF90_NOWRITE

   CHARACTER (len=*), INTENT(IN) :: path_netcdf_file
   TYPE(var_meta_info), INTENT(IN) :: var_real_5d_meta !< meta information for variable

   REAL (KIND=wp), INTENT(OUT) :: var_real_5d(1:var_real_5d_meta%diminfo(1)%dimsize, &
    &                               1:var_real_5d_meta%diminfo(2)%dimsize, &
    &                               1:var_real_5d_meta%diminfo(3)%dimsize, &
    &                               1:var_real_5d_meta%diminfo(4)%dimsize, &
    &                               1:var_real_5d_meta%diminfo(5)%dimsize) !< 4D real variable


  !local variables
  INTEGER :: ncid !< id for netcdf file
  INTEGER :: n !< counter

  CHARACTER (len=20) :: varname !< name of variable
  INTEGER :: varid !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  INTEGER :: dimid  !< id of dimension

  INTEGER :: ndim  !< number of dimensions of variable
  INTEGER :: length!< length of dimension


  ! open netcdf file 
  CALL check_netcdf(nf90_open(TRIM(path_netcdf_file),NF90_NOWRITE, ncid))

  ! first get information for variable
  varname = TRIM(var_real_5d_meta%varname)
  CALL check_netcdf(nf90_inq_varid(ncid, TRIM(varname), varid))
  ! second  check for dimension size
  ndim = var_real_5d_meta%n_dim
  DO n=1,ndim
    dimname = TRIM(var_real_5d_meta%diminfo(n)%dimname)
     CALL check_netcdf(nf90_inq_dimid(ncid,TRIM(dimname), dimid))
     CALL check_netcdf(nf90_inquire_dimension(ncid,dimid,len=length))
     IF (length /= var_real_5d_meta%diminfo(n)%dimsize) THEN
       CALL abort_extpar('Dimension size of input file in variable does not match')
     ENDIF
  ENDDO
  ! third get variable
  CALL check_netcdf(nf90_get_var(ncid,varid,var_real_5d) )

  ! close netcdf file 
  CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE netcdf_get_var_real_5d



  !> specific subroutine to read 3D integer variable from netcdf file
  SUBROUTINE netcdf_get_var_int_3d_i8(path_netcdf_file,var_int_3d_meta,var_int_3d)

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension
  USE netcdf, ONLY: nf90_inq_varid
  USE netcdf, ONLY: nf90_get_var
  USE netcdf, ONLY: nf90_open, nf90_close 
  USE netcdf, ONLY: NF90_INT, NF90_NOWRITE

   CHARACTER (len=*), INTENT(IN) :: path_netcdf_file
   TYPE(var_meta_info), INTENT(IN) :: var_int_3d_meta !< meta information for variable

   INTEGER (KIND=i8), INTENT(OUT) :: var_int_3d(1:var_int_3d_meta%diminfo(1)%dimsize, &
    &                               1:var_int_3d_meta%diminfo(2)%dimsize, &
    &                               1:var_int_3d_meta%diminfo(3)%dimsize) !< 3D integer variable

  !local variables
  INTEGER :: ncid !< id for netcdf file
  INTEGER :: n !< counter

  CHARACTER (len=20) :: varname !< name of variable
  INTEGER :: varid !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  INTEGER :: dimid  !< id of dimension

  INTEGER :: ndim  !< number of dimensions of variable
  INTEGER :: length!< length of dimension


  ! open netcdf file 
  CALL check_netcdf(nf90_open(TRIM(path_netcdf_file),NF90_NOWRITE, ncid))

  ! first get information for variable
  varname = TRIM(var_int_3d_meta%varname)
  CALL check_netcdf(nf90_inq_varid(ncid, TRIM(varname), varid))
  ! second  check for dimension size
  ndim = var_int_3d_meta%n_dim
  DO n=1,ndim
    dimname = TRIM(var_int_3d_meta%diminfo(n)%dimname)
     CALL check_netcdf(nf90_inq_dimid(ncid,TRIM(dimname), dimid))
     CALL check_netcdf(nf90_inquire_dimension(ncid,dimid,len=length))
     IF (length /= var_int_3d_meta%diminfo(n)%dimsize) THEN
       CALL abort_extpar('Dimension size of input file in variable does not match')
     ENDIF
  ENDDO
  ! third get variable
  CALL check_netcdf(nf90_get_var(ncid,varid,var_int_3d) )

  ! close netcdf file 
  CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE netcdf_get_var_int_3d_i8

  !> specific subroutine to read 3D integer variable from netcdf file
  SUBROUTINE netcdf_get_var_int_3d_i4(path_netcdf_file,var_int_3d_meta,var_int_3d)

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension
  USE netcdf, ONLY: nf90_inq_varid
  USE netcdf, ONLY: nf90_get_var
  USE netcdf, ONLY: nf90_open, nf90_close 
  USE netcdf, ONLY: NF90_INT, NF90_NOWRITE



   CHARACTER (len=*), INTENT(IN) :: path_netcdf_file
   TYPE(var_meta_info), INTENT(IN) :: var_int_3d_meta !< meta information for variable

   INTEGER (KIND=i4), INTENT(OUT) :: var_int_3d(1:var_int_3d_meta%diminfo(1)%dimsize, &
    &                               1:var_int_3d_meta%diminfo(2)%dimsize, &
    &                               1:var_int_3d_meta%diminfo(3)%dimsize) !< 3D integer variable

  !local variables
  INTEGER :: ncid !< id for netcdf file
  INTEGER :: n !< counter

  CHARACTER (len=20) :: varname !< name of variable
  INTEGER :: varid !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  INTEGER :: dimid  !< id of dimension

  INTEGER :: ndim  !< number of dimensions of variable
  INTEGER :: length!< length of dimension


  ! open netcdf file 
  CALL check_netcdf(nf90_open(TRIM(path_netcdf_file),NF90_NOWRITE, ncid))

  ! first get information for variable
  varname = TRIM(var_int_3d_meta%varname)
  CALL check_netcdf(nf90_inq_varid(ncid, TRIM(varname), varid))
  ! second  check for dimension size
  ndim = var_int_3d_meta%n_dim
  DO n=1,ndim
    dimname = TRIM(var_int_3d_meta%diminfo(n)%dimname)
     CALL check_netcdf(nf90_inq_dimid(ncid,TRIM(dimname), dimid))
     CALL check_netcdf(nf90_inquire_dimension(ncid,dimid,len=length))
     IF (length /= var_int_3d_meta%diminfo(n)%dimsize) THEN
       CALL abort_extpar('Dimension size of input file in variable does not match')
     ENDIF
  ENDDO
  ! third get variable
  CALL check_netcdf(nf90_get_var(ncid,varid,var_int_3d) )

  ! close netcdf file 
  CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE netcdf_get_var_int_3d_i4


  
   !> specific subroutine to read 4D integer variable from netcdf file
  SUBROUTINE netcdf_get_var_int_4d(path_netcdf_file,var_int_4d_meta,var_int_4d)

  USE mo_utilities_extpar, ONLY: abort_extpar

  USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension
  USE netcdf, ONLY: nf90_inq_varid
  USE netcdf, ONLY: nf90_get_var
  USE netcdf, ONLY: nf90_open, nf90_close 
  USE netcdf, ONLY: NF90_INT, NF90_NOWRITE



   CHARACTER (len=*), INTENT(IN) :: path_netcdf_file
   TYPE(var_meta_info), INTENT(IN) :: var_int_4d_meta !< meta information for variable

   INTEGER (KIND=i8) :: var_int_4d(1:var_int_4d_meta%diminfo(1)%dimsize, &
    &                              1:var_int_4d_meta%diminfo(2)%dimsize, &
    &                              1:var_int_4d_meta%diminfo(3)%dimsize, & 
    &                              1:var_int_4d_meta%diminfo(4)%dimsize) !< 4D integer variable


  !local variables
  INTEGER :: ncid !< id for netcdf file
  INTEGER :: n !< counter

  CHARACTER (len=20) :: varname !< name of variable
  INTEGER :: varid !< netcdf varid of variable

  CHARACTER (len=12) :: dimname  !< name of dimension
  INTEGER :: dimid  !< id of dimension

  INTEGER :: ndim  !< number of dimensions of variable
  INTEGER :: length!< length of dimension


  ! open netcdf file 
  CALL check_netcdf(nf90_open(TRIM(path_netcdf_file),NF90_NOWRITE, ncid))

  ! first get information for variable
  varname = TRIM(var_int_4d_meta%varname)
  CALL check_netcdf(nf90_inq_varid(ncid, TRIM(varname), varid))
  ! second  check for dimension size
  ndim = var_int_4d_meta%n_dim
  DO n=1,ndim
    dimname = TRIM(var_int_4d_meta%diminfo(n)%dimname)
     CALL check_netcdf(nf90_inq_dimid(ncid,TRIM(dimname), dimid))
     CALL check_netcdf(nf90_inquire_dimension(ncid,dimid,len=length))
     IF (length /= var_int_4d_meta%diminfo(n)%dimsize) THEN
       CALL abort_extpar('Dimension size of input file in variable does not match')
     ENDIF
  ENDDO
  ! third get variable
  CALL check_netcdf(nf90_get_var(ncid,varid,var_int_4d) )

  ! close netcdf file 
  CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE netcdf_get_var_int_4d

    
  !> set date for an invariant field of the external parameters in a GRIB message 
  !! the convention at DWD is to set the date for the invariant fields to
  !! year 1, january 1, 00:00 hour
  SUBROUTINE get_date_const_field(dataDate,dataTime)
  INTEGER (KIND=i8), INTENT(OUT)  :: dataDate  !< date, for edition independent use of GRIB_API dataDate as Integer in the format ccyymmdd
    INTEGER (KIND=i8), INTENT(OUT)  :: dataTime  !< time, for edition independent use GRIB_API dataTime in the format hhmm

    !local variables

    INTEGER  :: cc
    INTEGER  :: yy
    INTEGER  :: mm
    INTEGER  :: dd
    INTEGER  :: hh
    INTEGER  :: minute

    cc = 0
    yy = 1
    mm = 1
    dd = 1
    hh = 0
    minute=0
    ! dataDate format ccyymmdd
    ! dataTime format hhmm (hour minute)
    dataDate = dd + (100*mm) + (10000*yy) + (1000000*cc)
    dataTime = minute + (100*hh)

  END  SUBROUTINE get_date_const_field

   !> set date for an monthly climatology field of the external parameters in a GRIB message with GRIB_API
  !! the convention at DWD is to set the date for the invariant fields to
  !! year 1111, month mm, day 11, 00:00 hour
  !! the grib message should have been previously defined, pass the grib_id to this subroutine
  SUBROUTINE set_date_mm_extpar_field(mm,dataDate,dataTime)
    INTEGER, INTENT(IN)   :: mm     !< month
    INTEGER (KIND=i8), INTENT(OUT)  :: dataDate  !< date, for edition independent use of GRIB_API dataDate as Integer in the format ccyymmdd
    INTEGER (KIND=i8), INTENT(OUT)  :: dataTime  !< time, for edition independent use GRIB_API dataTime in the format hhmm
    !local variables

    INTEGER  :: cc
    INTEGER  :: yy
    INTEGER  :: dd
    INTEGER  :: hh
    INTEGER  :: minute

    cc = 11
    yy = 11
    dd = 11
    hh = 0
    minute=0
    ! dataDate format ccyymmdd
    ! dataTime format hhmm (hour minute)
    dataDate = dd + (100*mm) + (10000*yy) + (1000000*cc)
    dataTime = minute + (100*hh)

  END  SUBROUTINE set_date_mm_extpar_field

  !> open netcdf-file and get netcdf unit file number ncid
  SUBROUTINE open_new_netcdf_file(netcdf_filename, dim_list, global_attributes, ncid)
    USE netcdf, ONLY: nf90_create 
    USE netcdf, ONLY: NF90_CLOBBER, NF90_GLOBAL
    USE netcdf, ONLY: nf90_def_dim
    USE netcdf, ONLY: nf90_put_att
    USE netcdf, ONLY: nf90_enddef
    CHARACTER (len=*), INTENT(IN)      :: netcdf_filename     !< filename for the netcdf file
    TYPE(dim_meta_info), INTENT(IN)    :: dim_list(:)         !< dimensions for netcdf file
    TYPE(netcdf_attributes), INTENT(IN), OPTIONAL :: global_attributes(:)  !< structure with global attributes
    INTEGER, INTENT(OUT) :: ncid                      !< netcdf unit file number
    ! local variables
    INTEGER :: ndims !< number of dimension
    INTEGER :: ng_att!< number of global attributes
    INTEGER, ALLOCATABLE :: dimids(:) !< list of netcdf dim ids
    INTEGER :: errorcode
    INTEGER :: n !< counter

     PRINT *,' CALL  nf90_create'

    CALL check_netcdf( nf90_create(TRIM(netcdf_filename),NF90_CLOBBER,ncid))
    ndims = SIZE(dim_list)
    PRINT *,'size of dim_list ndims: ',ndims

    ALLOCATE(dimids(1:ndims), STAT=errorcode)
    IF(errorcode /= 0) CALL abort_extpar('Cant allocate dimids')

    ! define dimensions
    DO n=1, ndims
    PRINT *,'dim n: ',n
    PRINT *,'TRIM(dim_list(n)%dimname): ', TRIM(dim_list(n)%dimname)
      CALL check_netcdf( nf90_def_dim(ncid,                      &
        &                             TRIM(dim_list(n)%dimname), &
        &                             dim_list(n)%dimsize,       &
        &                             dimids(n)))
    ENDDO

    ! put global attributes
    IF (PRESENT(global_attributes)) THEN
      ng_att=SIZE(global_attributes)
      PRINT *,'number of attributes ng_att: ',ng_att

      DO n=1, ng_att
      PRINT *,'attribute n: ',n
        CALL check_netcdf( nf90_put_att(ncid,NF90_GLOBAL, &
          &                TRIM(global_attributes(n)%attname), &
          &                TRIM(global_attributes(n)%attributetext)))
      ENDDO
    ENDIF

    ! end of definition
    CALL check_netcdf(nf90_enddef(ncid))


  END SUBROUTINE open_new_netcdf_file

  !> close netcdf-file with unit file number ncid
  SUBROUTINE close_netcdf_file(ncid)
    USE netcdf, ONLY:  nf90_close
    INTEGER, INTENT(IN) :: ncid                       !< netcdf unit file number
    !! close netcdf file 
    CALL check_netcdf( nf90_close( ncid))
  END SUBROUTINE close_netcdf_file



END MODULE mo_io_utilities














