!+ Fortran module with netcdf output routines for FLAKE data on the target grid
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_4         2011/04/21 Hermann Asensio
!  clean up
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with netcdf output routines for FLAKE data on the target grid
!> ouptut routines
!> \author Hermann Asensio
MODULE mo_flake_output_nc


  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4
  USE mo_logging

  !> data type structures form module GRID_structures
  USE mo_grid_structures, ONLY: reg_lonlat_grid
  USE mo_grid_structures, ONLY: rotated_lonlat_grid
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid
  USE mo_grid_structures, ONLY: target_grid_def

  USE mo_io_utilities, ONLY: var_meta_info
  USE mo_io_utilities, ONLY: netcdf_attributes

  USE mo_io_utilities, ONLY: dim_meta_info

  USE mo_io_utilities, ONLY: vartype_int 
  USE mo_io_utilities, ONLY: vartype_real
  USE mo_io_utilities, ONLY: vartype_char

  USE mo_io_utilities, ONLY: netcdf_put_var
  USE mo_io_utilities, ONLY: open_new_netcdf_file
  USE mo_io_utilities, ONLY: close_netcdf_file
  USE mo_io_utilities, ONLY: netcdf_def_grid_mapping

  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_flake_data, ONLY: flake_depth_undef !< default value for undefined lake depth

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: write_netcdf_buffer_flake
  PUBLIC :: write_netcdf_cosmo_grid_flake
  PUBLIC :: write_netcdf_icon_grid_flake
  PUBLIC :: read_netcdf_buffer_flake


  CONTAINS

    !> netcdf output of FLAKE derived buffer fields
    SUBROUTINE write_netcdf_buffer_flake(netcdf_filename,  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     flake_tot_npixel)


  USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                         def_dimension_info_buffer


  USE mo_var_meta_data, ONLY: lon_geo_meta, &
    &                         lat_geo_meta, &
    &                         no_raw_data_pixel_meta, &
    &                         def_com_target_fields_meta  
  
  USE mo_var_meta_data, ONLY: def_flake_fields_meta

  USE mo_var_meta_data, ONLY: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta

  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
  TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
  REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
  INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
  REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system

  REAL (KIND=wp), INTENT(IN)  :: lake_depth(:,:,:) !< lake depth
  REAL (KIND=wp), INTENT(IN)  :: fr_lake(:,:,:)     !< fraction of fresh water (lakes)
  INTEGER (KIND=i8), INTENT(IN) :: flake_tot_npixel(:,:,:)  
                                   !< total number of flake raw data pixels on target grid (dimension (ie,je,ke))

  ! local variables

  INTEGER :: ndims  
  INTEGER :: ncid
  INTEGER (KIND=i8) :: undefined_i

  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file
  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)
  INTEGER :: errorcode !< error status variable
  INTEGER :: n !< counter

  WRITE(logging%fileunit,*)'Enter routine write_netcdf_buffer_flake'

  !-------------------------------------------------------------
  ! define global attributes
  CALL set_global_att_flake(global_attributes)

  !set up dimensions for buffer
  CALL  def_dimension_info_buffer(tg)
  ! dim_3d_tg

  ! define meta information for various land use related variables (FLAKE) for netcdf output
  CALL def_flake_fields_meta(dim_3d_tg)

  ! lake_depth_meta, fr_lake_meta, &
  !  &       flake_tot_npixel_meta

  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_3d_tg)
  ! lon_geo_meta and lat_geo_meta
  !set up dimensions for buffer netcdf output 
  ndims = 3
  undefined_i = undef_int
  ALLOCATE(dim_list(1:ndims),STAT=errorcode)
  IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')
  dim_list = dim_3d_tg

  !-----------------------------------------------------------------

  CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
      &                       dim_list=dim_list,                  &
      &                       global_attributes=global_attributes, &
      &                       ncid=ncid)

  !-----------------------------------------------------------------
  ! 3D variables flake_tot_npixel
  ! lon
  CALL netcdf_put_var(ncid,lon_geo,lon_geo_meta,undefined)

  ! lat
  CALL netcdf_put_var(ncid,lat_geo,lat_geo_meta,undefined)

  ! fr_lake
  CALL netcdf_put_var(ncid,fr_lake,fr_lake_meta,undefined)

  ! lake_depth
  CALL netcdf_put_var(ncid,lake_depth,lake_depth_meta,undefined)

  ! flake_tot_npixel
  CALL netcdf_put_var(ncid,flake_tot_npixel,flake_tot_npixel_meta,undefined_i)


  CALL close_netcdf_file(ncid)

  END SUBROUTINE write_netcdf_buffer_flake
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------

  !> netcdf output of FLAKE derived COSMO fields
  SUBROUTINE write_netcdf_cosmo_grid_flake(netcdf_filename,  &
    &                                     cosmo_grid,       &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     flake_tot_npixel)

  
  USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                         def_dimension_info_buffer


  USE mo_var_meta_data, ONLY: lon_geo_meta, &
    &                         lat_geo_meta, &
    &                         no_raw_data_pixel_meta, &
    &                         def_com_target_fields_meta  
   
  USE mo_var_meta_data, ONLY: nc_grid_def_cosmo, &
    &                         set_nc_grid_def_cosmo
    
  USE mo_var_meta_data, ONLY: dim_rlon_cosmo, &
    &                         dim_rlat_cosmo, &
    &                         dim_2d_cosmo,   &
    &                         rlon_meta,      &
    &                         rlat_meta,      &
    &                         def_dimension_info_cosmo

  USE mo_cosmo_grid, ONLY: lon_rot, lat_rot

  USE mo_var_meta_data, ONLY: def_flake_fields_meta

  
  USE mo_var_meta_data, ONLY: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta

  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
  TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid
  TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
  REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
  INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
  REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system
  
  REAL (KIND=wp), INTENT(IN)  :: lake_depth(:,:,:) !< lake depth
  REAL (KIND=wp), INTENT(IN)  :: fr_lake(:,:,:)     !< fraction of fresh water (lakes)
  INTEGER (KIND=i8), INTENT(IN) :: flake_tot_npixel(:,:,:)
                                   !< total number of flake raw data pixels on target grid (dimension (ie,je,ke))


  ! local variables
  INTEGER :: ndims  
  INTEGER :: ncid
  INTEGER :: varid
  INTEGER (KIND=i8) :: undefined_i
  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file

  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)

  INTEGER :: errorcode !< error status variable
  CHARACTER (len=80):: grid_mapping !< netcdf attribute grid mapping
  CHARACTER (len=80):: coordinates  !< netcdf attribute coordinates
  INTEGER :: n !< counter

  WRITE(logging%fileunit,*)'Enter write_netcdf_cosmo_grid_flake'

  !-------------------------------------------------------------
  ! define global attributes
  CALL set_global_att_flake(global_attributes)

  !set up dimensions for buffer
  CALL  def_dimension_info_buffer(tg)
  ! dim_3d_tg

  !set up dimensions for COSMO grid
  CALL def_dimension_info_cosmo(cosmo_grid)
  ! dim_rlon_cosmo, dim_rlat_cosmo, dim_2d_cosmo, rlon_meta, rlat_meta

 
  ! set mapping parameters for netcdf
  grid_mapping="rotated_pole"
  coordinates="lon lat"
  CALL set_nc_grid_def_cosmo(cosmo_grid,grid_mapping)
  ! nc_grid_def_cosmo

   ! define meta information for various land use related variables (FLAKE) for netcdf output
  CALL def_flake_fields_meta(dim_2d_cosmo,coordinates,grid_mapping)
  ! lake_depth_meta, fr_lake_meta, &
  !  &       flake_tot_npixel_meta

  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_2d_cosmo,coordinates,grid_mapping)
  ! lon_geo_meta and lat_geo_meta

  !set up dimensions for buffer netcdf output 
  ndims = 2
  ALLOCATE(dim_list(1:ndims),STAT=errorcode)
  IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')

  dim_list(1) = dim_rlon_cosmo(1) ! rlon
  dim_list(2) = dim_rlat_cosmo(1) ! rlat

  undefined_i = undef_int
  
  CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
      &                       dim_list=dim_list,                  &
      &                       global_attributes=global_attributes, &
      &                       ncid=ncid)
  !-----------------------------------------------------------------


  ! rlon
  CALL netcdf_put_var(ncid,lon_rot(1:cosmo_grid%nlon_rot),rlon_meta,undefined)

  ! rlat
  CALL netcdf_put_var(ncid,lat_rot(1:cosmo_grid%nlat_rot),rlat_meta,undefined)

  !-----------------------------------------------------------------
  ! lon
  CALL netcdf_put_var(ncid,lon_geo(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
    &                 lon_geo_meta,undefined)
  ! lat
  CALL netcdf_put_var(ncid,lat_geo(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
    &                 lat_geo_meta,undefined)

  ! lake_depth
  CALL netcdf_put_var(ncid,lake_depth(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
    &                 lake_depth_meta,undefined)
  
  ! fr_lake
  CALL netcdf_put_var(ncid,fr_lake(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
    &                 fr_lake_meta,undefined)
  ! flake_tot_npixel
  CALL netcdf_put_var(ncid,flake_tot_npixel(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
    &                 flake_tot_npixel_meta,undefined_i)

  CALL close_netcdf_file(ncid)


  END SUBROUTINE write_netcdf_cosmo_grid_flake
  !-----------------------------------------------------------------------

  !> netcdf output of FLAKE derived ICON fields
  SUBROUTINE write_netcdf_icon_grid_flake(netcdf_filename,  &
    &                                     icon_grid,       &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     flake_tot_npixel)


  USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                         def_dimension_info_buffer


  USE mo_var_meta_data, ONLY: lon_geo_meta, &
    &                         lat_geo_meta, &
    &                         no_raw_data_pixel_meta, &
    &                         def_com_target_fields_meta  
   

  USE mo_var_meta_data, ONLY:  dim_icon, &
    &                          def_dimension_info_icon

  USE mo_var_meta_data, ONLY: nc_grid_def_icon, &
    &                         set_nc_grid_def_icon

  USE mo_var_meta_data, ONLY: def_flake_fields_meta


  
  USE mo_var_meta_data, ONLY: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta

  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
  TYPE(icosahedral_triangular_grid), INTENT(IN) :: icon_grid !< structure which contains the definition of the ICON grid
  TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
  REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
  INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
  REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system
  
  REAL (KIND=wp), INTENT(IN)  :: lake_depth(:,:,:) !< lake depth
  REAL (KIND=wp), INTENT(IN)  :: fr_lake(:,:,:)     !< fraction of fresh water (lakes)
  INTEGER (KIND=i8), INTENT(IN) :: flake_tot_npixel(:,:,:)
                                   !< total number of flake raw data pixels on target grid (dimension (ie,je,ke))



  ! local variables

  INTEGER (KIND=i8) :: undefined_i

  INTEGER :: ndims  
  INTEGER :: ncid

  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file
  TYPE(dim_meta_info), TARGET :: dim_2d_icon(1:2)
  TYPE(dim_meta_info), TARGET :: dim_1d_icon(1:1)

  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)

  INTEGER :: errorcode !< error status variable

  CHARACTER (len=80):: grid_mapping !< netcdf attribute grid mapping
  CHARACTER (len=80):: coordinates  !< netcdf attribute coordinates

  INTEGER :: n !< counter

  !-------------------------------------------------------------
  ! define global attributes
  CALL set_global_att_flake(global_attributes)

  !set up dimensions for buffer
  CALL  def_dimension_info_buffer(tg)
  ! dim_3d_tg

  !set up dimensions for ICON grid
  CALL def_dimension_info_icon(icon_grid)
  ! dim_icon
   ! define meta information for various land use related variables (FLAKE) for netcdf output
  CALL def_flake_fields_meta(dim_icon)
  ! lake_depth_meta, fr_lake_meta, &
  !  &       flake_tot_npixel_meta

  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_icon)
  ! lon_geo_meta and lat_geo_meta

  ! set mapping parameters for netcdf
  grid_mapping="lon_lat_on_sphere"
  coordinates="lon lat"

  CALL set_nc_grid_def_icon(grid_mapping)
  ! nc_grid_def_icon

  undefined_i = undef_int
  !set up dimensions for buffer netcdf output 
  ndims = 1
  ALLOCATE(dim_list(1:ndims),STAT=errorcode)
  IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')

  dim_list(1) = dim_icon(1) ! cell
  dim_1d_icon =  dim_icon(1) ! cell

   CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
      &                       dim_list=dim_list,                  &
      &                       global_attributes=global_attributes, &
      &                       ncid=ncid)
  !-----------------------------------------------------------------

  !-----------------------------------------------------------------
    ! lon
    CALL netcdf_put_var(ncid,lon_geo(1:icon_grid%ncell,1,1),lon_geo_meta,undefined)

    ! lat
    CALL netcdf_put_var(ncid,lat_geo(1:icon_grid%ncell,1,1),lat_geo_meta,undefined)

    ! lake_depth
    CALL netcdf_put_var(ncid,lake_depth(1:icon_grid%ncell,1,1),lake_depth_meta,undefined)

    ! fr_lake
    CALL netcdf_put_var(ncid,fr_lake(1:icon_grid%ncell,1,1),fr_lake_meta,undefined)

    ! flake_tot_npixel
    CALL netcdf_put_var(ncid,flake_tot_npixel(1:icon_grid%ncell,1,1), &
      &                 flake_tot_npixel_meta,undefined_i)


  CALL close_netcdf_file(ncid)


  END SUBROUTINE write_netcdf_icon_grid_flake
  !-----------------------------------------------------------------------



  !-----------------------------------------------------------------------

  !----------------------------------------------------------------------- 
  !-----------------------------------------------------------------------
  !> set global attributes for netcdf with flake data
  SUBROUTINE set_global_att_flake(global_attributes)
    TYPE(netcdf_attributes), INTENT(INOUT) :: global_attributes(1:6)

    !local variables
    CHARACTER(len=10) :: ydate
    CHARACTER(len=10) :: ytime
    CHARACTER(len=2)  :: cc
    CHARACTER(len=2)  :: yy
    CHARACTER(len=2)  :: mm
    CHARACTER(len=2)  :: dd
    CHARACTER(len=2)  :: hh
    CHARACTER(len=2)  :: minute



    ! define global attributes
    
    global_attributes(1)%attname = 'title'
    global_attributes(1)%attributetext='Land Use data'
    global_attributes(2)%attname = 'institution'
    global_attributes(2)%attributetext='Deutscher Wetterdienst'

    global_attributes(3)%attname = 'source'
    global_attributes(3)%attributetext='FLAKE data'

    CALL DATE_AND_TIME(ydate,ytime)
    READ(ydate,'(4A2)') cc,yy,mm,dd
    READ(ytime,'(2A2)') hh, minute

    ydate=TRIM(cc)//TRIM(yy)//'-'//TRIM(mm)//'-'//TRIM(dd)
    ytime=TRIM(hh)//':'//TRIM(minute) 

    global_attributes(4)%attname = 'history'
    global_attributes(4)%attributetext=TRIM(ydate)//'T'//TRIM(ytime)//' flake_to_buffer'

    global_attributes(5)%attname = 'references'
    global_attributes(5)%attributetext=''

    global_attributes(6)%attname = 'comment'
    global_attributes(6)%attributetext=''

  END SUBROUTINE set_global_att_flake
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------


  !----------------------------------------------------------------------------
  !----------------------------------------------------------------------------

  !> read FLAKE derived buffer fields
    SUBROUTINE read_netcdf_buffer_flake(netcdf_filename,  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     flake_tot_npixel)


  USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                         def_dimension_info_buffer


  USE mo_var_meta_data, ONLY: lon_geo_meta, &
    &                         lat_geo_meta, &
    &                         no_raw_data_pixel_meta, &
    &                         def_com_target_fields_meta  
  
  USE mo_var_meta_data, ONLY: def_flake_fields_meta


  
  USE mo_var_meta_data, ONLY: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta


  !USE mo_io_utilities, ONLY: netcdf_get_var_real_3d, netcdf_get_var_real_4d
  !USE mo_io_utilities, ONLY: netcdf_get_var_int_3d, netcdf_get_var_int_4d

  USE mo_io_utilities, ONLY: netcdf_get_var

  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
  TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
  REAL(KIND=wp), INTENT(OUT)          :: undefined       !< value to indicate undefined grid elements 
  INTEGER, INTENT(OUT)                :: undef_int       !< value to indicate undefined grid elements
  REAL (KIND=wp), INTENT(OUT)  :: lake_depth(:,:,:) !< lake depth
  REAL (KIND=wp), INTENT(OUT)  :: fr_lake(:,:,:)     !< fraction of fresh water (lakes)
  INTEGER (KIND=i8), INTENT(OUT) :: flake_tot_npixel(:,:,:)
                                    !< total number of flake raw data pixels on target grid (dimension (ie,je,ke))

  ! local variables
  INTEGER :: errorcode !< error status variable
  INTEGER :: n !< counter

  WRITE(logging%fileunit,*)'ENTER read_netcdf_buffer_flake'


  !set up dimensions for buffer
  !CALL  def_dimension_info_buffer(tg)
  ! dim_3d_tg

  
  ! define meta information for various land use related variables (FLAKE) for netcdf output
  CALL def_flake_fields_meta(dim_3d_tg)

  ! lake_depth_meta, fr_lake_meta, &
  !  &       flake_tot_npixel_meta

  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_3d_tg)
  ! lon_geo_meta and lat_geo_meta

  IF (verbose >= idbg_low ) WRITE(logging%fileunit,*)'CALL read netcdf data Land Use'

  CALL netcdf_get_var(TRIM(netcdf_filename),lake_depth_meta,lake_depth)
  IF (verbose >= idbg_low ) WRITE(logging%fileunit,*)'fr_land_flake read'

  CALL netcdf_get_var(TRIM(netcdf_filename),flake_tot_npixel_meta,flake_tot_npixel)
  IF (verbose >= idbg_low ) WRITE(logging%fileunit,*)'flake_tot_npixel read'

  CALL netcdf_get_var(TRIM(netcdf_filename),fr_lake_meta,fr_lake)
  IF (verbose >= idbg_low ) WRITE(logging%fileunit,*)'flake_class_fraction read'


  END SUBROUTINE read_netcdf_buffer_flake
  !-----------------------------------------------------------------------
 
END Module mo_flake_output_nc

