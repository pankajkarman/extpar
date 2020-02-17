!+ Fortran module with netcdf output routines for CRU data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_4         2011/04/21 Hermann Asensio
!  Hermann Asensio
! V2_0         2013/06/04 Martina Messmer
!  add a new parameter for CRU temperature elevation (CLM Community)
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with netcdf output routines for CRU data
!> \author Hermann Asensio
MODULE mo_cru_output_nc
  
  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4
                                
  USE mo_grid_structures,       ONLY: rotated_lonlat_grid,         &
       &                              icosahedral_triangular_grid, &
       &                              target_grid_def

  USE mo_io_utilities,          ONLY: netcdf_attributes,    &
       &                              dim_meta_info,        &
       &                              netcdf_get_var,       &       
       &                              open_new_netcdf_file, &
       &                              netcdf_put_var,       &
       &                              close_netcdf_file

  USE mo_var_meta_data,        ONLY: dim_3d_tg,                  &
       &                             def_dimension_info_buffer,  &
       &                             crutemp_meta,               &
       &                             def_crutemp_meta,           &
       &                             cruelev_meta,               &
       &                             def_cruelev_meta,           &
       &                             lon_geo_meta,               &
       &                             lat_geo_meta,               &
       &                             def_com_target_fields_meta, &
       &                             set_nc_grid_def_cosmo,      &
       &                             dim_rlon_cosmo,             &
       &                             dim_rlat_cosmo,             &
       &                             dim_2d_cosmo,               &
       &                             rlon_meta,                  &
       &                             rlat_meta,                  &
       &                             def_dimension_info_cosmo,   &
       &                             dim_icon,                   &
       &                             def_dimension_info_icon,    &
       &                             set_nc_grid_def_icon

  USE mo_cosmo_grid,          ONLY: lon_rot, lat_rot

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: write_netcdf_buffer_cru, &
       &    write_netcdf_cosmo_grid_cru, &
       &    write_netcdf_icon_grid_cru, &
       &    read_netcdf_buffer_cru

  CONTAINS

  SUBROUTINE write_netcdf_buffer_cru(netcdf_filename,   &
       &                                     tg,        &
       &                                     undefined, &
       &                                     lon_geo,   &
       &                                     lat_geo,   &
       &                                     crutemp,   &
       &                                     cruelev)

    CHARACTER (len=*), INTENT(IN)       :: netcdf_filename !< filename for the netcdf file
    TYPE(target_grid_def), INTENT(IN)   :: tg !< structure with target grid description
    REAL(KIND=wp), INTENT(IN)           :: undefined, &       !< value to indicate undefined grid elements
         &                                 lon_geo(:,:,:), &  !< longitude coordinates of the target grid in the geographical system
         &                                 lat_geo(:,:,:), &  !< latitude coordinates of the target grid in the geographical system
         &                                 crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke)
    REAL(KIND=wp), OPTIONAL, INTENT(IN) :: cruelev(:,:,:)  !< cru elevation , cruelev(ie,je,ke)

    ! local variables
    INTEGER (KIND=i4)                   :: ndims, ncid, errorcode
    INTEGER, PARAMETER                  :: nglob_atts=6

    TYPE(dim_meta_info), ALLOCATABLE    :: dim_list(:) !< dimensions for netcdf file

    TYPE(netcdf_attributes)             :: global_attributes(nglob_atts)

    CALL logging%info('Enter routine: write_netcdf_buffer_cru')
    !-------------------------------------------------------------
    ! define global attributes
    CALL set_global_att_crutemp(global_attributes)

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)
    ! dim_3d_tg

    ! define meta information for variable crutemp for netcdf output
    CALL def_crutemp_meta(dim_3d_tg)
    ! crutemp_meta

    IF(PRESENT(cruelev)) THEN
      CALL def_cruelev_meta(dim_3d_tg)
    ENDIF
    ! cruelev_meta

    ! define meta information for target field variables lon_geo, lat_geo
    CALL def_com_target_fields_meta(dim_3d_tg)
    ! lon_geo_meta and lat_geo_meta


    !set up dimensions for buffer netcdf output
    ndims = 3
    ALLOCATE(dim_list(1:ndims),STAT=errorcode)
    IF (errorcode /= 0 ) CALL logging%error('Cant allocate array dim_list',__FILE__,__LINE__)
    dim_list = dim_3d_tg

    !-----------------------------------------------------------------

    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
         &                    dim_list=dim_list,                       &
         &                    global_attributes=global_attributes,     &
         &                    ncid=ncid)

    ! crutemp
    CALL netcdf_put_var(ncid,crutemp,crutemp_meta,undefined)

    ! cruelev
    IF(PRESENT(cruelev)) THEN
      CALL netcdf_put_var(ncid,cruelev,cruelev_meta,undefined)
    ENDIF

    ! lon
    CALL netcdf_put_var(ncid,lon_geo,lon_geo_meta,undefined)

    ! lat
    CALL netcdf_put_var(ncid,lat_geo,lat_geo_meta,undefined)

    CALL close_netcdf_file(ncid)

    CALL logging%info('Exit routine: write_netcdf_buffer_cru')

  END SUBROUTINE write_netcdf_buffer_cru
  !------------------------------------------------------------------

  SUBROUTINE write_netcdf_cosmo_grid_cru(netcdf_filename,  &
       &                                     cosmo_grid,       &
       &                                     tg,         &
       &                                     undefined, &
       &                                     lon_geo,     &
       &                                     lat_geo, &
       &                                     crutemp, &
       &                                     cruelev)

    CHARACTER (len=*), INTENT(IN)         :: netcdf_filename !< filename for the netcdf file
    TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid
    TYPE(target_grid_def), INTENT(IN)     :: tg !< structure with target grid description
    REAL(KIND=wp), INTENT(IN)             :: undefined, &       !< value to indicate undefined grid elements
         &                                   lon_geo(:,:,:), &  !< longitude coordinates of the target grid in the geographical system
         &                                   lat_geo(:,:,:), &  !< latitude coordinates of the target grid in the geographical system
         &                                   crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke)
    REAL(KIND=wp), OPTIONAL, INTENT(IN)   :: cruelev(:,:,:)  !< cru elevation (ie,je,ke)

    ! local variables
    INTEGER(KIND=i4)                      :: ndims, ncid, errorcode
    INTEGER (KINd=i4), PARAMETER          :: nglob_atts=6

    TYPE(dim_meta_info), ALLOCATABLE      :: dim_list(:) !< dimensions for netcdf file
    TYPE(netcdf_attributes)               :: global_attributes(nglob_atts)

    CHARACTER (len=80)                    :: grid_mapping, & !< netcdf attribute grid mapping
         &                                   coordinates  !< netcdf attribute coordinates

    CALL logging%info('Enter routine: write_netcdf_cosmo_grid_cru')
    !-------------------------------------------------------------
    ! define global attributes
    CALL set_global_att_crutemp(global_attributes)

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

    ! define meta information for variable crutemp for netcdf output
    CALL def_crutemp_meta(dim_2d_cosmo,coordinates,grid_mapping)
    ! crutemp_meta

    IF(PRESENT(cruelev)) THEN
      CALL def_cruelev_meta(dim_2d_cosmo,coordinates,grid_mapping)
      ! cruelev_meta
    ENDIF

    ! define meta information for target field variables lon_geo, lat_geo
    CALL def_com_target_fields_meta(dim_2d_cosmo,coordinates,grid_mapping)
    ! lon_geo_meta and lat_geo_meta

    !set up dimensions for buffer netcdf output
    ndims = 2
    ALLOCATE(dim_list(1:ndims),STAT=errorcode)
    IF (errorcode /= 0 ) CALL logging%error('Cant allocate array dim_list',__FILE__,__LINE__)

    dim_list(1) = dim_rlon_cosmo(1) ! rlon
    dim_list(2) = dim_rlat_cosmo(1) ! rlat


    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
         &                       dim_list=dim_list,                  &
         &                       global_attributes=global_attributes, &
         &                       ncid=ncid)
    !-----------------------------------------------------------------


    !-----------------------------------------------------------------
    ! start with real 1d variables

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

    ! crutemp
    CALL netcdf_put_var(ncid,crutemp(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
         &                crutemp_meta,undefined)

    IF(PRESENT(cruelev)) THEN
      !cruelev
      CALL netcdf_put_var(ncid,cruelev(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
           &                cruelev_meta,undefined)
    ENDIF

    CALL close_netcdf_file(ncid)

    CALL logging%info('Exit routine: write_netcdf_cosmo_grid_cru')

  END SUBROUTINE write_netcdf_cosmo_grid_cru
  !-----------------------------------------------------------------------

  SUBROUTINE write_netcdf_icon_grid_cru(netcdf_filename,  &
       &                                     icon_grid,       &
       &                                     tg,         &
       &                                     undefined, &
       &                                     lon_geo,     &
       &                                     lat_geo, &
       &                                     crutemp, &
       &                                     cruelev)

    CHARACTER (len=*), INTENT(IN)                 :: netcdf_filename !< filename for the netcdf file
    TYPE(icosahedral_triangular_grid), INTENT(IN) :: icon_grid !< structure which contains the definition of the ICON grid
    TYPE(target_grid_def), INTENT(IN)             :: tg !< structure with target grid description
    REAL(KIND=wp), INTENT(IN)                     :: undefined, &       !< value to indicate undefined grid elements
         &                                           lon_geo(:,:,:), &  !< longitude coordinates of the target grid in the geographical system
         &                                           lat_geo(:,:,:), &  !< latitude coordinates of the target grid in the geographical system
         &                                           crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke)

    REAL(KIND=wp), OPTIONAL, INTENT(IN)           :: cruelev(:,:,:)  !< cru elevation , cruelev(ie,je,ke)

    ! local variables
    INTEGER(KIND=i4)                              :: ndims, ncid, errorcode
    INTEGER(KIND=i4), PARAMETER                   :: nglob_atts=6

    TYPE(dim_meta_info), ALLOCATABLE              :: dim_list(:) !< dimensions for netcdf file
    TYPE(netcdf_attributes)                       :: global_attributes(nglob_atts)

    CHARACTER (len=80)                            :: grid_mapping !< netcdf attribute grid mapping

    CALL logging%info('Enter routine: write_netcdf_icon_grid_cru')

    !-------------------------------------------------------------
    ! define global attributes
    CALL set_global_att_crutemp(global_attributes)

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)
    ! dim_3d_tg

    !set up dimensions for ICON grid
    CALL def_dimension_info_icon(icon_grid)
    ! dim_icon

    ! define meta information for variable crutemp for netcdf output
    CALL def_crutemp_meta(dim_icon)
    ! crutemp_meta

    IF(PRESENT(cruelev)) THEN
      CALL def_cruelev_meta(dim_icon)
      ! cruelev_meta
    ENDIF

    ! define meta information for target field variables lon_geo, lat_geo
    CALL def_com_target_fields_meta(dim_icon)
    ! lon_geo_meta and lat_geo_meta

    ! set mapping parameters for netcdf
    grid_mapping="lon_lat_on_sphere"

    CALL set_nc_grid_def_icon(grid_mapping)
    ! nc_grid_def_icon

    !set up dimensions for buffer netcdf output
    ndims = 1
    ALLOCATE(dim_list(1:ndims),STAT=errorcode)
    IF (errorcode /= 0 ) CALL logging%error('Cant allocate array dim_list',__FILE__,__LINE__)

    dim_list = dim_icon(1) ! cell

    !-----------------------------------------------------------------
    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
         &                       dim_list=dim_list,                  &
         &                       global_attributes=global_attributes, &
         &                       ncid=ncid)
    !-----------------------------------------------------------------
    ! lon
    CALL netcdf_put_var(ncid,lon_geo(1:icon_grid%ncell,1,1),lon_geo_meta,undefined)

    ! lat
    CALL netcdf_put_var(ncid,lat_geo(1:icon_grid%ncell,1,1),lat_geo_meta,undefined)

    ! crutemp
    CALL netcdf_put_var(ncid,crutemp(1:icon_grid%ncell,1,1),crutemp_meta,undefined)

    !cruelev
    IF(PRESENT(cruelev)) THEN
      CALL netcdf_put_var(ncid,cruelev(1:icon_grid%ncell,1,1), cruelev_meta,undefined)
    ENDIF

    CALL close_netcdf_file(ncid)

    CALL logging%info('Exit routine: write_netcdf_icon_grid_cru')

  END SUBROUTINE write_netcdf_icon_grid_cru
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !> set global attributes for netcdf with soiltype data
  SUBROUTINE set_global_att_crutemp(global_attributes)
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
    global_attributes(1)%attributetext='CRU Climatology'
    global_attributes(2)%attname = 'institution'
    global_attributes(2)%attributetext='DWD'

    global_attributes(3)%attname = 'source'
    global_attributes(3)%attributetext='CRU'

    CALL DATE_AND_TIME(ydate,ytime)
    READ(ydate,'(4A2)') cc,yy,mm,dd
    READ(ytime,'(2A2)') hh, minute

    ydate=TRIM(cc)//TRIM(yy)//'-'//TRIM(mm)//'-'//TRIM(dd)
    ytime=TRIM(hh)//':'//TRIM(minute)

    global_attributes(4)%attname = 'history'
    global_attributes(4)%attributetext=TRIM(ydate)//'T'//TRIM(ytime)//' cru_to_buffer'

    global_attributes(5)%attname = 'references'
    global_attributes(5)%attributetext='http://www.cru.uea.ac.uk/cru/data/temperature/'

    global_attributes(6)%attname = 'comment'
    global_attributes(6)%attributetext='CRU climatological temperature data regridded at DWD'

  END SUBROUTINE set_global_att_crutemp

  SUBROUTINE read_netcdf_buffer_cru(netcdf_filename,  &
       &                                     tg,         &
       &                                     crutemp,    &
       &                                      cruelev)

    CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
    TYPE(target_grid_def), INTENT(IN)  :: tg !< structure with target grid description
    REAL(KIND=wp), INTENT(OUT)         :: crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke)
    REAL(KIND=wp), OPTIONAL,INTENT(OUT):: cruelev(:,:,:)  !< cru elevation , cruelev(ie,je,ke)

    CALL logging%info('Enter routine: read_netcdf_buffer_cru')

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)

    ! define meta information for variable crutemp for netcdf output
    CALL def_crutemp_meta(dim_3d_tg)
    ! crutemp_meta

    CALL netcdf_get_var(TRIM(netcdf_filename),crutemp_meta,crutemp)

    IF(PRESENT(cruelev)) THEN
      CALL def_cruelev_meta(dim_3d_tg)
      CALL netcdf_get_var(TRIM(netcdf_filename),cruelev_meta,cruelev)
    ENDIF

    CALL logging%info('Exit routine: read_netcdf_buffer_cru')
  END SUBROUTINE read_netcdf_buffer_cru

END MODULE mo_cru_output_nc
