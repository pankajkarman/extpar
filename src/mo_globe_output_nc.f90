!+ Fortran module for netcdf output of GLOBE data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_4         2011/04/21 Hermann Asensio
!  clean up
! V1_7         2013/01/25 Guenther Zaengl 
!   Parallel threads for ICON and COSMO using Open-MP, 
!   Several bug fixes and optimizations for ICON search algorithm, 
!   particularly for the special case of non-contiguous domains; 
!   simplified namelist control for ICON  
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for netcdf output of GLOBE data
!> \author Hermann Asensio
MODULE mo_globe_output_nc
  
  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  !> data type structures form module GRID_structures
  USE mo_grid_structures, ONLY: reg_lonlat_grid
  USE mo_grid_structures, ONLY: rotated_lonlat_grid
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid
  USE mo_grid_structures, ONLY: target_grid_def

  USE mo_cosmo_grid,      ONLY: cosmo_grid, nborder

  USE mo_topo_data,       ONLY: nhori

  USE mo_io_utilities, ONLY: var_meta_info
  USE mo_io_utilities, ONLY: netcdf_attributes

  USE mo_io_utilities, ONLY: dim_meta_info

  USE mo_io_utilities, ONLY: netcdf_put_var
  USE mo_io_utilities, ONLY: open_new_netcdf_file
  USE mo_io_utilities, ONLY: close_netcdf_file
  USE mo_io_utilities, ONLY: netcdf_def_grid_mapping

  USE mo_io_utilities, ONLY: vartype_int 
  USE mo_io_utilities, ONLY: vartype_real
  USE mo_io_utilities, ONLY: vartype_char

  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: write_netcdf_buffer_globe
  PUBLIC :: write_netcdf_cosmo_grid_globe
  PUBLIC :: write_netcdf_icon_grid_globe

  PUBLIC :: read_netcdf_buffer_globe



  CONTAINS

    !> create a netcdf file for the fields derived from GLOBE data to the buffer 
   SUBROUTINE write_netcdf_buffer_globe(netcdf_filename,  &
     &                                     tg,         &
     &                                     undefined, &
     &                                     undef_int,   &
     &                                     lon_geo,     &
     &                                     lat_geo, &
     &                                     fr_land_globe, &
     &                                     hh_globe,            &
     &                                     stdh_globe,          &
     &                                     theta_globe,         &
     &                                     aniso_globe,         &
     &                                     slope_globe,         &
     &                                     z0_topo,             &
     &                                     lrad,                &
     &                                     nhori,               &
     &                                     vertex_param,        &
     &                                     slope_asp_globe,     &
     &                                     slope_ang_globe,     &
     &                                     horizon_globe,       &
     &                                     skyview_globe)

   USE mo_var_meta_data, ONLY: dim_3d_tg, dim_4d_tg,    &
    &                         def_dimension_info_buffer

   USE mo_globe_tg_fields, ONLY: add_parameters_domain

   USE mo_var_meta_data, ONLY: lon_geo_meta, &
     &                         lat_geo_meta, &
     &                         no_raw_data_pixel_meta, &
     &                         def_com_target_fields_meta  
     
   USE mo_var_meta_data, ONLY: def_globe_meta, def_globe_vertex_meta
   USE mo_var_meta_data, ONLY: dim_buffer_vertex

   USE mo_var_meta_data, ONLY: hh_globe_meta, fr_land_globe_meta, &
    &       stdh_globe_meta, theta_globe_meta, &
    &       aniso_globe_meta, slope_globe_meta, &
    &       hh_vert_meta, npixel_vert_meta, z0_topo_meta, &
    &       slope_asp_globe_meta, slope_ang_globe_meta,   &
    &       horizon_globe_meta, skyview_globe_meta

   
   CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
   TYPE(target_grid_def)              :: tg !< structure with target grid description
   REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
   INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
   REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
   REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system

   REAL(KIND=wp), INTENT(IN)  :: hh_globe(:,:,:)  !< mean height 
   REAL(KIND=wp), INTENT(IN)  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height

   REAL(KIND=wp), INTENT(IN)  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
   REAL(KIND=wp), INTENT(IN)  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
   REAL(KIND=wp), INTENT(IN)  :: slope_globe(:,:,:) !< sso parameter, mean slope
   REAL(KIND=wp), INTENT(IN)  :: fr_land_globe(:,:,:) !< fraction land due to GLOBE raw data
   REAL(KIND=wp), INTENT(IN)  :: z0_topo(:,:,:) !< roughness length due to orography

   LOGICAL,         INTENT(IN)  :: lrad    
   INTEGER(KIND=i4),INTENT(IN)  :: nhori    

   TYPE(add_parameters_domain), INTENT(IN), OPTIONAL :: vertex_param  !< additional external parameters for ICON domain

   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: slope_asp_globe(:,:,:)   !< lradtopo parameter, slope_aspect
   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: slope_ang_globe(:,:,:)   !< lradtopo parameter, slope_angle
   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: horizon_globe  (:,:,:,:) !< lradtopo parameter, horizon
   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: skyview_globe  (:,:,:)   !< lradtopo parameter, skyview


   ! local variables
  INTEGER :: n_3d_real = 0 !< number of 3D real variables
  INTEGER :: n_3d_real_buffer_cell = 0 !< number of 3D real variables wich are defined as mean of the cell
  INTEGER :: n_3d_real_buffer_vertex = 0 !< number of 3D real variables wich are defined on the vertices of the cell



  INTEGER :: n_3d_int = 0 !< number of 3D integer variables

  INTEGER :: ndims 
  INTEGER :: ncid
  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file

  INTEGER :: nvertex !< total number of vertices


  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)

  INTEGER :: errorcode !< error status variable

  INTEGER :: n !< counter

  INTEGER (KIND=i8) :: istart, iend, jstart, jend
  INTEGER (KIND=i8) :: tmp_nlon, tmp_nlat

  PRINT *,'ENTER write_netcdf_buffer_globe'

  PRINT *,'set_global_att_globe'

  !-------------------------------------------------------------
  ! define global attributes
  CALL set_global_att_globe(global_attributes)
  PRINT *,'def_dimension_info_buffer'


  ! correct dimensions in case of lradtopo
  IF (lrad) THEN
    tmp_nlon = cosmo_grid%nlon_rot
    tmp_nlat = cosmo_grid%nlat_rot
    cosmo_grid%nlon_rot = tmp_nlon - 2 * nborder
    cosmo_grid%nlat_rot = tmp_nlat - 2 * nborder
    tg%ie = cosmo_grid%nlon_rot
    tg%je = cosmo_grid%nlat_rot
  ENDIF


   !set up dimensions for buffer
  CALL  def_dimension_info_buffer(tg,nhori=nhori)
  ! dim_3d_tg, dim_4d_tg
  PRINT *,'target grid tg: ',tg%ie, tg%je, tg%ke, tg%minlon, tg%maxlon, tg%minlat, tg%maxlat
  PRINT *,'dim_3d_tg: ', dim_3d_tg
  PRINT *,'dim_4d_tg: ', dim_4d_tg
  PRINT *,'undefined: ', undefined
  PRINT *,'undef_int: ', undef_int


  
  PRINT *,'def_com_target_fields_meta'
  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_3d_tg)
  ! lon_geo_meta and lat_geo_meta
   PRINT *,'def_globe_meta'
  ! define meta information for various GLOBE data related variables for netcdf output
  IF (lrad) THEN
    CALL def_globe_meta(dim_3d_tg,diminfohor=dim_4d_tg)
    !  hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta, z0_topo_meta, 
    !         slope_asp_globe_meta, slope_ang_globe_meta, 
    !         horizon_globe_meta, skyview_globe_meta
  ELSE
    CALL def_globe_meta(dim_3d_tg)
    !  hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta, z0_topo_meta
  ENDIF
  PRINT *,'set dimensions'
  !set up dimensions for buffer netcdf output 
  IF (PRESENT(vertex_param))  THEN
    nvertex = SIZE(vertex_param%npixel_vert,1)

    CALL def_globe_vertex_meta(nvertex)
    ! dim_buffer_vertex
    !  hh_vert_meta, npixel_vert_meta
    ndims = SIZE(dim_3d_tg) + 3
  ELSE
    ndims = SIZE(dim_3d_tg)
  ENDIF
  IF(lrad) ndims = ndims + 1
  ALLOCATE(dim_list(1:ndims),STAT=errorcode)
  IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')
  IF (PRESENT(vertex_param))  THEN
    dim_list(1:3) = dim_3d_tg
    dim_list(4:6) = dim_buffer_vertex(1:3)
  ELSE
    IF(lrad) THEN
      dim_list = dim_4d_tg
    ELSE
      dim_list = dim_3d_tg
    ENDIF
  ENDIF

  !-----------------------------------------------------------------
  CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
      &                       dim_list=dim_list,                  &
      &                       global_attributes=global_attributes, &
      &                       ncid=ncid)

  ! correct start and stop indices if needed
  IF (lrad) THEN
    istart = nborder + 1
    jstart = nborder + 1
    iend   = nborder + cosmo_grid%nlon_rot 
    jend   = nborder + cosmo_grid%nlat_rot 
  ELSE
    istart = 1
    jstart = 1
    iend   = cosmo_grid%nlon_rot
    jend   = cosmo_grid%nlat_rot      
  ENDIF


  ! lon
  CALL netcdf_put_var(ncid,lon_geo(istart:iend,jstart:jend,:),lon_geo_meta,undefined)

  ! lat
  CALL netcdf_put_var(ncid,lat_geo(istart:iend,jstart:jend,:),lat_geo_meta,undefined)

  ! hh_globe
  CALL netcdf_put_var(ncid,hh_globe(istart:iend,jstart:jend,:),hh_globe_meta,undefined)

  ! stdh_globe
  CALL netcdf_put_var(ncid,stdh_globe(istart:iend,jstart:jend,:),stdh_globe_meta,undefined)

  ! theta_globe
  CALL netcdf_put_var(ncid,theta_globe(istart:iend,jstart:jend,:),theta_globe_meta,undefined)

  ! aniso_globe
  CALL netcdf_put_var(ncid,aniso_globe(istart:iend,jstart:jend,:),aniso_globe_meta,undefined)

  ! slope_globe
  CALL netcdf_put_var(ncid,slope_globe(istart:iend,jstart:jend,:),slope_globe_meta,undefined)

  ! fr_land_globe
  CALL netcdf_put_var(ncid,fr_land_globe(istart:iend,jstart:jend,:),fr_land_globe_meta,undefined)

  ! z0_topo
  CALL netcdf_put_var(ncid,z0_topo(istart:iend,jstart:jend,:),z0_topo_meta,undefined)

  IF (PRESENT(vertex_param)) THEN
  ! hh_vert
  CALL netcdf_put_var(ncid,vertex_param%hh_vert(1:nvertex,1:1,1:1), &
    &                 hh_vert_meta,undefined)
  ENDIF

  ! slope_asp_globe
  IF (PRESENT(slope_asp_globe)) THEN
    CALL netcdf_put_var(ncid,slope_asp_globe(istart:iend,jstart:jend,:),slope_asp_globe_meta,undefined)
  ENDIF

  ! slope_ang_globe
  IF (PRESENT(slope_ang_globe)) THEN
    CALL netcdf_put_var(ncid,slope_ang_globe(istart:iend,jstart:jend,:),slope_ang_globe_meta,undefined)
  ENDIF

  ! horizon_globe
  IF (PRESENT(horizon_globe)) THEN
    CALL netcdf_put_var(ncid,horizon_globe(istart:iend,jstart:jend,:,:),horizon_globe_meta,undefined)
  ENDIF

  ! skyview_globe
  IF (PRESENT(skyview_globe)) THEN
    CALL netcdf_put_var(ncid,skyview_globe(istart:iend,jstart:jend,:),skyview_globe_meta,undefined)
  ENDIF

  CALL close_netcdf_file(ncid)
   
  END SUBROUTINE write_netcdf_buffer_globe

!> create a netcdf file for the fields derived from GLOBE data to the COSMO grid
  SUBROUTINE write_netcdf_cosmo_grid_globe(netcdf_filename,  &
     &                                     cosmo_grid,       &
     &                                     tg,         &
     &                                     undefined, &
     &                                     undef_int,   &
     &                                     lon_geo,     &
     &                                     lat_geo, &
     &                                     fr_land_globe, &
     &                                     hh_globe,            &
     &                                     stdh_globe,          &
     &                                     theta_globe,         &
     &                                     aniso_globe,         &
     &                                     slope_globe,         &
     &                                     z0_topo,             & 
     &                                     lrad,                &
     &                                     nhori,               &
     &                                     slope_asp_globe,     &
     &                                     slope_ang_globe,     &
     &                                     horizon_globe,       &
     &                                     skyview_globe)
   
   USE mo_var_meta_data, ONLY: nc_grid_def_cosmo, &
    &                         set_nc_grid_def_cosmo


   USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                         def_dimension_info_buffer

   USE mo_var_meta_data, ONLY: lon_geo_meta, &
     &                         lat_geo_meta, &
     &                         no_raw_data_pixel_meta, &
     &                         def_com_target_fields_meta  
     
   USE mo_var_meta_data, ONLY: def_globe_meta

   USE mo_var_meta_data, ONLY: hh_globe_meta, fr_land_globe_meta, &
    &       stdh_globe_meta, theta_globe_meta, &
    &       aniso_globe_meta, slope_globe_meta, z0_topo_meta, &
    &       slope_asp_globe_meta, slope_ang_globe_meta,   &
    &       horizon_globe_meta, skyview_globe_meta

   USE mo_var_meta_data, ONLY: dim_rlon_cosmo, &
    &                         dim_rlat_cosmo, &
    &                         dim_nhori_cosmo, &
    &                         dim_2d_cosmo,   &
    &                         dim_3d_cosmo,   &
    &                         rlon_meta,      &
    &                         rlat_meta,      &
    &                         def_dimension_info_cosmo

   USE mo_cosmo_grid, ONLY: lon_rot, lat_rot


   
   CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
   TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid
   TYPE(target_grid_def), INTENT(IN)  :: tg !< structure with target grid description
   REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
   INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
   REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
   REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system

   REAL(KIND=wp), INTENT(IN)  :: hh_globe(:,:,:)  !< mean height 
   REAL(KIND=wp), INTENT(IN)  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height

   REAL(KIND=wp), INTENT(IN)  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
   REAL(KIND=wp), INTENT(IN)  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
   REAL(KIND=wp), INTENT(IN)  :: slope_globe(:,:,:) !< sso parameter, mean slope
   REAL(KIND=wp), INTENT(IN)  :: fr_land_globe(:,:,:) !< fraction land due to GLOBE raw data
   REAL(KIND=wp), INTENT(IN)  :: z0_topo(:,:,:) !< roughness length due to orography

   LOGICAL,       INTENT(IN)  :: lrad    
   INTEGER(KIND=i4), INTENT(IN) :: nhori

   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: slope_asp_globe(:,:,:)   !< lradtopo parameter, slope_aspect
   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: slope_ang_globe(:,:,:)   !< lradtopo parameter, slope_angle
   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: horizon_globe  (:,:,:,:) !< lradtopo parameter, horizon
   REAL(KIND=wp), INTENT(IN), OPTIONAL  :: skyview_globe  (:,:,:)   !< lradtopo parameter, skyview

   ! local variables

  INTEGER :: ndims  
  INTEGER :: ncid
  INTEGER :: varid
  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file

  INTEGER :: nvertex !< total number of vertices

  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)

  INTEGER :: errorcode !< error status variable

  INTEGER :: n !< counter

  CHARACTER (len=80):: grid_mapping !< netcdf attribute grid mapping
  CHARACTER (len=80):: coordinates  !< netcdf attribute coordinates

  INTEGER(KIND=i8) :: istart, iend, jstart, jend

  PRINT *,'ENTER write_netcdf_cosmo_grid_globe'

  !-------------------------------------------------------------
  ! define global attributes
  CALL set_global_att_globe(global_attributes)

  !set up dimensions for buffer
  CALL  def_dimension_info_buffer(tg,nhori=nhori)
  ! dim_3d_tg, dim_4d_tg

  !set up dimensions for COSMO grid
  CALL def_dimension_info_cosmo(cosmo_grid,nhori=nhori)
  ! dim_rlon_cosmo, dim_rlat_cosmo, dim_2d_cosmo, rlon_meta, rlat_meta
  ! set mapping parameters for netcdf
  grid_mapping="rotated_pole"
  coordinates="lon lat"

  CALL set_nc_grid_def_cosmo(cosmo_grid,grid_mapping)
  ! nc_grid_def_cosmo


  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_2d_cosmo,coordinates,grid_mapping)
  ! lon_geo_meta and lat_geo_meta

  ! define meta information for various GLOBE data related variables for netcdf output
  IF(lrad) THEN
    CALL def_globe_meta(dim_2d_cosmo,coordinates=coordinates,grid_mapping=grid_mapping,diminfohor=dim_3d_cosmo)
    !  hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta, z0_topo_meta
    !         slope_asp_globe_meta, slope_ang_globe_meta, 
    !         horizon_globe_meta, skyview_globe_meta
  ELSE
    CALL def_globe_meta(dim_2d_cosmo,coordinates=coordinates,grid_mapping=grid_mapping)
    !  hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta, z0_topo_meta
  ENDIF

  PRINT *,'SET dimensions'


  !set up dimensions for netcdf output 
   ndims = SIZE(dim_2d_cosmo)
   IF (lrad) ndims = ndims + 1
   ALLOCATE(dim_list(1:ndims),STAT=errorcode)
   IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')

   dim_list(1) = dim_rlon_cosmo(1) ! rlon
   dim_list(2) = dim_rlat_cosmo(1) ! rlat
   IF (lrad) dim_list(3) = dim_nhori_cosmo(1) ! nhori

   !-----------------------------------------------------------------
    PRINT *,' CALL open_new_netcdf_file'
    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
      &                       dim_list=dim_list,                  &
      &                       global_attributes=global_attributes, &
      &                       ncid=ncid)
    !-----------------------------------------------------------------
    ! start with real 1d variables

    IF (lrad) THEN
      istart = nborder + 1
      jstart = nborder + 1
      iend   = nborder + cosmo_grid%nlon_rot 
      jend   = nborder + cosmo_grid%nlat_rot 
    ELSE
      istart = 1
      jstart = 1
      iend   = cosmo_grid%nlon_rot
      jend   = cosmo_grid%nlat_rot      
    ENDIF

    ! rlon, rlat
    CALL netcdf_put_var(ncid,lon_rot(istart:iend),rlon_meta,undefined)

    CALL netcdf_put_var(ncid,lat_rot(jstart:jend),rlat_meta,undefined)

    ! lon
    CALL netcdf_put_var(ncid,lon_geo(istart:iend,jstart:jend,1), &
      &                 lon_geo_meta,undefined)

    ! lat
    CALL netcdf_put_var(ncid,lat_geo(istart:iend,jstart:jend,1), &
      &                 lat_geo_meta,undefined)

    ! hh_globe
    CALL netcdf_put_var(ncid, hh_globe(istart:iend,jstart:jend,1), &
      &                 hh_globe_meta,undefined)

    ! stdh_globe
    CALL netcdf_put_var(ncid,stdh_globe(istart:iend,jstart:jend,1), &
      &                 stdh_globe_meta,undefined)

    ! theta_globe
    CALL netcdf_put_var(ncid,theta_globe(istart:iend,jstart:jend,1), &
      &                 theta_globe_meta,undefined)

    ! aniso_globe
    CALL netcdf_put_var(ncid,aniso_globe(istart:iend,jstart:jend,1), &
      &                 aniso_globe_meta,undefined)

    ! slope_globe
    CALL netcdf_put_var(ncid,slope_globe(istart:iend,jstart:jend,1), &
      &                 slope_globe_meta,undefined)
    
    ! fr_land_globe
    CALL netcdf_put_var(ncid,fr_land_globe(istart:iend,jstart:jend,1), &
      &                 fr_land_globe_meta,undefined)

    ! z0_topo
    CALL netcdf_put_var(ncid,z0_topo(istart:iend,jstart:jend,1), &
      &                 z0_topo_meta,undefined)

    ! slope_asp_globe
    IF (PRESENT(slope_asp_globe)) THEN
      CALL netcdf_put_var(ncid,slope_asp_globe(istart:iend,jstart:jend,1),slope_asp_globe_meta,undefined)
    ENDIF
  
    ! slope_ang_globe
    IF (PRESENT(slope_ang_globe)) THEN
      CALL netcdf_put_var(ncid,slope_ang_globe(istart:iend,jstart:jend,1),slope_ang_globe_meta,undefined)
    ENDIF
  
    ! horizon_globe
    IF (PRESENT(horizon_globe)) THEN
      CALL netcdf_put_var(ncid,horizon_globe(istart:iend,jstart:jend,1,:),horizon_globe_meta,undefined)
    ENDIF
  
    ! skyview_globe
    IF (PRESENT(skyview_globe)) THEN
      CALL netcdf_put_var(ncid,skyview_globe(istart:iend,jstart:jend,1),skyview_globe_meta,undefined)
    ENDIF

     !-----------------------------------------------------------------

    CALL netcdf_def_grid_mapping(ncid, nc_grid_def_cosmo, varid)

    CALL close_netcdf_file(ncid)

   
  END SUBROUTINE write_netcdf_cosmo_grid_globe

!> create a netcdf file for the fields derived from GLOBE data to the ICON grid 
   SUBROUTINE write_netcdf_icon_grid_globe(netcdf_filename,  &
     &                                     icon_grid,       &
     &                                     tg,         &
     &                                     undefined, &
     &                                     undef_int,   &
     &                                     lon_geo,     &
     &                                     lat_geo, &
     &                                     fr_land_globe, &
     &                                     hh_globe,            &
     &                                     stdh_globe,          &
     &                                     theta_globe,         &
     &                                     aniso_globe,         &
     &                                     slope_globe,         &
     &                                     z0_topo,             &
     &                                     vertex_param)

   USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                         def_dimension_info_buffer

   USE mo_globe_tg_fields, ONLY: add_parameters_domain

   USE mo_var_meta_data, ONLY: lon_geo_meta, &
     &                         lat_geo_meta, &
     &                         no_raw_data_pixel_meta, &
     &                         def_com_target_fields_meta  
     
   USE mo_var_meta_data, ONLY: def_globe_meta, def_globe_vertex_meta

   USE mo_var_meta_data, ONLY: hh_globe_meta, fr_land_globe_meta, &
    &       stdh_globe_meta, theta_globe_meta, &
    &       aniso_globe_meta, slope_globe_meta, &
    &       hh_vert_meta, npixel_vert_meta, z0_topo_meta
  
   USE mo_var_meta_data, ONLY:  dim_icon, &
     &                          def_dimension_info_icon

   USE mo_var_meta_data, ONLY: nc_grid_def_icon, &
     &                         set_nc_grid_def_icon



   
   CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
   TYPE(icosahedral_triangular_grid), INTENT(IN) :: icon_grid !< structure which contains the definition of the ICON grid
   TYPE(target_grid_def), INTENT(IN)  :: tg !< structure with target grid description
   REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
   INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
   REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
   REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system

   REAL(KIND=wp), INTENT(IN)  :: hh_globe(:,:,:)  !< mean height 
   REAL(KIND=wp), INTENT(IN)  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height

   REAL(KIND=wp), INTENT(IN)  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
   REAL(KIND=wp), INTENT(IN)  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
   REAL(KIND=wp), INTENT(IN)  :: slope_globe(:,:,:) !< sso parameter, mean slope
   REAL(KIND=wp), INTENT(IN)  :: fr_land_globe(:,:,:) !< fraction land due to GLOBE raw data
   REAL(KIND=wp), INTENT(IN)  :: z0_topo(:,:,:) !< roughness length due to orography


   TYPE(add_parameters_domain), INTENT(IN) :: vertex_param  !< additional external parameters for ICON domain

   ! local variables

  
  INTEGER :: ndims  
  INTEGER :: ncid
  INTEGER :: varid

  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file
  TYPE(dim_meta_info), TARGET :: dim_icon_cell(1:1)
  TYPE(dim_meta_info), TARGET :: dim_icon_vertex(1:1)

  INTEGER :: nvertex !< total number of vertices
  
  CHARACTER (len=80):: grid_mapping !< netcdf attribute grid mapping
  CHARACTER (len=80):: coordinates  !< netcdf attribute coordinates

  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)

  INTEGER :: errorcode !< error status variable

  INTEGER :: n !< counter

  PRINT *,'ENTER write_netcdf_icon_grid_globe'
  PRINT *,'set_global_att_globe'
  !-------------------------------------------------------------
  ! define global attributes
  CALL set_global_att_globe(global_attributes)
PRINT *,'def_dimension_info_buffer'

   !set up dimensions for buffer
  CALL  def_dimension_info_buffer(tg)
  ! dim_3d_tg
  PRINT *,'def_com_target_fields_meta'


   !set up dimensions for ICON grid
  CALL def_dimension_info_icon(icon_grid)
  ! dim_icon

  ! set mapping parameters for netcdf
  grid_mapping="lon_lat_on_sphere"
  coordinates="lon lat"
  CALL set_nc_grid_def_icon(grid_mapping)
  ! nc_grid_def_icon
  
  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_icon)
  ! lon_geo_meta and lat_geo_meta


  ! define meta information for various GLOBE data related variables for netcdf output
  CALL def_globe_meta(dim_icon)

  !  hh_globe_meta, fr_land_globe_meta, &
  !         stdh_globe_meta, theta_globe_meta, &
  !         aniso_globe_meta, slope_globe_meta, &
  !         hh_vert_meta, npixel_vert_meta, z0_topo_meta
  !\TODO HA: this is a "quick fix" for ICON, find a better solution
  hh_globe_meta%varname = 'topography_c'

  !set up dimensions for buffer netcdf output 
   nvertex = icon_grid%nvertex
   CALL def_globe_vertex_meta(nvertex)
   !  hh_vert_meta, npixel_vert_meta
 PRINT *,'set dimensions'

    ndims = 2
  ALLOCATE(dim_list(1:ndims),STAT=errorcode)
  IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')
    dim_list(1) = dim_icon(1) ! cell
    dim_list(2) = dim_icon(2) ! vertex

    dim_icon_cell = dim_icon(1)     ! cell
    dim_icon_vertex =  dim_icon(2) ! vertex


    
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

   ! hh_globe
   CALL netcdf_put_var(ncid,hh_globe(1:icon_grid%ncell,1,1),hh_globe_meta,undefined)

   ! stdh_globe
   CALL netcdf_put_var(ncid,stdh_globe(1:icon_grid%ncell,1,1),stdh_globe_meta,undefined)

   ! theta_globe
   CALL netcdf_put_var(ncid,theta_globe(1:icon_grid%ncell,1,1),theta_globe_meta,undefined)

   ! aniso_globe
   CALL netcdf_put_var(ncid,aniso_globe(1:icon_grid%ncell,1,1),aniso_globe_meta,undefined)

   ! slope_globe
   CALL netcdf_put_var(ncid,slope_globe(1:icon_grid%ncell,1,1),slope_globe_meta,undefined)
      
   ! fr_land_globe
   CALL netcdf_put_var(ncid,fr_land_globe(1:icon_grid%ncell,1,1),fr_land_globe_meta,undefined)

   ! z0_topo
   CALL netcdf_put_var(ncid,z0_topo(1:icon_grid%ncell,1,1),z0_topo_meta,undefined)

   ! for vertex_param%hh_vert
   CALL netcdf_put_var(ncid,vertex_param%hh_vert(1:icon_grid%nvertex,1,1),hh_vert_meta,undefined)

   !-----------------------------------------------------------------

   CALL close_netcdf_file(ncid)


   END SUBROUTINE write_netcdf_icon_grid_globe

  !----------------------------------------------------------------------------
  !----------------------------------------------------------------------------
  !----------------------------------------------------------------------- 
  !-----------------------------------------------------------------------
  !> set global attributes for netcdf with GLOBE data
  SUBROUTINE set_global_att_globe(global_attributes)
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
    global_attributes(1)%attributetext='GLOBE data '
    global_attributes(2)%attname = 'institution'
    global_attributes(2)%attributetext='Deutscher Wetterdienst'

    global_attributes(3)%attname = 'source'
    global_attributes(3)%attributetext='GLOBE, Global Land One-km Base Elevation'

    CALL DATE_AND_TIME(ydate,ytime)
    READ(ydate,'(4A2)') cc,yy,mm,dd
    READ(ytime,'(2A2)') hh, minute

    ydate=TRIM(cc)//TRIM(yy)//'-'//TRIM(mm)//'-'//TRIM(dd)
    ytime=TRIM(hh)//':'//TRIM(minute) 

    global_attributes(4)%attname = 'history'
    global_attributes(4)%attributetext=TRIM(ydate)//'T'//TRIM(ytime)//' globe_to_buffer'

    global_attributes(5)%attname = 'references'
    global_attributes(5)%attributetext='http://www.ngdc.noaa.gov/mgg/topo/globe.html'

    global_attributes(6)%attname = 'comment'
    global_attributes(6)%attributetext=''

  END SUBROUTINE set_global_att_globe
  !-----------------------------------------------------------------------

  !> read netcdf file for the fields derived from GLOBE data from the buffer 
   SUBROUTINE read_netcdf_buffer_globe(netcdf_filename,  &
     &                                     tg,         &
     &                                     undefined, &
     &                                     undef_int,   &
     &                                     fr_land_globe, &
     &                                     hh_globe,            &
     &                                     stdh_globe,          &
     &                                     theta_globe,         &
     &                                     aniso_globe,         &
     &                                     slope_globe,         &
     &                                     z0_topo,             &
     &                                     lrad,                &
     &                                     nhori,               &
     &                                     vertex_param,        &
     &                                     slope_asp_globe,     &
     &                                     slope_ang_globe,     &
     &                                     horizon_globe,       &
     &                                     skyview_globe)


   USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                          dim_4d_tg, &
    &                         def_dimension_info_buffer

   USE mo_globe_tg_fields, ONLY: add_parameters_domain

   USE mo_var_meta_data, ONLY: lon_geo_meta, &
     &                         lat_geo_meta, &
     &                         no_raw_data_pixel_meta, &
     &                         def_com_target_fields_meta  
     
   USE mo_var_meta_data, ONLY: def_globe_meta, def_globe_vertex_meta

   USE mo_var_meta_data, ONLY: hh_globe_meta, fr_land_globe_meta, &
    &       stdh_globe_meta, theta_globe_meta, &
    &       aniso_globe_meta, slope_globe_meta, &
    &       hh_vert_meta, npixel_vert_meta, z0_topo_meta, &
    &       slope_asp_globe_meta, slope_ang_globe_meta, &
    &       horizon_globe_meta, skyview_globe_meta

   USE mo_io_utilities, ONLY: netcdf_get_var

   
   CHARACTER (len=*), INTENT(IN)         :: netcdf_filename !< filename for the netcdf file
   TYPE(target_grid_def), INTENT(IN)     :: tg !< structure with target grid description
   INTEGER(KIND=i4),INTENT(IN),OPTIONAL  :: nhori    
   LOGICAL,         INTENT(IN),OPTIONAL  :: lrad  

   REAL(KIND=wp), INTENT(OUT)          :: undefined       !< value to indicate undefined grid elements 
   INTEGER, INTENT(OUT)                :: undef_int       !< value to indicate undefined grid elements

   REAL(KIND=wp), INTENT(OUT)  :: hh_globe(:,:,:)  !< mean height 
   REAL(KIND=wp), INTENT(OUT)  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height

   REAL(KIND=wp), INTENT(OUT)  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
   REAL(KIND=wp), INTENT(OUT)  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
   REAL(KIND=wp), INTENT(OUT)  :: slope_globe(:,:,:) !< sso parameter, mean slope
   REAL(KIND=wp), INTENT(OUT)  :: fr_land_globe(:,:,:) !< fraction land due to GLOBE raw data
   REAL(KIND=wp), INTENT(OUT)  :: z0_topo(:,:,:) !< roughness length due to orography


   TYPE(add_parameters_domain), INTENT(INOUT), OPTIONAL :: vertex_param  !< additional external parameters for ICON domain


   REAL(KIND=wp),INTENT(INOUT), OPTIONAL  :: slope_asp_globe(:,:,:)   !< lradtopo parameter, slope_aspect
   REAL(KIND=wp),INTENT(INOUT), OPTIONAL  :: slope_ang_globe(:,:,:)   !< lradtopo parameter, slope_angle
   REAL(KIND=wp),INTENT(INOUT), OPTIONAL  :: horizon_globe  (:,:,:,:) !< lradtopo parameter, horizon
   REAL(KIND=wp),INTENT(INOUT), OPTIONAL  :: skyview_globe  (:,:,:)   !< lradtopo parameter, skyview

   ! local variables

   LOGICAL :: lzrad
   INTEGER :: nvertex !< total number of vertices

  INTEGER :: errorcode !< error status variable

  INTEGER :: n !< counter

  REAL(KIND=wp), ALLOCATABLE :: topography_v(:,:,:) !< altitude ob vertices for ICON


  lzrad = .FALSE.
  IF (PRESENT(lrad)) lzrad=lrad


  PRINT *,'def_dimension_info_buffer'
   !set up dimensions for buffer
  IF (PRESENT(nhori)) THEN
    CALL  def_dimension_info_buffer(tg,nhori=nhori)
    ! dim_3d_tg, dim_4d_tg
  ELSE
    CALL  def_dimension_info_buffer(tg)
    ! dim_3d_tg
  ENDIF
  PRINT *,'def_com_target_fields_meta'
  ! define meta information for target field variables lon_geo, lat_geo 
  CALL def_com_target_fields_meta(dim_3d_tg)
  ! lon_geo_meta and lat_geo_meta
   PRINT *,'def_globe_meta'
  ! define meta information for various GLOBE data related variables for netcdf output
  IF (lzrad) THEN
    CALL def_globe_meta(dim_3d_tg,diminfohor=dim_4d_tg)
    !  hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta, z0_topo_meta
    !         slope_asp_globe_meta, slope_ang_globe_meta, 
    !         horizon_globe_meta, skyview_globe_meta
  ELSE
    CALL def_globe_meta(dim_3d_tg)
    !  hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta, z0_topo_meta
  ENDIF
  PRINT *,'set dimensions'
  !set up dimensions for buffer netcdf output 
  IF (PRESENT(vertex_param))  THEN
    nvertex = SIZE(vertex_param%hh_vert,1)
   ! PRINT *,'nvertex: ',nvertex

    CALL def_globe_vertex_meta(nvertex)
    !  hh_vert_meta, npixel_vert_meta
    ALLOCATE(topography_v(1:nvertex,1:1,1:1),STAT=errorcode)
    IF (errorcode /= 0 ) CALL abort_extpar('Cant ALLOCATE topography_c')

  ENDIF

  CALL netcdf_get_var(TRIM(netcdf_filename),hh_globe_meta,hh_globe)
  PRINT *,'hh_globe read'

  CALL netcdf_get_var(TRIM(netcdf_filename),stdh_globe_meta,stdh_globe)
  PRINT *,'stdh_globe read'

  CALL netcdf_get_var(TRIM(netcdf_filename),theta_globe_meta,theta_globe)
  PRINT *,'theta_globe read'

  CALL netcdf_get_var(TRIM(netcdf_filename),aniso_globe_meta,aniso_globe)
  PRINT *,'aniso_globe read'

  CALL netcdf_get_var(TRIM(netcdf_filename),slope_globe_meta,slope_globe)
  PRINT *,'slope_globe read'

  CALL netcdf_get_var(TRIM(netcdf_filename),fr_land_globe_meta,fr_land_globe)
  PRINT *,'fr_land_globe read'

   CALL netcdf_get_var(TRIM(netcdf_filename),z0_topo_meta,z0_topo)
  PRINT *,'z0 read'



  IF (PRESENT(vertex_param))  THEN
    CALL netcdf_get_var(TRIM(netcdf_filename),hh_vert_meta,topography_v)
    vertex_param%hh_vert(1:nvertex,1:1,1:1) = topography_v(1:nvertex,1:1,1:1)
    
    PRINT *,'vertex_param%hh_vert read'

  ENDIF

  ! slope_asp_globe
  IF (PRESENT(slope_asp_globe)) THEN
    CALL netcdf_get_var(TRIM(netcdf_filename),slope_asp_globe_meta,slope_asp_globe)
    PRINT *,'slope_asp_globe read'
  ENDIF

  ! slope_ang_globe
  IF (PRESENT(slope_ang_globe)) THEN
    CALL netcdf_get_var(TRIM(netcdf_filename),slope_ang_globe_meta,slope_ang_globe)
    PRINT *,'slope_ang_globe read'
  ENDIF

  ! horizon_globe
  IF (PRESENT(horizon_globe)) THEN
    CALL netcdf_get_var(TRIM(netcdf_filename),horizon_globe_meta,horizon_globe)
    PRINT *,'horizon_globe read'
  ENDIF

  ! skyview_globe
  IF (PRESENT(skyview_globe)) THEN
    CALL netcdf_get_var(TRIM(netcdf_filename),skyview_globe_meta,skyview_globe)
    PRINT *,'skyview_globe read'
  ENDIF



   
   END SUBROUTINE read_netcdf_buffer_globe



!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
END MODULE mo_globe_output_nc

