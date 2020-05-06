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
!  Parallel threads for ICON and COSMO using Open-MP, 
!  Several bug fixes and optimizations for ICON search algorithm, 
!  particularly for the special case of non-contiguous domains; 
!  simplified namelist control for ICON  
! V2_0         2013/06/04 Anne Roches
!  introduction of the topographical corrected radiation parameters
! V2_0         2013/06/04 Martina Messmer
!  renaming of all variables that contained a 'globe' in 'topo' 
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for netcdf output of GLOBE data
!> \author Hermann Asensio
MODULE mo_topo_output_nc

  USE mo_logging
  USE mo_kind,                  ONLY: wp, i4

  USE mo_grid_structures,       ONLY: target_grid_def, &
       &                              igrid_icon, &
       &                              igrid_cosmo 
  USE mo_cosmo_grid,            ONLY: cosmo_grid, nborder
  USE mo_icon_grid_data,        ONLY: icon_grid
                                
  USE mo_topo_tg_fields,        ONLY: add_parameters_domain
                                
  USE mo_topo_data,             ONLY: itopo_type, topo_gl, topo_aster
                                
  USE mo_io_utilities,          ONLY: netcdf_attributes, dim_meta_info,        &
       &                              netcdf_get_var, netcdf_put_var,          &
       &                              open_new_netcdf_file, close_netcdf_file

  USE mo_var_meta_data,         ONLY: dim_3d_tg, dim_4d_tg, def_dimension_info_buffer,   &
       &                              lon_geo_meta, lat_geo_meta,                        &
       &                              def_com_target_fields_meta, def_topo_meta,         &
       &                              def_topo_vertex_meta, dim_buffer_vertex,           & 
       &                              hh_topo_meta, fr_land_topo_meta,                   &
       &                              hh_topo_max_meta, hh_topo_min_meta,                &         
       &                              stdh_topo_meta, theta_topo_meta,                   &
       &                              aniso_topo_meta, slope_topo_meta,                  &
       &                              hh_vert_meta, z0_topo_meta,                        &
       &                              slope_asp_topo_meta, slope_ang_topo_meta,          &
       &                              horizon_topo_meta, skyview_topo_meta,              &
       &                              sgsl_meta
  
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: write_netcdf_buffer_topo, &
       &    read_netcdf_buffer_topo

CONTAINS

  !> create a netcdf file for the fields derived from GLOBE data to the buffer 
  SUBROUTINE write_netcdf_buffer_topo(netcdf_filename,&
       &                                  tg,            &
       &                                  undefined,     &
       &                                  igrid_type,    &
       &                                  lon_geo,       &
       &                                  lat_geo,       &
       &                                  fr_land_topo,  &
       &                                  hh_topo,       &
       &                                  stdh_topo,     &
       &                                  z0_topo,       &
       &                                  lrad,          &
       &                                  lsso,          &
       &                                  lsgsl,         &
       &                                  nhori,         &
       &                                  hh_topo_max,   &
       &                                  hh_topo_min,   &
       &                                  theta_topo,    &     
       &                                  aniso_topo,    &
       &                                  slope_topo,    &
       &                                  slope_asp_topo,&
       &                                  slope_ang_topo,&
       &                                  horizon_topo,  &
       &                                  skyview_topo,  &
       &                                  vertex_param,  &
       &                                  sgsl)

    CHARACTER (len=*), INTENT(IN)                     :: netcdf_filename !< filename for the netcdf file

    TYPE(target_grid_def)                             :: tg !< structure with target grid description

    INTEGER(KIND=i4), INTENT(IN)                      :: &
         &                                               igrid_type, &
         &                                               nhori    

    REAL(KIND=wp), INTENT(IN)                         :: undefined, &       !< value to indicate undefined grid elements 
         &                                               lon_geo(:,:,:), &  !< longitude coordinates target grid in the geog. system
         &                                               lat_geo(:,:,:), &  !< latitude coordinates target grid in the geog. system
         &                                               hh_topo(:,:,:), &  !< mean height 
         &                                               stdh_topo(:,:,:), & !< standard deviation of subgrid scale orographic height
         &                                               fr_land_topo(:,:,:), & !< fraction land due to GLOBE raw data
         &                                               z0_topo(:,:,:), & !< roughness length due to orography
         &                                               theta_topo(:,:,:), & !< sso parameter, angle of principal axis
         &                                               aniso_topo(:,:,:), & !< sso parameter, anisotropie factor
         &                                               slope_topo(:,:,:), & !< sso parameter, mean slope
         &                                               hh_topo_max(:,:,:), &!< sso parameter, max of topographie in grid point
         &                                               hh_topo_min(:,:,:), &!< sso parameter, min of topographie in grid point
         &                                               slope_asp_topo(:,:,:), &   !< lradtopo parameter, slope_aspect
         &                                               slope_ang_topo(:,:,:), &   !< lradtopo parameter, slope_angle
         &                                               horizon_topo  (:,:,:,:), & !< lradtopo parameter, horizon
         &                                               skyview_topo  (:,:,:), &   !< lradtopo parameter, skyview
         &                                               sgsl(:,:,:)                !< subgrid-slope

    LOGICAL,INTENT(IN)                                :: lrad, lsso, lsgsl

    TYPE(add_parameters_domain), INTENT(IN)           :: vertex_param  !< additional external parameters for ICON domain

    ! local variables
    INTEGER(KIND=i4)                                  :: ndims, ncid, nvertex, & 
         &                                               istart, iend, jstart, jend, &
         &                                               tmp_nlon, tmp_nlat, errorcode

    INTEGER(KIND=i4), PARAMETER                       :: nglob_atts=6
    TYPE(dim_meta_info), ALLOCATABLE                  :: dim_list(:) !< dimensions for netcdf file

    TYPE(netcdf_attributes)                           :: global_attributes(nglob_atts)


    CALL logging%info('Enter routine: write_netcdf_buffer_topo')

    !-------------------------------------------------------------
    ! define global attributes
    CALL set_global_att_topo(global_attributes)

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
    IF (lrad) THEN
      CALL  def_dimension_info_buffer(tg,nhori=nhori)
    ELSE
      CALL def_dimension_info_buffer(tg)
    ENDIF

    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_3d_tg)
    ! lon_geo_meta and lat_geo_meta

    ! define meta information for various GLOBE data related variables for netcdf output
    IF (lrad) THEN
      CALL def_topo_meta(dim_3d_tg,itopo_type,diminfohor=dim_4d_tg)
      !  hh_topo_meta, fr_land_topo_meta, &
      !         stdh_topo_meta, theta_topo_meta, &
      !         aniso_topo_meta, slope_topo_meta, &
      !         hh_vert_meta, npixel_vert_meta, z0_topo_meta, 
      !         slope_asp_topo_meta, slope_ang_topo_meta, 
      !         horizon_topo_meta, skyview_topo_meta
    ELSE
      CALL def_topo_meta(dim_3d_tg,itopo_type)
      !  hh_topo_meta, fr_land_topo_meta, &
      !         stdh_topo_meta, theta_topo_meta, &
      !         aniso_topo_meta, slope_topo_meta, &
      !         hh_vert_meta, npixel_vert_meta, z0_topo_meta
    ENDIF
    !set up dimensions for buffer netcdf output 
    IF (igrid_type == igrid_icon)  THEN
      nvertex = SIZE(vertex_param%npixel_vert,1)

      CALL def_topo_vertex_meta(nvertex)
      ! dim_buffer_vertex
      !  hh_vert_meta, npixel_vert_meta
      ndims = SIZE(dim_3d_tg) + 3
    ELSE
      ndims = SIZE(dim_3d_tg)
    ENDIF
    IF(lrad) ndims = ndims + 1
    ALLOCATE(dim_list(1:ndims),STAT=errorcode)
    IF (errorcode /= 0 ) CALL logging%error('Cant allocate array dim_list',__FILE__,__LINE__)
    IF (igrid_type == igrid_icon)  THEN
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
    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename), &
         &                       dim_list=dim_list,                   &
         &                       global_attributes=global_attributes, &
         &                       ncid=ncid)

    ! correct start and stop indices if needed
    SELECT CASE (igrid_type)
    CASE(igrid_cosmo)
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
    CASE(igrid_icon)
      istart = 1
      jstart = 1
      iend   = icon_grid%ncell
      jend   = 1
    END SELECT


    ! lon
    CALL netcdf_put_var(ncid,lon_geo(istart:iend,jstart:jend,:),lon_geo_meta,undefined)

    ! lat
    CALL netcdf_put_var(ncid,lat_geo(istart:iend,jstart:jend,:),lat_geo_meta,undefined)

    ! hh_topo
    CALL netcdf_put_var(ncid,hh_topo(istart:iend,jstart:jend,:),hh_topo_meta,undefined)

    ! stdh_topo
    CALL netcdf_put_var(ncid,stdh_topo(istart:iend,jstart:jend,:),stdh_topo_meta,undefined)

    IF (igrid_type == igrid_icon)  THEN
      CALL netcdf_put_var(ncid,hh_topo_max(istart:iend,jstart:jend,:),hh_topo_max_meta,undefined)     

      CALL netcdf_put_var(ncid,hh_topo_min(istart:iend,jstart:jend,:),hh_topo_min_meta,undefined)     
      
      ! hh_vert
      CALL netcdf_put_var(ncid,vertex_param%hh_vert(1:nvertex,1:1,1:1), &
           &                 hh_vert_meta,undefined)
    ENDIF

    !sso fields
    IF (lsso) THEN
      ! theta_topo
      CALL netcdf_put_var(ncid,theta_topo(istart:iend,jstart:jend,:),theta_topo_meta,undefined)

      ! aniso_topo
      CALL netcdf_put_var(ncid,aniso_topo(istart:iend,jstart:jend,:),aniso_topo_meta,undefined)

      ! slope_topo
      CALL netcdf_put_var(ncid,slope_topo(istart:iend,jstart:jend,:),slope_topo_meta,undefined)
    ENDIF

    ! fr_land_topo
    CALL netcdf_put_var(ncid,fr_land_topo(istart:iend,jstart:jend,:),fr_land_topo_meta,undefined)

    ! z0_topo
    CALL netcdf_put_var(ncid,z0_topo(istart:iend,jstart:jend,:),z0_topo_meta,undefined)

    ! lrad fields
    IF (lrad) THEN
      ! slope_asp_topo
      CALL netcdf_put_var(ncid,slope_asp_topo(istart:iend,jstart:jend,:),slope_asp_topo_meta,undefined)

      ! slope_ang_topo
      CALL netcdf_put_var(ncid,slope_ang_topo(istart:iend,jstart:jend,:),slope_ang_topo_meta,undefined)

      ! horizon_topo
      CALL netcdf_put_var(ncid,horizon_topo(istart:iend,jstart:jend,:,:),horizon_topo_meta,undefined)

      ! skyview_topo
      CALL netcdf_put_var(ncid,skyview_topo(istart:iend,jstart:jend,:),skyview_topo_meta,undefined)
    ENDIF

    IF (lsgsl) THEN
      ! subgrid-slope
      CALL netcdf_put_var(ncid,sgsl(istart:iend,jstart:jend,:),sgsl_meta,undefined)
    ENDIF
    CALL close_netcdf_file(ncid)

    CALL logging%info('Exit routine: write_netcdf_buffer_topo')

  END SUBROUTINE write_netcdf_buffer_topo

  !----------------------------------------------------------------------- 
  !-----------------------------------------------------------------------
  !> set global attributes for netcdf with GLOBE data
  SUBROUTINE set_global_att_topo(global_attributes)

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
    SELECT CASE(itopo_type)
    CASE(topo_aster)
      global_attributes(1)%attributetext='ASTER data '
    CASE(topo_gl)
      global_attributes(1)%attributetext='GLOBE data '
    END SELECT
    global_attributes(2)%attname = 'institution'
    global_attributes(2)%attributetext='Deutscher Wetterdienst'

    global_attributes(3)%attname = 'source'
    SELECT CASE(itopo_type)
    CASE(topo_aster)
      global_attributes(3)%attributetext='ASTER,The Advanced Spaceborne Thermal Emission '// & !_br 21.02.14 splitted too long line
           & 'and Reflection Radiometer, 1 arc-second digital elevation model' !_br 21.02.14
    CASE(topo_gl)
      global_attributes(3)%attributetext='GLOBE, Global Land One-km Base Elevation'
    END SELECT


    CALL DATE_AND_TIME(ydate,ytime)
    READ(ydate,'(4A2)') cc,yy,mm,dd
    READ(ytime,'(2A2)') hh, minute

    ydate=TRIM(cc)//TRIM(yy)//'-'//TRIM(mm)//'-'//TRIM(dd)
    ytime=TRIM(hh)//':'//TRIM(minute) 

    global_attributes(4)%attname = 'history'
    global_attributes(4)%attributetext=TRIM(ydate)//'T'//TRIM(ytime)//' topo_to_buffer'

    global_attributes(5)%attname = 'references'
    SELECT CASE(itopo_type)
    CASE(topo_aster)
      global_attributes(5)%attributetext='http://www.jspacesystems.or.jp/ersdac/GDEM/E/4.html'
    CASE(topo_gl)
      global_attributes(5)%attributetext='http://www.ngdc.noaa.gov/mgg/topo/globe.html'
    END SELECT

    global_attributes(6)%attname = 'comment'
    global_attributes(6)%attributetext=''

  END SUBROUTINE set_global_att_topo

  !> read netcdf file for the fields derived from GLOBE data from the buffer 
  SUBROUTINE read_netcdf_buffer_topo(netcdf_filename, &
       &                             tg,              &
       &                             igrid_type,      &
       &                             fr_land_topo,    &
       &                             hh_topo,         &
       &                             stdh_topo,       &
       &                             z0_topo,         &
       &                             lrad,            &
       &                             lsso,            &
       &                             lsgsl,           &
       &                             nhori,           &
       &                             vertex_param,    &
       &                             hh_topo_max,     &
       &                             hh_topo_min,     &
       &                             theta_topo,      &
       &                             aniso_topo,      &
       &                             slope_topo,      &
       &                             slope_asp_topo,  &
       &                             slope_ang_topo,  &
       &                             horizon_topo,    &
       &                             skyview_topo, &
       &                             sgsl)

    CHARACTER (len=*), INTENT(IN)               :: netcdf_filename !< filename for the netcdf file
    TYPE(target_grid_def), INTENT(IN)           :: tg !< structure with target grid description
    INTEGER(KIND=i4),INTENT(IN)                 :: nhori, igrid_type

    LOGICAL,         INTENT(IN)                 :: lrad, lsso, lsgsl  
                                                
    REAL(KIND=wp), INTENT(OUT)                  :: hh_topo(:,:,:), &  !< mean height 
         &                                         stdh_topo(:,:,:), & !< standard deviation of subgrid scale orographic height
         &                                         fr_land_topo(:,:,:), & !< fraction land due to GLOBE raw data
         &                                         z0_topo(:,:,:) !< roughness length due to orography
                                                
    REAL(KIND=wp), INTENT(OUT)                  :: hh_topo_max(:,:,:), & !< sso parameter, max of topographie in grid point
         &                                         hh_topo_min(:,:,:), & !< sso parameter, min of topographie in grid point
         &                                         theta_topo(:,:,:), & !< sso parameter, angle of principal axis
         &                                         aniso_topo(:,:,:), & !< sso parameter, anisotropie factor
         &                                         slope_topo(:,:,:) !< sso parameter, mean slope

    TYPE(add_parameters_domain), INTENT(INOUT)  :: vertex_param  !< additional external parameters for ICON domain

    REAL(KIND=wp), INTENT(INOUT)                :: slope_asp_topo(:,:,:), &   !< lradtopo parameter, slope_aspect
         &                                         slope_ang_topo(:,:,:), &   !< lradtopo parameter, slope_angle
         &                                         horizon_topo  (:,:,:,:), & !< lradtopo parameter, horizon
         &                                         skyview_topo  (:,:,:), &   !< lradtopo parameter, skyview
         &                                         sgsl(:,:,:) !< subgrid-slope

    ! local variables
    INTEGER(KIND=i4)                            :: nvertex, errorcode
    REAL(KIND=wp), ALLOCATABLE                  :: topography_v(:,:,:) !< altitude ob vertices for ICON

    CALL logging%info('Enter routine: read_netcdf_buffer_topo')

    !set up dimensions for buffer
    IF (lrad) THEN
      CALL  def_dimension_info_buffer(tg,nhori=nhori)
    ELSE
      CALL def_dimension_info_buffer(tg)
    ENDIF

    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_3d_tg)
    ! lon_geo_meta and lat_geo_meta
    ! define meta information for various GLOBE data related variables for netcdf output
    IF (lrad) THEN
      CALL def_topo_meta(dim_3d_tg,itopo_type,diminfohor=dim_4d_tg)
    ELSE
      CALL def_topo_meta(dim_3d_tg,itopo_type)
    ENDIF

    !set up dimensions for buffer netcdf output 
    IF (igrid_type == igrid_icon) THEN
      nvertex = SIZE(vertex_param%hh_vert,1)
      CALL def_topo_vertex_meta(nvertex)
      !  hh_vert_meta, npixel_vert_meta
      ALLOCATE(topography_v(1:nvertex,1:1,1:1),STAT=errorcode)
      IF (errorcode /= 0 ) CALL logging%error('Cant ALLOCATE topography_c',__FILE__,__LINE__)
    ENDIF

    CALL netcdf_get_var(TRIM(netcdf_filename),hh_topo_meta,hh_topo)

    CALL netcdf_get_var(TRIM(netcdf_filename),stdh_topo_meta,stdh_topo)

    IF (igrid_type == igrid_icon) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),hh_topo_min_meta,hh_topo_min)
    ENDIF

    IF (igrid_type == igrid_icon) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),hh_topo_max_meta,hh_topo_max)
    ENDIF
    
    ! sso fields
    IF (lsso) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),theta_topo_meta,theta_topo)

      CALL netcdf_get_var(TRIM(netcdf_filename),aniso_topo_meta,aniso_topo)

      CALL netcdf_get_var(TRIM(netcdf_filename),slope_topo_meta,slope_topo)
    ENDIF

    CALL netcdf_get_var(TRIM(netcdf_filename),fr_land_topo_meta,fr_land_topo)

    CALL netcdf_get_var(TRIM(netcdf_filename),z0_topo_meta,z0_topo)

    IF (igrid_type == igrid_icon) THEN
      CALL netcdf_get_var(TRIM(netcdf_filename),hh_vert_meta,topography_v)
      vertex_param%hh_vert(1:nvertex,1:1,1:1) = topography_v(1:nvertex,1:1,1:1)
    ENDIF

    ! lrad fields
    IF (lrad) THEN
      ! slope_asp_topo
      CALL netcdf_get_var(TRIM(netcdf_filename),slope_asp_topo_meta,slope_asp_topo)

      ! slope_ang_topo
      CALL netcdf_get_var(TRIM(netcdf_filename),slope_ang_topo_meta,slope_ang_topo)

      ! horizon_topo
      CALL netcdf_get_var(TRIM(netcdf_filename),horizon_topo_meta,horizon_topo)

      ! skyview_topo
      CALL netcdf_get_var(TRIM(netcdf_filename),skyview_topo_meta,skyview_topo)
    ENDIF

    IF (lsgsl) THEN
      ! subgrid-slope
      CALL netcdf_get_var(TRIM(netcdf_filename),sgsl_meta,sgsl)
    ENDIF

    CALL logging%info('Exit routine: read_netcdf_buffer_topo')

  END SUBROUTINE read_netcdf_buffer_topo

END MODULE mo_topo_output_nc
