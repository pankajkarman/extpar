!+ Fortran module with netcdf output routines for external parameters
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  write optical thickness of aeorosls into seperate variables 
!  in the netcdf output for ICON and COSMO grids        
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with netcdf output routines for external parameters 
!> ouptut routines
!> \author Hermann Asensio
MODULE mo_extpar_output_nc


  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  !> data type structures form module GRID_structures
  USE mo_grid_structures, ONLY: reg_lonlat_grid
  USE mo_grid_structures, ONLY: rotated_lonlat_grid
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid
  USE mo_grid_structures, ONLY: target_grid_def

  USE mo_io_utilities, ONLY: var_meta_info
  USE mo_io_utilities, ONLY: netcdf_attributes

  USE mo_io_utilities, ONLY: netcdf_write_varlist
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

  USE mo_glc2000_lookup_tables, ONLY: nclass_glc2000
  USE mo_ndvi_data, ONLY: ntime_ndvi
  USE mo_aot_data, ONLY: ntype_aot, ntime_aot

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: write_netcdf_buffer_extpar
  PUBLIC :: write_netcdf_cosmo_grid_extpar
  PUBLIC :: write_netcdf_icon_grid_extpar

  CONTAINS

    !> netcdf output of GLC2000 derived buffer fields
    SUBROUTINE write_netcdf_buffer_extpar(netcdf_filename,  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     name_lookup_table_glc2000, &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, &
    &                                     lu_class_fraction,    &
    &                                     ice_lu, &
    &                                     z0_lu, &
    &                                     root_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     soiltype_fao, &
    &                                     ndvi_max,  &
    &                                     ndvi_field_mom,&
    &                                     ndvi_ratio_mom, &
    &                                     hh_globe,      &
    &                                     stdh_globe,    &
    &                                     theta_globe,   &
    &                                     aniso_globe,   &
    &                                     slope_globe,   &
    &                                     aot_tg, &
    &                                     crutemp )

  USE mo_var_meta_data, ONLY: dim_3d_tg, &
    &                         def_dimension_info_buffer

  USE mo_var_meta_data, ONLY: lon_geo_meta, &
    &                         lat_geo_meta, &
    &                         no_raw_data_pixel_meta, &
    &                         def_com_target_fields_meta  
  
  USE mo_var_meta_data, ONLY: def_lu_fields_meta

  USE mo_var_meta_data, ONLY: dim_lu_tg

  USE mo_var_meta_data, ONLY: fr_land_lu_meta, lu_tot_npixel_meta, &
    &       lu_class_fraction_meta, lu_class_npixel_meta, &
    &       ice_lu_meta, z0_lu_meta, &
    &       plcov_mx_lu_meta, plcov_mn_lu_meta, &
    &       lai_mx_lu_meta, lai_mn_lu_meta, &
    &       rs_min_lu_meta, urban_lu_meta, &
    &       for_d_lu_meta, for_e_lu_meta, &
    &       emissivity_lu_meta, root_lu_meta
    
  USE mo_var_meta_data, ONLY: def_soil_meta
  USE mo_var_meta_data, ONLY: fr_land_soil_meta, soiltype_fao_meta

  
  USE mo_var_meta_data, ONLY: dim_ndvi_tg
  USE mo_var_meta_data, ONLY: ndvi_max_meta, &
      &                         ndvi_field_mom_meta, &
      &                         ndvi_ratio_mom_meta,&
      &                         def_ndvi_meta


  USE mo_var_meta_data, ONLY: def_globe_meta, def_globe_vertex_meta
  USE mo_var_meta_data, ONLY: dim_buffer_cell, dim_buffer_vertex

  USE mo_var_meta_data, ONLY: hh_globe_meta, fr_land_globe_meta, &
    &       stdh_globe_meta, theta_globe_meta, &
    &       aniso_globe_meta, slope_globe_meta, &
    &       hh_vert_meta, npixel_vert_meta

  USE mo_var_meta_data, ONLY: dim_aot_tg, dim_aot_ty, &
    &                         def_aot_tg_meta
  USE mo_var_meta_data, ONLY: aot_tg_meta, aer_bc_meta,   & 
    &                         aer_dust_meta, aer_org_meta,&
    &                         aer_so4_meta, aer_ss_meta

  USE mo_var_meta_data, ONLY: crutemp_meta, &
    &                         def_crutemp_meta

  USE mo_var_meta_data, ONLY: def_flake_fields_meta
  USE mo_var_meta_data, ONLY: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta
  USE mo_flake_data, ONLY: flake_depth_undef !< default value for undefined lake depth

  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
  TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
  REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
  INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
  CHARACTER (len=*), INTENT(IN)      :: name_lookup_table_glc2000 !< name of lookup table
  REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN)  :: lu_class_fraction(:,:,:,:)  !< fraction for each lu class on target grid (dimension (ie,je,ke,nclass_glc2000))
  REAL (KIND=wp), INTENT(IN)  :: fr_land_lu(:,:,:) !< fraction land due to lu raw data
  REAL (KIND=wp), INTENT(IN)  :: ice_lu(:,:,:)     !< fraction of ice due to lu raw data
  REAL (KIND=wp), INTENT(IN)  :: z0_lu(:,:,:)      !< roughness length due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: root_lu(:,:,:)    !< root depth due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: plcov_mx_lu(:,:,:)!< plant cover maximum due to lu land use data
  !REAL (KIND=wp), INTENT(IN)  :: plcov_mn_lu(:,:,:)!< plant cover minimum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: lai_mx_lu(:,:,:)  !< Leaf Area Index maximum due to lu land use data
  !REAL (KIND=wp), INTENT(IN)  :: lai_mn_lu(:,:,:)  !< Leaf Area Index minimum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: rs_min_lu(:,:,:)  !< minimal stomata resistance due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: urban_lu(:,:,:)   !< urban fraction due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: for_d_lu(:,:,:)   !< deciduous forest (fraction) due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: for_e_lu(:,:,:)   !< evergreen forest (fraction) due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: emissivity_lu(:,:,:) !< longwave emissivity due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: lake_depth(:,:,:) !< lake depth
  REAL (KIND=wp), INTENT(IN)  :: fr_lake(:,:,:)     !< fraction of fresh water (lakes)
  INTEGER(KIND=i4), INTENT(INOUT) :: soiltype_fao(:,:,:) !< soiltype due to FAO Digital Soil map of the World
  REAL (KIND=wp), INTENT(IN) :: ndvi_max(:,:,:) !< field for ndvi maximum
  REAL (KIND=wp), INTENT(IN) :: ndvi_field_mom(:,:,:,:) !< field for monthly mean ndvi data (12 months)
  REAL (KIND=wp), INTENT(IN) :: ndvi_ratio_mom(:,:,:,:) !< field for monthly ndvi ratio (12 months)
  REAL(KIND=wp), INTENT(IN)  :: hh_globe(:,:,:)  !< mean height 
  REAL(KIND=wp), INTENT(IN)  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height
  REAL(KIND=wp), INTENT(IN)  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
  REAL(KIND=wp), INTENT(IN)  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
  REAL(KIND=wp), INTENT(IN)  :: slope_globe(:,:,:) !< sso parameter, mean slope
  REAL (KIND=wp), INTENT(IN)  :: aot_tg(:,:,:,:,:) !< aerosol optical thickness, aot_tg(ie,je,ke,ntime,ntype)
  REAL(KIND=wp), INTENT(IN)  :: crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke) 

  ! local variables
  INTEGER :: ndims  
  INTEGER :: ncid
  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file
  TYPE(dim_meta_info), TARGET :: dim_5d_buffer(1:5)
  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)
  INTEGER :: errorcode !< error status variable
  INTEGER :: n !< counter
    PRINT *,'ENTER write_netcdf_buffer_extpar'

    !-------------------------------------------------------------
    ! define global attributes CHARACTER (LEN=*) :: name_lookup_table_glc2000
    CALL set_global_att_extpar(global_attributes,name_lookup_table_glc2000)

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)
    ! dim_3d_tg

    ! define meta information for various land use related variables (GLC2000) for netcdf output
    CALL def_lu_fields_meta(tg,nclass_glc2000,dim_3d_tg)
    ! dim_lu_tg
    ! fr_land_lu_meta, lu_tot_npixel_meta, &
    !  &       lu_class_fraction_meta, lu_class_npixel_meta, &
    !  &       ice_lu_meta, z0_lu_meta, &
    !  &       plcov_mx_lu_meta, plcov_mn_lu_meta, &
    !  &       lai_mx_lu_meta, lai_mn_lu_meta, &
    !  &       rs_min_lu_meta, urban_lu_meta, &
    !  &       for_d_lu_meta, for_e_lu_meta, &
    !  &       emissivity_lu_meta, root_lu_meta

    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_3d_tg)
    ! lon_geo_meta and lat_geo_meta
    
    ! define meta information for various land use related variables (FLAKE) for netcdf output
    CALL def_flake_fields_meta(dim_3d_tg)
    ! lake_depth_meta, fr_lake_meta, &
    !  &       flake_tot_npixel_meta
    
    CALL def_soil_meta(dim_3d_tg)
    !  fr_land_soil_meta, soiltype_fao_meta

    !define meta information for various NDVI data related variables for netcdf output
    CALL def_ndvi_meta(tg,ntime_ndvi,dim_3d_tg)
    ! dim_ndvi_tg, ndvi_max_meta, ndvi_field_mom_meta, ndvi_ratio_mom_meta

    ! define meta information for various GLOBE data related variables for netcdf output
    CALL def_globe_meta(dim_3d_tg)
    ! hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta

    ! define dimensions and meta information for variable aot_tg for netcdf output
    CALL def_aot_tg_meta(tg,ntime_aot,ntype_aot,dim_3d_tg)
    ! dim_aot_tg and aot_tg_meta

    ! define meta information for variable crutemp for netcdf output
    CALL def_crutemp_meta(dim_3d_tg)
    ! crutemp_meta

    !set up dimensions for buffer netcdf output 
    ndims = 6
    ALLOCATE(dim_list(1:ndims),STAT=errorcode)
    IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')
    
    dim_list(1) = dim_3d_tg(1) ! ie
    dim_list(2) = dim_3d_tg(2) ! je
    dim_list(3) = dim_3d_tg(3) ! ke
    dim_list(4)%dimname = 'nclass' ! nclass_glc2000
    dim_list(4)%dimsize = nclass_glc2000 
    dim_list(5)%dimname = 'ntime'
    dim_list(5)%dimsize = ntime_aot
    dim_list(6)%dimname = 'ntype'
    dim_list(6)%dimsize = ntype_aot

    !-----------------------------------------------------------------
    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
      &                       dim_list=dim_list,                  &
      &                       global_attributes=global_attributes, &
      &                       ncid=ncid)

    ! soiltype
    CALL netcdf_put_var(ncid,soiltype_fao,soiltype_fao_meta,undef_int)

    ! fr_land_lu
    CALL netcdf_put_var(ncid,fr_land_lu,fr_land_lu_meta,undefined)

    ! ice_lu
    CALL netcdf_put_var(ncid,ice_lu,ice_lu_meta,undefined)

    ! plcov_mx_lu
    CALL netcdf_put_var(ncid,plcov_mx_lu,plcov_mx_lu_meta,undefined)

    ! lai_mx_lu
    CALL netcdf_put_var(ncid,lai_mx_lu,lai_mx_lu_meta,undefined)

    ! rs_min_lu
    CALL netcdf_put_var(ncid,rs_min_lu,rs_min_lu_meta,undefined)

    ! urban_lu
    CALL netcdf_put_var(ncid,urban_lu,urban_lu_meta,undefined)

    ! for_d_lu
    CALL netcdf_put_var(ncid,for_d_lu,for_d_lu_meta,undefined)

    ! for_e_lu
    CALL netcdf_put_var(ncid,for_e_lu,for_e_lu_meta,undefined)

    ! emissivity_lu
    CALL netcdf_put_var(ncid,emissivity_lu,emissivity_lu_meta,undefined)

    ! root_lu
    CALL netcdf_put_var(ncid,root_lu,root_lu_meta,undefined)

    ! z0_lu
    CALL netcdf_put_var(ncid,z0_lu,z0_lu_meta,undefined)

    ! lon
    CALL netcdf_put_var(ncid,lon_geo,lon_geo_meta,undefined)

    ! lat
    CALL netcdf_put_var(ncid,lat_geo,lat_geo_meta,undefined)

    ! lai_mn_lu
    !CALL netcdf_put_var(ncid,lai_mn_lu,lai_mn_lu,undefined)
    !
    ! plcov_mn_lu
    !CALL netcdf_put_var(ncid,plcov_mn_lu,plcov_mn_lu_meta,undefined)

    ! ndvi_max
    CALL netcdf_put_var(ncid,ndvi_max,ndvi_max_meta,undefined)

    ! hh_globe
    CALL netcdf_put_var(ncid,hh_globe,hh_globe_meta,undefined)

    ! stdh_globe
    CALL netcdf_put_var(ncid,stdh_globe,stdh_globe_meta,undefined)

    ! theta_globe
    CALL netcdf_put_var(ncid,theta_globe,theta_globe_meta,undefined)

    ! aniso_globe
    CALL netcdf_put_var(ncid,aniso_globe,aniso_globe_meta,undefined)

    ! slope_globe
    CALL netcdf_put_var(ncid,slope_globe,slope_globe_meta,undefined)

    ! crutemp
    CALL netcdf_put_var(ncid,crutemp,crutemp_meta,undefined)

    ! fr_lake
    CALL netcdf_put_var(ncid,fr_lake,fr_lake_meta,undefined)

    ! lake_depth
    CALL netcdf_put_var(ncid,lake_depth,lake_depth_meta,undefined)

    !-----------------------------------------------------------------

    ! lu_class_fraction
    CALL netcdf_put_var(ncid,lu_class_fraction,lu_class_fraction_meta,undefined)
    !-----------------------------------------------------------------
           
    ! ndvi_field_mom
    CALL netcdf_put_var(ncid,ndvi_field_mom,ndvi_field_mom_meta,undefined)

    ! ndvi_ratio_mom
    CALL netcdf_put_var(ncid,ndvi_ratio_mom,ndvi_ratio_mom_meta,undefined)

    !-----------------------------------------------------------------
    ! aot_tg
    CALL netcdf_put_var(ncid,aot_tg,aot_tg_meta,undefined)

    CALL close_netcdf_file(ncid)

  END SUBROUTINE write_netcdf_buffer_extpar
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------

  !> netcdf output of GLC2000 derived COSMO fields
  SUBROUTINE write_netcdf_cosmo_grid_extpar(netcdf_filename,  &
    &                                     cosmo_grid,       &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     name_lookup_table_glc2000, &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, &
    &                                     lu_class_fraction,    &
    &                                     ice_lu, &
    &                                     z0_lu, &
    &                                     z0_glc2000, &
    &                                     z0_topo,  &
    &                                     root_lu, &
    &                                     plcov_mn_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mn_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     soiltype_fao, &
    &                                     ndvi_max,  &
    &                                     ndvi_field_mom,&
    &                                     ndvi_ratio_mom, &
    &                                     hh_globe,            &
    &                                     stdh_globe,          &
    &                                     theta_globe,         &
    &                                     aniso_globe,         &
    &                                     slope_globe,   &
    &                                     aot_tg, &
    &                                     crutemp )
  

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

  USE mo_var_meta_data, ONLY: def_lu_fields_meta, def_glc2000_fields_meta

  USE mo_var_meta_data, ONLY: dim_lu_tg

  USE mo_var_meta_data, ONLY: fr_land_lu_meta, lu_tot_npixel_meta, &
    &       lu_class_fraction_meta, lu_class_npixel_meta, &
    &       ice_lu_meta, z0_lu_meta, z0_glc2000_meta, &
    &       plcov_mx_lu_meta, plcov_mn_lu_meta, &
    &       lai_mx_lu_meta, lai_mn_lu_meta, &
    &       rs_min_lu_meta, urban_lu_meta, &
    &       for_d_lu_meta, for_e_lu_meta, &
    &       emissivity_lu_meta, root_lu_meta

  USE mo_var_meta_data, ONLY: def_soil_meta
  USE mo_var_meta_data, ONLY: fr_land_soil_meta, soiltype_fao_meta
  
  USE mo_var_meta_data, ONLY: dim_ndvi_tg
  USE mo_var_meta_data, ONLY: ndvi_max_meta, &
      &                         ndvi_field_mom_meta, &
      &                         ndvi_ratio_mom_meta,&
      &                         def_ndvi_meta

  USE mo_var_meta_data, ONLY: def_globe_meta, def_globe_vertex_meta
  USE mo_var_meta_data, ONLY: dim_buffer_cell, dim_buffer_vertex

  USE mo_var_meta_data, ONLY: hh_globe_meta, fr_land_globe_meta, &
   &       stdh_globe_meta, theta_globe_meta, &
   &       aniso_globe_meta, slope_globe_meta, &
   &       hh_vert_meta, npixel_vert_meta, z0_topo_meta, &
!roa nc>
   &       hh_fis_meta
!roa nc<


  USE mo_var_meta_data, ONLY: dim_aot_tg, dim_aot_ty, &
    &                         def_aot_tg_meta
  USE mo_var_meta_data, ONLY: aot_tg_meta, aer_bc_meta,   & 
    &                         aer_dust_meta, aer_org_meta,&
    &                         aer_so4_meta, aer_ss_meta

  USE mo_var_meta_data, ONLY: crutemp_meta, &
    &                         def_crutemp_meta

  USE mo_var_meta_data, ONLY: def_flake_fields_meta
  USE mo_var_meta_data, ONLY: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta
  USE mo_flake_data, ONLY: flake_depth_undef !< default value for undefined lake depth

!roa nc>
  USE mo_physical_constants, ONLY: grav
!roa nc<

  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
  TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid
  TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
  REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
  INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
  CHARACTER (len=*), INTENT(IN)      :: name_lookup_table_glc2000 !< name of lookup table
  REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN)  :: lu_class_fraction(:,:,:,:)  !< fraction for each lu class on target grid (dimension (ie,je,ke,nclass_glc2000))
  REAL (KIND=wp), INTENT(IN)  :: fr_land_lu(:,:,:) !< fraction land due to lu raw data
  REAL (KIND=wp), INTENT(IN)  :: ice_lu(:,:,:)     !< fraction of ice due to lu raw data
  REAL (KIND=wp), INTENT(IN)  :: z0_lu(:,:,:)      !< roughness length 
  REAL (KIND=wp), INTENT(IN)  :: z0_glc2000(:,:,:)      !< roughness length due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: z0_topo(:,:,:)      !< roughness length due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: root_lu(:,:,:)    !< root depth due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: plcov_mx_lu(:,:,:)!< plant cover maximum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: plcov_mn_lu(:,:,:)!< plant cover minimum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: lai_mx_lu(:,:,:)  !< Leaf Area Index maximum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: lai_mn_lu(:,:,:)  !< Leaf Area Index minimum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: rs_min_lu(:,:,:)  !< minimal stomata resistance due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: urban_lu(:,:,:)   !< urban fraction due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: for_d_lu(:,:,:)   !< deciduous forest (fraction) due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: for_e_lu(:,:,:)   !< evergreen forest (fraction) due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: emissivity_lu(:,:,:) !< longwave emissivity due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: lake_depth(:,:,:) !< lake depth
  REAL (KIND=wp), INTENT(IN)  :: fr_lake(:,:,:)     !< fraction of fresh water (lakes)
  INTEGER(KIND=i4), INTENT(IN) :: soiltype_fao(:,:,:) !< soiltype due to FAO Digital Soil map of the World
  REAL (KIND=wp), INTENT(IN) :: ndvi_max(:,:,:) !< field for ndvi maximum
  REAL (KIND=wp), INTENT(IN) :: ndvi_field_mom(:,:,:,:) !< field for monthly mean ndvi data (12 months)
  REAL (KIND=wp), INTENT(IN) :: ndvi_ratio_mom(:,:,:,:) !< field for monthly ndvi ratio (12 months)
  REAL(KIND=wp), INTENT(IN)  :: hh_globe(:,:,:)  !< mean height 
  REAL(KIND=wp), INTENT(IN)  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height
  REAL(KIND=wp), INTENT(IN)  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
  REAL(KIND=wp), INTENT(IN)  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
  REAL(KIND=wp), INTENT(IN)  :: slope_globe(:,:,:) !< sso parameter, mean slope
  REAL (KIND=wp), INTENT(IN)  :: aot_tg(:,:,:,:,:) !< aerosol optical thickness, aot_tg(ie,je,ke,ntime,ntype)
  REAL(KIND=wp), INTENT(IN)  :: crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke) 

  ! local variables
  REAL(KIND=wp), ALLOCATABLE :: var_real_2d(:,:)
  INTEGER :: n_4d_real = 0 !< number of 4D real variables
  INTEGER :: n_3d_real = 0 !< number of 3D real variables
  INTEGER :: n_2d_real = 0 !< number of 2D real variables
  INTEGER :: n_1d_real = 0 !< number of 1D real variables
  INTEGER :: n_3d_int = 0 !< number of 3D integer variables
  INTEGER :: n_2d_int = 0 !< number of 2D integer variables

  INTEGER :: ndims  
  INTEGER :: ncid
  INTEGER :: varid
  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file
  TYPE(dim_meta_info), TARGET :: dim_nclass(1:3)
  TYPE(dim_meta_info), TARGET :: dim_3d_ndvi(1:3)
  TYPE(dim_meta_info), TARGET :: dim_4d_aot(1:4)
  TYPE(dim_meta_info), POINTER :: pdiminfo

  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)
  INTEGER :: errorcode !< error status variable

  CHARACTER (len=80):: grid_mapping !< netcdf attribute grid mapping
  CHARACTER (len=80):: coordinates  !< netcdf attribute coordinates
  INTEGER :: n !< counter

    PRINT *,'Enter write_netcdf_cosmo_grid_extpar'

    !-------------------------------------------------------------
    ! define global attributes
    CALL set_global_att_extpar(global_attributes,name_lookup_table_glc2000)
    PRINT *,'global_attributes: ', global_attributes

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
    PRINT *,'def_com_target_fields_meta'
    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_2d_cosmo,coordinates,grid_mapping)
    ! lon_geo_meta and lat_geo_meta

    ! define meta information for various land use related variables (GLC2000) for netcdf output
    PRINT *,'def_lu_fields_meta'
    CALL def_lu_fields_meta(tg,nclass_glc2000,dim_2d_cosmo,coordinates,grid_mapping)
    ! dim_lu_tg
    ! fr_land_lu_meta, lu_tot_npixel_meta, &
    !  &       lu_class_fraction_meta, lu_class_npixel_meta, &
    !  &       ice_lu_meta, z0_lu_meta, &
    !  &       plcov_mx_lu_meta, plcov_mn_lu_meta, &
    !  &       lai_mx_lu_meta, lai_mn_lu_meta, &
    !  &       rs_min_lu_meta, urban_lu_meta, &
    !  &       for_d_lu_meta, for_e_lu_meta, &
    !  &       emissivity_lu_meta, root_lu_meta

    PRINT *,'def_glc2000_fields_meta'
    CALL def_glc2000_fields_meta(tg,nclass_glc2000,dim_2d_cosmo,coordinates,grid_mapping)

    PRINT *,'def_soil_meta'
    CALL def_soil_meta(dim_2d_cosmo,coordinates,grid_mapping)
    !  fr_land_soil_meta, soiltype_fao_meta

    !define meta information for various NDVI data related variables for netcdf output
    PRINT *,'def_ndvi_meta'
    CALL def_ndvi_meta(tg,ntime_ndvi,dim_2d_cosmo,coordinates,grid_mapping)
    ! dim_ndvi_tg, ndvi_max_meta, ndvi_field_mom_meta, ndvi_ratio_mom_meta

    ! define meta information for various GLOBE data related variables for netcdf output
    PRINT *,'def_globe_meta'
    CALL def_globe_meta(dim_2d_cosmo,coordinates,grid_mapping)
    !  hh_globe_meta, fr_land_globe_meta, &
    !  stdh_globe_meta, theta_globe_meta, &
    !  aniso_globe_meta, slope_globe_meta, &
    !  hh_vert_meta, npixel_vert_meta

    ! define dimensions and meta information for variable aot_tg for netcdf output
     PRINT *,'def_aot_tg_meta'
    CALL def_aot_tg_meta(tg,ntime_aot,ntype_aot,dim_2d_cosmo,coordinates,grid_mapping)
    ! dim_aot_tg and aot_tg_meta
    ! dim_aot_ty, aer_bc_meta, aer_dust_meta, aer_org_meta, aer_so4_meta, aer_ss_meta

    ! define meta information for variable crutemp for netcdf output
     PRINT *,'def_crutemp_meta'
    CALL def_crutemp_meta(dim_2d_cosmo,coordinates,grid_mapping)
    ! crutemp_meta
     
    ! define meta information for various land use related variables (FLAKE) for netcdf output
    PRINT *,'def_flake_fields_meta'

    CALL def_flake_fields_meta(dim_2d_cosmo,coordinates,grid_mapping)
    ! lake_depth_meta, fr_lake_meta, &
    !  &       flake_tot_npixel_meta

    ALLOCATE(var_real_2d(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot), STAT=errorcode)
    IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate var_real_2d')

    !set up dimensions for buffer netcdf output 
    ndims = 5
    PRINT *,'ALLOCATE(dim_list(1:ndims))'

    ALLOCATE(dim_list(1:ndims),STAT=errorcode)
    IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')

    dim_list(1) = dim_rlon_cosmo(1) ! rlon
    dim_list(2) = dim_rlat_cosmo(1) ! rlat
    dim_list(3)%dimname = 'nclass'  ! nclass_glc2000
    dim_list(3)%dimsize = nclass_glc2000 
    dim_list(4)%dimname = 'ntime'
    dim_list(4)%dimsize = ntime_aot
    dim_list(5)%dimname = 'ntype'
    dim_list(5)%dimsize = ntype_aot

    dim_nclass(1) = dim_list(1)
    dim_nclass(2) = dim_list(2)
    dim_nclass(3) = dim_list(3)
    
    dim_3d_ndvi(1) = dim_rlon_cosmo(1) ! rlon
    dim_3d_ndvi(2) = dim_rlat_cosmo(1) ! rlat
    dim_3d_ndvi(3) = dim_list(4) ! dim_ndvi_tg(4)

    dim_4d_aot(1) = dim_rlon_cosmo(1) ! rlon
    dim_4d_aot(2) = dim_rlat_cosmo(1) ! rlat
    dim_4d_aot(3) = dim_list(4) ! dim_aot_tg(4) ! ntime
    dim_4d_aot(4) = dim_list(5) ! dim_aot_tg(5) ! ntype

    !-----------------------------------------------------------------
    PRINT *,' CALL open_new_netcdf_file'
    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
      &                       dim_list=dim_list,                  &
      &                       global_attributes=global_attributes, &
      &                       ncid=ncid)
    !-----------------------------------------------------------------
    ! start with real 1d variables
     
    ! rlon
    CALL netcdf_put_var(ncid,lon_rot(1:cosmo_grid%nlon_rot),rlon_meta,undefined)

    CALL netcdf_put_var(ncid,lat_rot(1:cosmo_grid%nlat_rot),rlat_meta,undefined)

         
    ! fr_land_lu
    var_real_2d(:,:) = fr_land_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,fr_land_lu_meta,undefined)

    ! ice_lu
    var_real_2d(:,:) = ice_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,ice_lu_meta,undefined)

    ! plcov_mx_lu
    var_real_2d(:,:) = plcov_mx_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,plcov_mx_lu_meta,undefined)

    ! lai_mx_lu
    var_real_2d(:,:) = lai_mx_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,lai_mx_lu_meta,undefined)

    ! rs_min_lu
    var_real_2d(:,:) = rs_min_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,rs_min_lu_meta,undefined)

    ! urban_lu
    var_real_2d(:,:) = urban_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,urban_lu_meta,undefined)

    ! for_d_lu
    var_real_2d(:,:) = for_d_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,for_d_lu_meta,undefined)

    ! for_e_lu
    var_real_2d(:,:) = for_e_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,for_e_lu_meta,undefined)

    ! emissivity_lu
    var_real_2d(:,:) = emissivity_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,emissivity_lu_meta,undefined)

    ! root_lu
    var_real_2d(:,:) = root_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,root_lu_meta,undefined)

    ! z0_lu
    var_real_2d(:,:) = z0_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,z0_lu_meta,undefined)

    ! lon
    var_real_2d(:,:) = lon_geo(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,lon_geo_meta,undefined)

    ! lat
    var_real_2d(:,:) = lat_geo(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,lat_geo_meta,undefined)
!roa nc>
    ! lai_mn_lu
    var_real_2d(:,:) = lai_mn_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    !lai_mn_lu%diminfo => dim_2d_cosmo
    !lai_mn_lu%grid_mapping = TRIM(grid_mapping)
    !lai_mn_lu%coordinates = TRIM(coordinates)
    !CALL netcdf_put_var(ncid,var_real_2d,rs_min_lu_meta,undefined)
    CALL netcdf_put_var(ncid,var_real_2d,lai_mn_lu_meta,undefined)
    !
    ! plcov_mn_lu
    var_real_2d(:,:) = plcov_mn_lu(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    !plcov_mn_lu_meta%diminfo => dim_2d_cosmo
    !plcov_mn_lu_meta%grid_mapping = TRIM(grid_mapping)
    !plcov_mn_lu_meta%coordinates = TRIM(coordinates)
    CALL netcdf_put_var(ncid,var_real_2d,plcov_mn_lu_meta,undefined)
!roa nc<
    ! ndvi_max
    var_real_2d(:,:) = ndvi_max(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,ndvi_max_meta,undefined)

    ! hh_globe
    var_real_2d(:,:) = hh_globe(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,hh_globe_meta,undefined)
!roa nc>
    ! hh_fis
    var_real_2d(:,:) = grav * hh_globe(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,hh_fis_meta,undefined)
!roa nc<
    ! stdh_globe
    var_real_2d(:,:) = stdh_globe(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,stdh_globe_meta,undefined)

    ! theta_globe
    var_real_2d(:,:) = theta_globe(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,theta_globe_meta,undefined)

    ! aniso_globe
    var_real_2d(:,:) = aniso_globe(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,aniso_globe_meta,undefined)

    ! slope_globe
    var_real_2d(:,:) = slope_globe(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,slope_globe_meta,undefined)

    ! crutemp
    var_real_2d(:,:) = crutemp(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,crutemp_meta,undefined)

    ! fr_lake
    var_real_2d(:,:) = fr_lake(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,fr_lake_meta,undefined)

    ! lake_depth
    var_real_2d(:,:) = lake_depth(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1)
    CALL netcdf_put_var(ncid,var_real_2d,lake_depth_meta,undefined)

    !-----------------------------------------------------------------
    ! soiltype
    CALL netcdf_put_var(ncid,soiltype_fao(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1), &
      &                 soiltype_fao_meta,undef_int)

    !-----------------------------------------------------------------
    ! lu_class_fraction
    CALL netcdf_put_var(ncid,&
                       & lu_class_fraction(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:nclass_glc2000), &
                       & lu_class_fraction_meta, &
                       & undefined)

    !-----------------------------------------------------------------
    ! ndvi_field_mom
    CALL netcdf_put_var(ncid,&
                       & ndvi_field_mom(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:ntime_ndvi), &
                       & ndvi_field_mom_meta, &
                       & undefined)

    ! ndvi_ratio_mom
    CALL netcdf_put_var(ncid,&
                       & ndvi_ratio_mom(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:ntime_ndvi), &
                       & ndvi_ratio_mom_meta, &
                       & undefined)

    !-----------------------------------------------------------------
    ! aot
     n=1 ! aot_bc
     CALL netcdf_put_var(ncid,aot_tg(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:ntime_aot,1), &
       &                 aer_bc_meta, undefined)
     n=2 ! aot_dust
     CALL netcdf_put_var(ncid,aot_tg(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:ntime_aot,2), &
       &                 aer_dust_meta, undefined)

     n=3 ! aot_org
     CALL netcdf_put_var(ncid,aot_tg(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:ntime_aot,3), &
       &                 aer_org_meta, undefined)

     n=4 ! aot_so4
     CALL netcdf_put_var(ncid,aot_tg(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:ntime_aot,4), &
       &                 aer_so4_meta, undefined)

     n=5 ! aot_ss
     CALL netcdf_put_var(ncid,aot_tg(1:cosmo_grid%nlon_rot,1:cosmo_grid%nlat_rot,1,1:ntime_aot,5), &
       &                 aer_ss_meta, undefined)



    !-----------------------------------------------------------------
    CALL netcdf_def_grid_mapping(ncid, nc_grid_def_cosmo, varid)

    CALL close_netcdf_file(ncid)

  END SUBROUTINE write_netcdf_cosmo_grid_extpar
  !-----------------------------------------------------------------------

  !> netcdf output of GLC2000 derived ICON fields
  SUBROUTINE write_netcdf_icon_grid_extpar(netcdf_filename,  &
    &                                     icon_grid,       &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     name_lookup_table_glc2000, &
    &                                     lon_geo,     &
    &                                     lat_geo, &
    &                                     fr_land_lu, &
    &                                     lu_class_fraction,    &
    &                                     ice_lu, &
    &                                     z0_lu, &
    &                                     root_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu, &
    &                                     lake_depth, &
    &                                     fr_lake,    &
    &                                     soiltype_fao, &
    &                                     ndvi_max,  &
    &                                     ndvi_field_mom,&
    &                                     ndvi_ratio_mom, &
    &                                     hh_globe,            &
    &                                     stdh_globe,          &
    &                                     theta_globe,         &
    &                                     aniso_globe,         &
    &                                     slope_globe,   &
    &                                     vertex_param,  &
    &                                     aot_tg, &
    &                                     crutemp )

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

  USE mo_var_meta_data, ONLY: def_lu_fields_meta

  USE mo_var_meta_data, ONLY: dim_lu_tg

  USE mo_var_meta_data, ONLY: fr_land_lu_meta, lu_tot_npixel_meta, &
    &       lu_class_fraction_meta, lu_class_npixel_meta, &
    &       ice_lu_meta, z0_lu_meta, &
    &       plcov_mx_lu_meta, plcov_mn_lu_meta, &
    &       lai_mx_lu_meta, lai_mn_lu_meta, &
    &       rs_min_lu_meta, urban_lu_meta, &
    &       for_d_lu_meta, for_e_lu_meta, &
    &       emissivity_lu_meta, root_lu_meta
 
  USE mo_var_meta_data, ONLY: def_soil_meta
  USE mo_var_meta_data, ONLY: soiltype_fao_meta

  USE mo_var_meta_data, ONLY: dim_ndvi_tg
  USE mo_var_meta_data, ONLY: ndvi_max_meta, &
    &                         ndvi_field_mom_meta, &
    &                         ndvi_ratio_mom_meta,&
    &                         def_ndvi_meta

 USE mo_var_meta_data, ONLY: def_globe_meta, def_globe_vertex_meta
 USE mo_var_meta_data, ONLY: dim_buffer_cell, dim_buffer_vertex

 USE mo_var_meta_data, ONLY: hh_globe_meta, fr_land_globe_meta, &
   &       stdh_globe_meta, theta_globe_meta, &
   &       aniso_globe_meta, slope_globe_meta, &
   &       hh_vert_meta, npixel_vert_meta

  USE mo_var_meta_data, ONLY: dim_aot_tg, dim_aot_ty, &
    &                         def_aot_tg_meta
  USE mo_var_meta_data, ONLY: aot_tg_meta, aer_bc_meta,   & 
    &                         aer_dust_meta, aer_org_meta,&
    &                         aer_so4_meta, aer_ss_meta

 USE mo_var_meta_data, ONLY: crutemp_meta, &
    &                         def_crutemp_meta

  USE mo_var_meta_data, ONLY: def_flake_fields_meta
  USE mo_var_meta_data, ONLY: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta
  USE mo_flake_data, ONLY: flake_depth_undef !< default value for undefined lake depth


 USE mo_globe_tg_fields, ONLY: add_parameters_domain


  CHARACTER (len=*), INTENT(IN)      :: netcdf_filename !< filename for the netcdf file
  TYPE(icosahedral_triangular_grid), INTENT(IN) :: icon_grid !< structure which contains the definition of the ICON grid
  TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
  REAL(KIND=wp), INTENT(IN)          :: undefined       !< value to indicate undefined grid elements 
  INTEGER, INTENT(IN)                :: undef_int       !< value to indicate undefined grid elements
  REAL (KIND=wp), INTENT(IN) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
  REAL (KIND=wp), INTENT(IN) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system
  CHARACTER (len=*), INTENT(IN)      :: name_lookup_table_glc2000 !< name of lookup table
  REAL (KIND=wp), INTENT(IN)  :: lu_class_fraction(:,:,:,:)  !< fraction for each lu class on target grid (dimension (ie,je,ke,nclass_glc2000))
  REAL (KIND=wp), INTENT(IN)  :: fr_land_lu(:,:,:) !< fraction land due to lu raw data
  REAL (KIND=wp), INTENT(IN)  :: ice_lu(:,:,:)     !< fraction of ice due to lu raw data
  REAL (KIND=wp), INTENT(IN)  :: z0_lu(:,:,:)      !< roughness length due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: root_lu(:,:,:)    !< root depth due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: plcov_mx_lu(:,:,:)!< plant cover maximum due to lu land use data
  !REAL (KIND=wp), INTENT(IN)  :: plcov_mn_lu(:,:,:)!< plant cover minimum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: lai_mx_lu(:,:,:)  !< Leaf Area Index maximum due to lu land use data
  !REAL (KIND=wp), INTENT(IN)  :: lai_mn_lu(:,:,:)  !< Leaf Area Index minimum due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: rs_min_lu(:,:,:)  !< minimal stomata resistance due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: urban_lu(:,:,:)   !< urban fraction due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: for_d_lu(:,:,:)   !< deciduous forest (fraction) due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: for_e_lu(:,:,:)   !< evergreen forest (fraction) due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: emissivity_lu(:,:,:) !< longwave emissivity due to lu land use data
  REAL (KIND=wp), INTENT(IN)  :: lake_depth(:,:,:) !< lake depth
  REAL (KIND=wp), INTENT(IN)  :: fr_lake(:,:,:)     !< fraction of fresh water (lakes)
  INTEGER(KIND=i4), INTENT(IN) :: soiltype_fao(:,:,:) !< soiltype due to FAO Digital Soil map of the World
  REAL (KIND=wp), INTENT(IN) :: ndvi_max(:,:,:) !< field for ndvi maximum
  REAL (KIND=wp), INTENT(IN) :: ndvi_field_mom(:,:,:,:) !< field for monthly mean ndvi data (12 months)
  REAL (KIND=wp), INTENT(IN) :: ndvi_ratio_mom(:,:,:,:) !< field for monthly ndvi ratio (12 months)
  REAL(KIND=wp), INTENT(IN)  :: hh_globe(:,:,:)  !< mean height 
  REAL(KIND=wp), INTENT(IN)  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height
  REAL(KIND=wp), INTENT(IN)  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
  REAL(KIND=wp), INTENT(IN)  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
  REAL(KIND=wp), INTENT(IN)  :: slope_globe(:,:,:) !< sso parameter, mean slope
  TYPE(add_parameters_domain), INTENT(IN) :: vertex_param !< additional external parameters for ICON domain
  REAL (KIND=wp), INTENT(IN)  :: aot_tg(:,:,:,:,:) !< aerosol optical thickness, aot_tg(ie,je,ke,ntime,ntype)
  REAL(KIND=wp), INTENT(IN)  :: crutemp(:,:,:)  !< cru climatological temperature , crutemp(ie,je,ke) 
  ! local variables

  INTEGER :: ndims 
  INTEGER :: ncid
  INTEGER :: varid
  TYPE(dim_meta_info), ALLOCATABLE :: dim_list(:) !< dimensions for netcdf file
  TYPE(dim_meta_info), TARGET :: dim_2d_lu(1:2)
  TYPE(dim_meta_info), TARGET :: dim_2d_ndvi(1:2)
  TYPE(dim_meta_info), TARGET :: dim_1d_icon(1:1)
  TYPE(dim_meta_info), TARGET :: dim_1d_icon_v(1:1)
  TYPE(dim_meta_info), TARGET :: dim_3d_aot(1:3)

  INTEGER, PARAMETER :: nglob_atts=6
  TYPE(netcdf_attributes) :: global_attributes(nglob_atts)

  INTEGER :: errorcode !< error status variable

  CHARACTER (len=80):: grid_mapping !< netcdf attribute grid mapping
  CHARACTER (len=80):: coordinates  !< netcdf attribute coordinates
  INTEGER :: n !< counter
  INTEGER :: nvertex !< total number of vertices


    !-------------------------------------------------------------
    !set up dimensions for buffer netcdf output 
    ndims = 5
    ALLOCATE(dim_list(1:ndims),STAT=errorcode)
    IF (errorcode /= 0 ) CALL abort_extpar('Cant allocate array dim_list')

    dim_list(1)%dimname = 'cell'
    dim_list(1)%dimsize = icon_grid%ncell 
    dim_list(2)%dimname = 'nclass' ! nclass_glc2000
    dim_list(2)%dimsize = nclass_glc2000 
    dim_list(3)%dimname = 'ntime'
    dim_list(3)%dimsize = ntime_aot
    dim_list(4)%dimname = 'ntype'
    dim_list(4)%dimsize = ntype_aot
    dim_list(5)%dimname = 'vertex'
    dim_list(5)%dimsize = icon_grid%nvertex

    dim_1d_icon =  dim_list(1) ! cell
    dim_1d_icon_v =  dim_list(5) ! nvertex
    
    dim_2d_lu(1) = dim_list(1) ! cell
    dim_2d_lu(2) = dim_list(2) ! nclass_glc2000

    dim_2d_ndvi(1) = dim_list(1) ! cell
    dim_2d_ndvi(2) = dim_list(3) ! ntime_ndvi

    dim_3d_aot(1) = dim_list(1) ! cell
    dim_3d_aot(2) = dim_list(3) ! ntime
    dim_3d_aot(3) = dim_list(4) ! ntype

    ! define global attributes
    CALL set_global_att_extpar(global_attributes,name_lookup_table_glc2000)

    !set up dimensions for buffer
    CALL  def_dimension_info_buffer(tg)
    ! dim_3d_tg

    !set up dimensions for ICON grid
    CALL def_dimension_info_icon(icon_grid)
    ! dim_icon

    ! define meta information for various land use related variables (GLC2000) for netcdf output
    CALL def_lu_fields_meta(tg,nclass_glc2000,dim_1d_icon)
    ! dim_lu_tg
    ! fr_land_lu_meta, lu_tot_npixel_meta, &
    !  &       lu_class_fraction_meta, lu_class_npixel_meta, &
    !  &       ice_lu_meta, z0_lu_meta, &
    !  &       plcov_mx_lu_meta, plcov_mn_lu_meta, &
    !  &       lai_mx_lu_meta, lai_mn_lu_meta, &
    !  &       rs_min_lu_meta, urban_lu_meta, &
    !  &       for_d_lu_meta, for_e_lu_meta, &
    !  &       emissivity_lu_meta, root_lu_meta

    ! define meta information for target field variables lon_geo, lat_geo 
    CALL def_com_target_fields_meta(dim_1d_icon)
    ! lon_geo_meta and lat_geo_meta

    ! set mapping parameters for netcdf
    grid_mapping="lon_lat_on_sphere"
    coordinates="lon lat"

    CALL set_nc_grid_def_icon(grid_mapping)
    ! nc_grid_def_icon
    
    CALL def_soil_meta(dim_1d_icon)
    !  fr_land_soil_meta, soiltype_fao_meta
    
    !define meta information for various NDVI data related variables for netcdf output
    CALL def_ndvi_meta(tg,ntime_ndvi,dim_1d_icon)
    ! dim_ndvi_tg, ndvi_max_meta, ndvi_field_mom_meta, ndvi_ratio_mom_meta

      ! define meta information for various GLOBE data related variables for netcdf output
    CALL def_globe_meta(dim_1d_icon)

    !  hh_globe_meta, fr_land_globe_meta, &
    !         stdh_globe_meta, theta_globe_meta, &
    !         aniso_globe_meta, slope_globe_meta, &
    !         hh_vert_meta, npixel_vert_meta
    !\TODO HA: this is a "quick fix" for ICON, find a better solution
    hh_globe_meta%varname = 'topography_c'

    CALL def_globe_vertex_meta(icon_grid%nvertex)
    ! dim_buffer_vertex
    !  hh_vert_meta, npixel_vert_meta
    
    ! define dimensions and meta information for variable aot_tg for netcdf output
    CALL def_aot_tg_meta(tg,ntime_aot,ntype_aot,dim_1d_icon)
    ! dim_aot_tg and aot_tg_meta
    ! dim_aot_ty, aer_bc_meta, aer_dust_meta, aer_org_meta, aer_so4_meta, aer_ss_meta

    ! define meta information for variable crutemp for netcdf output
    CALL def_crutemp_meta(dim_1d_icon)
    ! crutemp_meta

    ! define meta information for various land use related variables (FLAKE) for netcdf output
    CALL def_flake_fields_meta(dim_1d_icon)
    ! lake_depth_meta, fr_lake_meta, &
    !  &       flake_tot_npixel_meta

   
   
    !-----------------------------------------------------------------
    PRINT *,' CALL open_new_netcdf_file'
    CALL open_new_netcdf_file(netcdf_filename=TRIM(netcdf_filename),   &
        &                       dim_list=dim_list,                  &
        &                       global_attributes=global_attributes, &
        &                       ncid=ncid)
    !-----------------------------------------------------------------
    ! soiltype
    CALL netcdf_put_var(ncid,soiltype_fao(1:icon_grid%ncell,1,1),soiltype_fao_meta,undef_int)

    !-----------------------------------------------------------------
    n=1 ! fr_land_lu
    CALL netcdf_put_var(ncid,fr_land_lu(1:icon_grid%ncell,1,1),fr_land_lu_meta,undefined)

    n=2 ! ice_lu
    CALL netcdf_put_var(ncid,ice_lu(1:icon_grid%ncell,1,1),ice_lu_meta,undefined)

    n=3 ! plcov_mx_lu
    CALL netcdf_put_var(ncid,plcov_mx_lu(1:icon_grid%ncell,1,1),plcov_mx_lu_meta,undefined)

    n=4 ! lai_mx_lu
    CALL netcdf_put_var(ncid,lai_mx_lu(1:icon_grid%ncell,1,1),lai_mx_lu_meta,undefined)

    n=5 ! rs_min_lu
    CALL netcdf_put_var(ncid,rs_min_lu(1:icon_grid%ncell,1,1),rs_min_lu_meta,undefined)

    n=6 ! urban_lu
    CALL netcdf_put_var(ncid,urban_lu(1:icon_grid%ncell,1,1),urban_lu_meta,undefined)

    n=7 ! for_d_lu
    CALL netcdf_put_var(ncid,for_d_lu(1:icon_grid%ncell,1,1),for_d_lu_meta,undefined)

    n=8 ! for_e_lu
    CALL netcdf_put_var(ncid,for_e_lu(1:icon_grid%ncell,1,1),for_e_lu_meta,undefined)

    n=9 ! emissivity_lu
    CALL netcdf_put_var(ncid, emissivity_lu(1:icon_grid%ncell,1,1),emissivity_lu_meta,undefined)

    n=10 ! root_lu
    CALL netcdf_put_var(ncid,root_lu(1:icon_grid%ncell,1,1),root_lu_meta,undefined)

    n=11 ! z0_lu
    CALL netcdf_put_var(ncid,z0_lu(1:icon_grid%ncell,1,1),z0_lu_meta,undefined)

    n=12 ! lon
    CALL netcdf_put_var(ncid,lon_geo(1:icon_grid%ncell,1,1),lon_geo_meta,undefined)

    n=13 ! lat
    CALL netcdf_put_var(ncid,lat_geo(1:icon_grid%ncell,1,1),lat_geo_meta,undefined)

    !n=14 ! lai_mn_lu
    !CALL netcdf_put_var(ncid,lai_mn_lu(1:icon_grid%ncell,1,1),lai_mn_lu_meta,undefined)
    !
    !n=15 ! plcov_mn_lu
    !CALL netcdf_put_var(ncid,plcov_mn_lu(1:icon_grid%ncell,1,1),plcov_mn_lu_meta,undefined)

    n=14 ! ndvi_max
    CALL netcdf_put_var(ncid,ndvi_max(1:icon_grid%ncell,1,1),ndvi_max_meta,undefined)

    n=15 ! hh_globe
    CALL netcdf_put_var(ncid,hh_globe(1:icon_grid%ncell,1,1),hh_globe_meta,undefined)

    n=16 ! stdh_globe
    CALL netcdf_put_var(ncid,stdh_globe(1:icon_grid%ncell,1,1),stdh_globe_meta,undefined)

    n=17 ! theta_globe
    CALL netcdf_put_var(ncid,theta_globe(1:icon_grid%ncell,1,1),theta_globe_meta,undefined)

    n=18 ! aniso_globe
    CALL netcdf_put_var(ncid,aniso_globe(1:icon_grid%ncell,1,1),aniso_globe_meta,undefined)

    n=19 ! slope_globe
    CALL netcdf_put_var(ncid,slope_globe(1:icon_grid%ncell,1,1),slope_globe_meta,undefined)

    n=20 ! crutemp
    CALL netcdf_put_var(ncid,crutemp(1:icon_grid%ncell,1,1),crutemp_meta,undefined)

     n=21 ! fr_lake
     CALL netcdf_put_var(ncid,fr_lake(1:icon_grid%ncell,1,1),fr_lake_meta,undefined)

     n=22 ! lake_depth
     CALL netcdf_put_var(ncid,lake_depth(1:icon_grid%ncell,1,1),lake_depth_meta,undefined)

    n=23 ! for vertex_param%hh_vert
    nvertex = icon_grid%nvertex
    PRINT *,'nvertex ', nvertex
    CALL netcdf_put_var(ncid,vertex_param%hh_vert(1:nvertex,1,1),hh_vert_meta,undefined)

    n=1 ! lu_class_fraction

    CALL netcdf_put_var(ncid,lu_class_fraction(1:icon_grid%ncell,1,1,1:nclass_glc2000),&
      &                 lu_class_fraction_meta,undefined)

     n=2 ! ndvi_field_mom
     CALL netcdf_put_var(ncid,ndvi_field_mom(1:icon_grid%ncell,1,1,1:ntime_ndvi), &
       &                 ndvi_field_mom_meta, undefined)

     n=3 ! ndvi_ratio_mom
     CALL netcdf_put_var(ncid,ndvi_ratio_mom(1:icon_grid%ncell,1,1,1:ntime_ndvi), &
       &                 ndvi_ratio_mom_meta, undefined)

     n=1 ! aot_bc
     CALL netcdf_put_var(ncid,aot_tg(1:icon_grid%ncell,1,1,1:ntime_aot,1), &
       &                 aer_bc_meta, undefined)
     n=2 ! aot_dust
     CALL netcdf_put_var(ncid,aot_tg(1:icon_grid%ncell,1,1,1:ntime_aot,2), &
       &                 aer_dust_meta, undefined)

     n=3 ! aot_org
     CALL netcdf_put_var(ncid,aot_tg(1:icon_grid%ncell,1,1,1:ntime_aot,3), &
       &                 aer_org_meta, undefined)

     n=4 ! aot_so4
     CALL netcdf_put_var(ncid,aot_tg(1:icon_grid%ncell,1,1,1:ntime_aot,4), &
       &                 aer_so4_meta, undefined)

     n=5 ! aot_ss
     CALL netcdf_put_var(ncid,aot_tg(1:icon_grid%ncell,1,1,1:ntime_aot,5), &
       &                 aer_ss_meta, undefined)



     !-----------------------------------------------------------------

      CALL close_netcdf_file(ncid)



  END SUBROUTINE write_netcdf_icon_grid_extpar
  !-----------------------------------------------------------------------

  !----------------------------------------------------------------------- 
  !-----------------------------------------------------------------------
  !> set global attributes for netcdf with lu data
  SUBROUTINE set_global_att_extpar(global_attributes,name_lookup_table_glc2000)
    TYPE(netcdf_attributes), INTENT(INOUT) :: global_attributes(1:6)
    CHARACTER (LEN=*) :: name_lookup_table_glc2000

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
    global_attributes(1)%attributetext='external parameter'
    global_attributes(2)%attname = 'institution'
    global_attributes(2)%attributetext='Deutscher Wetterdienst'

    global_attributes(3)%attname = 'rawdata'
    global_attributes(3)%attributetext='GLC2000, FAO DSMW, GLOBE, Lake Database'

    global_attributes(4)%attname = 'note'
    global_attributes(4)%attributetext='Landuse data look-up table: '//TRIM(name_lookup_table_glc2000)

    CALL DATE_AND_TIME(ydate,ytime)
    READ(ydate,'(4A2)') cc,yy,mm,dd
    READ(ytime,'(2A2)') hh, minute

    ydate=TRIM(cc)//TRIM(yy)//'-'//TRIM(mm)//'-'//TRIM(dd)
    ytime=TRIM(hh)//':'//TRIM(minute) 

    global_attributes(5)%attname = 'history'
    global_attributes(5)%attributetext=TRIM(ydate)//'T'//TRIM(ytime)//' extpar_consistency_check'

    global_attributes(6)%attname = 'comment'
    global_attributes(6)%attributetext='Experimental generation of external parameters '//CHAR(10)// &
      &                                 'for numerical atmospheric models COSMO and ICON.'

  END SUBROUTINE set_global_att_extpar
  !-----------------------------------------------------------------------

    
 
END Module mo_extpar_output_nc

