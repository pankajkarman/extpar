!+ Fortran module with definitions of meta information for variables and dimension for the output
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  Add meta information for aerosol types         
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with definitions of meta information for variables and dimension for the output
!> \author Hermann Asensio
MODULE mo_var_meta_data

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4

  USE mo_io_utilities, ONLY: dim_meta_info
  USE mo_io_utilities, ONLY: var_meta_info
  USE mo_io_utilities, ONLY: vartype_int, vartype_real, vartype_char
  USE mo_io_utilities, ONLY: netcdf_grid_mapping

  USE mo_grid_structures, ONLY: target_grid_def
  USE mo_grid_structures, ONLY: rotated_lonlat_grid
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid

  USE mo_utilities_extpar, ONLY: abort_extpar


  IMPLICIT NONE

  PRIVATE

  PUBLIC :: dim_3d_tg, def_dimension_info_buffer

  PUBLIC :: dim_rlon_cosmo, dim_rlat_cosmo, dim_2d_cosmo, def_dimension_info_cosmo
  PUBLIC :: rlon_meta, rlat_meta

  PUBLIC :: dim_icon, def_dimension_info_icon

  PUBLIC :: dim_aot_tg, dim_aot_ty
  PUBLIC :: aot_tg_meta, aer_bc_meta, aer_dust_meta, aer_org_meta, aer_so4_meta, aer_ss_meta

  PUBLIC :: def_aot_tg_meta
  PUBLIC :: aot_type_shortname

  PUBLIC :: lon_geo_meta, lat_geo_meta, no_raw_data_pixel_meta, def_com_target_fields_meta

  PUBLIC :: crutemp_meta, def_crutemp_meta

  PUBLIC :: nc_grid_def_cosmo, set_nc_grid_def_cosmo

  PUBLIC :: nc_grid_def_icon, set_nc_grid_def_icon

  PUBLIC :: dim_glc2000_tg

  PUBLIC :: fr_land_glc2000_meta, glc2000_tot_npixel_meta, &
    &       glc2000_class_fraction_meta, glc2000_class_npixel_meta, &
    &       ice_glc2000_meta, z0_glc2000_meta, &
    &       plcov_mx_glc2000_meta, plcov_mn_glc2000_meta, &
    &       lai_mx_glc2000_meta, lai_mn_glc2000_meta, &
    &       rs_min_glc2000_meta, urban_glc2000_meta, &
    &       for_d_glc2000_meta, for_e_glc2000_meta, &
    &       emissivity_glc2000_meta, root_glc2000_meta
  
  PUBLIC :: dim_glcc_tg

  PUBLIC :: fr_land_glcc_meta, glcc_tot_npixel_meta, &
    &       glcc_class_fraction_meta, glcc_class_npixel_meta, &
    &       ice_glcc_meta, z0_glcc_meta, &
    &       plcov_mx_glcc_meta, plcov_mn_glcc_meta, &
    &       lai_mx_glcc_meta, lai_mn_glcc_meta, &
    &       rs_min_glcc_meta, urban_glcc_meta, &
    &       for_d_glcc_meta, for_e_glcc_meta, &
    &       emissivity_glcc_meta, root_glcc_meta

  PUBLIC :: def_glcc_fields_meta

  PUBLIC :: dim_lu_tg

  PUBLIC :: fr_land_lu_meta, lu_tot_npixel_meta, &
    &       lu_class_fraction_meta, lu_class_npixel_meta, &
    &       ice_lu_meta, z0_lu_meta, &
    &       plcov_mx_lu_meta, plcov_mn_lu_meta, &
    &       lai_mx_lu_meta, lai_mn_lu_meta, &
    &       rs_min_lu_meta, urban_lu_meta, &
    &       for_d_lu_meta, for_e_lu_meta, &
    &       emissivity_lu_meta, root_lu_meta, &
    &       fr_ocean_lu_meta
  
  PUBLIC :: def_lu_fields_meta

  PUBLIC :: def_glc2000_fields_meta
  PUBLIC :: dim_buffer_cell, dim_buffer_vertex

  PUBLIC :: hh_globe_meta, fr_land_globe_meta, &
    &       stdh_globe_meta, theta_globe_meta, &
    &       aniso_globe_meta, slope_globe_meta, &
    &       hh_vert_meta, npixel_vert_meta,    &
    &       hh_fis_meta, z0_topo_meta
  
  PUBLIC :: def_globe_meta, def_globe_vertex_meta

  PUBLIC :: def_soil_meta
  PUBLIC :: fr_land_soil_meta, soiltype_fao_meta

  PUBLIC :: lake_depth_meta, fr_lake_meta, &
    &       flake_tot_npixel_meta

  PUBLIC :: def_flake_fields_meta


  PUBLIC :: dim_ndvi_tg, def_ndvi_meta
  PUBLIC :: ndvi_max_meta, ndvi_field_mom_meta, ndvi_ratio_mom_meta
  
  TYPE(dim_meta_info), TARGET :: dim_3d_tg(1:3)
  TYPE(dim_meta_info), TARGET :: dim_rlon_cosmo(1:1)
  TYPE(dim_meta_info), TARGET :: dim_rlat_cosmo(1:1)
  TYPE(dim_meta_info), TARGET :: dim_2d_cosmo(1:2)
  TYPE(dim_meta_info), TARGET :: dim_icon(1:7)
  TYPE(dim_meta_info), TARGET :: dim_buffer_cell(1:3)
  TYPE(dim_meta_info), TARGET :: dim_buffer_vertex(1:3)
  TYPE(dim_meta_info), TARGET,ALLOCATABLE :: dim_aot_tg(:) !< dimensions for field with all aerosol types
  TYPE(dim_meta_info), TARGET,ALLOCATABLE :: dim_aot_ty(:) !< dimensions for fields with single aerosol types 
  TYPE(dim_meta_info), TARGET,ALLOCATABLE :: dim_glc2000_tg(:)
  TYPE(dim_meta_info), TARGET,ALLOCATABLE :: dim_glcc_tg(:)

  TYPE(dim_meta_info), TARGET, ALLOCATABLE :: dim_lu_tg(:)
  TYPE(dim_meta_info), TARGET, ALLOCATABLE :: dim_ndvi_tg(:)

  TYPE(var_meta_info) :: aot_tg_meta !< additional information for variable aot_tg with all aerosol fields
  TYPE(var_meta_info) :: aer_bc_meta !< additional information for variable with aerosol optical thickness of black carbon
  TYPE(var_meta_info) :: aer_dust_meta !< additional information for variable with aerosol optical thickness of dust 
  TYPE(var_meta_info) :: aer_org_meta !< additional information for variable with aerosol optical thickness of organic matter
  TYPE(var_meta_info) :: aer_so4_meta !< additional information for variable with aerosol optical thickness of sulfate
  TYPE(var_meta_info) :: aer_ss_meta !< additional information for variable with aerosol optical thickness of sea salt

  TYPE(var_meta_info) :: ndvi_max_meta !< additional information for variable 
  TYPE(var_meta_info) :: ndvi_field_mom_meta !< additional information for variable 
  TYPE(var_meta_info) :: ndvi_ratio_mom_meta !< additional information for variable 

  TYPE(var_meta_info) :: crutemp_meta !< additional information for variable crutemp

  TYPE(var_meta_info) :: lon_geo_meta !< additional information for variable lon_geo_meta
  TYPE(var_meta_info) :: lat_geo_meta !< additional information for variable lat_geo_meta
  TYPE(var_meta_info) :: no_raw_data_pixel_meta !< additional information for variable no_raw_data_pixel_meta

  TYPE(var_meta_info) :: rlon_meta !< additional information for variable
  TYPE(var_meta_info) :: rlat_meta !< additional information for variable

  TYPE(var_meta_info) :: fr_land_glc2000_meta  !< additional information for variable
  TYPE(var_meta_info) :: glc2000_tot_npixel_meta !< additional information for variable
  TYPE(var_meta_info) :: glc2000_class_npixel_meta !< additional information for variable
  TYPE(var_meta_info) :: glc2000_class_fraction_meta !< additional information for variable
  TYPE(var_meta_info) :: ice_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: z0_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: root_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: plcov_mx_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: plcov_mn_glc2000_meta !< additional information for variable 
  TYPE(var_meta_info) :: lai_mx_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: lai_mn_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: rs_min_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: urban_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: for_d_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: for_e_glc2000_meta !< additional information for variable
  TYPE(var_meta_info) :: emissivity_glc2000_meta !< additional information for variable

  
  TYPE(var_meta_info) :: fr_land_glcc_meta  !< additional information for variable
  TYPE(var_meta_info) :: glcc_tot_npixel_meta !< additional information for variable
  TYPE(var_meta_info) :: glcc_class_npixel_meta !< additional information for variable
  TYPE(var_meta_info) :: glcc_class_fraction_meta !< additional information for variable
  TYPE(var_meta_info) :: ice_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: z0_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: root_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: plcov_mx_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: plcov_mn_glcc_meta !< additional information for variable 
  TYPE(var_meta_info) :: lai_mx_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: lai_mn_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: rs_min_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: urban_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: for_d_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: for_e_glcc_meta !< additional information for variable
  TYPE(var_meta_info) :: emissivity_glcc_meta !< additional information for variable



  TYPE(var_meta_info) :: fr_land_lu_meta  !< additional information for variable
  TYPE(var_meta_info) :: lu_tot_npixel_meta !< additional information for variable
  TYPE(var_meta_info) :: lu_class_npixel_meta !< additional information for variable
  TYPE(var_meta_info) :: lu_class_fraction_meta !< additional information for variable
  TYPE(var_meta_info) :: ice_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: z0_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: root_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: plcov_mx_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: plcov_mn_lu_meta !< additional information for variable 
  TYPE(var_meta_info) :: lai_mx_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: lai_mn_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: rs_min_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: urban_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: for_d_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: for_e_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: emissivity_lu_meta !< additional information for variable
  TYPE(var_meta_info) :: fr_ocean_lu_meta  !< additional information for variable

  TYPE(var_meta_info) :: hh_globe_meta  !< additional information for variable
  TYPE(var_meta_info) :: hh_fis_meta    !< additional information for variable
  TYPE(var_meta_info) :: fr_land_globe_meta  !< additional information for variable
  TYPE(var_meta_info) :: stdh_globe_meta  !< additional information for variable
  TYPE(var_meta_info) :: theta_globe_meta  !< additional information for variable
  TYPE(var_meta_info) :: aniso_globe_meta  !< additional information for variable
  TYPE(var_meta_info) :: slope_globe_meta  !< additional information for variable
  TYPE(var_meta_info) :: hh_vert_meta  !< additional information for variable
  TYPE(var_meta_info) :: npixel_vert_meta  !< additional information for variable
  TYPE(var_meta_info) :: z0_topo_meta  !< additional information for variable

  TYPE(var_meta_info) :: fr_land_soil_meta !< additional information for variable
  TYPE(var_meta_info) :: soiltype_fao_meta !< additional information for variable

  TYPE(var_meta_info) :: lake_depth_meta !< additional information for variable
  TYPE(var_meta_info) :: fr_lake_meta !< additional information for variable
  TYPE(var_meta_info) :: flake_tot_npixel_meta !< additional information for variable


  TYPE(netcdf_grid_mapping) :: nc_grid_def_cosmo !< mapping parameters for netcdf
  TYPE(netcdf_grid_mapping) :: nc_grid_def_icon !< mapping parameters for netcdf

  CHARACTER (len=1), PARAMETER :: c_undef = "-" !< default character for undefined string
  CHARACTER (len=80) :: aot_type_shortname(1:5) !< short names for optical thickness of aerosol types for GRIB_API



  CONTAINS
  

  !> define buffer dimensions for netcdf output
  SUBROUTINE def_dimension_info_buffer(tg)
    TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description

    ! set meta information for strucutre dim_3d_tg
    dim_3d_tg(1)%dimname = 'ie'
    dim_3d_tg(1)%dimsize = tg%ie
    dim_3d_tg(2)%dimname = 'je'
    dim_3d_tg(2)%dimsize = tg%je
    dim_3d_tg(3)%dimname = 'ke'
    dim_3d_tg(3)%dimsize = tg%ke

  END SUBROUTINE def_dimension_info_buffer
  

  !> define COSMO grid dimensions for netcdf output
  SUBROUTINE def_dimension_info_cosmo(cosmo_grid)
    TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid

    ! set meta information for strucutre dim_rlon_cosmo
    dim_rlon_cosmo(1)%dimname = 'rlon'
    dim_rlon_cosmo(1)%dimsize = cosmo_grid%nlon_rot

    ! set meta information for strucutre dim_rlat_cosmo
    dim_rlat_cosmo(1)%dimname = 'rlat'
    dim_rlat_cosmo(1)%dimsize = cosmo_grid%nlat_rot

    ! set meta information for strucutre dim_2d_cosmo
    dim_2d_cosmo(1) = dim_rlon_cosmo(1)
    dim_2d_cosmo(2) = dim_rlat_cosmo(1)

    ! set meta information for variable rlon

    rlon_meta%varname = 'rlon'
    rlon_meta%n_dim = 1
    rlon_meta%diminfo => dim_rlon_cosmo
    rlon_meta%vartype = vartype_real !REAL variable
    rlon_meta%standard_name = "grid longitude" 
    rlon_meta%long_name =  "longitude in rotated pole grid"
    rlon_meta%shortName = c_undef
    rlon_meta%units =  "degrees"
    rlon_meta%grid_mapping = c_undef
    rlon_meta%coordinates = c_undef
    
    ! set meta information for variable rlat

    rlat_meta%varname = 'rlat'
    rlat_meta%n_dim = 1
    rlat_meta%diminfo => dim_rlat_cosmo
    rlat_meta%vartype = vartype_real !REAL variable
    rlat_meta%standard_name = "grid latitude" 
    rlat_meta%long_name =  "latitude in rotated pole grid"
    rlat_meta%shortName = c_undef
    rlat_meta%units =  "degrees"
    rlat_meta%grid_mapping = c_undef
    rlat_meta%coordinates = c_undef


  END SUBROUTINE def_dimension_info_cosmo


  !> define ICON grid dimensions for netcdf output
  SUBROUTINE def_dimension_info_icon(icon_grid)
    TYPE(icosahedral_triangular_grid), INTENT(IN) :: icon_grid !< structure which contains the definition of the ICON grid

    ! set meta information for strucutre dim_icon
    dim_icon(1)%dimname = 'cell'
    dim_icon(1)%dimsize = icon_grid%ncell ! total number of cells in ICON grid
    dim_icon(2)%dimname = 'vertex'
    dim_icon(2)%dimsize = icon_grid%nvertex ! total number of vertices in ICON grid
    dim_icon(3)%dimname = 'edge'
    dim_icon(3)%dimsize = icon_grid%nedge ! total number of edges in ICON grid

    dim_icon(4)%dimname = 'nc'
    dim_icon(4)%dimsize = icon_grid%ncells_per_edge ! number of cell per edge in ICON grid
    dim_icon(5)%dimname = 'nv'
    dim_icon(5)%dimsize = icon_grid%nvertex_per_cell ! number of vertices per cell
    dim_icon(6)%dimname = 'ne'
    dim_icon(6)%dimsize = icon_grid%nedges_per_vertex ! number of edges per vertex
    dim_icon(7)%dimname = 'no'
    dim_icon(7)%dimsize = icon_grid%nchilds_per_cell ! number of childs per cell

  END SUBROUTINE def_dimension_info_icon
  

  !> define meta information for variable crutemp for netcdf output
  SUBROUTINE def_crutemp_meta(diminfo,coordinates,grid_mapping)
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)
    n_dim = SIZE(diminfo)


  
    crutemp_meta%varname = 'tem_clim'
    crutemp_meta%n_dim = n_dim
    crutemp_meta%diminfo => diminfo
    crutemp_meta%vartype = vartype_real !REAL variable
    crutemp_meta%standard_name = 'CRU T'
    crutemp_meta%long_name = 'CRU near surface temperature climatology'
    crutemp_meta%shortName = 'T_2M_CL'
    crutemp_meta%units = 'K'
    crutemp_meta%grid_mapping = gridmp
    crutemp_meta%coordinates = coord
    
  END SUBROUTINE def_crutemp_meta

  !> define meta information for soil data for netcdf output
  SUBROUTINE def_soil_meta(diminfo,coordinates,grid_mapping)
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)
    n_dim = SIZE(diminfo)

  
    fr_land_soil_meta%varname = 'fr_land_soil'
    fr_land_soil_meta%n_dim = n_dim
    fr_land_soil_meta%diminfo => diminfo
    fr_land_soil_meta%vartype = vartype_real !REAL variable
    fr_land_soil_meta%standard_name = 'FR_LAND soil'
    fr_land_soil_meta%long_name = 'Fraction Land due to FAO Digital Soil Map of the World'
    fr_land_soil_meta%shortName = 'FR_LAND'
    fr_land_soil_meta%units = c_undef
    fr_land_soil_meta%grid_mapping = gridmp
    fr_land_soil_meta%coordinates = coord

     
    soiltype_fao_meta%varname = 'soiltype_fao'
    soiltype_fao_meta%n_dim = n_dim
    soiltype_fao_meta%diminfo => diminfo
    soiltype_fao_meta%vartype = vartype_int !REAL variable
    soiltype_fao_meta%standard_name = 'SOILTYP'
    soiltype_fao_meta%long_name = 'soil type derived from FAO Digital Soil Map of the World'
    soiltype_fao_meta%shortName = 'SOILTYP'
    soiltype_fao_meta%units = c_undef
    soiltype_fao_meta%grid_mapping = gridmp
    soiltype_fao_meta%coordinates = coord

    
  END SUBROUTINE def_soil_meta

  !> define meta information for NDVI data for netcdf output
  SUBROUTINE def_ndvi_meta(tg,ntime,diminfo,coordinates,grid_mapping)
    TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
    INTEGER (KIND=i4), INTENT(IN) :: ntime !< number of times
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)
    n_dim = SIZE(diminfo)

    ! set meta information for strucutre dim_ndvi_tg
    IF (ALLOCATED(dim_ndvi_tg)) DEALLOCATE(dim_ndvi_tg)
    ALLOCATE(dim_ndvi_tg(1:n_dim+1))
    SELECT CASE(n_dim)
    !CASE (1)
    !  dim_ndvi_tg(1)%dimname = 'ie'
    !  dim_ndvi_tg(1)%dimsize = tg%ie
    !  dim_ndvi_tg(2)%dimname = 'ntime'
    !  dim_ndvi_tg(2)%dimsize = ntime
    !CASE (2)
    !  dim_ndvi_tg(1)%dimname = 'ie'
    !  dim_ndvi_tg(1)%dimsize = tg%ie
    !  dim_ndvi_tg(2)%dimname = 'je'
    !  dim_ndvi_tg(2)%dimsize = tg%je
    !  dim_ndvi_tg(3)%dimname = 'ntime'
    !  dim_ndvi_tg(3)%dimsize = ntime
    !CASE (3)
    !  dim_ndvi_tg(1)%dimname = 'ie'
    !  dim_ndvi_tg(1)%dimsize = tg%ie
    !  dim_ndvi_tg(2)%dimname = 'je'
    !  dim_ndvi_tg(2)%dimsize = tg%je
    !  dim_ndvi_tg(3)%dimname = 'ke'
    !  dim_ndvi_tg(3)%dimsize = tg%ke
    !  dim_ndvi_tg(4)%dimname = 'ntime'
    !  dim_ndvi_tg(4)%dimsize = ntime
      CASE (1)
      dim_ndvi_tg(1)%dimname = diminfo(1)%dimname 
      dim_ndvi_tg(1)%dimsize = diminfo(1)%dimsize
      dim_ndvi_tg(2)%dimname = 'ntime'
      dim_ndvi_tg(2)%dimsize = ntime
    CASE (2)
      dim_ndvi_tg(1)%dimname = diminfo(1)%dimname
      dim_ndvi_tg(1)%dimsize = diminfo(1)%dimsize
      dim_ndvi_tg(2)%dimname = diminfo(2)%dimname
      dim_ndvi_tg(2)%dimsize = diminfo(2)%dimsize
      dim_ndvi_tg(3)%dimname = 'ntime'
      dim_ndvi_tg(3)%dimsize = ntime
    CASE (3)
      dim_ndvi_tg(1)%dimname = diminfo(1)%dimname
      dim_ndvi_tg(1)%dimsize = diminfo(1)%dimsize
      dim_ndvi_tg(2)%dimname = diminfo(2)%dimname
      dim_ndvi_tg(2)%dimsize = diminfo(2)%dimsize
      dim_ndvi_tg(3)%dimname = diminfo(3)%dimname
      dim_ndvi_tg(3)%dimsize = diminfo(3)%dimsize
      dim_ndvi_tg(4)%dimname = 'ntime'
      dim_ndvi_tg(4)%dimsize = ntime
    END SELECT

  
    ndvi_max_meta%varname = 'ndvi_max'
    ndvi_max_meta%n_dim = n_dim
    ndvi_max_meta%diminfo => diminfo
    ndvi_max_meta%vartype = vartype_real !REAL variable
    ndvi_max_meta%standard_name = 'NDVI_MAX soil'
    ndvi_max_meta%long_name = 'NDVI yearly maximum for climatology 1998-2003'
    ndvi_max_meta%shortName = 'NDVI_MAX'
    ndvi_max_meta%units = c_undef
    ndvi_max_meta%grid_mapping = gridmp
    ndvi_max_meta%coordinates = coord
     
    ndvi_field_mom_meta%varname = 'ndvi_field_mom'
    ndvi_field_mom_meta%n_dim = n_dim + 1
    ndvi_field_mom_meta%diminfo => dim_ndvi_tg
    ndvi_field_mom_meta%vartype = vartype_real !REAL variable
    ndvi_field_mom_meta%standard_name = 'NDVI'
    ndvi_field_mom_meta%long_name = 'monthly mean NDVI climatology'
    ndvi_field_mom_meta%shortName = 'NDVI'
    ndvi_field_mom_meta%units = c_undef
    ndvi_field_mom_meta%grid_mapping = gridmp
    ndvi_field_mom_meta%coordinates = coord

    ndvi_ratio_mom_meta%varname = 'ndvi_ratio_mom'
    ndvi_ratio_mom_meta%n_dim = n_dim + 1
    ndvi_ratio_mom_meta%diminfo => dim_ndvi_tg
    ndvi_ratio_mom_meta%vartype = vartype_real !REAL variable
    ndvi_ratio_mom_meta%standard_name = 'NDVIRATIO'
    ndvi_ratio_mom_meta%long_name = '(monthly) proportion of actual value/maximum normalized differential vegetation index'
    ndvi_ratio_mom_meta%shortName = 'NDVIRATIO'
    ndvi_ratio_mom_meta%units = c_undef
    ndvi_ratio_mom_meta%grid_mapping = gridmp
    ndvi_ratio_mom_meta%coordinates = coord

    
  END SUBROUTINE def_ndvi_meta


  

  !> define dimensions and meta information for variable aot_tg for netcdf output
  SUBROUTINE def_aot_tg_meta(tg,ntime,ntype,diminfo,coordinates,grid_mapping)
    TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
    INTEGER (KIND=i8), INTENT(IN) :: ntime !< number of times
    INTEGER (KIND=i8), INTENT(IN) :: ntype !< number of types of aerosols
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)
    n_dim = SIZE(diminfo)

    ! set meta information for strucutre dim_aot_tg
    IF (ALLOCATED(dim_aot_tg)) DEALLOCATE(dim_aot_tg)
    ALLOCATE(dim_aot_tg(1:n_dim+2))

    ! set meta information for strucutre dim_aot_ty
    IF (ALLOCATED(dim_aot_ty)) DEALLOCATE(dim_aot_ty)
    ALLOCATE(dim_aot_ty(1:n_dim+1))


    SELECT CASE(n_dim)
    CASE (1)
      dim_aot_tg(1)%dimname = diminfo(1)%dimname
      dim_aot_tg(1)%dimsize = diminfo(1)%dimsize
      dim_aot_tg(2)%dimname = 'ntime'
      dim_aot_tg(2)%dimsize = ntime
      dim_aot_tg(3)%dimname = 'ntype'
      dim_aot_tg(3)%dimsize = ntype

      dim_aot_ty(1) = dim_aot_tg(1)
      dim_aot_ty(2) = dim_aot_tg(2)

    CASE (2)
      dim_aot_tg(1)%dimname = diminfo(1)%dimname
      dim_aot_tg(1)%dimsize = diminfo(1)%dimsize
      dim_aot_tg(2)%dimname = diminfo(2)%dimname
      dim_aot_tg(2)%dimsize = diminfo(2)%dimsize 
      dim_aot_tg(3)%dimname = 'ntime'
      dim_aot_tg(3)%dimsize = ntime
      dim_aot_tg(4)%dimname = 'ntype'
      dim_aot_tg(4)%dimsize = ntype

      dim_aot_ty(1) = dim_aot_tg(1)
      dim_aot_ty(2) = dim_aot_tg(2)
      dim_aot_ty(3) = dim_aot_tg(3)



    CASE (3)
      dim_aot_tg(1)%dimname = diminfo(1)%dimname
      dim_aot_tg(1)%dimsize = diminfo(1)%dimsize
      dim_aot_tg(2)%dimname = diminfo(2)%dimname
      dim_aot_tg(2)%dimsize = diminfo(2)%dimsize
      dim_aot_tg(3)%dimname = diminfo(3)%dimname
      dim_aot_tg(3)%dimsize = diminfo(3)%dimsize
      dim_aot_tg(4)%dimname = 'ntime'
      dim_aot_tg(4)%dimsize = ntime
      dim_aot_tg(5)%dimname = 'ntype'
      dim_aot_tg(5)%dimsize = ntype

      dim_aot_ty(1) = dim_aot_tg(1)
      dim_aot_ty(2) = dim_aot_tg(2)
      dim_aot_ty(3) = dim_aot_tg(3)
      dim_aot_ty(4) = dim_aot_tg(4)



    END SELECT
    ! set meta information for strucutre dim_aot_tg

    
    ! set meta information for variable aot_tg
    aot_tg_meta%varname = 'aot_tg'
    aot_tg_meta%n_dim = n_dim + 2
    aot_tg_meta%diminfo => dim_aot_tg
    aot_tg_meta%vartype = vartype_real !REAL variable
    aot_tg_meta%standard_name = 'aot'
    aot_tg_meta%long_name = 'aerosol optical thickness'
    aot_tg_meta%shortName = 'AOT'
    aot_tg_meta%units = c_undef
    aot_tg_meta%grid_mapping = gridmp
    aot_tg_meta%coordinates = coord

    aot_type_shortname(1) = 'AER_BC'
    aot_type_shortname(2) = 'AER_DUST'
    aot_type_shortname(3) = 'AER_ORG'
    aot_type_shortname(4) = 'AER_SO4'
    aot_type_shortname(5) = 'AER_SS'

    aer_bc_meta%varname = 'AER_BC'
    aer_bc_meta%n_dim = n_dim + 1
    aer_bc_meta%diminfo => dim_aot_ty
    aer_bc_meta%vartype = vartype_real !REAL variable
    aer_bc_meta%standard_name = 'aot bc'
    aer_bc_meta%long_name = 'aerosol optical thickness of black carbon'
    aer_bc_meta%shortName = 'AER_BC'
    aer_bc_meta%units = c_undef
    aer_bc_meta%grid_mapping = gridmp
    aer_bc_meta%coordinates = coord

    aer_dust_meta%varname = 'AER_DUST'
    aer_dust_meta%n_dim = n_dim + 1
    aer_dust_meta%diminfo => dim_aot_ty
    aer_dust_meta%vartype = vartype_real !REAL variable
    aer_dust_meta%standard_name = 'aot dust'
    aer_dust_meta%long_name = 'aerosol optical thickness of dust'
    aer_dust_meta%shortName = 'AER_DUST'
    aer_dust_meta%units = c_undef
    aer_dust_meta%grid_mapping = gridmp
    aer_dust_meta%coordinates = coord

    aer_org_meta%varname = 'AER_ORG'
    aer_org_meta%n_dim = n_dim + 1
    aer_org_meta%diminfo => dim_aot_ty
    aer_org_meta%vartype = vartype_real !REAL variable
    aer_org_meta%standard_name = 'aot org'
    aer_org_meta%long_name = 'aerosol optical thickness of organic matter'
    aer_org_meta%shortName = 'AER_ORG'
    aer_org_meta%units = c_undef
    aer_org_meta%grid_mapping = gridmp
    aer_org_meta%coordinates = coord

    aer_so4_meta%varname = 'AER_SO4'
    aer_so4_meta%n_dim = n_dim + 1
    aer_so4_meta%diminfo => dim_aot_ty
    aer_so4_meta%vartype = vartype_real !REAL variable
    aer_so4_meta%standard_name = 'aot so4'
    aer_so4_meta%long_name = 'aerosol optical thickness of sulfate'
    aer_so4_meta%shortName = 'AER_SO4'
    aer_so4_meta%units = c_undef
    aer_so4_meta%grid_mapping = gridmp
    aer_so4_meta%coordinates = coord

    aer_ss_meta%varname = 'AER_SS'
    aer_ss_meta%n_dim = n_dim + 1
    aer_ss_meta%diminfo => dim_aot_ty
    aer_ss_meta%vartype = vartype_real !REAL variable
    aer_ss_meta%standard_name = 'aot ss'
    aer_ss_meta%long_name = 'aerosol optical thickness of sea salt'
    aer_ss_meta%shortName = 'AER_SS'
    aer_ss_meta%units = c_undef
    aer_ss_meta%grid_mapping = gridmp
    aer_ss_meta%coordinates = coord


  END SUBROUTINE def_aot_tg_meta


  ! define meta information for target field variables lon_geo, lat_geo and no_raw_data_pixel
  SUBROUTINE def_com_target_fields_meta(diminfo,coordinates,grid_mapping)
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)
    n_dim = SIZE(diminfo)


   
    lon_geo_meta%varname = 'lon'
    lon_geo_meta%n_dim = n_dim
    lon_geo_meta%diminfo => diminfo
    lon_geo_meta%vartype = vartype_real !REAL variable
    lon_geo_meta%standard_name = 'longitude'
    lon_geo_meta%long_name = 'geographical longitude'
    lon_geo_meta%shortName = 'RLON'
    lon_geo_meta%units =  'degrees_east'
    lon_geo_meta%grid_mapping = gridmp
    lon_geo_meta%coordinates = coord


    lat_geo_meta%varname = 'lat'
    lat_geo_meta%n_dim = n_dim
    lat_geo_meta%diminfo => diminfo
    lat_geo_meta%vartype = vartype_real !REAL variable
    lat_geo_meta%standard_name = 'latitude'
    lat_geo_meta%long_name = 'geographical latitude'
    lat_geo_meta%shortName = 'RLAT'
    lat_geo_meta%units =  'degrees_north'
    lat_geo_meta%grid_mapping = gridmp
    lat_geo_meta%coordinates = coord


    no_raw_data_pixel_meta%varname = 'no_raw_data_pixel'
    no_raw_data_pixel_meta%n_dim = n_dim
    no_raw_data_pixel_meta%diminfo => diminfo
    no_raw_data_pixel_meta%vartype = vartype_int !REAL variable
    no_raw_data_pixel_meta%standard_name = 'number pixels'
    no_raw_data_pixel_meta%long_name = 'number of raw data pixel in target grid element'
    no_raw_data_pixel_meta%shortName = c_undef
    no_raw_data_pixel_meta%units = c_undef
    no_raw_data_pixel_meta%grid_mapping = gridmp
    no_raw_data_pixel_meta%coordinates = coord


  END SUBROUTINE def_com_target_fields_meta

  !> define meta information for GLC2000 target fields
  SUBROUTINE def_glc2000_fields_meta(tg,nclass_glc2000,diminfo,coordinates,grid_mapping)
    TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
    INTEGER (KIND=i4) :: nclass_glc2000 !< GLC2000 has 23 classes for the land use description
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)

    n_dim = SIZE(diminfo)

    ! set meta information for strucutre dim_glc2000_tg
    IF (ALLOCATED(dim_glc2000_tg)) DEALLOCATE(dim_glc2000_tg)
    ALLOCATE(dim_glc2000_tg(1:n_dim+1))
    SELECT CASE(n_dim)
    CASE (1)
      dim_glc2000_tg(1)%dimname = diminfo(1)%dimname 
      dim_glc2000_tg(1)%dimsize = diminfo(1)%dimsize
      dim_glc2000_tg(2)%dimname = 'nclass'
      dim_glc2000_tg(2)%dimsize = nclass_glc2000
    CASE (2)
      dim_glc2000_tg(1)%dimname = diminfo(1)%dimname
      dim_glc2000_tg(1)%dimsize = diminfo(1)%dimsize
      dim_glc2000_tg(2)%dimname = diminfo(2)%dimname
      dim_glc2000_tg(2)%dimsize = diminfo(2)%dimsize
      dim_glc2000_tg(3)%dimname = 'nclass'
      dim_glc2000_tg(3)%dimsize = nclass_glc2000
    CASE (3)
      dim_glc2000_tg(1)%dimname = diminfo(1)%dimname
      dim_glc2000_tg(1)%dimsize = diminfo(1)%dimsize
      dim_glc2000_tg(2)%dimname = diminfo(2)%dimname
      dim_glc2000_tg(2)%dimsize = diminfo(2)%dimsize
      dim_glc2000_tg(3)%dimname = diminfo(3)%dimname
      dim_glc2000_tg(3)%dimsize = diminfo(3)%dimsize
      dim_glc2000_tg(4)%dimname = 'nclass'
      dim_glc2000_tg(4)%dimsize = nclass_glc2000
    END SELECT




    ! fr_land_glc2000_meta
    fr_land_glc2000_meta%varname = 'fr_land_glc2000'
    fr_land_glc2000_meta%n_dim = n_dim
    fr_land_glc2000_meta%diminfo => diminfo
    fr_land_glc2000_meta%vartype = vartype_real !REAL variable
    fr_land_glc2000_meta%standard_name = 'fr_land'
    fr_land_glc2000_meta%long_name = 'Fraction land due to GLC2000 Data'
    fr_land_glc2000_meta%shortName = 'FR_LAND'
    fr_land_glc2000_meta%units =  c_undef
    fr_land_glc2000_meta%grid_mapping = gridmp
    fr_land_glc2000_meta%coordinates = coord


   ! glc2000_tot_npixel_meta
    glc2000_tot_npixel_meta%varname = 'glc2000_tot_npixel'
    glc2000_tot_npixel_meta%n_dim = n_dim
    glc2000_tot_npixel_meta%diminfo => diminfo
    glc2000_tot_npixel_meta%vartype = vartype_int !INTEGER variable
    glc2000_tot_npixel_meta%standard_name = 'npixel'
    glc2000_tot_npixel_meta%long_name = 'number of raw data pixel in target grid element'
    glc2000_tot_npixel_meta%shortName = c_undef
    glc2000_tot_npixel_meta%units = c_undef
    glc2000_tot_npixel_meta%grid_mapping = gridmp
    glc2000_tot_npixel_meta%coordinates = coord

     
    ! glc2000_class_fraction_meta
    glc2000_class_fraction_meta%varname = 'glc2000_class_fraction'
    glc2000_class_fraction_meta%n_dim = n_dim + 1
    glc2000_class_fraction_meta%diminfo => dim_glc2000_tg
    glc2000_class_fraction_meta%vartype = vartype_real !REAL variable
    glc2000_class_fraction_meta%standard_name = 'Landuse class fraction'
    glc2000_class_fraction_meta%long_name = 'Fraction of GLC2000 land use classes in target grid element'
    glc2000_class_fraction_meta%shortName = c_undef
    glc2000_class_fraction_meta%units =  c_undef
    glc2000_class_fraction_meta%grid_mapping = c_undef
    glc2000_class_fraction_meta%coordinates = c_undef

    ! glc2000_class_npixel_meta
    glc2000_class_npixel_meta%varname = 'glc2000_class_npixel'
    glc2000_class_npixel_meta%n_dim = n_dim + 1
    glc2000_class_npixel_meta%diminfo => dim_glc2000_tg
    glc2000_class_npixel_meta%vartype = vartype_int !INTEGER variable
    glc2000_class_npixel_meta%standard_name = 'npixel landuse class'
    glc2000_class_npixel_meta%long_name = 'number of pixels of GLC2000 land use classes in target grid element'
    glc2000_class_npixel_meta%shortName = c_undef
    glc2000_class_npixel_meta%units = c_undef
    glc2000_class_npixel_meta%grid_mapping = c_undef
    glc2000_class_npixel_meta%coordinates = c_undef

    ! ice_glc2000_meta
    ice_glc2000_meta%varname = 'ice_glc2000'
    ice_glc2000_meta%n_dim = n_dim
    ice_glc2000_meta%diminfo => diminfo
    ice_glc2000_meta%vartype = vartype_real !REAL variable
    ice_glc2000_meta%standard_name = 'Ice fraction'
    ice_glc2000_meta%long_name = 'Ice fraction due to GLC2000 Data'
    ice_glc2000_meta%shortName = c_undef
    ice_glc2000_meta%units =  c_undef
    ice_glc2000_meta%grid_mapping = gridmp
    ice_glc2000_meta%coordinates = coord

    ! z0_glc2000_meta
    z0_glc2000_meta%varname = 'z0_glc2000'
    z0_glc2000_meta%n_dim = n_dim
    z0_glc2000_meta%diminfo => diminfo
    z0_glc2000_meta%vartype = vartype_real !REAL variable
    z0_glc2000_meta%standard_name = 'Z0 vegetation'
    z0_glc2000_meta%long_name =  'Roughness length z0 due to GLC2000 land use data'
    z0_glc2000_meta%shortName = 'Z0_LU'
    z0_glc2000_meta%units = 'm'
    z0_glc2000_meta%grid_mapping = gridmp
    z0_glc2000_meta%coordinates = coord

    ! root_glc2000_meta
    root_glc2000_meta%varname = 'root_glc2000'
    root_glc2000_meta%n_dim = n_dim
    root_glc2000_meta%diminfo => diminfo
    root_glc2000_meta%vartype = vartype_real !REAL variable
    root_glc2000_meta%standard_name = 'ROOT'
    root_glc2000_meta%long_name = 'Root depth due to GLC2000 land use data'
    root_glc2000_meta%shortName = 'ROOTDP'
    root_glc2000_meta%units =  'm'
    root_glc2000_meta%grid_mapping = gridmp
    root_glc2000_meta%coordinates = coord

    ! plcov_mx_glc2000_meta
    plcov_mx_glc2000_meta%varname = 'plcov_mx_glc2000'
    plcov_mx_glc2000_meta%n_dim = n_dim
    plcov_mx_glc2000_meta%diminfo => diminfo
    plcov_mx_glc2000_meta%vartype = vartype_real !REAL variable
    plcov_mx_glc2000_meta%standard_name = 'Plant cover maximum'
    plcov_mx_glc2000_meta%long_name = 'Plant cover maximum due to GLC2000 land use data'
    plcov_mx_glc2000_meta%shortName = 'PLCOV_MX'
    plcov_mx_glc2000_meta%units =  c_undef
    plcov_mx_glc2000_meta%grid_mapping = gridmp
    plcov_mx_glc2000_meta%coordinates = coord


    ! plcov_mn_glc2000_meta
    plcov_mn_glc2000_meta%varname = 'plcov_mn_glc2000'
    plcov_mn_glc2000_meta%n_dim = n_dim
    plcov_mn_glc2000_meta%diminfo => diminfo
    plcov_mn_glc2000_meta%vartype = vartype_real !REAL variable
    plcov_mn_glc2000_meta%standard_name = 'Plant cover minimum'
    plcov_mn_glc2000_meta%long_name = 'Plant cover minimum due to GLC2000 land use data'
    plcov_mn_glc2000_meta%shortName = 'PLCOV_MN'
    plcov_mn_glc2000_meta%units =  c_undef
    plcov_mn_glc2000_meta%grid_mapping = gridmp
    plcov_mn_glc2000_meta%coordinates = coord

    ! lai_mx_glc2000_meta
    lai_mx_glc2000_meta%varname = 'lai_mx_glc2000'
    lai_mx_glc2000_meta%n_dim = n_dim
    lai_mx_glc2000_meta%diminfo => diminfo
    lai_mx_glc2000_meta%vartype = vartype_real !REAL variable
    lai_mx_glc2000_meta%standard_name = 'LAI max'
    lai_mx_glc2000_meta%long_name = 'Leaf Area Index Maximum'
    lai_mx_glc2000_meta%shortName = 'LAI_MX'
    lai_mx_glc2000_meta%units =  c_undef
    lai_mx_glc2000_meta%grid_mapping = gridmp
    lai_mx_glc2000_meta%coordinates = coord


    ! lai_mn_glc2000_meta
    lai_mn_glc2000_meta%varname = 'lai_mn_glc2000'
    lai_mn_glc2000_meta%n_dim = n_dim
    lai_mn_glc2000_meta%diminfo => diminfo
    lai_mn_glc2000_meta%vartype = vartype_real !REAL variable
    lai_mn_glc2000_meta%standard_name = 'LAI Minimum'
    lai_mn_glc2000_meta%long_name = 'Leaf Area Minimum'
    lai_mn_glc2000_meta%shortName = 'LAI_MN'
    lai_mn_glc2000_meta%units =  c_undef
    lai_mn_glc2000_meta%grid_mapping = gridmp
    lai_mn_glc2000_meta%coordinates = coord


    ! rs_min_glc2000_meta
    rs_min_glc2000_meta%varname = 'rs_min_glc2000'
    rs_min_glc2000_meta%n_dim = n_dim
    rs_min_glc2000_meta%diminfo => diminfo
    rs_min_glc2000_meta%vartype = vartype_real !REAL variable
    rs_min_glc2000_meta%standard_name = 'RSMIN'
    rs_min_glc2000_meta%long_name = 'Minimal stomata resistence'
    rs_min_glc2000_meta%shortName = 'RSMIN'
    rs_min_glc2000_meta%units =  's/m'
    rs_min_glc2000_meta%grid_mapping =gridmp
    rs_min_glc2000_meta%coordinates = coord

    ! urban_glc2000_meta
    urban_glc2000_meta%varname = 'urban_glc2000'
    urban_glc2000_meta%n_dim = n_dim
    urban_glc2000_meta%diminfo => diminfo
    urban_glc2000_meta%vartype = vartype_real !REAL variable
    urban_glc2000_meta%standard_name = 'URBAN'
    urban_glc2000_meta%long_name = 'Urban land use fraction'
    urban_glc2000_meta%shortName = 'URBAN'
    urban_glc2000_meta%units =  c_undef
    urban_glc2000_meta%grid_mapping = gridmp
    urban_glc2000_meta%coordinates = coord


    ! for_d_glc2000_meta
    for_d_glc2000_meta%varname = 'for_d_glc2000'
    for_d_glc2000_meta%n_dim = n_dim
    for_d_glc2000_meta%diminfo => diminfo
    for_d_glc2000_meta%vartype = vartype_real !REAL variable
    for_d_glc2000_meta%standard_name = 'FOREST_D'
    for_d_glc2000_meta%long_name = 'Fraction of deciduous forest'
    for_d_glc2000_meta%shortName = 'FOR_D'
    for_d_glc2000_meta%units =  c_undef
    for_d_glc2000_meta%grid_mapping = gridmp
    for_d_glc2000_meta%coordinates = coord


    ! for_e_glc2000_meta
    for_e_glc2000_meta%varname = 'for_e_glc2000'
    for_e_glc2000_meta%n_dim = n_dim
    for_e_glc2000_meta%diminfo => diminfo
    for_e_glc2000_meta%vartype = vartype_real !REAL variable
    for_e_glc2000_meta%standard_name = 'FOREST_E'
    for_e_glc2000_meta%long_name = 'Fraction of evergreen forest'
    for_e_glc2000_meta%shortName =  'FOR_E'

    for_e_glc2000_meta%units =  c_undef
    for_e_glc2000_meta%grid_mapping = gridmp
    for_e_glc2000_meta%coordinates = coord


    ! emissivity_glc2000_meta
    emissivity_glc2000_meta%varname = 'emissivity_glc2000'
    emissivity_glc2000_meta%n_dim = n_dim
    emissivity_glc2000_meta%diminfo => diminfo
    emissivity_glc2000_meta%vartype = vartype_real !REAL variable
    emissivity_glc2000_meta%standard_name = 'EMIS_RAD'
    emissivity_glc2000_meta%long_name = 'longwave surface emissivity'
    emissivity_glc2000_meta%shortName = 'EMIS_RAD'
    emissivity_glc2000_meta%units =  c_undef
    emissivity_glc2000_meta%grid_mapping = gridmp
    emissivity_glc2000_meta%coordinates = coord



  END SUBROUTINE def_glc2000_fields_meta

    !> define meta information for GLCC target fields
  SUBROUTINE def_glcc_fields_meta(tg,nclass_glcc,diminfo,coordinates,grid_mapping)
    TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
    INTEGER (KIND=i4) :: nclass_glcc !< GLCC has 23 classes for the land use description
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)

    n_dim = SIZE(diminfo)

    
    ! set meta information for strucutre dim_glcc_tg
    IF (ALLOCATED(dim_glcc_tg)) DEALLOCATE(dim_glcc_tg)
    ALLOCATE(dim_glcc_tg(1:n_dim+1))
    SELECT CASE(n_dim)
    CASE (1)
      dim_glcc_tg(1)%dimname = diminfo(1)%dimname 
      dim_glcc_tg(1)%dimsize = diminfo(1)%dimsize
      dim_glcc_tg(2)%dimname = 'nclass'
      dim_glcc_tg(2)%dimsize = nclass_glcc
    CASE (2)
      dim_glcc_tg(1)%dimname = diminfo(1)%dimname
      dim_glcc_tg(1)%dimsize = diminfo(1)%dimsize
      dim_glcc_tg(2)%dimname = diminfo(2)%dimname
      dim_glcc_tg(2)%dimsize = diminfo(2)%dimsize
      dim_glcc_tg(3)%dimname = 'nclass'
      dim_glcc_tg(3)%dimsize = nclass_glcc
    CASE (3)
      dim_glcc_tg(1)%dimname = diminfo(1)%dimname
      dim_glcc_tg(1)%dimsize = diminfo(1)%dimsize
      dim_glcc_tg(2)%dimname = diminfo(2)%dimname
      dim_glcc_tg(2)%dimsize = diminfo(2)%dimsize
      dim_glcc_tg(3)%dimname = diminfo(3)%dimname
      dim_glcc_tg(3)%dimsize = diminfo(3)%dimsize
      dim_glcc_tg(4)%dimname = 'nclass'
      dim_glcc_tg(4)%dimsize = nclass_glcc
    END SELECT



    ! fr_land_glcc_meta
    fr_land_glcc_meta%varname = 'fr_land_glcc'
    fr_land_glcc_meta%n_dim = n_dim
    fr_land_glcc_meta%diminfo => diminfo
    fr_land_glcc_meta%vartype = vartype_real !REAL variable
    fr_land_glcc_meta%standard_name = 'fr_land'
    fr_land_glcc_meta%long_name = 'Fraction land due to GLCC Data'
    fr_land_glcc_meta%shortName = 'FR_LAND'
    fr_land_glcc_meta%units =  c_undef
    fr_land_glcc_meta%grid_mapping = gridmp
    fr_land_glcc_meta%coordinates = coord


   ! glcc_tot_npixel_meta
    glcc_tot_npixel_meta%varname = 'glcc_tot_npixel'
    glcc_tot_npixel_meta%n_dim = n_dim
    glcc_tot_npixel_meta%diminfo => dim_glcc_tg
    glcc_tot_npixel_meta%vartype = vartype_int !INTEGER variable
    glcc_tot_npixel_meta%standard_name = 'npixel'
    glcc_tot_npixel_meta%long_name = 'number of raw data pixel in target grid element'
    glcc_tot_npixel_meta%shortName = c_undef
    glcc_tot_npixel_meta%units = c_undef
    glcc_tot_npixel_meta%grid_mapping = gridmp
    glcc_tot_npixel_meta%coordinates = coord

     
    ! glcc_class_fraction_meta
    glcc_class_fraction_meta%varname = 'glcc_class_fraction'
    glcc_class_fraction_meta%n_dim = n_dim + 1
    glcc_class_fraction_meta%diminfo => dim_glcc_tg
    glcc_class_fraction_meta%vartype = vartype_real !REAL variable
    glcc_class_fraction_meta%standard_name = 'Landuse class fraction'
    glcc_class_fraction_meta%long_name = 'Fraction of GLCC land use classes in target grid element'
    glcc_class_fraction_meta%shortName = c_undef
    glcc_class_fraction_meta%units =  c_undef
    glcc_class_fraction_meta%grid_mapping = c_undef
    glcc_class_fraction_meta%coordinates = c_undef

    ! glcc_class_npixel_meta
    glcc_class_npixel_meta%varname = 'glcc_class_npixel'
    glcc_class_npixel_meta%n_dim = n_dim + 1
    glcc_class_npixel_meta%diminfo => dim_glcc_tg
    glcc_class_npixel_meta%vartype = vartype_int !INTEGER variable
    glcc_class_npixel_meta%standard_name = 'npixel landuse class'
    glcc_class_npixel_meta%long_name = 'number of pixels of GLCC land use classes in target grid element'
    glcc_class_npixel_meta%shortName = c_undef
    glcc_class_npixel_meta%units = c_undef
    glcc_class_npixel_meta%grid_mapping = c_undef
    glcc_class_npixel_meta%coordinates = c_undef

    ! ice_glcc_meta
    ice_glcc_meta%varname = 'ice_glcc'
    ice_glcc_meta%n_dim = n_dim
    ice_glcc_meta%diminfo => diminfo
    ice_glcc_meta%vartype = vartype_real !REAL variable
    ice_glcc_meta%standard_name = 'Ice fraction'
    ice_glcc_meta%long_name = 'Ice fraction due to GLCC Data'
    ice_glcc_meta%shortName = c_undef
    ice_glcc_meta%units =  c_undef
    ice_glcc_meta%grid_mapping = gridmp
    ice_glcc_meta%coordinates = coord

    ! z0_glcc_meta
    z0_glcc_meta%varname = 'z0_glcc'
    z0_glcc_meta%n_dim = n_dim
    z0_glcc_meta%diminfo => diminfo
    z0_glcc_meta%vartype = vartype_real !REAL variable
    z0_glcc_meta%standard_name = 'Z0 vegetation'
    z0_glcc_meta%long_name =  'Roughness length z0 due to GLCC land use data'
    z0_glcc_meta%shortName = 'Z0_LU'
    z0_glcc_meta%units = 'm'
    z0_glcc_meta%grid_mapping = gridmp
    z0_glcc_meta%coordinates = coord

    ! root_glcc_meta
    root_glcc_meta%varname = 'root_glcc'
    root_glcc_meta%n_dim = n_dim
    root_glcc_meta%diminfo => diminfo
    root_glcc_meta%vartype = vartype_real !REAL variable
    root_glcc_meta%standard_name = 'ROOT'
    root_glcc_meta%long_name = 'Root depth due to GLCC land use data'
    root_glcc_meta%shortName = 'ROOTDP'
    root_glcc_meta%units =  'm'
    root_glcc_meta%grid_mapping = gridmp
    root_glcc_meta%coordinates = coord

    ! plcov_mx_glcc_meta
    plcov_mx_glcc_meta%varname = 'plcov_mx_glcc'
    plcov_mx_glcc_meta%n_dim = n_dim
    plcov_mx_glcc_meta%diminfo => diminfo
    plcov_mx_glcc_meta%vartype = vartype_real !REAL variable
    plcov_mx_glcc_meta%standard_name = 'Plant cover maximum'
    plcov_mx_glcc_meta%long_name = 'Plant cover maximum due to GLCC land use data'
    plcov_mx_glcc_meta%shortName = 'PLCOV_MX'
    plcov_mx_glcc_meta%units =  c_undef
    plcov_mx_glcc_meta%grid_mapping = gridmp
    plcov_mx_glcc_meta%coordinates = coord


    ! plcov_mn_glcc_meta
    plcov_mn_glcc_meta%varname = 'plcov_mn_glcc'
    plcov_mn_glcc_meta%n_dim = n_dim
    plcov_mn_glcc_meta%diminfo => diminfo
    plcov_mn_glcc_meta%vartype = vartype_real !REAL variable
    plcov_mn_glcc_meta%standard_name = 'Plant cover minimum'
    plcov_mn_glcc_meta%long_name = 'Plant cover minimum due to GLCC land use data'
    plcov_mn_glcc_meta%shortName = 'PLCOV_MN'
    plcov_mn_glcc_meta%units =  c_undef
    plcov_mn_glcc_meta%grid_mapping = gridmp
    plcov_mn_glcc_meta%coordinates = coord

    ! lai_mx_glcc_meta
    lai_mx_glcc_meta%varname = 'lai_mx_glcc'
    lai_mx_glcc_meta%n_dim = n_dim
    lai_mx_glcc_meta%diminfo => diminfo
    lai_mx_glcc_meta%vartype = vartype_real !REAL variable
    lai_mx_glcc_meta%standard_name = 'LAI max'
    lai_mx_glcc_meta%long_name = 'Leaf Area Index Maximum'
    lai_mx_glcc_meta%shortName = 'LAI_MX'
    lai_mx_glcc_meta%units =  c_undef
    lai_mx_glcc_meta%grid_mapping = gridmp
    lai_mx_glcc_meta%coordinates = coord


    ! lai_mn_glcc_meta
    lai_mn_glcc_meta%varname = 'lai_mn_glcc'
    lai_mn_glcc_meta%n_dim = n_dim
    lai_mn_glcc_meta%diminfo => diminfo
    lai_mn_glcc_meta%vartype = vartype_real !REAL variable
    lai_mn_glcc_meta%standard_name = 'LAI Minimum'
    lai_mn_glcc_meta%long_name = 'Leaf Area Minimum'
    lai_mn_glcc_meta%shortName = 'LAI_MN'
    lai_mn_glcc_meta%units =  c_undef
    lai_mn_glcc_meta%grid_mapping = gridmp
    lai_mn_glcc_meta%coordinates = coord


    ! rs_min_glcc_meta
    rs_min_glcc_meta%varname = 'rs_min_glcc'
    rs_min_glcc_meta%n_dim = n_dim
    rs_min_glcc_meta%diminfo => diminfo
    rs_min_glcc_meta%vartype = vartype_real !REAL variable
    rs_min_glcc_meta%standard_name = 'RSMIN'
    rs_min_glcc_meta%long_name = 'Minimal stomata resistence'
    rs_min_glcc_meta%shortName = 'RSMIN'
    rs_min_glcc_meta%units =  's/m'
    rs_min_glcc_meta%grid_mapping = gridmp
    rs_min_glcc_meta%coordinates = coord

    ! urban_glcc_meta
    urban_glcc_meta%varname = 'urban_glcc'
    urban_glcc_meta%n_dim = n_dim
    urban_glcc_meta%diminfo => diminfo
    urban_glcc_meta%vartype = vartype_real !REAL variable
    urban_glcc_meta%standard_name = 'URBAN'
    urban_glcc_meta%long_name = 'Urban land use fraction'
    urban_glcc_meta%shortName = 'URBAN'
    urban_glcc_meta%units =  c_undef
    urban_glcc_meta%grid_mapping = gridmp
    urban_glcc_meta%coordinates = coord


    ! for_d_glcc_meta
    for_d_glcc_meta%varname = 'for_d_glcc'
    for_d_glcc_meta%n_dim = n_dim
    for_d_glcc_meta%diminfo => diminfo
    for_d_glcc_meta%vartype = vartype_real !REAL variable
    for_d_glcc_meta%standard_name = 'FOREST_D'
    for_d_glcc_meta%long_name = 'Fraction of deciduous forest'
    for_d_glcc_meta%shortName = 'FOR_D'
    for_d_glcc_meta%units =  c_undef
    for_d_glcc_meta%grid_mapping = gridmp
    for_d_glcc_meta%coordinates = coord


    ! for_e_glcc_meta
    for_e_glcc_meta%varname = 'for_e_glcc'
    for_e_glcc_meta%n_dim = n_dim
    for_e_glcc_meta%diminfo => diminfo
    for_e_glcc_meta%vartype = vartype_real !REAL variable
    for_e_glcc_meta%standard_name = 'FOREST_E'
    for_e_glcc_meta%long_name = 'Fraction of evergreen forest'
    for_e_glcc_meta%shortName =  'FOR_E'

    for_e_glcc_meta%units =  c_undef
    for_e_glcc_meta%grid_mapping = gridmp
    for_e_glcc_meta%coordinates = coord


    ! emissivity_glcc_meta
    emissivity_glcc_meta%varname = 'emissivity_glcc'
    emissivity_glcc_meta%n_dim = n_dim
    emissivity_glcc_meta%diminfo => diminfo
    emissivity_glcc_meta%vartype = vartype_real !REAL variable
    emissivity_glcc_meta%standard_name = 'EMIS_RAD'
    emissivity_glcc_meta%long_name = 'longwave surface emissivity'
    emissivity_glcc_meta%shortName = 'EMIS_RAD'
    emissivity_glcc_meta%units =  c_undef
    emissivity_glcc_meta%grid_mapping = gridmp
    emissivity_glcc_meta%coordinates = coord



  END SUBROUTINE def_glcc_fields_meta


  
  !> define meta information for  landuse target fields
  SUBROUTINE def_lu_fields_meta(tg,nclass_lu,diminfo,coordinates,grid_mapping)
    TYPE(target_grid_def), INTENT(IN) :: tg !< structure with target grid description
    INTEGER (KIND=i4) :: nclass_lu !< GLC2000 has 23 classes for the land use description
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping


    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)

    n_dim = SIZE(diminfo)

    ! set meta information for strucutre dim_ndvi_tg
    IF (ALLOCATED(dim_lu_tg)) DEALLOCATE(dim_lu_tg)
    ALLOCATE(dim_lu_tg(1:n_dim+1))
    SELECT CASE(n_dim)
    CASE (1)
      dim_lu_tg(1)%dimname = diminfo(1)%dimname 
      dim_lu_tg(1)%dimsize = diminfo(1)%dimsize
      dim_lu_tg(2)%dimname = 'nclass_lu'
      dim_lu_tg(2)%dimsize = nclass_lu
    CASE (2)
      dim_lu_tg(1)%dimname = diminfo(1)%dimname
      dim_lu_tg(1)%dimsize = diminfo(1)%dimsize
      dim_lu_tg(2)%dimname = diminfo(2)%dimname
      dim_lu_tg(2)%dimsize = diminfo(2)%dimsize
      dim_lu_tg(3)%dimname = 'nclass_lu'
      dim_lu_tg(3)%dimsize = nclass_lu
    CASE (3)
      dim_lu_tg(1)%dimname = diminfo(1)%dimname
      dim_lu_tg(1)%dimsize = diminfo(1)%dimsize
      dim_lu_tg(2)%dimname = diminfo(2)%dimname
      dim_lu_tg(2)%dimsize = diminfo(2)%dimsize
      dim_lu_tg(3)%dimname = diminfo(3)%dimname
      dim_lu_tg(3)%dimsize = diminfo(3)%dimsize
      dim_lu_tg(4)%dimname = 'nclass_lu'
      dim_lu_tg(4)%dimsize = nclass_lu
    END SELECT
    
    ! fr_land_lu_meta
    fr_land_lu_meta%varname = 'fr_land'
    fr_land_lu_meta%n_dim = n_dim
    fr_land_lu_meta%diminfo => diminfo
    fr_land_lu_meta%vartype = vartype_real !REAL variable
    fr_land_lu_meta%standard_name = 'fr_land'
    fr_land_lu_meta%long_name = 'Fraction land'
    fr_land_lu_meta%shortName = 'FR_LAND'
    fr_land_lu_meta%units =  c_undef
    fr_land_lu_meta%grid_mapping = gridmp
    fr_land_lu_meta%coordinates = coord


   ! lu_tot_npixel_meta
    lu_tot_npixel_meta%varname = 'lu_tot_npixel'
    lu_tot_npixel_meta%n_dim = n_dim
    lu_tot_npixel_meta%diminfo => diminfo
    lu_tot_npixel_meta%vartype = vartype_int !INTEGER variable
    lu_tot_npixel_meta%standard_name = 'npixel'
    lu_tot_npixel_meta%long_name = 'number of raw data pixel in target grid element'
    lu_tot_npixel_meta%shortName = c_undef
    lu_tot_npixel_meta%units = c_undef
    lu_tot_npixel_meta%grid_mapping = gridmp
    lu_tot_npixel_meta%coordinates = coord

     
    ! lu_class_fraction_meta
    lu_class_fraction_meta%varname = 'lu_class_fraction'
    lu_class_fraction_meta%n_dim = n_dim + 1
    lu_class_fraction_meta%diminfo => dim_lu_tg
    lu_class_fraction_meta%vartype = vartype_real !REAL variable
    lu_class_fraction_meta%standard_name = 'Landuse class fraction'
    lu_class_fraction_meta%long_name = 'Fraction of GLC2000 land use classes in target grid element'
    lu_class_fraction_meta%shortName = c_undef
    lu_class_fraction_meta%units =  c_undef
    lu_class_fraction_meta%grid_mapping = c_undef
    lu_class_fraction_meta%coordinates = c_undef

    ! lu_class_npixel_meta
    lu_class_npixel_meta%varname = 'lu_class_npixel'
    lu_class_npixel_meta%n_dim = n_dim + 1
    lu_class_npixel_meta%diminfo => dim_lu_tg
    lu_class_npixel_meta%vartype = vartype_int !INTEGER variable
    lu_class_npixel_meta%standard_name = 'npixel landuse class'
    lu_class_npixel_meta%long_name = 'number of pixels of GLC2000 land use classes in target grid element'
    lu_class_npixel_meta%shortName = c_undef
    lu_class_npixel_meta%units = c_undef
    lu_class_npixel_meta%grid_mapping = c_undef
    lu_class_npixel_meta%coordinates = c_undef

    ! ice_lu_meta
    ice_lu_meta%varname = 'ice'
    ice_lu_meta%n_dim = n_dim
    ice_lu_meta%diminfo => diminfo
    ice_lu_meta%vartype = vartype_real !REAL variable
    ice_lu_meta%standard_name = 'Ice fraction'
    ice_lu_meta%long_name = 'Ice fraction due to GLC2000 Data'
    ice_lu_meta%shortName = c_undef
    ice_lu_meta%units =  c_undef
    ice_lu_meta%grid_mapping = gridmp
    ice_lu_meta%coordinates = coord

    ! z0_lu_meta
    z0_lu_meta%varname = 'z0'
    z0_lu_meta%n_dim = n_dim
    z0_lu_meta%diminfo => diminfo
    z0_lu_meta%vartype = vartype_real !REAL variable
    z0_lu_meta%standard_name = 'Z0'
    z0_lu_meta%long_name = 'Roughness length'
    z0_lu_meta%shortName = 'Z0'
    z0_lu_meta%units =  c_undef
    z0_lu_meta%grid_mapping = gridmp
    z0_lu_meta%coordinates = coord

    ! root_lu_meta
    root_lu_meta%varname = 'root'
    root_lu_meta%n_dim = n_dim
    root_lu_meta%diminfo => diminfo
    root_lu_meta%vartype = vartype_real !REAL variable
    root_lu_meta%standard_name = 'ROOT'
    root_lu_meta%long_name = 'Root depth due to GLC2000 land use data'
    root_lu_meta%shortName = 'ROOTDP'
    root_lu_meta%units =  'm'
    root_lu_meta%grid_mapping = gridmp
    root_lu_meta%coordinates = coord

    ! plcov_mx_lu_meta
    plcov_mx_lu_meta%varname = 'plcov_mx'
    plcov_mx_lu_meta%n_dim = n_dim
    plcov_mx_lu_meta%diminfo => diminfo
    plcov_mx_lu_meta%vartype = vartype_real !REAL variable
    plcov_mx_lu_meta%standard_name = 'Plant cover maximum'
    plcov_mx_lu_meta%long_name = 'Plant cover maximum due to GLC2000 land use data'
    plcov_mx_lu_meta%shortName = 'PLCOV_MX'
    plcov_mx_lu_meta%units =  c_undef
    plcov_mx_lu_meta%grid_mapping = gridmp
    plcov_mx_lu_meta%coordinates = coord


    ! plcov_mn_lu_meta
    plcov_mn_lu_meta%varname = 'plcov_mn'
    plcov_mn_lu_meta%n_dim = n_dim
    plcov_mn_lu_meta%diminfo => diminfo
    plcov_mn_lu_meta%vartype = vartype_real !REAL variable
    plcov_mn_lu_meta%standard_name = 'Plant cover minimum'
    plcov_mn_lu_meta%long_name = 'Plant cover minimum due to GLC2000 land use data'
    plcov_mn_lu_meta%shortName = 'PLCOV_MN'
    plcov_mn_lu_meta%units =  c_undef
    plcov_mn_lu_meta%grid_mapping = gridmp
    plcov_mn_lu_meta%coordinates = coord

    ! lai_mx_lu_meta
    lai_mx_lu_meta%varname = 'lai_mx'
    lai_mx_lu_meta%n_dim = n_dim
    lai_mx_lu_meta%diminfo => diminfo
    lai_mx_lu_meta%vartype = vartype_real !REAL variable
    lai_mx_lu_meta%standard_name = 'LAI max'
    lai_mx_lu_meta%long_name = 'Leaf Area Index Maximum'
    lai_mx_lu_meta%shortName = 'LAI_MX'
    lai_mx_lu_meta%units =  c_undef
    lai_mx_lu_meta%grid_mapping = gridmp
    lai_mx_lu_meta%coordinates = coord


    ! lai_mn_lu_meta
    lai_mn_lu_meta%varname = 'lai_mn'
    lai_mn_lu_meta%n_dim = n_dim
    lai_mn_lu_meta%diminfo => diminfo
    lai_mn_lu_meta%vartype = vartype_real !REAL variable
    lai_mn_lu_meta%standard_name = 'LAI Minimum'
    lai_mn_lu_meta%long_name = 'Leaf Area Minimum'
    lai_mn_lu_meta%shortName = 'LAI_MN'
    lai_mn_lu_meta%units =  c_undef
    lai_mn_lu_meta%grid_mapping =gridmp
    lai_mn_lu_meta%coordinates = coord


    ! rs_min_lu_meta
    rs_min_lu_meta%varname = 'rs_min'
    rs_min_lu_meta%n_dim = n_dim
    rs_min_lu_meta%diminfo => diminfo
    rs_min_lu_meta%vartype = vartype_real !REAL variable
    rs_min_lu_meta%standard_name = 'RSMIN'
    rs_min_lu_meta%long_name = 'Minimal stomata resistence'
    rs_min_lu_meta%shortName = 'RSMIN'
    rs_min_lu_meta%units =  's/m'
    rs_min_lu_meta%grid_mapping = gridmp
    rs_min_lu_meta%coordinates = coord

    ! urban_lu_meta
    urban_lu_meta%varname = 'urban'
    urban_lu_meta%n_dim = n_dim
    urban_lu_meta%diminfo => diminfo
    urban_lu_meta%vartype = vartype_real !REAL variable
    urban_lu_meta%standard_name = 'URBAN'
    urban_lu_meta%long_name = 'Urban land use fraction'
    urban_lu_meta%shortName = 'URBAN'
    urban_lu_meta%units =  c_undef
    urban_lu_meta%grid_mapping = gridmp
    urban_lu_meta%coordinates = coord


    ! for_d_lu_meta
    for_d_lu_meta%varname = 'for_d'
    for_d_lu_meta%n_dim = n_dim
    for_d_lu_meta%diminfo => diminfo
    for_d_lu_meta%vartype = vartype_real !REAL variable
    for_d_lu_meta%standard_name = 'FOREST_D'
    for_d_lu_meta%long_name = 'Fraction of deciduous forest'
    for_d_lu_meta%shortName = 'FOR_D'
    for_d_lu_meta%units =  c_undef
    for_d_lu_meta%grid_mapping = gridmp
    for_d_lu_meta%coordinates = coord


    ! for_e_lu_meta
    for_e_lu_meta%varname = 'for_e'
    for_e_lu_meta%n_dim = n_dim
    for_e_lu_meta%diminfo => diminfo
    for_e_lu_meta%vartype = vartype_real !REAL variable
    for_e_lu_meta%standard_name = 'FOREST_E'
    for_e_lu_meta%long_name = 'Fraction of evergreen forest'
    for_e_lu_meta%shortName = 'FOR_E' 
    for_e_lu_meta%units =  c_undef
    for_e_lu_meta%grid_mapping = gridmp
    for_e_lu_meta%coordinates = coord


    ! emissivity_lu_meta
    emissivity_lu_meta%varname = 'emissivity'
    emissivity_lu_meta%n_dim = n_dim
    emissivity_lu_meta%diminfo => diminfo
    emissivity_lu_meta%vartype = vartype_real !REAL variable
    emissivity_lu_meta%standard_name = 'EMIS_RAD'
    emissivity_lu_meta%long_name = 'longwave surface emissivity'
    emissivity_lu_meta%shortName = 'EMIS_RAD'
    emissivity_lu_meta%units =  c_undef
    emissivity_lu_meta%grid_mapping = gridmp
    emissivity_lu_meta%coordinates = coord

    ! fr_ocean_lu_meta
    fr_ocean_lu_meta%varname = 'fr_ocean'
    fr_ocean_lu_meta%n_dim = n_dim
    fr_ocean_lu_meta%diminfo => diminfo
    fr_ocean_lu_meta%vartype = vartype_real !REAL variable
    fr_ocean_lu_meta%standard_name = 'fr_ocean'
    fr_ocean_lu_meta%long_name = 'Fraction ocean'
    fr_ocean_lu_meta%shortName = 'FR_OCEAN'
    fr_ocean_lu_meta%units =  c_undef
    fr_ocean_lu_meta%grid_mapping = gridmp
    fr_ocean_lu_meta%coordinates = coord

  END SUBROUTINE def_lu_fields_meta

  
  !> define meta information for flake data for netcdf output
  SUBROUTINE def_flake_fields_meta(diminfo,coordinates,grid_mapping)
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)
    n_dim = SIZE(diminfo)



  
    lake_depth_meta%varname = 'lake_depth'
    lake_depth_meta%n_dim = n_dim
    lake_depth_meta%diminfo => diminfo
    lake_depth_meta%vartype = vartype_real !REAL variable
    lake_depth_meta%standard_name = 'DEPTH_LK'
    lake_depth_meta%long_name = 'Lake depth'
    lake_depth_meta%shortName = 'DEPTH_LK'
    lake_depth_meta%units = 'm'
    lake_depth_meta%grid_mapping = gridmp
    lake_depth_meta%coordinates = coord

     
    fr_lake_meta%varname = 'fr_lake'
    fr_lake_meta%n_dim = n_dim
    fr_lake_meta%diminfo => diminfo
    fr_lake_meta%vartype = vartype_real !REAL variable
    fr_lake_meta%standard_name = 'FR_LAKE'
    fr_lake_meta%long_name = 'fraction lake'
    fr_lake_meta%shortName = 'FR_LAKE'
    fr_lake_meta%units = c_undef
    fr_lake_meta%grid_mapping = gridmp
    fr_lake_meta%coordinates = coord

    ! flake_tot_npixel_meta
    flake_tot_npixel_meta%varname = 'flake_tot_npixel'
    flake_tot_npixel_meta%n_dim = n_dim
    flake_tot_npixel_meta%diminfo => diminfo
    flake_tot_npixel_meta%vartype = vartype_int !INTEGER variable
    flake_tot_npixel_meta%standard_name = 'npixel'
    flake_tot_npixel_meta%long_name = 'number of raw data pixel in target grid element'
    flake_tot_npixel_meta%shortName = c_undef
    flake_tot_npixel_meta%units = c_undef
    flake_tot_npixel_meta%grid_mapping = gridmp
    flake_tot_npixel_meta%coordinates = coord


    
  END SUBROUTINE def_flake_fields_meta





  !> define meta information for target fields derived from GLOBE data
  SUBROUTINE def_globe_meta(diminfo,coordinates,grid_mapping)
    TYPE(dim_meta_info),TARGET :: diminfo(:)     !< pointer to dimensions of variable
    CHARACTER (len=80), OPTIONAL :: coordinates  !< netcdf attribute coordinates
    CHARACTER (len=80), OPTIONAL :: grid_mapping !< netcdf attribute grid mapping

    ! local variables
    INTEGER  :: n_dim      !< number of dimensions
    CHARACTER (len=80) :: gridmp
    CHARACTER (len=80) :: coord

    gridmp = c_undef
    coord = c_undef
    
    IF (PRESENT(grid_mapping)) gridmp = TRIM(grid_mapping)
    IF (PRESENT(coordinates)) coord = TRIM(coordinates)
    n_dim = SIZE(diminfo)


    ! set meta information for strucutre dim_buffer_cell
    dim_buffer_cell = dim_3d_tg
  
    hh_globe_meta%varname = 'altitude'
    hh_globe_meta%n_dim = n_dim
    hh_globe_meta%diminfo => diminfo
    hh_globe_meta%vartype = vartype_real !REAL variable
    hh_globe_meta%standard_name = 'HSURF'
    hh_globe_meta%long_name = 'Geometric Height of the earths surface above sea level'
    hh_globe_meta%shortName = 'HSURF'
    hh_globe_meta%units = 'm'
    hh_globe_meta%grid_mapping = gridmp
    hh_globe_meta%coordinates = coord

    hh_fis_meta%varname = 'fis'
    hh_fis_meta%n_dim = n_dim
    hh_fis_meta%diminfo => diminfo
    hh_fis_meta%vartype = vartype_real !REAL variable
    hh_fis_meta%standard_name = 'FIS'
    hh_fis_meta%long_name = 'Geopotential (S)'
    hh_fis_meta%shortName = 'FIS'
    hh_fis_meta%units = 'm**2 s**-2'
    hh_fis_meta%grid_mapping = gridmp
    hh_fis_meta%coordinates = coord


     
    stdh_globe_meta%varname = 'sso_stdh'
    stdh_globe_meta%n_dim = n_dim
    stdh_globe_meta%diminfo => diminfo
    stdh_globe_meta%vartype = vartype_real !REAL variable
    stdh_globe_meta%standard_name = 'SSO_STDH'
    stdh_globe_meta%long_name = 'Standard deviation of subgrid scale orography'
    stdh_globe_meta%shortName = 'SSO_STDH'
    stdh_globe_meta%units = 'm'
    stdh_globe_meta%grid_mapping = gridmp
    stdh_globe_meta%coordinates = coord
    
    theta_globe_meta%varname = 'sso_theta'
    theta_globe_meta%n_dim = n_dim
    theta_globe_meta%diminfo => diminfo
    theta_globe_meta%vartype = vartype_real !REAL variable
    theta_globe_meta%standard_name = 'SSO_THETA'
    theta_globe_meta%long_name = 'Angle of sub-gridscale orography'
    theta_globe_meta%shortName = 'SSO_THETA'
    theta_globe_meta%units = 'rad'
    theta_globe_meta%grid_mapping = gridmp
    theta_globe_meta%coordinates = coord
    
    aniso_globe_meta%varname = 'sso_gamma'
    aniso_globe_meta%n_dim = n_dim
    aniso_globe_meta%diminfo => diminfo
    aniso_globe_meta%vartype = vartype_real !REAL variable
    aniso_globe_meta%standard_name = 'SSO_GAMMA'
    aniso_globe_meta%long_name = 'Anisotropy of sub-gridscale orography'
    aniso_globe_meta%shortName = 'SSO_GAMMA'
    aniso_globe_meta%units =  c_undef
    aniso_globe_meta%grid_mapping = gridmp
    aniso_globe_meta%coordinates = coord
    
    slope_globe_meta%varname = 'sso_sigma'
    slope_globe_meta%n_dim = n_dim
    slope_globe_meta%diminfo => diminfo
    slope_globe_meta%vartype = vartype_real !REAL variable
    slope_globe_meta%standard_name = 'SSO_SIGMA'
    slope_globe_meta%long_name = 'Slope of sub-gridscale orography'
    slope_globe_meta%shortName = 'SSO_SIGMA'
    slope_globe_meta%units = c_undef
    slope_globe_meta%grid_mapping = gridmp
    slope_globe_meta%coordinates = coord
    
    fr_land_globe_meta%varname = 'fr_land_globe'
    fr_land_globe_meta%n_dim = n_dim
    fr_land_globe_meta%diminfo => diminfo
    fr_land_globe_meta%vartype = vartype_real !REAL variable
    fr_land_globe_meta%standard_name = 'FR_LAND'
    fr_land_globe_meta%long_name = 'fraction land due to GLOBE data'
    fr_land_globe_meta%shortName = 'FR_LAND'
    fr_land_globe_meta%units =  c_undef
    fr_land_globe_meta%grid_mapping = gridmp
    fr_land_globe_meta%coordinates = coord

    ! z0_topo_meta
    z0_topo_meta%varname = 'z0_topo'
    z0_topo_meta%n_dim = n_dim
    z0_topo_meta%diminfo => diminfo
    z0_topo_meta%vartype = vartype_real !REAL variable
    z0_topo_meta%standard_name = 'Z0'
    z0_topo_meta%long_name = 'Roughness length'
    z0_topo_meta%shortName = 'Z0'
    z0_topo_meta%units =  c_undef
    z0_topo_meta%grid_mapping = gridmp
    z0_topo_meta%coordinates = coord


    
  END SUBROUTINE def_globe_meta

  !> define meta information for target fields defined on vertices derived from GLOBE data
  SUBROUTINE def_globe_vertex_meta(nvertex)
  INTEGER, INTENT(IN) :: nvertex !< total number of vertices

    ! set meta information for strucutre dim_buffer_vertex
    dim_buffer_vertex(1)%dimname = 'vertex'
    dim_buffer_vertex(1)%dimsize = nvertex
    dim_buffer_vertex(2)%dimname = 'je_v'
    dim_buffer_vertex(2)%dimsize = 1
    dim_buffer_vertex(3)%dimname = 'ke_v'
    dim_buffer_vertex(3)%dimsize = 1

    hh_vert_meta%varname = 'topography_v'
    hh_vert_meta%n_dim = 3
    hh_vert_meta%diminfo => dim_buffer_vertex
    hh_vert_meta%vartype = vartype_real !REAL variable
    hh_vert_meta%standard_name = 'HSURF_v'
    hh_vert_meta%long_name = 'topographic height at cell vertices'
    hh_vert_meta%shortName = 'HSURF'
    hh_vert_meta%units = 'm'
    hh_vert_meta%grid_mapping = c_undef
    hh_vert_meta%coordinates = c_undef
    
    npixel_vert_meta%varname = 'npixel_vert'
    npixel_vert_meta%n_dim = 3
    npixel_vert_meta%diminfo => dim_buffer_vertex
    npixel_vert_meta%vartype = vartype_int !INTEGER variable
    npixel_vert_meta%standard_name = 'npixel'
    npixel_vert_meta%long_name = 'number of raw data pixel'
    npixel_vert_meta%shortName = c_undef
    npixel_vert_meta%units = c_undef
    npixel_vert_meta%grid_mapping = c_undef
    npixel_vert_meta%coordinates = c_undef
  END SUBROUTINE def_globe_vertex_meta




  

  !> set projection information for netcdf output for the COSMO grid
  SUBROUTINE set_nc_grid_def_cosmo(cosmo_grid,grid_mapping)
    TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid
    CHARACTER (len=80), INTENT(IN):: grid_mapping !< netcdf attribute grid mapping
    INTEGER :: errorcode

    IF (.NOT.ALLOCATED(nc_grid_def_cosmo%map_param)) THEN
      ALLOCATE(nc_grid_def_cosmo%map_param(1:3),STAT=errorcode)
      IF (errorcode /= 0 ) CALL abort_extpar('Cant nc_grid_def_cosmo%map_param')
    ENDIF
    nc_grid_def_cosmo%grid_mapping_varname =  TRIM(grid_mapping)
    nc_grid_def_cosmo%grid_mapping_name%attname='grid_mapping_name'
    nc_grid_def_cosmo%grid_mapping_name%attributetext = 'rotated_latitude_longitude' ! netcdf attribute with grid mapping name according to cf, see e.g. http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/apf.html
    nc_grid_def_cosmo%n_r_att = 3 ! number of projection parameters
    nc_grid_def_cosmo%map_param(1)%attname     = 'grid_north_pole_longitude'
    nc_grid_def_cosmo%map_param(1)%att_value_r = REAL(cosmo_grid%pollon) ! this is for type conversion
    nc_grid_def_cosmo%map_param(2)%attname     = 'grid_north_pole_latitude'
    nc_grid_def_cosmo%map_param(2)%att_value_r = REAL(cosmo_grid%pollat) ! this is for type conversion
    nc_grid_def_cosmo%map_param(3)%attname     = 'north_pole_grid_longitude'
    nc_grid_def_cosmo%map_param(3)%att_value_r = REAL(cosmo_grid%polgam) ! this is for type conversion

  END SUBROUTINE set_nc_grid_def_cosmo
  

  !> set projection information for netcdf output for the ICON grid
  SUBROUTINE set_nc_grid_def_icon(grid_mapping)
    USE mo_physical_constants, ONLY: re !< av. radius of the earth [m]
    CHARACTER (len=80), INTENT(IN):: grid_mapping !< netcdf attribute grid mapping
    INTEGER :: errorcode

    ALLOCATE(nc_grid_def_icon%map_param(1:2),STAT=errorcode)
    IF (errorcode /= 0 ) CALL abort_extpar('Cant nc_grid_def_icon%map_param')
    nc_grid_def_icon%grid_mapping_varname = TRIM(grid_mapping)
    nc_grid_def_icon%grid_mapping_name%attname='grid_mapping_name'
    nc_grid_def_icon%grid_mapping_name%attributetext = 'latitude_longitude'  ! netcdf attribute with grid mapping name according to cf, see e.g. http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/apf.html
    nc_grid_def_icon%n_r_att = 2 ! number of projection parameters
    nc_grid_def_icon%map_param(1)%attname     = 'semi_major_axis'
    nc_grid_def_icon%map_param(1)%att_value_r = REAL(re)  ! type conversion to standard real
    nc_grid_def_icon%map_param(2)%attname     = 'inverse_flattening'
    nc_grid_def_icon%map_param(2)%att_value_r = 0.

 END SUBROUTINE set_nc_grid_def_icon


END MODULE mo_var_meta_data


