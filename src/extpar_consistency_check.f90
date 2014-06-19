!+  Fortran main program for consistency check of external parameters
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  small bug fixes accroding to Fortran compiler warnings
! V1_2         2011/03/25 Hermann Asensio
!  update to support ICON refinement grids
!  introduce simple hight correction for T_CL field
!  Add DWD_min_lake_depth for minimal lake depth
! V1_3         2011/04/19 Hermann Asensio
!  introduce Globcover 2009 land use data set for external parameters
!  additional concistency check for vegetation on ice grid elements (glaciers)
! add support for GRIB1 and GRIB2
! V1_4         2011/04/21 Hermann Asensio
!  add a manual correction for the lake depth of Lake Constance
! V1_5         2011/08/11 Hermann Asensio
!  bug fix in the concistency check for FR_LAKE
!  remove erroneous simple height correction of T_CL field
! V1_7         2013/01/25 Guenther Zaengl 
!   Parallel threads for ICON and COSMO using Open-MP, 
!   Several bug fixes and optimizations for ICON search algorithm, 
!   particularly for the special case of non-contiguous domains; 
!   simplified namelist control for ICON 
! V1_8         2013-03-12 Frank Brenner
!  introduced MODIS albedo dataset(s) as new external parameter(s)        
! V1_11        2013/04/16 Juergen Helmert
!  Adaptions for using special points and external land-sea mask 
! V1_12        2013-04-24 Frank Brenner
!  bug fix regarding old file paths         
! V2_0         2013-06-04 Martina Messmer
!  introduce HWSD soil data set for external parameters (Juergen Helmert)
!  introduce lradtopo parameters for external parameters (Anne Roches)
!  add possibility to omit the SSO parameters (Martina Messmer)
!  introduce a finer temperature climatology data set (CRU) for external
!  parameters (CLM Community)
!  adjusted consistency check for soiltype on ice grid elements for 
!  Globcover 2009
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran main program for consistency check of external parameters 
!>  
!> Purpose: read in the external paraemeters and check for consistency, especially for the land-sea mask.
!> \author Hermann Asensio
PROGRAM extpar_consistency_check

! Load the library information data:
  USE info_extpar, ONLY: info_define, info_readnl, info_print


USE mo_kind, ONLY: wp
USE mo_kind, ONLY: i4
USE mo_kind, ONLY: i8

USE mo_utilities_extpar, ONLY: free_un

USE mo_target_grid_data, ONLY: no_raw_data_pixel
USE mo_target_grid_data, ONLY: lon_geo
USE mo_target_grid_data, ONLY: lat_geo

USE mo_target_grid_data, ONLY: allocate_com_target_fields
USE mo_target_grid_data, ONLY: tg
 
USE mo_grid_structures, ONLY: rotated_lonlat_grid
USE mo_grid_structures, ONLY: icosahedral_triangular_grid
USE mo_grid_structures, ONLY: target_grid_def

USE mo_grid_structures, ONLY: igrid_icon
USE mo_grid_structures, ONLY: igrid_cosmo
USE mo_grid_structures, ONLY: igrid_gme

USE  mo_cosmo_grid, ONLY: COSMO_grid, &
  &                         lon_rot, &
  &                         lat_rot, &
  &                         allocate_cosmo_rc, &
  &                         get_cosmo_grid_info, &
  &                         calculate_cosmo_domain_coordinates

  
USE  mo_cosmo_grid, ONLY: calculate_cosmo_target_coordinates

USE mo_gme_grid, ONLY: gme_grid
USE mo_gme_grid, ONLY: spoke

USE  mo_icon_grid_data, ONLY: icon_grid !< structure which contains the definition of the ICON grid
USE  mo_icon_grid_data, ONLY: icon_grid_region

USE mo_icon_grid_data, ONLY:  nvertex_per_cell

USE mo_base_geometry,    ONLY:  geographical_coordinates, &
                                 cartesian_coordinates
  
USE mo_additional_geometry,   ONLY: cc2gc,                  &
                              gc2cc,                  &
                              arc_length,             &
                              cos_arc_length,         &
                              inter_section,          &
                              vector_product,         &
                              point_in_polygon_sp

                              

USE mo_icon_domain,          ONLY: icon_domain,         &
                              grid_cells,               &
                              grid_vertices,            &
                              construct_icon_domain,    &
                              destruct_icon_domain


USE mo_io_units,          ONLY: filename_max

USE mo_exception,         ONLY: message_text, message, finish

USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_math_constants,  ONLY: pi, pi_2, dbl_eps,rad2deg

USE mo_read_extpar_namelists, ONLY: read_namelists_extpar_check,&
                             read_namelists_extpar_special_points

USE mo_soil_routines, ONLY: read_namelists_extpar_soil

USE mo_soil_routines, ONLY: get_soil_data, &
                            get_dimension_soil_data

USE mo_soil_data, ONLY: allocate_raw_soil_fields,&
                        define_soiltype,         &
                        dsmw_legend,             &
                        soil_texslo,             &
                        dsmw_soil_unit,          &
                        n_unit,                  &
                        dsmw_grid,               &
                        lon_soil,                &
                        lat_soil

USE mo_soil_data, ONLY: undef_soiltype, default_soiltype, soiltype_ice, soiltype_water
USE mo_soil_data, ONLY: soil_data, FAO_data, HWSD_data, HWSD_map
USE mo_soil_data, ONLY: deep_soil

USE   mo_soil_tg_fields, ONLY:  fr_sand,fr_silt,fr_clay, &
                              & fr_oc,fr_bd,fr_dm
USE   mo_soil_tg_fields, ONLY:  fr_sand_deep,fr_silt_deep, &
                                fr_clay_deep, fr_oc_deep,  &
                                fr_bd_deep,fr_dm_deep
USE   mo_soil_tg_fields, ONLY:  fr_land_soil
USE   mo_soil_tg_fields, ONLY:  soiltype_fao, soiltype_deep
USE   mo_soil_tg_fields, ONLY:  allocate_soil_target_fields

USE mo_soil_consistency, ONLY:  calculate_soiltype

USE mo_soil_output_nc, ONLY: write_netcdf_soil_cosmo_grid
USE mo_soil_output_nc, ONLY: write_netcdf_soil_icon_grid
USE mo_soil_output_nc, ONLY: write_netcdf_soil_buffer

USE mo_soil_output_nc, ONLY: read_netcdf_soil_buffer

USE mo_target_grid_routines, ONLY: init_target_grid

USE mo_glc2000_tg_fields, ONLY: allocate_glc2000_target_fields

USE mo_glc2000_tg_fields, ONLY:  fr_land_glc2000, &
    &      glc2000_class_fraction,    &
    &      glc2000_class_npixel, &
    &      glc2000_tot_npixel, &
    &      ice_glc2000, &
    &      z0_glc2000, &
    &      root_glc2000, &
    &      plcov_mn_glc2000, &
    &      plcov_mx_glc2000, &
    &      lai_mn_glc2000, &
    &      lai_mx_glc2000, &
    &      rs_min_glc2000, &
    &      urban_glc2000,  &
    &      for_d_glc2000,  &
    &      for_e_glc2000, &
    &      emissivity_glc2000

USE mo_glcc_lookup_tables, ONLY: nclass_glcc
USE mo_glcc_lookup_tables, ONLY: init_glcc_lookup_tables
USE mo_glcc_lookup_tables, ONLY: glcc_legend
USE mo_glcc_lookup_tables, ONLY: ilookup_table_glcc, &
  &                              i_gme_lookup_table_glcc, &
  &                              i_cosmo_lookup_table_glcc, &
  &                              i_experimental_lookup_table_glcc
USE mo_glcc_lookup_tables, ONLY: z0_lt_glcc, lnz0_lt_glcc, plc_mn_lt_glcc, plc_mx_lt_glcc

USE mo_glcc_lookup_tables, ONLY: lai_mn_lt_glcc, lai_mx_lt_glcc, rd_lt_glcc, emiss_lt_glcc, rs_min_lt_glcc         

USE mo_glcc_tg_fields, ONLY:  fr_land_glcc, &
      &       glcc_class_fraction,    &
      &        glcc_class_npixel, &
      &        glcc_tot_npixel, &
      &        ice_glcc, &
      &        z0_glcc, &
      &        root_glcc, &
      &        plcov_mn_glcc, &
      &        plcov_mx_glcc, &
      &        lai_mn_glcc, &
      &        lai_mx_glcc, &
      &        rs_min_glcc, &
      &        urban_glcc,  &
      &        for_d_glcc,  &
      &        for_e_glcc, &
      &        emissivity_glcc, &
      &        allocate_glcc_target_fields

USE mo_glc2000_lookup_tables, ONLY: ilookup_table_glc2000, &
  &                                 get_name_glc2000_lookup_tables, &
  &                                 name_lookup_table_glc2000
USE mo_glc2000_lookup_tables, ONLY: nclass_glc2000

USE mo_globcover_data,   ONLY: max_tiles_lu  !_br 21.02.14 including definition of max_tiles_lu

USE mo_globcover_lookup_tables, ONLY: nclass_globcover

USE mo_globcover_lookup_tables, ONLY: get_name_globcover_lookup_tables
USE mo_glcc_lookup_tables, ONLY: get_name_glcc_lookup_tables
USE mo_ecoclimap_lookup_tables, ONLY: nclass_ecoclimap
USE mo_ecoclimap_lookup_tables, ONLY: get_name_ecoclimap_lookup_tables


USE mo_landuse_output_nc, ONLY: read_netcdf_buffer_glc2000
USE mo_landuse_output_nc, ONLY: read_netcdf_buffer_glcc
USE mo_landuse_output_nc, ONLY: read_netcdf_buffer_lu
USE mo_landuse_output_nc, ONLY: read_netcdf_buffer_ecoclimap

USE mo_landuse_routines, ONLY: read_namelists_extpar_land_use

USE mo_lu_tg_fields, ONLY :  i_lu_globcover, i_lu_glc2000, i_lu_glcc
USE mo_lu_tg_fields, ONLY :  i_lu_ecoclimap

USE mo_lu_tg_fields, ONLY: fr_land_lu, &
  &    fr_land_mask, &
  &        ice_lu, &
  &        z0_lu, &
  &        z0_tot, &
  &        root_lu, &
  &        plcov_mn_lu, &
  &        plcov_mx_lu, &
  &        lai_mn_lu, &
  &        lai_mx_lu, &
  &        rs_min_lu, &
  &        urban_lu,  &
  &        for_d_lu,  &
  &        for_e_lu, &
  &        emissivity_lu, &
  &        fr_ocean_lu, &
  &        lu_class_fraction,    &
  &        lu_class_npixel, &
  &        lu_tot_npixel,  &
  &        lai12_lu,     &
  &        z012_lu,      &
  &        plcov12_lu    


USE mo_lu_tg_fields, ONLY: allocate_lu_target_fields, allocate_add_lu_fields

USE mo_albedo_tg_fields, ONLY: alb_dry, &
   &                        alb_sat, &
   &                        alb_field_mom, &
   &                        alnid_field_mom, &
   &                        aluvd_field_mom, &
   &                        allocate_alb_target_fields, &
   &                        alb_interpol

USE mo_albedo_data, ONLY: ntime_alb
USE mo_albedo_data, ONLY: undef_alb, minimal_alb
USE mo_albedo_data, ONLY: wso_min,wso_max,csalb,csalbw,zalso
USE mo_albedo_data, ONLY: allocate_alb_interp_fields, &
    &                     alb_interp_data
USE mo_albedo_data, ONLY:  undef_alb, minimal_alb_dry, maximal_alb_dry, &
    &                      minimal_alb_sat, maximal_alb_sat, undef_alb_bs
USE mo_albedo_data, ONLY: ialb_type

USE mo_albedo_output_nc, ONLY: read_netcdf_buffer_alb

USE mo_albedo_routines, ONLY: get_pixel_ALB_data, &
                              open_netcdf_ALB_data, &
                              const_check_interpol_alb,&
                              read_namelists_extpar_alb

USE mo_bilinterpol, ONLY: get_4_surrounding_raw_data_indices, &
  &                       calc_weight_bilinear_interpol, &
  &                       calc_value_bilinear_interpol

USE mo_albedo_data, ONLY: alb_raw_data_grid, &
                          lon_alb, &
                          lat_alb, &
                          ntime_alb

USE mo_ndvi_tg_fields, ONLY: ndvi_field, &
    &                                ndvi_max, &
    &                                ndvi_field_mom, &
    &                                ndvi_ratio_mom, &
    &                                allocate_ndvi_target_fields

USE mo_ndvi_data, ONLY: ntime_ndvi
USE mo_ndvi_data, ONLY: undef_ndvi, minimal_ndvi

USE mo_ndvi_output_nc, ONLY: read_netcdf_buffer_ndvi

USE mo_topo_tg_fields, ONLY:  fr_land_topo,       &
    &                         hh_topo,            &
    &                         stdh_topo,          &
    &                         theta_topo,         &
    &                         aniso_topo,         &
    &                         slope_topo,         &
    &                         z0_topo,            &
    &                         slope_asp_topo,     &
    &                         slope_ang_topo,     &
    &                         horizon_topo,       &
    &                         skyview_topo,       &
    &                         allocate_topo_target_fields

USE mo_topo_tg_fields, ONLY: add_parameters_domain, &
    &        vertex_param, &
    &        allocate_additional_hh_param
   
USE mo_topo_output_nc, ONLY: read_netcdf_buffer_topo

USE mo_topo_routines, ONLY: read_namelists_extpar_orography

USE mo_topo_data, ONLY: lradtopo, nhori, max_tiles, itopo_type

USE mo_aot_target_fields, ONLY: allocate_aot_target_fields,&
    &                              aot_tg
    
USE mo_aot_output_nc, ONLY: read_netcdf_buffer_aot

USE mo_cru_target_fields, ONLY: allocate_cru_target_fields,   &
    &                              crutemp, crutemp2, cruelev,&
    &                              i_t_cru_fine, i_t_cru_coarse

USE mo_cru_data,  ONLY: read_namelists_extpar_t_clim

USE mo_cru_output_nc, ONLY: read_netcdf_buffer_cru

USE mo_aot_data, ONLY: ntype_aot, ntime_aot, iaot_type

USE mo_aot_data, ONLY: read_namelists_extpar_aerosol

USE mo_flake_tg_fields, ONLY: fr_lake, &
  &       lake_depth,    &
  &       flake_tot_npixel, &
  &       allocate_flake_target_fields
USE mo_flake_data, ONLY: flake_depth_undef !< default value for undefined lake depth
USE mo_flake_data, ONLY: flake_depth_default !< default value for default lake depth, 10 [m]
USE mo_flake_data, ONLY: DWD_max_lake_depth !< Maximum lake depth in [m] for FLAKE (50 m)
USE mo_flake_data, ONLY: DWD_min_lake_depth !< Minimal lake depth in [m] for FLAKE (1 m)

USE mo_flake_output_nc, ONLY: read_netcdf_buffer_flake

USE mo_lsm_output_nc, ONLY: read_netcdf_buffer_lsm

USE mo_extpar_output_nc, ONLY: write_netcdf_icon_grid_extpar, &
  &                        write_netcdf_cosmo_grid_extpar

#ifdef GRIBAPI
USE mo_extpar_output_grib, ONLY: write_cosmo_grid_extpar_grib, &
  &                              write_gme_grid_extpar_grib
#endif

USE mo_math_constants, ONLY: deg2rad, rad2deg
USE mo_physical_constants, ONLY: re ! av. radius of the earth [m]

USE mo_lradtopo,   ONLY: read_namelists_extpar_lradtopo

USE mo_search_target_grid, ONLY: find_nearest_target_grid_element


  IMPLICIT NONE

  CHARACTER (len=filename_max) :: namelist_grid_def
  CHARACTER (len=filename_max) :: netcdf_output_filename

  CHARACTER (len=filename_max) :: grib_output_filename
  CHARACTER (len=filename_max) :: grib_sample

  CHARACTER (len=filename_max) :: input_namelist_cosmo_grid !< file with input namelist with COSMO grid definition
  CHARACTER (len=filename_max) :: namelist_topo_data_input !< file with input namelist with GLOBE data information

  
  CHARACTER (len=filename_max) :: netcdf_out_filename      !< filename for netcdf file with data

  CHARACTER (len=filename_max) :: netcdf_in_filename      !< filename for netcdf file with data

  !-----------------------------------------------------------------------------------------------------------------------
  CHARACTER (len=filename_max) :: namelist_file !< filename with namelists for for EXTPAR settings
  ! soil
  CHARACTER (len=filename_max) :: soil_buffer_file  !< name for soil buffer file
  CHARACTER (len=filename_max) :: soil_output_file  !< name for soil output file
  CHARACTER (len=filename_max) :: soil_buffer_file_consistent !< name for soil buffer file after consistency check
  CHARACTER (len=filename_max) :: soil_output_file_consistent !< name for soil output file after consistency check

  CHARACTER (len=filename_max) :: soil_buffer_cons_output !< name for soil output file after consistency check

  CHARACTER (len=filename_max) :: raw_data_soil_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_soil_filename !< filename soil raw data
  CHARACTER (len=filename_max) :: raw_data_deep_soil_filename !< filename deep soil raw data


  ! orography
  CHARACTER (len=filename_max) :: orography_buffer_file !< name for orography buffer file
  CHARACTER (len=filename_max) :: orography_output_file !< name for orography output file
  
  CHARACTER (len=filename_max) :: raw_data_orography_path        !< path to raw data

  CHARACTER (len=filename_max) :: orography_buffer_cons_output  !< name for orography output file after consistency check

  ! land use

  CHARACTER (len=filename_max) :: raw_data_lu_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_lu_filename(1:max_tiles_lu) !< filename glc2000 raw data !_br 21.02.14
  CHARACTER(len=filename_max) :: name_lookup_table_lu !< name for look up table
  CHARACTER(len=filename_max) :: lu_dataset !< name of landuse data set


  CHARACTER (len=filename_max) :: lu_buffer_file !< name for glc2000 buffer file
  CHARACTER (len=filename_max) :: lu_output_file !< name for glc2000 output file
  CHARACTER (len=filename_max) :: glc2000_buffer_cons_output  !< name for glc200 output file after consistency check

  CHARACTER (len=filename_max) :: raw_data_glcc_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_glcc_filename !< filename glc2000 raw data

  CHARACTER (len=filename_max) :: glcc_buffer_file    !< name for glcc buffer file
  CHARACTER (len=filename_max) :: glcc_output_file    !< name for glcc output file
  CHARACTER (len=filename_max) :: glcc_buffer_cons_output  !< name for glcc output file after consistency check

  CHARACTER(len=filename_max) :: input_glc2000_namelist_file
  !-----------------------------------------------------------------------------------------------------------------------
  ! albedo
  CHARACTER (len=filename_max) :: raw_data_alb_path   !< path to albedo raw input data
  CHARACTER (len=filename_max) :: raw_data_alb_filename    !< raw data filename
  CHARACTER (len=filename_max) :: raw_data_alnid_filename  !< raw data filename, NI
  CHARACTER (len=filename_max) :: raw_data_aluvd_filename  !< raw data filename, UV

  CHARACTER (len=filename_max) :: alb_buffer_file    !< name for albedo buffer file
  CHARACTER (len=filename_max) :: alb_output_file    !< name for albedo output file
  !-----------------------------------------------------------------------------------------------------------------------
  CHARACTER (len=filename_max) :: land_sea_mask_file !< name for external land-sea-mask

  ! NDVI
  CHARACTER (len=filename_max) :: raw_data_ndvi_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_ndvi_filename !< filename NDVI raw data

  CHARACTER (len=filename_max) :: ndvi_buffer_file !< name for NDVI buffer file
  CHARACTER (len=filename_max) :: ndvi_output_file !< name for NDVI output file
  CHARACTER (len=filename_max) :: ndvi_buffer_cons_output  !< name for ndvi output file after consistency check

  ! temperature climatology
  CHARACTER (len=filename_max) :: namelist_file_t_clim !< filename with namelists for for EXTPAR settings
  CHARACTER (len=filename_max) :: raw_data_t_clim_path     !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_t_clim_filename !< filename temperature climatology raw data


  CHARACTER (len=filename_max) :: t_clim_buffer_file !< name for temperature climatology buffer
  CHARACTER (len=filename_max) :: t_clim_output_file !< name for temperature climatology output file
  CHARACTER (len=filename_max) :: t_clim_buffer_cons_output  !< name for t_clim output file after consistency check

  ! aerosol optical thickness
  
  CHARACTER (len=filename_max) :: raw_data_aot_path        !< path to raw data
  CHARACTER (len=filename_max) :: raw_data_aot_filename !< filename temperature climatology raw data

  CHARACTER (len=filename_max) :: flake_buffer_file  !< name for flake buffer file
  CHARACTER (len=filename_max) :: flake_buffer_cons_output  !< name for flake output file after consistency check

  CHARACTER (len=filename_max) :: aot_buffer_file !< name for aerosol buffer file
  CHARACTER (len=filename_max) :: aot_output_file !< name for aerosol output file

  CHARACTER (len=filename_max) :: aot_buffer_cons_output  !< name for aot output file after consistency check

  CHARACTER (len=filename_max) :: topo_files(1:max_tiles) !< filenames globe raw data
  INTEGER (KIND=i4)  :: ntiles_column
  INTEGER (KIND=i4)  :: ntiles_row
  INTEGER (KIND=i8)  :: it_cl_type
  INTEGER (KIND=i4)  :: isoil_data   
  LOGICAL            :: lsso_param
  LOGICAL            :: ldeep_soil

  REAL (KIND=wp) :: point_lon_geo !< longitude of a point in geographical system
  REAL (KIND=wp) :: point_lat_geo !< latitude of a point in geographical system

  REAL(KIND=wp) :: undefined !< value to indicate undefined grid elements 
  INTEGER (KIND=i4) :: undef_int   !< value for undefined integer

  REAL(KIND=wp), PARAMETER :: undefined_lu = 0.0_wp !< value to indicate undefined land use grid elements 
  REAL(KIND=wp) :: thr_cr !< control threshold
  REAL(KIND=wp) :: fill_value_real !< value to indicate undefined grid elements 
  INTEGER (KIND=i4) :: fill_value_int   !< value for undefined integer

  INTEGER (KIND=i4) :: default_value !< default value

  INTEGER (KIND=i4) :: index_tile   !< index for dummy test
  TYPE(geographical_coordinates) :: DWD_location !< geographical coordinates of DWD for dummy test

  TYPE(geographical_coordinates) :: ur   !< upper right point for test block
  TYPE(geographical_coordinates) :: ll   !< lower left point for test block

  INTEGER (KIND=i4), ALLOCATABLE :: time(:) !< array with time axis values (needed for netcdf cf convention)
  INTEGER (KIND=i4), PARAMETER :: mpy=12     !< month per year
  INTEGER (KIND=i4):: nmonth  !< index for month

  INTEGER :: i !< counter
  INTEGER :: j !< counter
  INTEGER :: k !< counter
  INTEGER :: t !< counter
  INTEGER :: ie !< counter
  INTEGER :: je !< counter
  INTEGER :: ke !< counter

  INTEGER :: jj !< counter
  INTEGER :: ii !< counter 

  INTEGER :: n
  INTEGER :: nloops

  INTEGER :: ni !< gme resolution flag
  INTEGER :: nnb !< number of neighbor grid elements with common edge
  INTEGER :: nv  !< number of vertices
  INTEGER :: n_index !< help variable
  INTEGER :: ne_ie(9) !< index for grid element neighbor
  INTEGER :: ne_je(9) !< index for grid element neighbor
  INTEGER :: ne_ke(9) !< index for grid element neighbor
  INTEGER :: ks1(6) !< index for grid element neighbor
  INTEGER :: ks2(6) !< index for grid element neighbor
  INTEGER :: ksd(6) !< index for grid element neighbor

  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid
  INTEGER  :: i_landuse_data !<integer switch to choose a land use raw data set
  INTEGER  :: i_lsm_data !<integer switch to choose a land sea mask data set
  INTEGER  :: ilookup_table_lu !< integer switch to choose a lookup table
  INTEGER  :: nclass_lu !< number of land use classes 

! variables for the T_CLIM
  LOGICAL ::  last=.FALSE. ! in TCL leave loop
  LOGICAL ::  foundtcl=.FALSE. ! in TCL 
  REAL(KIND=wp) :: nextd      !< next distance in T_CL search 

  ! variables for the ICON grid
  INTEGER :: first_dom                    !< index of first (global) model domain 
  INTEGER :: idom                         !< counter for domains
  
  ! variables for the "Erdmann Heise Formel"
  REAL (KIND=wp) :: dnorm  !< scale factor 
  REAL (KIND=wp) :: zlnorm = 2250._wp    !< scale factor [m]
  REAL (KIND=wp) :: alpha  = 1.E-05_wp !< scale factor [1/m] 
  REAL (KIND=wp) :: factor !< factor
  REAL (KIND=wp) :: zhp = 10.0_wp    !< height of Prandtl-layer [m]
  REAL (KIND=wp) :: zlnhp      !< ln of height of Prandtl-layer [m]
  ! lapse rate
  REAL (KIND=wp) :: lr = 0.0064_wp !< lapse rate [K/m] 

  LOGICAL :: l_use_glcc=.FALSE. !< flag if additional glcc data are present
  REAL :: lu_data_southern_boundary

  !HA tests
  REAL :: timestart
  REAL :: timeend
  REAL :: timediff

  INTEGER           :: db_ice_counter


  INTEGER :: count1, count2, count3
  REAL (KIND=wp) :: step

  REAL (KIND=wp) :: bwlon !< weight for bilinear interpolation
  REAL (KIND=wp) :: bwlat !< weight for bilinear interpolation
  REAL (KIND=wp)   :: alb_sw       !< value of the NDVI raw data pixel south west
  REAL (KIND=wp)   :: alb_se       !< value of the NDVI raw data pixel south east
  REAL (KIND=wp)   :: alb_ne       !< value of the NDVI raw data pixel north east
  REAL (KIND=wp)   :: alb_nw       !< value of the NDVI raw data pixel north west
  REAL (KIND=wp)   :: lon_alt

!Special Points
  INTEGER          :: number_special_points
  REAL (KIND=wp)   ::                           lon_geo_sp=-999.,           &
                                                lat_geo_sp=-999.,           &
                                                soiltype_sp=-999.,          &
                                                z0_sp=-999.,                &
                                                rootdp_sp=-999.,            &
                                                plcovmn_sp=-999.,           &
                                                plcovmx_sp=-999.,           &
                                                laimn_sp=-999.,               &
                                                laimx_sp=-999.
  INTEGER (KIND=i8) :: start_cell_id !< ID of starting cell for ICON search
  INTEGER (KIND=i8) :: isp,i_sp,j_sp,k_sp



  INTEGER (KIND=i8) :: western_column     !< the index of the western_column of raw data 
  INTEGER (KIND=i8) :: eastern_column     !< the index of the eastern_column of raw data 
  INTEGER (KIND=i8) :: northern_row       !< the index of the northern_row of raw data 
  INTEGER (KIND=i8) :: southern_row       !< the index of the southern_row of raw data
  LOGICAL :: gldata=.TRUE. ! Albedo data are global
  REAL (KIND=wp) :: point_lon       !< longitude coordinate in geographical system of input point 
  REAL (KIND=wp) :: point_lat       !< latitude coordinate in geographical system of input point
  REAL (KIND=wp) :: lon_geo_w,lon_geo_e,lat_geo_n,lat_geo_s
  INTEGER (KIND=i4) :: i_miss,errorcode,ncid_alb,time_index
  INTEGER (KIND=i4) :: point_reg_lon_index          !< longitude index of point for regular lon-lat grid
  INTEGER (KIND=i4) :: point_reg_lat_index          !< latitude index of point for regular lon-lat grid
  INTEGER (KIND=i4) :: nlon_reg !< number of columns
  INTEGER (KIND=i4) :: nlat_reg !< number of rows
  CHARACTER (len=filename_max) :: path_alb_file 
  CHARACTER (len=filename_max) :: path_alnid_file
  CHARACTER (len=filename_max) :: path_aluvd_file
  CHARACTER (len=filename_max) :: alb_source, alnid_source, aluvd_source
  CHARACTER (len=filename_max) :: namelist_alb_data_input

! for albedo consistency check
  REAL (KIND=wp) :: albvis_min, albnir_min, albuv_min  

! T_CL consistency check
  INTEGER (KIND=i4) :: iml, imu, ipl, ipu, jml, jmu, jpl, jpu, ntclct, l
  REAL    (KIND=wp) :: tclsum, elesum


  ! Print the default information to stdout:
  CALL info_define ('extpar_consistency_check')      ! Pre-define the program name as binary name
  CALL info_print ()                     ! Print the information to stdout
  !--------------------------------------------------------------------------------------------------------
  !--------------------------------------------------------------------------------------------------------
  !--------------------------------------------------------------------------------------------------------

  ! Get lradtopo and nhori value from namelist

  namelist_file = 'INPUT_RADTOPO'
  CALL read_namelists_extpar_lradtopo(namelist_file,lradtopo,nhori)

  ! Get lsso_param from namelist

  namelist_file = 'INPUT_ORO'
  CALL read_namelists_extpar_orography(namelist_file,          & 
                                       raw_data_orography_path,&  
                                       topo_files,             &  
                                       ntiles_column,          &
                                       ntiles_row,             &
                                       itopo_type,             &  
                                       lsso_param,             &  
                                       orography_buffer_file,  &
                                       orography_output_file)

  namelist_file = 'INPUT_SOIL'
  CALL read_namelists_extpar_soil(namelist_file,                     &
                                         isoil_data,                 &
                                         ldeep_soil,                 &
                                         raw_data_soil_path,         &
                                         raw_data_soil_filename,     &
                                         raw_data_deep_soil_filename,&
                                         soil_buffer_file,           &
                                         soil_output_file,           &
                                         soil_buffer_file_consistent,&
                                         soil_output_file_consistent)

  IF (ldeep_soil.eqv..TRUE. .and. isoil_data /= HWSD_data) THEN  !_br 21.02.14 replaced eq by eqv
    ldeep_soil = .FALSE.
    print*, '********* you can only use the deep soil if HWSD data is used *********'
    print*, '********* ldeep_soil is set to FALSE *********'
  ENDIF

  print*, 'isoil_data: ', isoil_data
  print*, 'ldeep_soil: ', ldeep_soil

  namelist_file_t_clim = 'INPUT_TCLIM'
  CALL read_namelists_extpar_t_clim(namelist_file_t_clim,     &
                                    it_cl_type,               &
                                    raw_data_t_clim_path,     &
                                    raw_data_t_clim_filename, &
                                    t_clim_buffer_file,       &
                                    t_clim_output_file) 
   
  print*, 'it_cl_type: ', it_cl_type


  namelist_alb_data_input = 'INPUT_ALB'

  !--------------------------------------------------------------
  ! get namelist for albedo fields
  !--------------------------------------------------------------
  CALL  read_namelists_extpar_alb(namelist_alb_data_input, &
    &                                  raw_data_alb_path, &
    &                                  raw_data_alb_filename, &
    &                                  raw_data_alnid_filename, &
    &                                  raw_data_aluvd_filename, &
    &                                  ialb_type,        &
    &                                  alb_buffer_file, &
    &                                  alb_output_file, &
    &                                  alb_source, &
    &                                  alnid_source, &
    &                                  aluvd_source)

!  print*, 'ialb_type: ', ialb_type

  !--------------------------------------------------------------
  ! get namelist for aerosol fields
  !--------------------------------------------------------------
  namelist_file = 'INPUT_AOT'
  CALL read_namelists_extpar_aerosol(namelist_file, &
   &                                  iaot_type,    &
   &                                  raw_data_aot_path, &
   &                                  raw_data_aot_filename, &
   &                                  aot_buffer_file, &
   &                                  aot_output_file)

  !--------------------------------------------------------------------------------------------------------
  ! get information on target grid, allocate target fields with coordinates and determin the coordinates 
  ! for th target grid

  namelist_grid_def = 'INPUT_grid_org'
  CALL  init_target_grid(namelist_grid_def)
  PRINT *,' target grid tg: ',tg%ie, tg%je, tg%ke, tg%minlon, tg%maxlon, tg%minlat, tg%maxlat
  igrid_type = tg%igrid_type

 
SELECT CASE(igrid_type) 
     CASE(igrid_icon) ! ICON GRID
     PRINT *,'icon_grid%ncell: ',icon_grid%ncell
     PRINT *,'icon_grid%nvertex: ',icon_grid%nvertex

     CALL  allocate_additional_hh_param(icon_grid%nvertex)

END SELECT


  !--------------------------------------------------------------------------------------------------------
  ! get info on raw data file
  namelist_file = 'INPUT_LU'

  !---------------------------------------------------------------------------
  CALL read_namelists_extpar_land_use(namelist_file, &
    &                                 i_landuse_data, &
    &                                 raw_data_lu_path, &
    &                                 raw_data_lu_filename, &
    &                                 ilookup_table_lu, &
    &                                 lu_buffer_file, &
    &                                 lu_output_file)

   SELECT CASE (i_landuse_data)
     CASE (i_lu_ecoclimap)
       lu_dataset = 'ECOCLIMAP'
       CALL get_name_ecoclimap_lookup_tables(ilookup_table_lu, name_lookup_table_lu)
       nclass_lu = nclass_ecoclimap
       lu_data_southern_boundary = -90.9
     CASE (i_lu_globcover)
       lu_dataset = 'GLOBCOVER2009'
       CALL get_name_globcover_lookup_tables(ilookup_table_lu, name_lookup_table_lu)
       nclass_lu = nclass_globcover
       lu_data_southern_boundary = -64.99
     CASE (i_lu_glc2000)
       lu_dataset = 'GLC2000'
       CALL get_name_glc2000_lookup_tables(ilookup_table_lu, name_lookup_table_lu)
       nclass_lu = nclass_glc2000
       lu_data_southern_boundary = -56.0
     CASE(i_lu_glcc)
       lu_dataset = 'GLCC'
       CALL get_name_glcc_lookup_tables(ilookup_table_lu, name_lookup_table_lu)
       nclass_lu = nclass_glcc
       lu_data_southern_boundary = -90.0
   END SELECT

  PRINT *,'name_lookup_table_lu: ',TRIM(name_lookup_table_lu)

  !--------------------------------------------------------------------------------------------------------
  ! get filenames from namelist
  !--------------------------------------------------------------------------------------------------------
  namelist_file = 'INPUT_CHECK'

  CALL read_namelists_extpar_check(namelist_file, &
                                     grib_output_filename, &
                                     grib_sample, &
                                     netcdf_output_filename, &
                                     orography_buffer_file, &
                                     soil_buffer_file, &
                                     lu_buffer_file, &
                                     glcc_buffer_file, &
                                     flake_buffer_file, &
                                     ndvi_buffer_file, &
                                     t_clim_buffer_file, &
                                     aot_buffer_file, &
                                     alb_buffer_file, &
                                     i_lsm_data, &
                                     land_sea_mask_file,&
                                     number_special_points )

  ! test for glcc data
  INQUIRE(file=TRIM(glcc_buffer_file),exist=l_use_glcc)
  IF (l_use_glcc) THEN
    CALL allocate_glcc_target_fields(tg)
    PRINT *,'GLCC fields allocated'
  ENDIF
  ! allocate Land use target fields
  CALL allocate_lu_target_fields(tg)
  CALL allocate_add_lu_fields(tg,nclass_lu)
  PRINT *,'Land Use fields allocated'

  CALL allocate_soil_target_fields(tg,ldeep_soil)
  PRINT *,'soil fields allocated'

  CALL allocate_ndvi_target_fields(tg,ntime_ndvi)
  PRINT *,'ntime_ndvi ', ntime_ndvi
  PRINT *,'NDVI fields allocated'

  CALL allocate_topo_target_fields(tg,nhori)
  PRINT *,'TOPO fields allocated'

  CALL allocate_aot_target_fields(tg, ntime_aot, ntype_aot)
  PRINT *,'aot fields allocated'

  CALL allocate_cru_target_fields(tg)
  PRINT *,'cru temperature field allocated'

  CALL allocate_flake_target_fields(tg)
  PRINT *,'flake parameter fields allocated'

  CALL allocate_alb_target_fields(tg,ntime_alb,ialb_type)
  PRINT *,'albedo fields allocated'


  !--------------------------------------------------------------------------------------------------------
  ! Start Input

  PRINT *,'Read in Land Use data'
  PRINT *,'read ', TRIM(lu_buffer_file)

  SELECT CASE (i_landuse_data)
  CASE (i_lu_ecoclimap)
    lu_dataset = 'ECOCLIMAP'
    CALL read_netcdf_buffer_ecoclimap(lu_buffer_file,  &
    &                                     tg,         &
    &                                     nclass_lu, &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     fr_land_lu, &
    &                                     lu_class_fraction,    &
    &                                     lu_class_npixel, &
    &                                     lu_tot_npixel, &
    &                                     ice_lu, &
    &                                     z012_lu, &
    &                                     root_lu, &
    &                                     plcov12_lu, &
    &                                     lai12_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu)


  CASE(i_lu_globcover, i_lu_glc2000 )   
    CALL read_netcdf_buffer_lu(lu_buffer_file,  &
    &                                     tg,         &
    &                                     nclass_lu, &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     fr_land_lu, &
    &                                     lu_class_fraction,    &
    &                                     lu_class_npixel, &
    &                                     lu_tot_npixel, &
    &                                     ice_lu, &
    &                                     z0_lu, &
    &                                     root_lu, &
    &                                     plcov_mn_lu, &
    &                                     plcov_mx_lu, &
    &                                     lai_mn_lu, &
    &                                     lai_mx_lu, &
    &                                     rs_min_lu, &
    &                                     urban_lu,  &
    &                                     for_d_lu,  &
    &                                     for_e_lu, &
    &                                     emissivity_lu)

  END SELECT

  IF (l_use_glcc) THEN
   PRINT *,'read ', TRIM(glcc_buffer_file)
  CALL read_netcdf_buffer_glcc(glcc_buffer_file,  &
    &                                     tg,         &
    &                                     undefined, &
    &                                     undef_int,   &
    &                                     fr_land_glcc, &
    &                                     glcc_class_fraction,    &
    &                                     glcc_class_npixel, &
    &                                     glcc_tot_npixel, &
    &                                     ice_glcc, &
    &                                     z0_glcc, &
    &                                     root_glcc, &
    &                                     plcov_mn_glcc, &
    &                                     plcov_mx_glcc, &
    &                                     lai_mn_glcc, &
    &                                     lai_mx_glcc, &
    &                                     rs_min_glcc, &
    &                                     urban_glcc,  &
    &                                     for_d_glcc,  &
    &                                     for_e_glcc, &
    &                                     emissivity_glcc)
  ENDIF



  PRINT *,'Read in soil data'
  IF(ldeep_soil) THEN
    CALL read_netcdf_soil_buffer(soil_buffer_file,    &
   &                                     tg,          &
   &                                     isoil_data,  &
   &                                     undefined,   &
   &                                     undef_int,   &
   &                                     fr_land_soil,&
   &                                     soiltype_fao,&
   &                                     soiltype_deep)

  ELSE
    CALL read_netcdf_soil_buffer(soil_buffer_file,    &
   &                                     tg,          &
   &                                     isoil_data,  &
   &                                     undefined,   &
   &                                     undef_int,   &
   &                                     fr_land_soil,&
   &                                     soiltype_fao)
  ENDIF

  PRINT *,'Read in albedo data'
  IF (ialb_type == 2) THEN
    CALL read_netcdf_buffer_alb(alb_buffer_file,  &
     &                           tg, &
     &                           ntime_alb, &
     &                           undefined, &
     &                           undef_int,   &
     &                           alb_dry=alb_dry, &
     &                           alb_sat=alb_sat)
  ELSE IF (ialb_type == 1) THEN
    CALL read_netcdf_buffer_alb(alb_buffer_file,  &
     &                           tg, &
     &                           ntime_alb, &
     &                           undefined, &
     &                           undef_int,   &
     &                           alb_field_mom, &
     &                           alnid_field_mom, &
     &                           aluvd_field_mom)
  ELSE IF (ialb_type == 3) THEN
    CALL read_netcdf_buffer_alb(alb_buffer_file,  &
     &                           tg, &
     &                           ntime_alb, &
     &                           undefined, &
     &                           undef_int,   &
     &                           alb_field_mom)
  ENDIF


  PRINT *,'Read in NDVI data'
  CALL read_netcdf_buffer_ndvi(ndvi_buffer_file,  &
   &                                     tg,         &
   &                                     ntime_ndvi, &
   &                                     undefined, &
   &                                     undef_int,   &
   &                                     ndvi_max,  &
   &                                     ndvi_field_mom,&
   &                                     ndvi_ratio_mom)

   
   PRINT *,'Read in orography data'
  
   SELECT CASE(igrid_type)
     CASE(igrid_icon) ! ICON GRID

       IF (lsso_param) THEN

         CALL read_netcdf_buffer_topo(orography_buffer_file, &
          &                                     tg,           &
          &                                     undefined,    &
          &                                     undef_int,    &
          &                                     fr_land_topo,&
          &                                     hh_topo,     &
          &                                     stdh_topo,   &
          &                                     z0_topo,      &
          &                                     theta_topo=theta_topo,&
          &                                     aniso_topo=aniso_topo,&
          &                                     slope_topo=slope_topo,&
          &                                     vertex_param=vertex_param)
       ELSE
          CALL read_netcdf_buffer_topo(orography_buffer_file, &
          &                                     tg,           &
          &                                     undefined,    &
          &                                     undef_int,    &
          &                                     fr_land_topo,&
          &                                     hh_topo,     &
          &                                     stdh_topo,   &
          &                                     z0_topo,      &
          &                                     vertex_param=vertex_param)
        ENDIF

     CASE DEFAULT

     IF (lradtopo) THEN
       IF (lsso_param) THEN
         CALL read_netcdf_buffer_topo(orography_buffer_file,&
         &                                     tg,           &
         &                                     undefined,    &
         &                                     undef_int,    &
         &                                     fr_land_topo,&
         &                                     hh_topo,     &
         &                                     stdh_topo,   &
         &                                     z0_topo,      &
         &                                     lrad=lradtopo,&
         &                                     nhori=nhori,  &
         &                                     theta_topo=theta_topo,&
         &                                     aniso_topo=aniso_topo,&
         &                                     slope_topo=slope_topo,&
         &                                     slope_asp_topo=slope_asp_topo,     &
         &                                     slope_ang_topo=slope_ang_topo,     &
         &                                     horizon_topo=horizon_topo,         &
         &                                     skyview_topo=skyview_topo)
       ELSE
        CALL read_netcdf_buffer_topo(orography_buffer_file,&
         &                                     tg,           &
         &                                     undefined,    &
         &                                     undef_int,    &
         &                                     fr_land_topo,&
         &                                     hh_topo,     &
         &                                     stdh_topo,   &
         &                                     z0_topo,      &
         &                                     lrad=lradtopo,&
         &                                     nhori=nhori,  &
         &                                     slope_asp_topo=slope_asp_topo,     &
         &                                     slope_ang_topo=slope_ang_topo,     &
         &                                     horizon_topo=horizon_topo,         &
         &                                     skyview_topo=skyview_topo)
      ENDIF

     ELSE
       IF (lsso_param) THEN
         CALL read_netcdf_buffer_topo(orography_buffer_file,&
         &                                     tg,           &
         &                                     undefined,    &
         &                                     undef_int,    &
         &                                     fr_land_topo,&
         &                                     hh_topo,     &
         &                                     stdh_topo,   &
         &                                     z0_topo,      &
         &                                     nhori=nhori,  &
         &                                     theta_topo=theta_topo,  &
         &                                     aniso_topo=aniso_topo,  &
         &                                     slope_topo=slope_topo)
       ELSE
        CALL read_netcdf_buffer_topo(orography_buffer_file,&
         &                                     tg,           &
         &                                     undefined,    &
         &                                     undef_int,    &
         &                                     fr_land_topo,&
         &                                     hh_topo,     &
         &                                     stdh_topo,   &
         &                                     z0_topo,      &
         &                                     nhori=nhori)
      ENDIF
     ENDIF


   END SELECT


   PRINT *,'Read in aot data'

   CALL read_netcdf_buffer_aot(aot_buffer_file,    &
   &                                     tg,       &
   &                                     ntype_aot,&
   &                                     ntime_aot,&
   &                                     aot_tg)


   PRINT *,'Read in cru data'
   SELECT CASE(it_cl_type)
   CASE(i_t_cru_fine)
     CALL read_netcdf_buffer_cru(t_clim_buffer_file,&
    &                                     tg,       &
    &                                     crutemp,  &
    &                                     cruelev)
   CASE(i_t_cru_coarse)
     CALL read_netcdf_buffer_cru(t_clim_buffer_file, &
    &                                     tg,        &
    &                                     crutemp)
   END SELECT
 
   CALL read_netcdf_buffer_cru(t_clim_buffer_file, &
    &                                     tg,      &
    &                                     crutemp)

    PRINT *,'Read in FLAKE'
  CALL read_netcdf_buffer_flake(flake_buffer_file,   &
    &                                     tg,        &
    &                                     undefined, &
    &                                     undef_int, &
    &                                     lake_depth,&
    &                                     fr_lake,   &
    &                                     flake_tot_npixel)


    IF (i_lsm_data == 2 .and. igrid_type == igrid_cosmo) THEN
    PRINT *,'Read in Land-Sea-Mask from file  ',land_sea_mask_file
    CALL read_netcdf_buffer_lsm(land_sea_mask_file,  &
   &                           tg, &
   &                           fr_land_mask)
    PRINT *,'Land-Sea-Mask file  ',land_sea_mask_file," is used for consistency tests."
    ELSE
    PRINT *,'External  Land-Sea-Mask is NOT used for consistency tests.'
    PRINT *,'External Land-Sea-Mask is only tested for the COSMO grid.'
    END IF



!------------------------------------------------------------------------------------------
!------------- land use data --------------------------------------------------------------
!------------------------------------------------------------------------------------------
      PRINT *,'determine land-sea mask'
      CALL CPU_TIME(timestart)

       IF (l_use_glcc) THEN
        WHERE (lat_geo <=lu_data_southern_boundary) ! glc2000 and globcover 2009 exclude Antarctica
          fr_land_lu = fr_land_glcc
        ENDWHERE
      ENDIF

      !set land-sea mask, spread at 0.5 due to poor accuracy of values in GRIB files
       WHERE (fr_land_lu < 0.5_wp) 
        fr_land_lu = MIN(0.49_wp,fr_land_lu)
      ELSEWHERE !   fr_land_glc2000 >= 0.5
        fr_land_lu = MAX(0.51_wp,fr_land_lu)
      ENDWHERE

       IF (i_lsm_data == 2) THEN
          fr_land_lu = fr_land_mask
      ENDIF

      CALL CPU_TIME(timeend)
      timediff = timeend - timestart
      PRINT *,'determine land-sea mask, WHERE, done in: ', timediff

      ! total roughness length
       z0_tot = z0_lu + z0_topo
       IF (l_use_glcc) THEN
        WHERE (lat_geo <=lu_data_southern_boundary) ! glc2000 and globcover 2009 exclude Antarctica
          ice_lu            = ice_glcc
          z0_lu             = z0_glcc 
          z0_tot            = z0_glcc + z0_topo
          root_lu           = root_glcc
          plcov_mn_lu       = plcov_mn_glcc
          plcov_mx_lu       = plcov_mx_glcc
          lai_mn_lu         = lai_mn_glcc
          lai_mx_lu         = lai_mx_glcc
          rs_min_lu         = rs_min_glcc
          urban_lu          = urban_glcc
          for_d_lu          = for_d_glcc
          for_e_lu          = for_e_glcc
          emissivity_lu     = emissivity_glcc
        ENDWHERE
      ENDIF

!------------------------------------------------------------------------------------------
!------------- land use data consistency  -------------------------------------------------
!------------------------------------------------------------------------------------------
      WHERE (fr_land_lu < 0.5)  ! set vegetation to undefined (0.) for water grid elements
        ! z0 and emissivity are not set to undefined_lu
        ice_lu            = undefined_lu
        root_lu           = undefined_lu
        plcov_mn_lu       = undefined_lu
        plcov_mx_lu       = undefined_lu
        lai_mn_lu         = undefined_lu
        lai_mx_lu         = undefined_lu
        rs_min_lu         = undefined_lu
        urban_lu          = undefined_lu
        for_d_lu          = undefined_lu
        for_e_lu          = undefined_lu
      ENDWHERE

!------------------------------------------------------------------------------------------
!------------- soil data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

      !undef_soiltype   = 0 ! \TODO read undef_soiltype from netcdf file (_Fill_Value)
      !default_soiltype = 5 ! default soil type loam
      !soiltype_ice     = 1   !< soiltype for ice
      !soiltype_water   = 9   !< soiltype for water
      PRINT *,'Soil data consistency check'

      CALL define_soiltype(isoil_data, ldeep_soil, &
                           undef_soiltype,         &
                           default_soiltype,       &
                           soiltype_ice,           &
                           soiltype_water,         &
                           soil_data)

      SELECT CASE (isoil_data)
      CASE(FAO_data, HWSD_map)

        WHERE (fr_land_lu < 0.5)  ! set water soiltype for water grid elements
          soiltype_fao = soiltype_water
        ELSEWHERE ! fr_land_lu >= 0.5, i.e. a land grid element
          ! (soiltyp(:,:) > 8 .OR. soiltyp(:,:) < 1))
          WHERE ((soiltype_fao == undef_soiltype).OR.(soiltype_fao > 8) ) ! land grid elements must have a valid soiltype
            !  WHERE ( (lat_geo < -60.).OR.(lat_geo > 65.) ) ! Arctic and Antarctica
            !     soiltype_fao = soiltype_ice  ! set soil type to ice for Arctic or Antarctic undefined points
            WHERE ( (lat_geo < -60.) ) ! Antarctica
              soiltype_fao = soiltype_ice  ! set soil type to ice for Antarctic undefined points

            ELSEWHERE  ! rest of the World 
              soiltype_fao = default_soiltype ! set default soiltype to loam
            ENDWHERE
          ENDWHERE
        ENDWHERE

      CALL CPU_TIME(timeend)
      timediff = timeend - timestart
      PRINT *,'soil data consitency check, WHERE, done in: ', timediff

      db_ice_counter = 0

      IF (i_landuse_data == i_lu_globcover) THEN
        DO k=1,tg%ke
        DO j=1,tg%je
        DO i=1,tg%ie
          IF  ( (soiltype_fao(i,j,k) /= soiltype_ice) .AND.  & 
               &    (fr_land_lu(i,j,k) > 0.5).AND. (ice_lu(i,j,k) > 0.5 )) THEN
            soiltype_fao(i,j,k) = soiltype_ice 
            db_ice_counter = db_ice_counter +1
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        !HA debug
        PRINT *,'number of grid elements set to ice soiltype: ', db_ice_counter
        PRINT *,'Soiltype range MIN/MAX: ', MINVAL(soiltype_fao),MAXVAL(soiltype_fao)

      ELSE   ! iI_lu_glc2000 or i_lu_glcc     

        DO k=1,tg%ke
        DO j=1,tg%je
        DO i=1,tg%ie
          IF  ( (soiltype_fao(i,j,k) /= soiltype_ice) .AND.  & 
               &    (fr_land_lu(i,j,k) > 0.5).AND. (ice_lu(i,j,k) ==  fr_land_lu(i,j,k)) ) THEN
            soiltype_fao(i,j,k) = soiltype_ice 
            db_ice_counter = db_ice_counter +1
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        !HA debug
        PRINT *,'number of grid elements set to ice soiltype: ', db_ice_counter
      ENDIF

    CASE(HWSD_data)

      IF (ldeep_soil) THEN

        CALL calculate_soiltype(tg,            &
     &                          ldeep_soil,     &
     &                          soiltype_fao,  &
     &                          fr_sand,       &
     &                          fr_silt,       &
     &                          fr_clay,       &
     &                          fr_oc,         &
     &                          fr_bd,         &
     &                          fr_dm,         &
     &                          fr_sand_deep,  &
     &                          fr_silt_deep,  &
     &                          fr_clay_deep,  &
     &                          fr_oc_deep,    &
     &                          fr_bd_deep,    &
     &                          fr_dm_deep,    &
     &                          soiltype_deep = soiltype_deep)
      ELSE
        CALL calculate_soiltype(tg,            &
     &                          ldeep_soil,     &
     &                          soiltype_fao,  &
     &                          fr_sand,       &
     &                          fr_silt,       &
     &                          fr_clay,       &
     &                          fr_oc,         &
     &                          fr_bd,         &
     &                          fr_dm)
      ENDIF

      soiltype_water = 9

      WHERE (fr_land_lu < 0.5)  ! set water soiltype for water grid elements
        soiltype_fao = soiltype_water
      ENDWHERE
      IF (ldeep_soil) THEN
        WHERE (fr_land_lu < 0.5)  ! set water soiltype for water grid elements 
          soiltype_deep = soiltype_water
        ENDWHERE
      ENDIF
      

    END SELECT

      !------------------------------------------------------------------------------------------

    SELECT CASE (isoil_data)
    CASE(FAO_data, HWSD_map)
      WHERE (soiltype_fao == soiltype_ice)  ! set vegetation to undefined (0.) for ice grid elements (e.g. glaciers)
        ! z0, rs_min and emissivity are not set to undefined_lu
        root_lu           = undefined_lu
        plcov_mn_lu       = undefined_lu
        plcov_mx_lu       = undefined_lu
        lai_mn_lu         = undefined_lu
        lai_mx_lu         = undefined_lu
        urban_lu          = undefined_lu
        for_d_lu          = undefined_lu
        for_e_lu          = undefined_lu
      ENDWHERE
    END SELECT
!------------------------------------------------------------------------------------------
!------------- soil data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
!------------- flake data consistency  ----------------------------------------------------
!------------------------------------------------------------------------------------------

      PRINT *,'flake data consistency check'
      CALL CPU_TIME(timestart)

     ! determine "fraction ocean" first before considering "fraction lake"
     ! fr_ocean should be determined by ocean model if available
     ! so use (1. - lsm_ocean_model) as mask instead of fr_land_topo from the orography data
     thr_cr = 0.99
     WHERE (fr_land_topo < thr_cr)
       fr_ocean_lu = 1. - fr_land_lu
       fr_lake = 0.0
     ELSEWHERE
       fr_ocean_lu = 0.0
       fr_lake = 1. - fr_land_lu
     ENDWHERE

     ! set Death Sea to "ocean water"
     WHERE ((hh_topo < -390.).AND. &
      &     (lon_geo > 35.).AND.(lon_geo < 36.).AND. &
      &     (lat_geo > 31.).AND.(lat_geo < 32.) )
       fr_ocean_lu = 1. - fr_land_lu
       fr_lake = 0.0
     ENDWHERE
     
     ! set Caspian Sea to "ocean water"
     WHERE ((hh_topo < 33.).AND. &
      &     (lon_geo > 46.).AND.(lon_geo < 55.).AND. &
      &     (lat_geo > 36.).AND.(lat_geo < 48.) )
       fr_ocean_lu = 1. - fr_land_lu
       fr_lake = 0.0
     ENDWHERE
     ! here fr_ocean_lu + fr_lake +fr_land_lu = 1
     ! fr_ocean_lu + fr_lake = fr_water
     ! fr_water + fr_land_lu = 1

     ! check consistency for "lake depth"
     WHERE (fr_land_lu >= 0.5)  
       lake_depth = flake_depth_undef ! set lake depth to flake_depth_undef (-1 m)
     ENDWHERE 
     WHERE (fr_ocean_lu >= 0.5)  
       lake_depth = flake_depth_undef ! set lake depth to flake_depth_undef (-1 m)
     ENDWHERE 

     WHERE ((fr_lake > 0.5).AND.(lake_depth < 0.0))
       lake_depth = flake_depth_default ! set lake depth to default value (10 m)
     ENDWHERE ! 
     ! restrict lake depth to maximum value (50 m)
     WHERE (lake_depth > DWD_max_lake_depth)
      lake_depth = DWD_max_lake_depth
     END WHERE

     ! restrict lake depth to minimal value (1 m)
     WHERE ( (lake_depth > 0.0).AND.(lake_depth < DWD_min_lake_depth ))
      lake_depth = DWD_min_lake_depth
     END WHERE

     ! ha
     ! at the coastline some target grid elements are marked as "lake" due to
     ! inconcistencies between the orography data (fr_land_topo) and landuse
     ! data (fr_land_lu).
     ! check neighbour target grid element for fr_ocean_lu

     DO nloops=1,3
     DO k=1,tg%ke
     DO j=1,tg%je
     DO i=1,tg%ie

       IF (fr_lake(i,j,k)>0.05) THEN ! concistency check for neighbour ocean elements

         SELECT CASE(igrid_type) ! get indices for neighbour grid elements

           CASE(igrid_icon) ! ICON GRID
             ! get neighbour grid indices for ICON grid
             ne_je(:) = 1
             ne_ke(:) = 1
             ne_ie(:) = 0
             nnb=icon_grid%nvertex_per_cell ! number of neighbours in ICON grid
             DO nv=1, nnb
               n_index = icon_grid_region%cells%neighbor_index(i,nv) ! get cell id of neighbour cells
               IF (n_index > 0) THEN
                 ne_ie(nv) = n_index
               ENDIF
             ENDDO  

           CASE(igrid_cosmo) ! COSMO grid
             nnb = 8
             ! northern neighbour
             ne_ie(1) = i
             ne_je(1) = MAX(1,j-1)
             ne_ke(1) = k
             ! north-eastern neighbour
             ne_ie(2) = MIN(tg%ie,i+1)
             ne_je(2) = MAX(1,j-1)
             ne_ke(2) = k
             ! eastern neighbour
             ne_ie(3) = MIN(tg%ie,i+1)
             ne_je(3) = j
             ne_ke(3) = k
             ! south-eastern neighbour
             ne_ie(4) = MIN(tg%ie,i+1)
             ne_je(4) = MIN(tg%je,j+1)
             ne_ke(4) = k
             ! southern neighbour
             ne_ie(5) = i
             ne_je(5) = MIN(tg%je,j+1)
             ne_ke(5) = k
             ! south-west neighbour
             ne_ie(6) = MAX(1,i-1)
             ne_je(6) = MIN(tg%je,j+1)
             ne_ke(6) = k
             ! western neighbour
             ne_ie(7) = MAX(1,i-1)
             ne_je(7) = j
             ne_ke(7) = k
             ! north-west neighbour
             ne_ie(8) = MAX(1,i-1)
             ne_je(8) = MAX(1,j-1)
             ne_ke(8) = k

           CASE(igrid_gme) ! GME grid
             ni = gme_grid%ni
             CALL spoke(i,j,k,ni,nnb,ks1,ks2,ksd)
             ne_ie(1:nnb)=ks1(1:nnb)
             ne_je(1:nnb)=ks2(1:nnb)
             ne_ke(1:nnb)=ksd(1:nnb)

         END SELECT

         DO n=1,nnb
           IF ((ne_ie(n)>= 1).AND.(ne_je(n)>=1).AND.(ne_ke(n)>=1)) THEN
           IF (fr_ocean_lu(ne_ie(n),ne_je(n),ne_ke(n))>0.5) THEN ! if the direct neighbour element is ocean,
             fr_lake(i,j,k) = 0.0                                ! set this grid element also to ocean.
             fr_ocean_lu(i,j,k) = 1.0 - fr_land_lu(i,j,k)
             lake_depth(i,j,k) = flake_depth_undef ! set lake depth to flake_depth_undef (-1 m)
           ENDIF  
           ENDIF
         ENDDO

       ENDIF ! check for ocean

     ENDDO
     ENDDO
     ENDDO
     ENDDO

     ! manual correction for lake depth of Lake Constance
     ! the raw database of the lake depth data appears not to be correct
     ! the main part of Lake Constance has a mean depth of 98 m, so set to DWD_max_lake_depth
     WHERE ((lon_geo > 8.8).AND.(lon_geo < 9.9).AND. &
      &     (lat_geo > 47.4).AND.(lat_geo < 48.4).AND. &
      &     (lake_depth > 9.7).AND.(lake_depth < 9.9) )
       lake_depth = DWD_max_lake_depth
     ENDWHERE
     
      
      CALL CPU_TIME(timeend)
      timediff = timeend - timestart
      PRINT *,'flake data consitency check, WHERE, done in: ', timediff

!------------------------------------------------------------------------------------------
!------------- Albedo data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------
      ! set default Albedo values for land grid elements with so far undefined or unexpected values
    IF (ialb_type /= 2) THEN

     PRINT *,'Albedo data consistency check'

     CALL CPU_TIME(timestart)


     nlon_reg = alb_raw_data_grid%nlon_reg
     nlat_reg = alb_raw_data_grid%nlat_reg

     path_alb_file = TRIM(raw_data_alb_path)//TRIM(raw_data_alb_filename)
     print *, TRIM(path_alb_file)

     CALL open_netcdf_ALB_data(path_alb_file, &
                               ncid_alb)

     CALL allocate_alb_interp_fields(mpy)
     
     CALL alb_interp_data()

     DO i=1,9
      step = 0.1667*(wso_max(i) - wso_min(i))
      zalso(i,7) = csalb(i) - wso_min(i)*csalbw(i)
      zalso(i,1) = csalb(i) - wso_max(i)*csalbw(i)
      DO t=1,5
         zalso(i,7-t) = csalb(i) - csalbw(i)*step*t
         zalso(i,7+t) = csalb(i) - csalbw(i)*step*t
      ENDDO
     ENDDO

     albvis_min=0.07
     CALL const_check_interpol_alb(alb_field_mom,fr_land_lu,albvis_min)

     albnir_min=0.10
     CALL const_check_interpol_alb(alnid_field_mom,fr_land_lu,albnir_min)

     albuv_min=0.02
     CALL const_check_interpol_alb(aluvd_field_mom,fr_land_lu,albuv_min)


     CALL CPU_TIME(timeend)
     timediff = timeend - timestart
     PRINT *,'albedo data consistency check, WHERE, done in: ', timediff    
    ENDIF

!------------------------------------------------------------------------------------------
!------------- soil albedo consistency check ----------------------------------------------
!------------------------------------------------------------------------------------------
    IF (ialb_type == 2) THEN

      ! set default soil albedo values for land grid elements with so far undefined values
      PRINT *,'soil albedo data consistency check'

      CALL CPU_TIME(timestart)

      WHERE (fr_land_lu < 0.5) ! set undefined albedo value (0.0) for water grid elements
        alb_dry = undef_alb_bs
        alb_sat = undef_alb_bs
      ELSEWHERE ! fr_land_lu >= 0.5
        WHERE (alb_dry < minimal_alb_dry) ! small albedo values at land grid elements
          alb_dry = minimal_alb_dry
        ENDWHERE
        WHERE (alb_dry > maximal_alb_dry) ! large albedo values at land grid elements
          alb_dry = maximal_alb_dry
        ENDWHERE
        WHERE (alb_sat < minimal_alb_sat) ! small albedo values at land grid elements
          alb_sat = minimal_alb_sat
        ENDWHERE
        WHERE (alb_sat > maximal_alb_sat) ! large albedo values at land grid elements
          alb_sat = maximal_alb_sat
        ENDWHERE
      ENDWHERE


     CALL CPU_TIME(timeend)
     timediff = timeend - timestart
     PRINT *,'albedo data consistency check, WHERE, done in:  ', timediff

    ENDIF

!------------------------------------------------------------------------------------------
!------------- NDVI data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

      ! set default NDVI values for land grid elements with so far undefined values or very small NDVI values
      PRINT *,'NDVI data consistency check'
      
      CALL CPU_TIME(timestart)
      !minimal_ndvi = 0.09 ! bare soil value
      !undef_ndvi   = 0.0  ! no vegetation

      WHERE (fr_land_lu < 0.5) ! set undefined NDVI value (0.0) for water grid elements
        ndvi_max = undef_ndvi
      ELSEWHERE ! fr_land_lu >= 0.5
        WHERE (ndvi_max <= minimal_ndvi) ! small NDVI values at land grid elements
          ndvi_max = minimal_ndvi
        ENDWHERE
      ENDWHERE
        
      FORALL (t=1:mpy) ! mpy = month per year = 12
        WHERE (fr_land_lu < 0.5) ! set undefined NDVI value (0.0) for water grid elements
          ndvi_field_mom(:,:,:,t) = undef_ndvi
          ndvi_ratio_mom(:,:,:,t) = undef_ndvi
         ELSEWHERE ! fr_land_lu >= 0.5
           WHERE (ndvi_max(:,:,:) <= minimal_ndvi) ! small NDVI values at land grid elements
              ndvi_field_mom(:,:,:,t) = minimal_ndvi
              ndvi_ratio_mom(:,:,:,t) = 1.0  ! minimal_ndvi / minimal_ndvi ! ndvi_max set to minimal_ndvi
            ENDWHERE 
            WHERE (ndvi_field_mom(:,:,:,t) <= minimal_ndvi) ! small NDVI values at land grid elements
              ndvi_field_mom(:,:,:,t) = minimal_ndvi
              ndvi_ratio_mom(:,:,:,t) = minimal_ndvi / ndvi_max(:,:,:)
            ENDWHERE
        ENDWHERE
      END FORALL
       
     CALL CPU_TIME(timeend)
     timediff = timeend - timestart
     PRINT *,'NDVI data consitency check, WHERE, done in:  ', timediff


!------------------------------------------------------------------------------------------
!------------- NDVI data consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
!-------------TC_L Correction ------------------------------------------------------
!------------------------------------------------------------------------------------------
  !gs 
  SELECT CASE(it_cl_type)
  CASE(i_t_cru_fine)

    PRINT*,'T_CL Correction' 
    crutemp2 = crutemp
    DO j=1,tg%je
      DO i=1,tg%ie
        last = .FALSE.
        IF ( fr_land_lu(i,j,1) < 0.5) THEN
          crutemp(i,j,1)  = -1.E20_wp
        ELSE
          IF ( crutemp(i,j,1) > 0.0 ) THEN
            foundtcl = .TRUE.
            !   PRINT*, 'CRUTEMP', i,j, crutemp(i,j,1)
            !   PRINT*, 'ELEV DOMAIN', hh_topo(i,j,1)
            !   PRINT*, 'ELEV CRU', cruelev(i,j,1)
            
            crutemp(i,j,1) = crutemp(i,j,1) + 0.65 * 0.01*( cruelev(i,j,1) - hh_topo(i,j,1) )   
            !    PRINT*, 'CRUTEMP', crutemp(i,j,1)
            
          ELSE
            ! PRINT*, 'TCL NOT DEFINED ',tg%ie, tg%je, i, j
            ! 3x3 search      
            foundtcl = .FALSE.
            DO jj=-1,2
              DO ii=-1,2
                IF (j+jj > 0 .and.  j+jj < tg%je .and. i+ii > 0 .and. i+ii < tg%ie) THEN
                  IF ( crutemp2(i+ii,j+jj,1) > 0.0 ) THEN
                    ! PRINT*, 'FOUND TCL ', i, j, ii, jj, crutemp2(i+ii,j+jj,1)                    
                    crutemp(i,j,1) = crutemp2(i+ii,j+jj,1) + 0.65 * 0.01*( cruelev(i+ii,j+jj,1) - hh_topo(i,j,1) )
                    foundtcl = .TRUE.
                    last = .TRUE.
                    exit       
                  ENDIF
                ENDIF   ! inside domain
              ENDDO
              IF  (last) THEN
                exit
              ENDIF
            ENDDO
            ! if still missing go along longitude
            IF (.NOT. foundtcl) THEN
              tclsum = 0._wp
              elesum = 0._wp
              ntclct = 0
              l = 1
              DO WHILE (.NOT. foundtcl .AND. l .le. (tg%ie / 6))
                iml=MAX(1,i-3*l)
                imu=i+2-3*l
                ipl=i+3*l-2
                ipu=MIN(tg%ie,i+3*l)
                jml=MAX(1,j-l)
                jmu=j-l
                jpl=j+l
                jpu=MIN(tg%je,j+l)
                IF (jml == jmu) THEN
                  DO ii=iml,ipu
                    IF ( crutemp2(ii,jml,1) > 0.0 ) THEN
                      tclsum = tclsum + crutemp2(ii,jml,1)
                      elesum = elesum + cruelev(ii,jml,1)
                      ntclct = ntclct + 1
                    ENDIF
                  ENDDO
                ELSE
                  jml = jml - 1
                ENDIF
                IF (jpl == jpu) THEN
                  DO ii=iml,ipu
                    IF ( crutemp2(ii,jpu,1) > 0.0 ) THEN
                      tclsum = tclsum + crutemp2(ii,jpu,1)
                      elesum = elesum + cruelev(ii,jpu,1)
                      ntclct = ntclct + 1
                    ENDIF
                  ENDDO
                ELSE
                  jpu = jpu + 1
                ENDIF
                IF (iml .LE. imu) THEN
                  DO jj = jml+1,jpu-1
                    DO ii = iml,imu
                      IF ( crutemp2(ii,jj,1) > 0.0 ) THEN
                        tclsum = tclsum + crutemp2(ii,jj,1)
                        elesum = elesum + cruelev(ii,jj,1)
                        ntclct = ntclct + 1
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
                IF (ipl .LE. ipu) THEN
                  DO jj = jml+1,jpu-1
                    DO ii = ipl,ipu
                      IF ( crutemp2(ii,jj,1) > 0.0 ) THEN
                        tclsum = tclsum + crutemp2(ii,jj,1)
                        elesum = elesum + cruelev(ii,jj,1)
                        ntclct = ntclct + 1
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
                IF (ntclct > 0) THEN
                  crutemp(i,j,1) = tclsum/REAL(ntclct,wp) + 0.0065 * (elesum/REAL(ntclct,wp) - hh_topo(i,j,1))
                  foundtcl = .TRUE.
                ELSE
                  l = l + 1
                ENDIF
              ENDDO  ! while 
            ENDIF    ! .not. foundtcl

            IF ( .NOT. foundtcl) THEN
              
              PRINT*, 'ERROR NO TEMPERATURE DATA FOR T_CL CORRECTION  AT'   
              PRINT *,i,j
              crutemp(i,j,1) = 288.15 - 0.0065 * hh_topo(i,j,1)
              
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDDO
  END SELECT
!------------------------------------------------------------------------------------------
!------------- Special Points        ------------------------------------------------------

     SELECT CASE(igrid_type)  
           CASE(igrid_cosmo) ! COSMO grid

            

     do isp=1,number_special_points
        IF (number_special_points<1) THEN
           write(*,*) 'No treatment of special points: Number of special points is ',number_special_points
           EXIT
        END IF

!------------------------------------------------------------------------------------------
! CAUTION! Tested only for COSMO!!!

  !--------------------------------------------------------------------------------------------------------
  ! get file
  
        write(namelist_file,'(A9,I1)') 'INPUT_SP_',isp

  !---------------------------------------------------------------------------

       CALL read_namelists_extpar_special_points(namelist_file, &
                                                lon_geo_sp,           &
                                                lat_geo_sp,           &
                                                soiltype_sp,          &
                                                z0_sp,                &
                                                rootdp_sp,            &
                                                plcovmn_sp,           &
                                                plcovmx_sp,           &
                                                laimn_sp,             &
                                                laimx_sp)

! Consider only well defined variables, default is -999.!
       IF ((lon_geo_sp < -360._wp ).OR.(lat_geo_sp < -90._wp)) THEN
          PRINT*,"CAUTION! Special points defined but not in target domain!"
       ELSE
         start_cell_id = 1


         CALL  find_nearest_target_grid_element(lon_geo_sp, &
                                              & lat_geo_sp, &
                                              & tg,            &
                                              & start_cell_id, &
                                              & i_sp,      &
                                              & j_sp,      &
                                              & k_sp)

         
write(*,'(A26,A10,A4,2X,I1)') "Consider special point in ",namelist_file," of ",number_special_points
write(*,'(A33,1X,2(F6.3,2X))') "Consider special point: lon,lat  ",lon_geo_sp,lat_geo_sp
write(*,'(A33,1X,2(I5,2X))') "Consider special point:  ie,je   ",i_sp,j_sp
write(*,'(A23,i4,2X,I4,A19,F6.4,2X,A4,F6.4)')"Consider special point: ",&
i_sp,j_sp," z0_tot old ",z0_tot (i_sp,j_sp,k_sp),"new ",z0_sp
write(*,'(A23,i4,2X,I4,A19,F6.4,2X,A4,F6.4)')"Consider special point: ",&
i_sp,j_sp," root_lu old ",root_lu(i_sp,j_sp,k_sp),"new ",rootdp_sp
write(*,'(A23,i4,2X,I4,A19,I5,2X,A4,I5)')"Consider special point: ",&
i_sp,j_sp," soiltype_fao old  ",soiltype_fao(i_sp,j_sp,k_sp),"new ",NINT(soiltype_sp)
write(*,'(A23,i4,2X,I4,A19,F6.4,2X,A4,F6.4)')"Consider special point: ",&
i_sp,j_sp," plcov_mn_lu  old  ",plcov_mn_lu (i_sp,j_sp,k_sp),"new ",plcovmn_sp
write(*,'(A23,i4,2X,I4,A19,F6.4,2X,A4,F6.4)')"Consider special point: ",&
i_sp,j_sp," plcov_mx_lu  old  ",plcov_mx_lu (i_sp,j_sp,k_sp),"new ",plcovmx_sp
write(*,'(A23,i4,2X,I4,A19,F6.4,2X,A4,F6.4)')"Consider special point: ",&
i_sp,j_sp," lai_mn_lu    old  ",lai_mn_lu (i_sp,j_sp,k_sp),"new ",laimn_sp
write(*,'(A23,i4,2X,I4,A19,F6.4,2X,A4,F6.4)')"Consider special point: ",&
i_sp,j_sp," lai_mx_lu    old  ",lai_mx_lu (i_sp,j_sp,k_sp),"new ",laimx_sp

          IF ((i_sp == 0).OR.(j_sp == 0)) PRINT*,"CAUTION! Special points out of range of target domain!"


! Consider only well defined variables, default is -999.!
             IF(z0_sp > 0._wp)          z0_tot (i_sp,j_sp,k_sp)      = z0_sp
             IF(rootdp_sp>0._wp)        root_lu(i_sp,j_sp,k_sp)      = rootdp_sp
             IF(soiltype_sp>0._wp )     soiltype_fao(i_sp,j_sp,k_sp) = NINT(soiltype_sp)
             IF(plcovmn_sp>0._wp)       plcov_mn_lu (i_sp,j_sp,k_sp) = plcovmn_sp
             IF(plcovmx_sp>0._wp)       plcov_mx_lu (i_sp,j_sp,k_sp) = plcovmx_sp
             IF(laimn_sp>0._wp)         lai_mn_lu (i_sp,j_sp,k_sp)   = laimn_sp
             IF(laimx_sp>0._wp)         lai_mx_lu (i_sp,j_sp,k_sp)   = laimx_sp

! Optional parameters for special point - not considered yet
    !                                     rs_min_lu, &
    !                                     urban_lu,  &
    !                                     for_d_lu,  &
    !                                     for_e_lu, &
    !                                     emissivity_lu, &
    !                                     soiltype_fao, &
    !                                     ndvi_max,  &
    !                                     ndvi_field_mom,&
    !                                     ndvi_ratio_mom, &
    !                                     hh_globe,            &
                     END IF

                  END DO ! Special Points  loop

           END SELECT
!------------------------------------------------------------------------------------------
!------------- TOPO Data Consistency ------------------------------------------------------
!------------------------------------------------------------------------------------------

           IF (lradtopo) THEN
             
             WHERE (skyview_topo .lt. 1.0 .AND. fr_land_lu .lt. 0.5)
               skyview_topo = 1.0
             ENDWHERE 
             WHERE (skyview_topo .gt. 1.0)
               skyview_topo = 1.0
             ENDWHERE
           END IF

!------------------------------------------------------------------------------------------
!------------- data output ----------------------------------------------------------------
!------------------------------------------------------------------------------------------
fill_value_real = -1.E20_wp
fill_value_int = -999

SELECT CASE(igrid_type)
  CASE(igrid_icon) ! ICON GRID
        
       PRINT *,'write out ', TRIM(netcdf_output_filename)

         CALL write_netcdf_icon_grid_extpar(TRIM(netcdf_output_filename),&
    &                                     icon_grid,                     &
    &                                     tg,                            &
    &                                     isoil_data,                    &
    &                                     ldeep_soil,                    &
    &                                     itopo_type,                    &
    &                                     lsso_param,                    &
    &                                     fill_value_real,               &
    &                                     fill_value_int,                &
    &                                     TRIM(name_lookup_table_lu),    &
    &                                     TRIM(lu_dataset),              &
    &                                     nclass_lu,                     &
    &                                     lon_geo,                       &
    &                                     lat_geo,                       & 
    &                                     fr_land_lu,                    &
    &                                     lu_class_fraction,             &
    &                                     ice_lu,                        &
    &                                     z0_tot,                        &
    &                                     root_lu,                       &
    &                                     plcov_mx_lu,                   &
    &                                     lai_mx_lu,                     &
    &                                     rs_min_lu,                     &
    &                                     urban_lu,                      &
    &                                     for_d_lu,                      &
    &                                     for_e_lu,                      &
    &                                     emissivity_lu,                 &
    &                                     lake_depth,                    &
    &                                     fr_lake,                       &
    &                                     soiltype_fao,                  &
    &                                     ndvi_max,                      &
    &                                     ndvi_field_mom,                &
    &                                     ndvi_ratio_mom,                &
    &                                     hh_topo,                       &
    &                                     stdh_topo,                     &
    &                                     vertex_param,                  &
    &                                     aot_tg,                        &
    &                                     crutemp,                       &
    &                                     alb_field_mom,                 &
    &                                     alnid_field_mom,               &
    &                                     aluvd_field_mom,               &
    &                                     fr_sand = fr_sand,             &
    &                                     fr_silt = fr_silt,             &
    &                                     fr_clay = fr_clay,             &
    &                                     fr_oc = fr_oc,                 &
    &                                     fr_bd = fr_bd,                 &
    &                                     fr_dm = fr_dm,                 &
    &                                     soiltype_deep = soiltype_deep, &
    &                                     fr_sand_deep = fr_sand_deep,   &
    &                                     fr_silt_deep = fr_silt_deep,   &
    &                                     fr_clay_deep = fr_clay_deep,   &
    &                                     fr_oc_deep = fr_oc_deep,       &
    &                                     fr_bd_deep = fr_bd_deep,       &
    &                                     fr_dm_deep = fr_dm_deep,       &
    &                                     theta_topo=theta_topo,         &
    &                                     aniso_topo=theta_topo,         &
    &                                     slope_topo=theta_topo)



  CASE(igrid_cosmo) ! COSMO grid


      PRINT *,'write out ', TRIM(netcdf_output_filename)
 
      CALL  write_netcdf_cosmo_grid_extpar(TRIM(netcdf_output_filename),&
     &                                     cosmo_grid,                  &
     &                                     tg,                          &
     &                                     isoil_data,                  &
     &                                     ldeep_soil,                  &
     &                                     itopo_type,                  &
     &                                     lsso_param,                  & 
     &                                     lradtopo,                    &  
     &                                     nhori,                       &
     &                                     fill_value_real,             &
     &                                     fill_value_int,              &
     &                                     TRIM(name_lookup_table_lu),  &
     &                                     TRIM(lu_dataset),            &
     &                                     i_landuse_data,              &
     &                                     nclass_lu,                   &
     &                                     lon_geo,                     &
     &                                     lat_geo,                     &
     &                                     fr_land_lu,                  &
     &                                     lu_class_fraction,           &
     &                                     ice_lu,                      &
     &                                     z0_tot,                      &
     &                                     z0_lu,                       &
     &                                     z0_topo,                     &
     &                                     z012_lu,                     &
     &                                     root_lu,                     &
     &                                     plcov_mn_lu,                 &
     &                                     plcov_mx_lu,                 &
     &                                     plcov12_lu,                  &
     &                                     lai_mn_lu,                   &
     &                                     lai_mx_lu,                   &
     &                                     lai12_lu,                    &
     &                                     rs_min_lu,                   &
     &                                     urban_lu,                    &
     &                                     for_d_lu,                    &
     &                                     for_e_lu,                    &
     &                                     emissivity_lu,               &
     &                                     lake_depth,                  &
     &                                     fr_lake,                     &
     &                                     soiltype_fao,                &
     &                                     ndvi_max,                    &
     &                                     ndvi_field_mom,              &
     &                                     ndvi_ratio_mom,              &
     &                                     hh_topo,                     &
     &                                     stdh_topo,                   &
     &                                     aot_tg,                      &
     &                                     crutemp,                     &
     &                                     alb_field_mom,               &
     &                                     alnid_field_mom,             &
     &                                     aluvd_field_mom,             &
     &                                     alb_dry = alb_dry,           &
     &                                     alb_sat = alb_sat,           &
     &                                     fr_sand = fr_sand,           &
     &                                     fr_silt = fr_silt,           &
     &                                     fr_clay = fr_clay,           &
     &                                     fr_oc = fr_oc,               &
     &                                     fr_bd = fr_bd,               &
     &                                     fr_dm = fr_dm,               &
     &                                     soiltype_deep=soiltype_deep, &
     &                                     fr_sand_deep=fr_sand_deep,   &
     &                                     fr_silt_deep=fr_silt_deep,   &
     &                                     fr_clay_deep=fr_clay_deep,   &
     &                                     fr_oc_deep=fr_oc_deep,       &
     &                                     fr_bd_deep=fr_bd_deep,       &
     &                                     fr_dm_deep=fr_dm_deep,       &
     &                                     theta_topo=theta_topo,       &
     &                                     aniso_topo=aniso_topo,       &
     &                                     slope_topo=slope_topo,       &
     &                                     slope_asp_topo=slope_asp_topo,&
     &                                     slope_ang_topo=slope_ang_topo,&
     &                                     horizon_topo=horizon_topo,    &
     &                                     skyview_topo=skyview_topo)

#ifdef GRIBAPI
     PRINT *,'write out ', TRIM(grib_output_filename)
     
     WHERE (crutemp < -1E19) crutemp = 9999

     CALL  write_cosmo_grid_extpar_grib(TRIM(grib_output_filename),    &
      &                                   TRIM(grib_sample),           &
      &                                     cosmo_grid,                &
      &                                     tg,                        &
      &                                     isoil_data,                &
      &                                     ldeep_soil,                &
      &                                     itopo_type,                &
      &                                     lsso_param,                &
      &                                     lradtopo,                  &
      &                                     nhori,                     &
      &                                     fill_value_real,           &
      &                                     fill_value_int,            &
      &                                     lon_geo,                   &
      &                                     lat_geo,                   &
      &                                     fr_land_lu,                & !&                                     ice_lu, &
      &                                     z0_tot,                    &
      &                                     root_lu,                   &
      &                                     plcov_mn_lu,               &
      &                                     plcov_mx_lu,               &
      &                                     lai_mn_lu,                 &
      &                                     lai_mx_lu,                 &
      &                                     rs_min_lu,                 &
      &                                     urban_lu,                  &
      &                                     for_d_lu,                  &
      &                                     for_e_lu,                  &
      &                                     emissivity_lu,             &
      &                                     lake_depth,                &
      &                                     fr_lake,                   &
      &                                     soiltype_fao,              &
      &                                     ndvi_max,                  &
      &                                     ndvi_field_mom,            &
      &                                     ndvi_ratio_mom,            &
      &                                     hh_topo,                   &
      &                                     stdh_topo,                 &
      &                                     aot_tg,                    &
      &                                     crutemp,                   &
      &                                     alb_field_mom,             &
      &                                     fr_sand = fr_sand,         &
      &                                     fr_silt = fr_silt,         &
      &                                     fr_clay = fr_clay,         &
      &                                     fr_oc = fr_oc,             &
      &                                     fr_bd = fr_bd,             &
      &                                     fr_dm = fr_dm,             &
      !&                                     soiltype_deep=soiltype_deep, &
      !&                                     fr_sand_deep=fr_sand_deep,   &
      !&                                     fr_silt_deep=fr_silt_deep,   &
      !&                                     fr_clay_deep=fr_clay_deep,   &
      !&                                     fr_oc_deep=fr_oc_deep,       &
      !&                                     fr_bd_deep=fr_bd_deep,       &
      !&                                     fr_dm_deep=fr_dm_deep,       &
      &                                     theta_topo=theta_topo,     &
      &                                     aniso_topo=aniso_topo,     &
      &                                     slope_topo=slope_topo,     &
      &                                     slope_asp_topo=slope_asp_topo,&
      &                                     slope_ang_topo=slope_ang_topo,&
      &                                     horizon_topo=horizon_topo,    &
      &                                     skyview_topo=skyview_topo)

#endif 
  
  CASE(igrid_gme) ! GME grid

#ifdef GRIBAPI  
     PRINT *,'write out ', TRIM(grib_output_filename)

       CALL  write_gme_grid_extpar_grib(TRIM(grib_output_filename),  &
            &                           TRIM(grib_sample),           &
            &                           gme_grid,                    &
            &                           tg,                          &
            &                           isoil_data,                  &
            &                           ldeep_soil,                  &
            &                           itopo_type,                  &
            &                           lsso_param,                  &
            &                           fill_value_real,             &
            &                           fill_value_int,              &
            &                           lon_geo,                     &
            &                           lat_geo,                     &
            &                           fr_land_lu,                  &   !&                                     ice_lu, &
            &                           z0_tot,                      &
            &                           root_lu,                     &
            &                           plcov_mn_lu,                 &
            &                           plcov_mx_lu,                 &
            &                           lai_mn_lu,                   &
            &                           lai_mx_lu,                   &
            &                           rs_min_lu,                   &
            &                           urban_lu,                    &
            &                           for_d_lu,                    &
            &                           for_e_lu,                    &
            &                           emissivity_lu,               &
            &                           lake_depth,                  &
            &                           fr_lake,                     &
            &                           soiltype_fao,                &
            &                           ndvi_max,                    &
            &                           ndvi_field_mom,              &
            &                           ndvi_ratio_mom,              &
            &                           hh_topo,                     &
            &                           stdh_topo,                   &
            &                           aot_tg,                      &
            &                           crutemp,                     & 
            &                           alb_field_mom,               &
!            &                           fr_sand = fr_sand,           &
!            &                           fr_silt = fr_silt,           &
!            &                           fr_clay = fr_clay,           &
!            &                           fr_oc = fr_oc,               &
!            &                           fr_bd = fr_bd,               &
!            &                           fr_dm = fr_dm,               &
 !           &                           soiltype_deep=soiltype_deep, &
 !           &                           fr_sand_deep=fr_sand_deep,   &
 !           &                           fr_silt_deep=fr_silt_deep,   &
 !           &                           fr_clay_deep=fr_clay_deep,   &
 !           &                           fr_oc_deep=fr_oc_deep,       &
 !           &                           fr_bd_deep=fr_bd_deep,       &
 !           &                           fr_dm_deep=fr_dm_deep,       &
            &                           theta_topo=theta_topo,       &
            &                           aniso_topo=aniso_topo,       &
            &                           slope_topo= slope_topo)
#else
  PRINT *,'program compiled without GRIB support! GME output is not possible!'
#endif

END SELECT


        

END PROGRAM extpar_consistency_check


