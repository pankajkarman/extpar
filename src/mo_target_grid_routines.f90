!+ Fortran module with routines to initialize and calculate target grid fields and coordinates
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
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with routines to initialize and calculate target grid fields and coordinates
!> \author Hermann Asensio
!>
MODULE mo_target_grid_routines

  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i4
  USE mo_kind, ONLY: i8
  USE mo_io_units, ONLY: filename_max
  USE mo_utilities_extpar, ONLY: abort_extpar

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: init_target_grid

  CONTAINS

  !> allocate target grid and determine coordinates for each grid element
  SUBROUTINE init_target_grid(namelist_grid_def)

  USE mo_read_extpar_namelists, ONLY: read_namelists_extpar_grid_def

  USE mo_math_constants,  ONLY: pi, pi_2, dbl_eps,rad2deg
  USE mo_grid_structures, ONLY: target_grid_def 
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid
  USE mo_grid_structures, ONLY: gme_triangular_grid
  USE mo_grid_structures, ONLY: igrid_icon
  USE mo_grid_structures, ONLY: igrid_cosmo
  USE mo_grid_structures, ONLY: igrid_gme

  USE mo_icon_grid_data, ONLY: icon_grid !< structure which contains the definition of the ICON grid
  USE mo_icon_grid_data, ONLY: icon_grid_region
  USE mo_icon_grid_data, ONLY: init_icon_dom_def, icon_dom_def
  USE mo_icon_grid_data, ONLY: icon_dom_nr, n_dom, nvertex_per_cell


  USE mo_icon_domain, ONLY: construct_icon_domain
  USE mo_icon_domain, ONLY: max_dom

  USE mo_icon_grid_routines, ONLY: read_domain_info_part
  USE mo_icon_grid_routines, ONLY: get_icon_grid_info
  USE mo_icon_grid_routines, ONLY: get_icon_domain_info
  USE mo_icon_grid_routines, ONLY: init_icon_grid

  USE mo_cosmo_grid, ONLY: cosmo_grid
  USE mo_cosmo_grid, ONLY: get_cosmo_grid_info
  USE mo_cosmo_grid, ONLY: lon_rot, lat_rot
  USE mo_cosmo_grid, ONLY: allocate_cosmo_rc
  USE mo_cosmo_grid, ONLY: calculate_cosmo_target_coordinates

  USE mo_gme_grid, ONLY: gme_grid
  USE mo_gme_grid, ONLY: xn, rlon_gme, rlat_gme
  USE mo_gme_grid, ONLY: get_gme_grid_info
  USE mo_gme_grid, ONLY: init_gme_grid

  USE mo_target_grid_data, ONLY: tg
  USE mo_target_grid_data, ONLY: lon_geo
  USE mo_target_grid_data, ONLY: lat_geo
  USE mo_target_grid_data, ONLY: no_raw_data_pixel
  USE mo_target_grid_data, ONLY: allocate_com_target_fields

  ! local variables
  INTEGER  :: parent_ids(1:max_dom)         !< ids of parent model domain
  CHARACTER (len=filename_max) :: icon_coor_files(1:max_dom) !< filnames of the ICON grid files with the coordinates
  CHARACTER(len=filename_max), INTENT(IN)  :: namelist_grid_def

  CHARACTER (len=filename_max) :: domain_def_namelist !< namelist file with domain definition

  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid
  INTEGER :: idom  !< ICON Domain Number
  INTEGER :: i,j,k !< counters

  ! get information on target grid
  !-----------------------------------------------------------------------------------------------
   
  CALL read_namelists_extpar_grid_def(namelist_grid_def, &
                                         igrid_type, &
                                         domain_def_namelist)

  !HA debug
  PRINT *,'namelist_grid_def: ',TRIM(namelist_grid_def)
  PRINT *,'igrid_type:', igrid_type
  PRINT *,'domain_def_namelist:', TRIM(domain_def_namelist)
  !--------------------------------------------------------------------------------------
  ! read in target grid information
  idom = 1
  SELECT CASE(igrid_type)
       !-----------------------------------------------------------------
       CASE(igrid_icon) ! ICON GRID

         CALL get_icon_grid_info(domain_def_namelist,idom,tg,icon_grid,icon_coor_files,parent_ids)
         CALL init_icon_dom_def(icon_grid%n_dom)
         CALL get_icon_domain_info(icon_grid,icon_coor_files,parent_ids,icon_dom_def)
         CALL init_icon_grid(icon_dom_def)

       !-----------------------------------------------------------------
       CASE(igrid_cosmo) ! COSMO grid
       CALL get_cosmo_grid_info(domain_def_namelist,tg,COSMO_grid)
        
       ! allocate structure cosmo_rot_coor
       CALL allocate_cosmo_rc(tg%ie,tg%je)
       PRINT *,'Allocated lon_rot and lat_rot'
        !-----------------------------------------------------------------
        CASE(igrid_gme) ! GME grid  
        PRINT *,'get_gme_grid_info'
        CALL get_gme_grid_info(domain_def_namelist,tg,gme_grid)

        PRINT *,'gme_grid: ',gme_grid
        PRINT *,'init_gme_grid'
        CALL init_gme_grid(gme_grid)

  END SELECT

  !-----------------------------------------------------------------
  !-----------------------------------------------------------------

  ! allocate target_fields
  CALL allocate_com_target_fields(tg)
  PRINT *,'Allocated lon_geo, lat_geo, no_raw_data_pixel'

  !-----------------------------------------------------------------

  SELECT CASE(igrid_type)
       !-----------------------------------------------------------------
       CASE(igrid_icon) ! ICON GRID
       k=1
       j=1
       DO i=1,tg%ie
          lon_geo(i,j,k) = rad2deg * icon_grid_region(icon_dom_nr)%cells%center(i)%lon ! convert von radians to degrees
          lat_geo(i,j,k) = rad2deg * icon_grid_region(icon_dom_nr)%cells%center(i)%lat ! convert von radians to degrees
       ENDDO

       PRINT *,'lon_geo and lat_geo determined for ICON grid'
       !-----------------------------------------------------------------
       CASE(igrid_cosmo) ! COSMO grid  
            
       CALL calculate_cosmo_target_coordinates(tg,cosmo_grid,lon_geo,lat_geo,lon_rot,lat_rot)

       PRINT *,'Cosmo domain coordinates determined with calculate_cosmo_target_coordinates'
       CASE(igrid_gme) ! GME grid 
          DO k=1, tg%ke
          DO j=1, tg%je
          DO i=1,tg%ie
            lon_geo(i,j,k) = rad2deg * rlon_gme(i-1,j,k) ! convert von radians to degrees
            lat_geo(i,j,k) = rad2deg * rlat_gme(i-1,j,k) ! convert von radians to degrees
          ENDDO
          ENDDO
          ENDDO
  END SELECT
  !-----------------------------------------------------------------

  END SUBROUTINE init_target_grid


END MODULE mo_target_grid_routines


