!+ Fortran modules to read Icon grid from netcdf file
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_2         2011/03/25 Hermann Asensio
!  update to support ICON refinement grids
! V1_3         2011/04/19 Hermann Asensio
!  clean up of code (namelist input for icon grid)
! V1_7         2013/01/25 Guenther Zaengl 
!   Parallel threads for ICON and COSMO using Open-MP, 
!   Several bug fixes and optimizations for ICON search algorithm, 
!   particularly for the special case of non-contiguous domains; 
!   simplified namelist control for ICON  
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran modules to read Icon grid from netcdf file
!!
MODULE mo_icon_grid_routines



  USE mo_kind,            ONLY: wp
  USE mo_io_units,        ONLY: filename_max
  USE mo_exception,       ONLY: message_text, message, finish



  USE mo_icon_domain, ONLY:  icon_domain
  USE mo_icon_domain, ONLY:  grid_cells
  USE mo_icon_domain, ONLY:  grid_vertices
  USE mo_icon_domain, ONLY:  construct_icon_domain
  USE mo_icon_domain, ONLY:  destruct_icon_domain

  USE mo_icon_domain, ONLY: max_dom
  USE netcdf

  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number
  USE mo_io_utilities, ONLY: check_netcdf
  USE mo_utilities_extpar, ONLY: abort_extpar



  IMPLICIT NONE

  PRIVATE

  PUBLIC :: inq_domain_dims

  PUBLIC :: get_icon_grid_info
  PUBLIC :: get_icon_domain_info
  PUBLIC :: init_icon_grid


  CONTAINS

!> get Information for ICON_grid from namelist INPUT_ICON_GRID
   SUBROUTINE get_icon_grid_info(input_namelist_file,tg,icon_grid,icon_coor_file)

   USE mo_io_units, ONLY: filename_max

   USE mo_grid_structures, ONLY: target_grid_def, icosahedral_triangular_grid, icon_grid_def
   USE mo_grid_structures, ONLY: igrid_icon

   USE mo_icon_grid_data, ONLY: nvertex_per_cell

   IMPLICIT NONE

   CHARACTER (len=filename_max), INTENT(IN) :: input_namelist_file !< file with input namelist with COSMO grid definition
   TYPE(target_grid_def), INTENT(OUT)      :: tg               !< structure with target grid description
   TYPE(icosahedral_triangular_grid), INTENT(OUT) :: icon_grid !< structure which contains the definition of the COSMO grid
   CHARACTER (len=filename_max),OPTIONAL,INTENT(OUT) :: icon_coor_file !< filname of the ICON grid file with the coordinates
   ! local variables

  INTEGER :: i_cell_type !< Cell shape. 3: triangluar grid, 4: quadrilateral grid, 6: hexagonal/pentagonal grid

  CHARACTER (len=filename_max) :: icon_grid_nc_file !< filname of the ICON grid files with the coordinates
  CHARACTER (len=filename_max) :: icon_grid_dir !< path to directory which contains the ICON grid files with the coordinates

  NAMELIST /icon_grid_info/ icon_grid_dir, icon_grid_nc_file, i_cell_type


  INTEGER  :: ierr !< error flag
  INTEGER  :: nuin !< unit number
  INTEGER :: i     !< counter
  CHARACTER (len=filename_max) :: filename

  INTEGER                :: ncell                   !< number of cells
  INTEGER                :: nvertex                 !< number of edges
  INTEGER                :: nedge                   !< number of vertices
  INTEGER                :: ncells_per_edge         !< number of cells per edge
  INTEGER                :: nedges_per_vertex       !< number of edges per vertex


   ! set default values for icon_grid_info
   i_cell_type = 3
   icon_grid_dir = ''
   icon_grid_nc_file = ''

   ! read in values from namelist file
   nuin = free_un()  ! function free_un returns free Fortran unit number
   OPEN(nuin,FILE=TRIM(input_namelist_file), IOSTAT=ierr)
       READ(nuin, NML=icon_grid_info, IOSTAT=ierr)
   CLOSE(nuin)

   filename = TRIM(icon_grid_dir)//'/'//TRIM(icon_grid_nc_file)
   WRITE(0,*) 'ICON grid file ', TRIM(filename)
   ! read in information from netcdf file with Icon coordinates (e.g. iconR2B04_DOM01.nc)
    CALL inq_domain_dims( TRIM(filename),      &
      &                      ncell,     &
      &                      nvertex,   &
      &                      nedge,            &
      &                      ncells_per_edge,   &
      &                      nvertex_per_cell,  &
      &                      nedges_per_vertex  )

   ! describe the icon grid from the results of subroutine inq_domain_dims
   icon_grid%nvertex_per_cell = nvertex_per_cell
   icon_grid%ncells_per_edge = ncells_per_edge
   icon_grid%nedges_per_vertex = nedges_per_vertex
   icon_grid%ncell = ncell
   icon_grid%nvertex = nvertex
   icon_grid%nedge = nedge
   icon_grid%nc_grid_file = TRIM(filename)

   !describe the target grid
   tg%igrid_type = igrid_icon      ! "2" is for the COSMO grid, "1" is for the ICON grid
   tg%ie = ncell
   tg%je = 1
   tg%ke = 1              ! third dimension with length 1

   IF (PRESENT(icon_coor_file)) THEN
     icon_coor_file = TRIM(filename)
   ENDIF

END SUBROUTINE get_icon_grid_info



   !> get Information for ICON domains from namelist INPUT_ICON_GRID and icon grid files
   SUBROUTINE get_icon_domain_info(icon_grid,icon_coor_file,icon_dom_def)

   USE mo_io_units, ONLY: filename_max

   USE mo_grid_structures, ONLY: target_grid_def, icosahedral_triangular_grid, icon_grid_def
   USE mo_grid_structures, ONLY: igrid_icon

   IMPLICIT NONE

   TYPE(icosahedral_triangular_grid), INTENT(IN) :: icon_grid !< structure which contains the definition of the COSMO grid
   CHARACTER (len=filename_max),INTENT(IN) :: icon_coor_file !< filename of the ICON grid file with the coordinates

   TYPE(icon_grid_def), INTENT(INOUT) :: icon_dom_def !< structure which contains the definition of the COSMO grid

   ! local variables
   CHARACTER (len=filename_max) :: filename
   INTEGER                :: ncell                   !< number of cells
   INTEGER                :: nvertex                 !< number of edges
   INTEGER                :: nedge                   !< number of vertices
   INTEGER                :: ncells_per_edge         !< number of cells per edge
   INTEGER                :: nvertex_per_cell        !< number of vertices per cell
   INTEGER                :: nedges_per_vertex       !< number of edges per vertex


    icon_dom_def%grid_file = TRIM(icon_coor_file)
    filename = TRIM(icon_coor_file)

    ! read in information from netcdf file with Icon coordinates (e.g. iconR2B04_DOM01.nc)
    CALL inq_domain_dims( TRIM(filename),      &
      &                      ncell,     &
      &                      nvertex,   &
      &                      nedge,            &
      &                      ncells_per_edge,   &
      &                      nvertex_per_cell,  &
      &                      nedges_per_vertex)

   ! describe the icon grid from the results of subroutine inq_domain_dims

   icon_dom_def%nvertex_per_cell  = nvertex_per_cell
   icon_dom_def%ncells_per_edge   = ncells_per_edge
   icon_dom_def%nedges_per_vertex = nedges_per_vertex
   icon_dom_def%ncell = ncell
   icon_dom_def%nvertex = nvertex
   icon_dom_def%nedge = nedge
   icon_dom_def%grid_file = TRIM(filename)

   !HA debug
   PRINT *,'filename: ',TRIM(filename)
   PRINT *,'ncell: ',ncell
   PRINT *,'nvertex: ',nvertex
   PRINT *,'nedge: ', nedge
   PRINT *,'nvertex_per_cell: ',nvertex_per_cell
   PRINT *,'nedges_per_vertex: ',nedges_per_vertex
   PRINT *,'ncell: ',ncell
   PRINT *,'nvertex: ',nvertex
   PRINT *,'nedge: ', nedge


END SUBROUTINE get_icon_domain_info


  !>
  !! inquire domain dimension information from netcdf file
  !!
  !! @par Revision History
  !! Initial realease by Hermann Asensio (2009-07-31)
  !!
  SUBROUTINE inq_domain_dims( filename,      &
                            ncell,     &
                            nvertex,   &
                            nedge,            &
                            ncells_per_edge,   &
                            nvertex_per_cell,  &
                            nedges_per_vertex  )

   CHARACTER(len=filename_max), INTENT(IN)  :: filename  !< filename of netcdf-file with ICON patch coordinates and variables
   
   
   INTEGER, INTENT(OUT)                      :: ncell                   !< number of cells
   INTEGER, INTENT(OUT)                      :: nvertex                 !< number of edges
   INTEGER, INTENT(OUT)                      :: nedge                   !< number of vertices
   INTEGER, INTENT(OUT)                      :: ncells_per_edge         !< number of cells per edge
   INTEGER, INTENT(OUT)                      :: nvertex_per_cell        !< number of vertices per cell
   INTEGER, INTENT(OUT)                      :: nedges_per_vertex       !< number of edges per vertex


   INTEGER :: ncid     !< unit number for netcdf file

    ! HA CODE
    !local variables

   INTEGER :: dimid                            !< id of dimension

   INTEGER             :: dim_two                 !< dimension 2 (for grid refinement)
   INTEGER             :: cell_grf                !< dimension for cell grid refinement
   INTEGER             :: edge_grf                !< dimension for edge grid refinement
   INTEGER             :: vert_grf                !< dimension for vertex grid refinenment




    CALL check_netcdf(nf90_open(TRIM(filename), NF90_NOWRITE, ncid))  ! open netcdf file, get ncid

    ! here I know that the dimension for array of cells is called 'cell', get the dimid for this dimension
    CALL check_netcdf(nf90_inq_dimid(ncid, 'cell', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=ncell))

    ! here I know that the dimension for array of vertices is called 'vertex', get the dimid for this dimension
    CALL check_netcdf(nf90_inq_dimid(ncid, 'vertex', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=nvertex))

    ! here I know that the dimension for array of edges is called 'edge', get the dimid for this dimension
    CALL check_netcdf(nf90_inq_dimid(ncid, 'edge', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=nedge))
    !
    !
    ! get further dimension nc, nv, ne and no
    CALL check_netcdf(nf90_inq_dimid(ncid, 'nc', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=ncells_per_edge))

    CALL check_netcdf(nf90_inq_dimid(ncid, 'nv', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=nvertex_per_cell))

    CALL check_netcdf(nf90_inq_dimid(ncid, 'ne', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=nedges_per_vertex))

    CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE inq_domain_dims

  
   !>
  !! read reduced grid information from netcdf file
  !!
  !! read_domain_info_part is an extension of the subroutine read_grid_info_part
  !! In the domain files for the ICON grid there are some additional variables like
  !! - refin_c_ctrl (refinement control flag for cells)
  !! - child_cell_id (domain ID of child cell)
  !! which are not available in the 'grid' files for the ICON grid levels.
  !! In this routine refin_c_ctrl is read from the file into the structure
  !! g%cells%refin_ctrl and
  !! child_cell_id is read into the structure g%cells%child_id
  !!
  !! @par Revision History
  !! Initial realease by Hermann Asensio (2009-07-31)

  !!
  SUBROUTINE read_domain_info_part(filename, g)

   USE mo_additional_geometry,   ONLY: gc2cc

   CHARACTER(len=filename_max), INTENT(IN)  :: filename  !< filename of netcdf-file with ICON grid coordinates and variables

   TYPE(icon_domain), INTENT(INOUT) :: g




    INTEGER :: ncid !< unit number for netcdf file


    ! HA CODE
    !local variables


    INTEGER :: varid                            !< id of variable
    CHARACTER (len=80) :: varname               !< name of variable
    CHARACTER (len=80) :: attname               !< name of attribute
       


    WRITE(message_text,'(a,a)') 'READ gridmap file: ', TRIM(filename)
    CALL message ('', TRIM(message_text)) 

    CALL check_netcdf(nf90_open(TRIM(filename), NF90_NOWRITE, ncid))

    varname='lon_cell_centre'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%center(:)%lon))


    varname='lat_cell_centre'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid,g%cells%center(:)%lat ))


    varname='longitude_vertices'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%verts%vertex(:)%lon ))



    varname='latitude_vertices'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%verts%vertex(:)%lat ))



    varname='vertex_of_cell'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid ,g%cells%vertex_index ))



    varname='cells_of_vertex'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%verts%cell_index ))


    varname='neighbor_cell_index'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%neighbor_index))


    CALL check_netcdf(nf90_close(ncid))

      ! calculate the cartesian coordinates of the cells
    g%cells%cc_center = gc2cc(g%cells%center)
    

    ! calculate the cartesian coordinates of the cells
    g%verts%cc_vertex = gc2cc(g%verts%vertex)


  END SUBROUTINE read_domain_info_part
!------------------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------------------

!> initalize Icon grid
!!
!! allocate the icon_grid_level and icon_grid_level(:)  here, pass icon_dom_def
  SUBROUTINE init_icon_grid(icon_dom_def)

   USE mo_icon_domain,          ONLY: icon_domain, &
     &                           grid_cells,               &
     &                           grid_vertices,            &
     &                           construct_icon_domain


   USE mo_icon_grid_data, ONLY:    icon_grid, &
     &                              icon_grid_region

   USE mo_io_units,          ONLY: filename_max

   USE mo_exception,         ONLY: message_text, message, finish

   USE mo_utilities_extpar, ONLY: abort_extpar

   USE mo_target_grid_data, ONLY: tg

   USE mo_target_grid_data, ONLY: allocate_com_target_fields, &
     &                       lon_geo, &
     &                       lat_geo

   USE mo_math_constants,  ONLY: pi, rad2deg

   USE mo_grid_structures, ONLY: target_grid_def, icosahedral_triangular_grid, icon_grid_def


  IMPLICIT NONE

  TYPE(icon_grid_def), INTENT(INOUT) :: icon_dom_def !< structure which contains the definition of the COSMO grid


  CHARACTER(len=filename_max) :: filename


  INTEGER :: i,j,k, ip, ic


  INTEGER                      :: i_nc       !< number of cells
  INTEGER                      :: i_ne       !< number of edges
  INTEGER                      :: i_nv       !< number of vertices
  INTEGER                      :: nc_p_e     !< number of cells per edge
  INTEGER                      :: nv_p_c     !< number of vertices per cell
  INTEGER                      :: ne_p_v     !< number of edges per vertex


  INTEGER :: errorcode !< error status variable

   !--------------------------------------------------------------------------------------------------------

    filename = TRIM(icon_dom_def%grid_file)
    i_nc = icon_dom_def%ncell        !< number of cells
    i_ne = icon_dom_def%nedge        !< number of edges
    i_nv = icon_dom_def%nvertex      !< number of vertices
    nc_p_e = icon_dom_def%ncells_per_edge  !< number of cells per edge
    nv_p_c = icon_dom_def%nvertex_per_cell !< number of vertices per cell
    ne_p_v = icon_dom_def%nedges_per_vertex!< number of edges per vertex

    !HA debug:
    print *,'i_nc, i_ne, i_nv: ', i_nc, i_ne, i_nv
    print *,'call construct_icon_domain'

    CALL construct_icon_domain(icon_grid_region, &
                               i_nc,             &
                               i_nv,             &
                               i_ne,             &
                               nc_p_e,           &
                               nv_p_c,           &
                               ne_p_v            )

    !HA debug:
    print *,'read target domains grid'

    ! read grid information and coordinates
    CALL  read_domain_info_part(filename, icon_grid_region)
    ! set known values for domain

  END SUBROUTINE init_icon_grid


END MODULE mo_icon_grid_routines


