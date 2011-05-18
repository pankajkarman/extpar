!+ Fortran modules to read Icon grid from netcdf file
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
!> Fortran modules to read Icon grid from netcdf file
!!
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

  PUBLIC :: inq_grid_dims
  PUBLIC :: inq_domain_dims
  PUBLIC :: read_grid_info_part
  PUBLIC :: read_domain_info_part
  PUBLIC :: read_gridref_nl
  PUBLIC :: allocate_icon_grid

  PUBLIC :: get_icon_grid_info



  CONTAINS


  
   !> get Information for ICON_grid from namelist INPUT_ICON_GRID
   SUBROUTINE get_icon_grid_info(input_namelist_file,idom,tg,icon_grid)

   USE mo_io_units, ONLY: filename_max

   USE mo_grid_structures, ONLY: target_grid_def, icosahedral_triangular_grid
   USE mo_grid_structures, ONLY: igrid_icon

   USE mo_icon_domain, ONLY: max_dom


   IMPLICIT NONE

   CHARACTER (len=filename_max), INTENT(IN) :: input_namelist_file !< file with input namelist with COSMO grid definition
   INTEGER, INTENT(OUT) :: idom !< ICON Domain Number 
  
   TYPE(target_grid_def), INTENT(OUT)      :: tg               !< structure with target grid description
   TYPE(icosahedral_triangular_grid), INTENT(OUT) :: icon_grid !< structure which contains the definition of the COSMO grid
   
   ! local variables
     
  INTEGER :: grid_levels
  INTEGER :: nroot
  LOGICAL :: lplane
  NAMELIST /grid_ini/ grid_levels, nroot, lplane


  REAL(wp) :: x_rot_angle
  REAL(wp) :: y_rot_angle
  REAL(wp) :: z_rot_angle
  INTEGER :: itype_optimize  !< Type of grid-optimization:
  ! = 0 : natural grid
  ! = 1 : Heikes optimization
  ! = 2 : equal area subdivision
  ! = 3 : c-grid small circle constraint
  ! = 4 : spring dynamics
  LOGICAL :: l_c_grid        !< Use C-grid constraint for last refinement level
  INTEGER :: maxlev_optim    !< For itype_optimize=1,4: highest level for which
  !< optimization is executed
   REAL(wp):: beta_spring     ! for spring dynamics: weighting of a larger or
  ! a smaller target grid length
  ! (choice: 1.11 larger target length: good for
  !          C-grid constraint,
  !          0.9  smaller target length: good for
  !          similar triangle shapes
  NAMELIST /grid_options/ x_rot_angle, y_rot_angle, z_rot_angle, &
      & itype_optimize, l_c_grid, maxlev_optim, &
      & beta_spring

   REAL(wp):: tria_arc_km     !< grid distance (km) for the planar grid option
  !< on the finest chosen level

   NAMELIST /plane_options/tria_arc_km

   INTEGER  :: grid_root                    !< number of partitions of the icosahedron
   INTEGER  :: start_lev                    !< level of (first) global model domain
   INTEGER  :: n_dom                        !< number of model domains
   INTEGER  :: parent_id(max_dom-1)         !< id of parent model domain

   LOGICAL  :: l_plot 
   LOGICAL  :: l_circ
   LOGICAL  :: l_rotate 
   REAL(wp), DIMENSION(max_dom-1) :: radius 
   REAL(wp), DIMENSION(max_dom-1) :: center_lon
   REAL(wp), DIMENSION(max_dom-1) :: center_lat
   REAL(wp), DIMENSION(max_dom-1) :: hwidth_lon 
   REAL(wp), DIMENSION(max_dom-1) :: hwidth_lat
   LOGICAL  ::l_indexmap

   NAMELIST /gridref_ini/ grid_root, start_lev, n_dom, parent_id, l_plot,   &
                         &  l_circ, l_rotate, radius, center_lon, center_lat, &
                         &  hwidth_lon, hwidth_lat, l_indexmap
  CHARACTER (len=filename_max) :: icon_grid_dir !< path to directory which contains the ICON grid files with the coordinates
  CHARACTER (len=filename_max) :: icon_grid_nc_files(1:max_dom) !< filnames of the ICON grid files with the coordinates
  NAMELIST /icon_grid_files/ icon_grid_dir, icon_grid_nc_files

  INTEGER :: icon_dom_nr !< number of icon domain for target grid
  INTEGER :: nproma     !< vector length
  INTEGER :: i_cell_type !< Cell shape. 3: triangluar grid, 4: quadrilateral grid, 6: hexagonal/pentagonal grid 
  !\TODO this type of information could be merged with igrid_type in namelist GRID_DEF
  NAMELIST /icon_grid_ctl/ icon_dom_nr, nproma, i_cell_type

  
  INTEGER  :: ierr !< error flag
  INTEGER  :: nuin !< unit number
  INTEGER :: i     !< counter
  CHARACTER (len=filename_max) :: filename
     
   INTEGER                :: ncell                   !< number of cells
   INTEGER                :: nvertex                 !< number of edges
   INTEGER                :: nedge                   !< number of vertices
   INTEGER                :: ncells_per_edge         !< number of cells per edge
   INTEGER                :: nvertex_per_cell        !< number of vertices per cell
   INTEGER                :: nedges_per_vertex       !< number of edges per vertex
   INTEGER                :: nchilds_per_cell        !< number of child cells per cell
   INTEGER                :: nchdom                  !< maximum number of child domains






     
    ! set default values for grid_ini
    nroot          = 2
    grid_levels    = 4
    lplane         = .false.

  
    ! set default values for grid_options
    l_c_grid       = .false.
    itype_optimize = 4
    x_rot_angle    = 0.0_wp
    y_rot_angle    = 0.0_wp
    z_rot_angle    = 0.0_wp
    maxlev_optim   = 100
    beta_spring    = 0.90_wp
       
    ! set default values for plane_options
    tria_arc_km    = 10.0_wp ! resolution in kilometers

    ! set default values for gridref_ini
    grid_root = 2
    start_lev = 4
    n_dom     = 2
    
    ! Note: the namelist arrays parent_id, radius etc. are not defined for the
    ! global domain, so the parent_id(1), i.e. the parent ID of the first nest,
    ! is always 1
    DO i = 1, max_dom-1
      parent_id(i) = i
    ENDDO
    
    l_circ     = .false.
    l_rotate   = .false.
    l_plot     = .false.
    l_indexmap = .false.
    
    radius     = 30.0_wp
    center_lat = 90.0_wp
    center_lon = 30.0_wp
    hwidth_lat = 20.0_wp
    hwidth_lon = 20.0_wp

    
   icon_grid_dir = '' ! set default values
   DO i=1,max_dom
     icon_grid_nc_files(i) = ''
   ENDDO
    
    ! set default values for icon_grid_ctl
    icon_dom_nr= 1
    nproma = 1 
    i_cell_type = 3





    ! read in values from namelist file

    nuin = free_un()  ! functioin free_un returns free Fortran unit number
   OPEN(nuin,FILE=TRIM(input_namelist_file), IOSTAT=ierr)

   READ(nuin, NML=grid_ini, IOSTAT=ierr)
   READ(nuin, NML=grid_options, IOSTAT=ierr)
   READ(nuin, NML=plane_options, IOSTAT=ierr)
   READ(nuin, NML=gridref_ini, IOSTAT=ierr)
   READ(nuin, NML=icon_grid_files, IOSTAT=ierr)
   READ(nuin, NML=icon_grid_ctl, IOSTAT=ierr)


   CLOSE(nuin)
   !! HA debug
   !PRINT *,'icon_grid_dir: ', TRIM(icon_grid_dir) 
   !DO i=1,max_dom
   !  PRINT *,'icon_grid_nc_files(i): ', TRIM(icon_grid_nc_files(i))
   !ENDDO
   PRINT *, 'icon_dom_nr: ', icon_dom_nr
   PRINT *, 'grid_levels: ', grid_levels
   PRINT *, 'start_lev: ', start_lev
   PRINT *, 'beta_spring: ', beta_spring





   !HA debug
   print *, 'after reading namelist ', TRIM(input_namelist_file)
    idom = icon_dom_nr
    PRINT *,'HA debug: idom ', idom
    PRINT *,'HA debug: icon_grid_dir ', TRIM(icon_grid_dir)
    PRINT *,'HA debug: icon_grid_nc_files(idom) ', TRIM(icon_grid_nc_files(idom))


    filename = TRIM(icon_grid_dir)//'/'//TRIM(icon_grid_nc_files(idom))
   PRINT *,'HA debug: filename ', TRIM(filename)

   ! read in filename with Icon coordinates (e.g. iconR2B04_DOM01.nc)
    CALL inq_domain_dims( TRIM(filename),      &
                            ncell,     &
                            nvertex,   &
                            nedge,            &
                            ncells_per_edge,   &
                            nvertex_per_cell,  &
                            nedges_per_vertex, &
                            nchilds_per_cell,   &
                            nchdom)


   ! describe the icon grid from the values above

   icon_grid%n_dom = n_dom
   icon_grid%start_lev = start_lev
   icon_grid%grid_root = grid_root
   icon_grid%nvertex_per_cell = nvertex_per_cell
   icon_grid%nchilds_per_cell = nchilds_per_cell 
   icon_grid%ncells_per_edge = ncells_per_edge
   icon_grid%nedges_per_vertex = nedges_per_vertex
   icon_grid%nchdom = nchdom
   icon_grid%ncell = ncell
   icon_grid%nvertex = nvertex
   icon_grid%nedge = nedge
   icon_grid%nc_grid_file = TRIM(filename)
   
   PRINT *,'HA debug:'
   PRINT *,'ncell: ',ncell
   PRINT *,'nvertex: ',nvertex
   PRINT *,'nedge: ', nedge
   PRINT *,'ncells_per_edge: ', ncells_per_edge
   PRINT *,'nvertex_per_cell: ',nvertex_per_cell
   PRINT *,'nedges_per_vertex: ',nedges_per_vertex
   PRINT *,'nchilds_per_cell: ', nchilds_per_cell
   PRINT *,'nchdom: ', nchdom
   PRINT *,'ncell: ',ncell
   PRINT *,'nvertex: ',nvertex
   PRINT *,'nedge: ', nedge
   

   !describe the target grid
   tg%igrid_type = igrid_icon      ! "2" is for the COSMO grid, "1" is for the ICON grid
   tg%ie = ncell
   tg%je = 1
   tg%ke = 1              ! third dimension with length 1     

      

END SUBROUTINE get_icon_grid_info



  
  !>
  !! read namelist with grid refinment information
  !!
  !! Read the namelist with grid refinment information and
  !! check for the values
  !!
  !! @par Revision History
  !! Initial realease by Hermann Asensio (2009-07-31)
  !!
  SUBROUTINE read_gridref_nl( input_namelist_file,       &
                              grid_root,      &
                              start_lev,      &
                              n_dom,          &
                              parent_id)
  CHARACTER(len=filename_max), INTENT(IN)  :: input_namelist_file  !< filename with grid refinment information
 ! Namelist variables
  INTEGER, INTENT(OUT)  :: grid_root                    !< number of partitions of the icosahedron
  INTEGER, INTENT(OUT)  :: start_lev                    !< level of (first) global model domain
  INTEGER, INTENT(OUT)  :: n_dom                        !< number of model domains
  INTEGER, INTENT(OUT)  :: parent_id(max_dom-1)         !< id of parent model domain

  LOGICAL  :: l_circ, l_rotate, l_plot, l_indexmap
  REAL(wp), DIMENSION(max_dom-1) :: radius, center_lon, center_lat, &
                                    hwidth_lon, hwidth_lat

  INTEGER  :: ierr !< error flag
  INTEGER  :: nuin !< unit number
  INTEGER :: i     !< counter


  
    NAMELIST /gridref_ini/ grid_root, start_lev, n_dom, parent_id, l_plot,   &
                           l_circ, l_rotate, radius, center_lon, center_lat, &
                           hwidth_lon, hwidth_lat, l_indexmap
   
   
   ! set default values for gridref_ini
    grid_root = 2
    start_lev = 4
    n_dom     = 2

    ! Note: the namelist arrays parent_id, radius etc. are not defined for the
    ! global domain, so the parent_id(1), i.e. the parent ID of the first nest,
    ! is always 1
    DO i = 1, max_dom-1
      parent_id(i) = i
    ENDDO

    l_circ     = .FALSE.
    l_rotate   = .FALSE.
    l_plot     = .FALSE.
    l_indexmap = .FALSE.

    radius     = 30.0_wp
    center_lat = 90.0_wp
    center_lon = 30.0_wp
    hwidth_lat = 20.0_wp
    hwidth_lon = 20.0_wp


   
   nuin = free_un()  ! functioin free_un returns free Fortran unit number

   open(nuin,FILE=input_namelist_file, IOSTAT=ierr)
              !print *, ierr
   read(nuin, NML=gridref_ini, IOSTAT=ierr)
              !print *, ierr

   close(nuin)

   IF (ierr /= 0) CALL finish('read_gridref_nl','Error in reading namelist!')



  END SUBROUTINE read_gridref_nl

  !>
  !! inquire grid dimension information from netcdf file
  !!
  !! @par Revision History
  !! Initial realease by Hermann Asensio (2009-07-31)

  !!
  SUBROUTINE inq_grid_dims( filename,      &
                            ncell,     &
                            nvertex,   &
                            nedge,            &
                            ncells_per_edge,   &
                            nvertex_per_cell,  &
                            nedges_per_vertex, &
                            nchilds_per_cell   &
                            )

   CHARACTER(len=filename_max), INTENT(IN)  :: filename  !< filename of netcdf-file with ICON grid coordinates and variables
   
   
   INTEGER, INTENT(OUT)                      :: ncell                   !< number of cells
   INTEGER, INTENT(OUT)                      :: nvertex                 !< number of edges
   INTEGER, INTENT(OUT)                      :: nedge                   !< number of vertices
   INTEGER, INTENT(OUT)                      :: ncells_per_edge         !< number of cells per edge
   INTEGER, INTENT(OUT)                      :: nvertex_per_cell        !< number of vertices per cell
   INTEGER, INTENT(OUT)                      :: nedges_per_vertex       !< number of edges per vertex
   INTEGER, INTENT(OUT)                      :: nchilds_per_cell        !< number of child cells per cell



   INTEGER :: ncid     !< unit number for netcdf file

    ! HA CODE
    !local variables

   INTEGER :: dimid                            !< id of dimension


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

    CALL check_netcdf(nf90_inq_dimid(ncid, 'no', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=nchilds_per_cell))


    CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE inq_grid_dims


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
                            nedges_per_vertex, &
                            nchilds_per_cell,  &
                            nchdom            &
                            )

   CHARACTER(len=filename_max), INTENT(IN)  :: filename  !< filename of netcdf-file with ICON patch coordinates and variables
   
   
   INTEGER, INTENT(OUT)                      :: ncell                   !< number of cells
   INTEGER, INTENT(OUT)                      :: nvertex                 !< number of edges
   INTEGER, INTENT(OUT)                      :: nedge                   !< number of vertices
   INTEGER, INTENT(OUT)                      :: ncells_per_edge         !< number of cells per edge
   INTEGER, INTENT(OUT)                      :: nvertex_per_cell        !< number of vertices per cell
   INTEGER, INTENT(OUT)                      :: nedges_per_vertex       !< number of edges per vertex
   INTEGER, INTENT(OUT)                      :: nchilds_per_cell        !< number of child cells per cell
   INTEGER, INTENT(OUT)                      :: nchdom                  !< maximum number of child domains


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

    CALL check_netcdf(nf90_inq_dimid(ncid, 'no', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=nchilds_per_cell))
    !
    !
    ! get further dimension two_grf, max_chdom, cell_grf, edge_grf, vert_grf
    !CALL check_netcdf(nf90_inq_dimid(ncid, 'two_grf', dimid)) 
    !! get the length of the dimension:
    !CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=dim_two))


    CALL check_netcdf(nf90_inq_dimid(ncid, 'max_chdom', dimid)) 
    ! get the length of the dimension:
    CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=nchdom))


    !CALL check_netcdf(nf90_inq_dimid(ncid, 'cell_grf', dimid)) 
    !! get the length of the dimension:
    !CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=cell_grf))


    !CALL check_netcdf(nf90_inq_dimid(ncid, 'edge_grf', dimid)) 
    !! get the length of the dimension:
    !CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=edge_grf))


    !CALL check_netcdf(nf90_inq_dimid(ncid, 'vert_grf', dimid)) 
    !! get the length of the dimension:
    !CALL check_netcdf(nf90_inquire_dimension(ncid, dimid, len=vert_grf))


    CALL check_netcdf(nf90_close(ncid))

  END SUBROUTINE inq_domain_dims




   !>
  !! read reduced grid information from netcdf file
  !!
  !! @par Revision History
  !! Initial realease by Hermann Asensio (2009-07-31)

  !!
  SUBROUTINE read_grid_info_part(filename, g)

  USE mo_additional_geometry,   ONLY: gc2cc
   CHARACTER(len=filename_max), INTENT(IN)  :: filename  !< filename of netcdf-file with ICON grid coordinates and variables

   TYPE(icon_domain), INTENT(INOUT) :: g




    INTEGER :: ncid !< unit number for netcdf file


    ! HA CODE
        !local variables


        INTEGER :: varid                            !< id of variable
        CHARACTER (len=80) :: varname               !< name of variable
        






    !PRINT *,'READ gridmap file: ', TRIM(filename)

    !WRITE(message_text,'(a,a)') 'READ gridmap file: ', TRIM(filename)
    !CALL message ('', TRIM(message_text)) 

    !PRINT *,'open netcdf file'

    CALL check_netcdf(nf90_open(TRIM(filename), NF90_NOWRITE, ncid))

    
    !CALL check_netcdf(nf90_inquire_variable(ncid,varid,varname,xtype, ndim, var_dimids, nAtts))
    !CALL check_netcdf(nf90_get_var(ncid,varid,lon,start=(/1/),count=(/length/))) ! read from netcdf file into lon(:)

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



    varname='parent_cell_index'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%parent_index ))



    varname='neighbor_cell_index'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%neighbor_index))

    varname='child_cell_index'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%child_index))



    CALL check_netcdf(nf90_close(ncid))

    ! calculate the cartesian coordinates of the cells
    g%cells%cc_center = gc2cc(g%cells%center)
    

    ! calculate the cartesian coordinates of the cells
    g%verts%cc_vertex = gc2cc(g%verts%vertex)





  END SUBROUTINE read_grid_info_part



  
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

    
    !CALL nf(nf90_inquire_variable(ncid,varid,varname,xtype, ndim, var_dimids, nAtts))
    !CALL nf(nf90_get_var(ncid,varid,lon,start=(/1/),count=(/length/))) ! read from netcdf file into lon(:)

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



    varname='parent_cell_index'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%parent_index ))



    varname='neighbor_cell_index'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%neighbor_index))

    varname='child_cell_index'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%child_index))

   ! child id variables

    varname='child_cell_id'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%child_id))



    ! refinment control ('boundary' of refinement area)

    varname='refin_c_ctrl'
    CALL check_netcdf(nf90_inq_varid(ncid,TRIM(varname),varid))
    CALL check_netcdf(nf90_get_var(ncid,varid, g%cells%refin_ctrl))

    ! get global attributes
    attname = 'grid_ID'
    CALL check_netcdf(nf90_get_att(ncid,nf90_global,attname,g%id))

    attname = 'parent_grid_ID'
    CALL check_netcdf(nf90_get_att(ncid,nf90_global,attname,g%parent_id))

    attname = 'grid_level'
    CALL check_netcdf(nf90_get_att(ncid,nf90_global,attname,g%level))

    attname = 'grid_root'
    CALL check_netcdf(nf90_get_att(ncid,nf90_global,attname,g%grid_root))





    CALL check_netcdf(nf90_close(ncid))

      ! calculate the cartesian coordinates of the cells
    g%cells%cc_center = gc2cc(g%cells%center)
    

    ! calculate the cartesian coordinates of the cells
    g%verts%cc_vertex = gc2cc(g%verts%vertex)






  END SUBROUTINE read_domain_info_part

!> allocate Icon grid
!!
!! allocate the icon_grid_level and icon_grid_level(:)  here, read in the namelist files etc
  SUBROUTINE allocate_icon_grid


   USE mo_icon_domain,          ONLY: icon_domain, &
     &                           grid_cells,               &
     &                           grid_vertices,            &
     &                           construct_icon_domain
   

   USE mo_icon_grid_data, ONLY:    ICON_grid, &
     &                              icon_grid_region, &
     &                              icon_grid_level
                          
   USE mo_io_units,          ONLY: filename_max

   USE mo_exception,         ONLY: message_text, message, finish

   USE mo_utilities_extpar, ONLY: abort_extpar

   USE mo_target_grid_data, ONLY: tg

   USE mo_target_grid_data, ONLY: allocate_com_target_fields, &
     &                       lon_geo, &
     &                       lat_geo

   USE mo_math_constants,  ONLY: pi, rad2deg


  IMPLICIT NONE

  CHARACTER(len=filename_max) :: input_namelist_file
  CHARACTER(len=filename_max) :: filename


   ! Namelist variables
  INTEGER :: grid_root                    !< number of partitions of the icosahedron
  INTEGER :: start_lev                    !< level of (first) global model domain
  INTEGER :: n_dom                        !< number of model domains
  INTEGER :: parent_id(max_dom-1)         !< id of parent model domain

   INTEGER :: i,j,k, ilev, idom, ip, iplev, ic,  iclev, istartlev

   
  INTEGER                      :: i_nc       !< number of cells
  INTEGER                      :: i_ne       !< number of edges
  INTEGER                      :: i_nv       !< number of vertices
  INTEGER                      :: nc_p_e     !< number of cells per edge
  INTEGER                      :: nv_p_c     !< number of vertices per cell
  INTEGER                      :: ne_p_v     !< number of edges per vertex
  INTEGER                      :: nchilds    !< number of child cells per cell

  INTEGER                      :: n_childdom !< actual number of child domains



  INTEGER :: first_dom

  INTEGER, ALLOCATABLE :: level_region(:)   ! level of region
  INTEGER :: errorcode !< error status variable







   !--------------------------------------------------------------------------------------------------------
  ! Read namelist NAMELIST_GRIDREF
  input_namelist_file='NAMELIST_GRIDREF'
  CALL read_gridref_nl(input_namelist_file,       &
                              grid_root,      &
                              start_lev,      &
                              n_dom,          &
                              parent_id)

  print *,'HA debug:'
  print *,'grid_root: ',grid_root
  print *,'start_lev: ',start_lev
  print *,'n_dom: ', n_dom
  print *,'parent_id: ', parent_id

    istartlev = 0

  ! allocate data structure for grid
   ALLOCATE (icon_grid_level(istartlev:start_lev))  
   ! start_lev is where the domain refinement starts,  DOM=1
   ! Loop over the levels
   DO ilev = istartlev, start_lev-1

    WRITE(filename,'(a,i0,a,i2.2,a)')'iconR', grid_root, 'B', ilev, '.nc'
    !HA debug:
    print *, 'filename: ',TRIM(filename)
    print *, 'inquire dimensions'

    ! inquire the dimension 
    CALL inq_grid_dims(TRIM(filename), i_nc, i_nv, i_ne, nc_p_e, nv_p_c, ne_p_v, nchilds)

    !HA debug:
    print *,'filename: ',TRIM(filename)
    print *,'i_nc, i_ne, i_nv: ', i_nc, i_ne, i_nv
    print *,'call construct grid'



   ! fill global structure ICON_grid
   ICON_grid%n_dom = n_dom
   ICON_grid%start_lev = start_lev
   ICON_grid%grid_root = grid_root
   ICON_grid%nvertex_per_cell = nv_p_c
   ICON_grid%nchilds_per_cell = nchilds

   print *,'n_dom: ', n_dom
   print *,'ICON_grid%n_dom: ', ICON_grid%n_dom




    
    ! construct (allocate and set default values) all elements of data type 'grid' 
    n_childdom = 0
    CALL construct_icon_domain(icon_grid_level(ilev),   &
                            max_dom,          &
                            n_childdom,       &
                            i_nc,             &
                            i_nv,             &
                            i_ne,             &
                            nc_p_e,           &
                            nv_p_c,           &
                            ne_p_v,           &
                            nchilds           &
                            )
   
     ! set known values for domain
      icon_grid_level(ilev)%id    = 0
      icon_grid_level(ilev)%level = ilev
      icon_grid_level(ilev)%parent_id = 0


      !HA debug:
      print *,'read grid'
      ! read grid information and coordinates
      CALL  read_grid_info_part(TRIM(filename), icon_grid_level(ilev))
      print *,'read reduced ouput done'

    ENDDO

    ! the icon grid domains are numbered from 1 to n_dom
    first_dom=1

    ALLOCATE(icon_grid_region(first_dom:n_dom))
    ALLOCATE(level_region(first_dom:n_dom))

    DO idom=first_dom,n_dom

      IF (idom==first_dom) then
        ilev                  = start_lev
        level_region(idom)    = ilev
      ELSE
        ilev = level_region(parent_id(idom-1)) + 1      ! we apologize for the inconvinience, this is to be improved...
        level_region(idom)    = ilev                    ! parent_id(:) is the array which is given in the namelist
      ENDIF

       WRITE(filename,'(a,i0,2(a,i2.2),a)') &
         'iconR', grid_root, 'B', ilev, '_DOM', idom, '.nc'

       WRITE(message_text,'(a,a)') 'Read domain file: ', TRIM(filename)
        CALL message ('', TRIM(message_text)) 

       !HA debug:
        print *,'filename: ',TRIM(filename)

        CALL  inq_domain_dims( filename, &
                            i_nc,    &
                            i_nv,    &
                            i_ne,    &
                            nc_p_e,  &
                            nv_p_c,  &
                            ne_p_v,  &
                            nchilds, &
                            n_childdom  &
                            )

       !HA debug:
        print *,'i_nc, i_ne, i_nv: ', i_nc, i_ne, i_nv
        print *,'call construct_icon_domain'

        CALL construct_icon_domain(icon_grid_region(idom),   &
                            max_dom,          &
                            n_childdom,       &
                            i_nc,             &
                            i_nv,             &
                            i_ne,             &
                            nc_p_e,           &
                            nv_p_c,           &
                            ne_p_v,           &
                            nchilds           &
                            )
        print *,'i_nc, i_ne, i_nv: ', i_nc, i_ne, i_nv
        print *,'idom: ',idom
        print *,'icon_grid_region(idom)%ncells ',icon_grid_region(idom)%ncells


       ! set known values for domain
          icon_grid_region(idom)%id    = idom
          icon_grid_region(idom)%level = ilev
        IF (idom==first_dom) then
          icon_grid_region(idom)%parent_id = 0
        ELSE
          icon_grid_region(idom)%parent_id = parent_id(idom-1) ! parent_id(:) is the array which is given in the namelist
        ENDIF

        IF (idom==first_dom) then
        ! put DOM=1 also in icon_grid_level hierachy
        CALL construct_icon_domain(icon_grid_level(start_lev),   &
                            max_dom,          &
                            n_childdom,       &
                            i_nc,             &
                            i_nv,             &
                            i_ne,             &
                            nc_p_e,           &
                            nv_p_c,           &
                            ne_p_v,           &
                            nchilds           &
                            )
   
         ! set known values for domain
          icon_grid_level(ilev)%id    = idom
          icon_grid_level(ilev)%level = ilev
          icon_grid_level(ilev)%parent_id = 0
       ENDIF

    
       !HA debug:
        print *,'read target domains grid'
       ! read grid information and coordinates
        CALL  read_domain_info_part(TRIM(filename), icon_grid_region(idom))
       IF (idom==first_dom) then
        ! copy data structure
        icon_grid_level(start_lev)=icon_grid_region(first_dom)
       ENDIF
        print *,'idom: ',idom
        print *,'icon_grid_region(idom)%ncells ',icon_grid_region(idom)%ncells


       ENDDO
      !  read grid quantities from files produced by grid refinement



       ! set target grid description
        print *,'set target grid description tg'
        idom = 1

        tg%igrid_type = 1  ! ICON grid
        tg%ie = icon_grid_region(idom)%ncells
        print *,'idom: ',idom
        print *,'icon_grid_region(idom)%ncells ',icon_grid_region(idom)%ncells

         print *,'ICON_grid%n_dom: ', ICON_grid%n_dom


        DO idom=first_dom+1, ICON_grid%n_dom ! the first icon domain (DOM01) does not necessarily have the maximum number of grid cells
        print *,'idom: ',idom
        print *,'icon_grid_region(idom)%ncells ',icon_grid_region(idom)%ncells

           IF (icon_grid_region(idom)%ncells > tg%ie) THEN
             tg%ie = icon_grid_region(idom)%ncells
           ENDIF
        ENDDO

        tg%je = ICON_grid%n_dom   ! use second dimension in target grid structure for different ICON domains
        tg%ke = 1

        ! target grid set
        ! allocate lon_geo and lat_geo
        CALL allocate_com_target_fields(tg)

        k = 1
        

        DO j=1, tg%je

          DO i=1, tg%ie
              lon_geo(i,j,k) =  rad2deg * icon_grid_region(j)%cells%center(i)%lon ! convert from radians to degrees
              lat_geo(i,j,k) =  rad2deg * icon_grid_region(j)%cells%center(i)%lat ! convert from radians to degrees

          ENDDO
      ENDDO




    


  END SUBROUTINE allocate_icon_grid





END MODULE mo_icon_grid_routines

