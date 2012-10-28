!+ Fortran module to find grid element index in ICON grid
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_2         2011/03/25 Hermann Asensio
!  add point in polygon test for ICON grid search
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!>
!! Fortran module to find grid element index in ICON grid
!!
!! @author Hermann Asensio, DWD
!!
!! @par Revision History
!! Initial realease by Hermann Asensio (2009-07-31)
!!
MODULE mo_search_icongrid


  USE mo_kind,            ONLY: wp, i4, i8
  USE mo_io_units,        ONLY: filename_max
  USE mo_exception,       ONLY: message_text, message, finish

  USE mo_additional_geometry,   ONLY: cc2gc,                  &
                                gc2cc,                  &
                                arc_length,             &
                                cos_arc_length,         &
                                scal_pro,               &
                                inter_section,          &
                                vector_product,         &
                                point_in_grid_element

 USE mo_base_geometry, ONLY: geographical_coordinates
 USE mo_base_geometry, ONLY: cartesian_coordinates

  USE mo_grid_structures, ONLY: icosahedral_triangular_grid, icon_grid_def
  USE mo_icon_domain,          ONLY: icon_domain,                &
                                grid_cells,               &
                                grid_vertices,            &
                                construct_icon_domain,    &
                                destruct_icon_domain

  IMPLICIT NONE

  PRIVATE

  CHARACTER(len=*), PARAMETER :: version = '$Id: mo_search_icongrid.f90,v 1.3 2011/04/19 08:11:30 for0adm Exp $'

  PUBLIC :: walk_to_nc
  PUBLIC :: find_nc
  PUBLIC :: find_nc_dom1
  PUBLIC :: find_nchild_nlev
  PUBLIC :: find_nearest_vert

  CONTAINS

  !> Find the nearest grid cell index in the ICON grid for given (geographical) target coordinates. 
  !!
  !! Search for nearest grid point for given (geographical) target coordinates.
  !! Start at the highest level (level = 0) and find the nearest grid element at this level.
  !! Proceed to the next level and check there for the four child cell which is the nearest grid cell.
  !! Repeat the last step until the first domain level (start_lev of the Namelist input for the grid generator) is reached.
  !! If further refinement domains are given, search also in the refinement domains and put result in array nearest_cell_ids(ndom).
  SUBROUTINE find_nc(target_geo_co,               &
    &                n_dom,                       &
    &                nvertex_per_cell,            &
    &                icon_dom_def,                &
    &                icon_grid_region,            &
    &                nearest_cell_ids)

    TYPE(geographical_coordinates), INTENT(IN)  :: target_geo_co    !< target coordinates in geographical system of point for which the nearest ICON grid cell is to be determined
    INTEGER, INTENT(IN) :: n_dom                        !< number of model domains
    INTEGER, INTENT(IN) :: nvertex_per_cell             !< number of vertices per cell
    TYPE(icon_grid_def), INTENT(IN) :: icon_dom_def !< structure which contains the definition of the ICON grid
    TYPE(icon_domain), INTENT(IN), TARGET :: icon_grid_region(1:n_dom)    !< Data structure with ICON domain with refinement domain,  dimension (1:ndom)

    INTEGER (KIND=i8), INTENT(OUT) :: nearest_cell_ids(1:n_dom)    !< array with ids of nearest cell for the domains

    ! local variables
    INTEGER (KIND=i8), SAVE  :: start_cell_id = 1   !< id of starting cell
    INTEGER            :: ilev             !< counter for level
    INTEGER            :: idom             !< counter for domain
    INTEGER :: nchilds_per_cell             !< number of child cells per cell
    INTEGER (KIND=i8)          :: clev_ncell_id    !< nearest cell id on current level
    INTEGER (KIND=i8)          :: nlev_ncell_id    !< id of nearest cell on next level

    TYPE(cartesian_coordinates)  :: target_cc_co     !< coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined

    TYPE(cartesian_coordinates)  :: cell_cc          !< of cell centre in cartesian system 
    TYPE(cartesian_coordinates)  :: neighbour_cc     !< of a neighbour cell centre in cartesian system
    INTEGER (KIND=i8)            :: nb_cell_id       !< cell id

    INTEGER (KIND=i8)            :: current_cell_id
    INTEGER (KIND=i8)            :: next_cell_id

    REAL(KIND=wp)                :: sp               !< arc length of  of geodesic arc with endpoints x0,x1 (normalized scalar product of the two points)
    REAL(KIND=wp)                :: sp_max           !< of the scalar product of two points (minimal distance)
    INTEGER :: undefined
    INTEGER :: clev  !< current level
    INTEGER :: ichild      !< counter for child index
    INTEGER :: id_child_i  !< id of next level child index
    INTEGER (KIND=i8) :: nearest_cell_id_dom1    !<  id of nearest cell for the domain 1
    INTEGER :: child_dom_id   ! id of child domain
    INTEGER :: idom_m          ! counter for domains
    INTEGER :: idom_o          ! counter for domains
    LOGICAL :: l_child_dom     ! logical switch if child domain exists
    TYPE(cartesian_coordinates)  :: cc_vertices(1:nvertex_per_cell) ! cartesian coordinates of vertices of grid element for point in polygon test
    INTEGER  (KIND=i8)           :: ivert            !< counter
    INTEGER  (KIND=i8)           :: vert_index       !< index
    INTEGER                      :: inflag

    TYPE(cartesian_coordinates)  :: vert_cc     !< coordinates of a vertex in cartesian system
    INTEGER (KIND=i8)            :: n_vert_id   !< vertex id
    INTEGER (KIND=i8)            :: nev         !< counter

    !!HA debug
   ! print *,'Entering subroutine find_nc'
    undefined = 0
    nearest_cell_ids = undefined ! set the nearest_cell_id for all domains to "undefined" as default
    ! transform geographical coordinates to cartesian coordinates of target point
    target_cc_co = gc2cc(target_geo_co)
    ilev            = 1 ! start with highest level
    nchilds_per_cell = icon_dom_def%nchilds_per_cell ! number of child cells per cell

    CALL walk_to_nc(icon_grid_region(ilev),   &
           &             target_cc_co,     &
           &             start_cell_id,    &
           &             nvertex_per_cell, &
           &             clev_ncell_id)
    nearest_cell_ids(ilev) = clev_ncell_id ! this is the nearest cell index for domain 1
    start_cell_id = clev_ncell_id ! save for next search
    ! check with a point in polygon test
    CALL control_nc( icon_grid_region(ilev), &
       &                  target_cc_co,     &
       &                  icon_dom_def%nedges_per_vertex,  &
       &                  nvertex_per_cell, &
       &                  nearest_cell_ids(ilev))

   idom = 1 ! start with domain index 1
    IF  (icon_grid_region(idom)%n_childdom == 0 ) then  ! no child domain exists
      l_child_dom = .false.
    ELSE
      l_child_dom = .true.
    ENDIF

    DO WHILE (l_child_dom)
      l_child_dom = .false.  ! set loop condition to .false., only set to .true. if there are more child domains (see below).

      child_dom_id = icon_grid_region(idom)%cells%child_id(nearest_cell_ids(idom)) ! the domain id for the child grid cells (higher resolution)
      IF (child_dom_id > 0 ) then ! if there is a refinement point, get index for next level
        CALL find_nchild_nlev(target_cc_co,       &
          &                   icon_grid_region(idom),         &
          &                   icon_grid_region(child_dom_id), &
          &                   nvertex_per_cell,               &
          &                   nchilds_per_cell,               &
          &                   nearest_cell_ids(idom),         &
          &                   nearest_cell_ids(child_dom_id))
         ! check with a point in polygon test
         CALL control_nc(icon_grid_region(child_dom_id), &
           &             target_cc_co,     &
           &             icon_dom_def%nedges_per_vertex,  &
           &             nvertex_per_cell, &
           &             nearest_cell_ids(child_dom_id))
        IF (icon_grid_region(idom)%n_childdom > 0 ) then  ! child domain exists
          l_child_dom = .true. ! continue with loop
          idom = child_dom_id  ! set idom to next domain id
        ENDIF
      ENDIF
    ENDDO ! WHILE(l_child_dom) loop
    
  END SUBROUTINE find_nc

  
  !> Find the nearest grid cell index in the ICON grid for given (geographical) target coordinates for Domain "1". 
  !!
  !! Search for nearest grid point for given (geographical) target coordinates.
  !! Start at the highest level (level = 0) and find the nearest grid element at this level.
  !! Proceed to the next level and check there for the four child cell which is the nearest grid cell.
  !! Repeat the last step until the first domain level (start_lev of the Namelist input for the grid generator) is reached.
  SUBROUTINE find_nc_dom1(target_cc_co,               &
                     icon_grid_level,             &
                     start_lev,                   &
                     nvertex_per_cell,            &
                     nchilds_per_cell,            &
                     nearest_cell_id_dom1)

    TYPE(cartesian_coordinates), INTENT(IN)  :: target_cc_co     !< coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined
    INTEGER, INTENT(IN) :: start_lev                    !< level of (first) global model domain
    TYPE(icon_domain), INTENT(IN), TARGET :: icon_grid_level(0:start_lev)     !< Data structure with ICON grid with levels above first domain, including domain "1", dimension (0:start_lev)
    INTEGER, INTENT(IN) :: nvertex_per_cell             !< number of vertices per cell
    INTEGER, INTENT(IN) :: nchilds_per_cell             !< number of child cells per cell
    INTEGER (KIND=i8), INTENT(OUT) :: nearest_cell_id_dom1    !<  id of nearest cell for the domain 1


    ! local variables

    INTEGER  (KIND=i8)          :: start_cell_id    !< id of starting cell
    INTEGER  (KIND=i8)         :: ilev             !< counter for level

    INTEGER  (KIND=i8)          :: clev_ncell_id    !< nearest cell id on current level
    INTEGER  (KIND=i8)          :: nlev_ncell_id    !< id of nearest cell on next level


    TYPE(cartesian_coordinates)  :: cell_cc          !< coordinates of cell centre in cartesian system 
    TYPE(cartesian_coordinates)  :: neighbour_cc     !< coordinates of a neighbour cell centre in cartesian system
    INTEGER (KIND=i8)            :: nb_cell_id       !< cell id

    INTEGER  (KIND=i8)           :: current_cell_id
    INTEGER  (KIND=i8)           :: next_cell_id
   !print *,'entering find_nc_dom1'
    start_cell_id   = 1 ! start with cell id 1
    ilev            = 0 ! start with highest level  
   CALL walk_to_nc(icon_grid_level(ilev), target_cc_co, start_cell_id, nvertex_per_cell, clev_ncell_id) 

   ! step down the hierachy
   DO ilev=0, start_lev -1 
     CALL find_nchild_nlev(target_cc_co,          &
                     icon_grid_level(ilev),       &
                     icon_grid_level(ilev+1),     &
                     nvertex_per_cell,            &
                     nchilds_per_cell,            &
                     clev_ncell_id,               &
                     nlev_ncell_id)

        clev_ncell_id = nlev_ncell_id

   ENDDO
   nearest_cell_id_dom1 = nlev_ncell_id ! this is the nearest cell index for domain 1
   ! print *,'done find_nc_dom1'
  END SUBROUTINE find_nc_dom1


  !> Find the nearest grid cell index in the ICON grid on the next level 
  !!
  !! Check the four child cell which is the nearest grid cell.
  SUBROUTINE find_nchild_nlev(target_cc_co,       &
                     icon_grid_current_level,     &
                     icon_grid_next_level,        &
                     nvertex_per_cell,            &
                     nchilds_per_cell,            &
                     clev_ncell_id,               &
                     nlev_ncell_id)

  
    TYPE(cartesian_coordinates), INTENT(IN)  :: target_cc_co    !< target coordinates in geographical system of point for which the nearest ICON grid cell is to be determined

    TYPE(icon_domain), INTENT(IN), TARGET :: icon_grid_current_level  !< Data structure with ICON grid level

    TYPE(icon_domain), INTENT(IN), TARGET :: icon_grid_next_level     !< Data structure with ICON grid with level +1



    INTEGER, INTENT(IN) :: nvertex_per_cell     !< number of vertices per cell

    INTEGER, INTENT(IN) :: nchilds_per_cell     !< number of child cells per cell

    INTEGER (KIND=i8), INTENT(IN) :: clev_ncell_id        !< nearest cell id on current level

    INTEGER (KIND=i8), INTENT(OUT):: nlev_ncell_id        !< id of nearest cell on next level


    ! local variables

    INTEGER (KIND=i8) :: id_child_vec(1:nchilds_per_cell)  !< child indeces on next level
    TYPE(cartesian_coordinates)  :: cell_cc_vec(1:nchilds_per_cell)  !> coordinates of cell centre in cartesian system
     REAL(KIND=wp)    :: sp_vec(1:nchilds_per_cell) !> cos arc length of  of geodesic arc with endpoints x0,x1 (normalized scalar product of the two points)
     INTEGER :: max_index

    id_child_vec(:) = icon_grid_current_level%cells%child_index(clev_ncell_id,:) ! get indices of child cells
    cell_cc_vec(:) = icon_grid_next_level%cells%cc_center(id_child_vec)! cartesian coordinates of child cell centres
    ! calculate a measure for the distance to target point
    sp_vec(:) = scal_pro(target_cc_co, cell_cc_vec(:))
    ! get the point wich is nearest to the target point
    max_index = MAXLOC(sp_vec,DIM=1)
    nlev_ncell_id = id_child_vec(max_index)

  END SUBROUTINE  find_nchild_nlev

  !> Go to the nearest grid cell in the ICON grid 
  !!
  !! Search for nearest grid point for given (geographical) target coordinates.
  !! Walk from a (given) starting cell in the direction of the target coordintes
  !! until the neighbour cells have a larger distance to the current cell.
  !! This algorithm works on global domains, for regional domains the search
  !! might get stuck at the boundaries of the domains.
  SUBROUTINE walk_to_nc(grid,             &
    &                   target_cc_co,     &
    &                   start_cell_id,    &
    &                   nvertex_per_cell, &
    &                   nearest_cell_id)
    TYPE(icon_domain), INTENT(IN)               :: grid             !> Data structure with ICON grid
    TYPE(cartesian_coordinates), INTENT(IN)     :: target_cc_co     !>  target coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined
    INTEGER (KIND=i8), INTENT(IN)               :: start_cell_id    !> id of starting cell
    INTEGER, INTENT(IN)                         :: nvertex_per_cell        !< number of vertices per cell
    INTEGER (KIND=i8), INTENT(OUT)              :: nearest_cell_id  !> id of nearest cell

    ! local variables
    TYPE(cartesian_coordinates)  :: cell_cc          !> coordinates of cell centre in cartesian system 
    TYPE(cartesian_coordinates)  :: neighbour_cc     !> coordinates of a neighbour cell centre in cartesian system
    INTEGER                      :: nb_cell_id       !> neighbour cell id

    INTEGER   (KIND=i8)          :: current_cell_id
    INTEGER   (KIND=i8)          :: next_cell_id

    REAL(KIND=wp)                :: sp               !> cos arc length of  of geodesic arc with endpoints x0,x1 (normalized scalar product of the two points)
    REAL(KIND=wp)                :: sp_max           !> maximum of the scalar product of two points (minimal distance)

    LOGICAL                      :: searching

    INTEGER                      :: nj ! counter

    !PRINT *,'entering walk_to_nc'
    searching = .TRUE.   ! set searching to "true"

    nearest_cell_id = start_cell_id ! initial setting
    current_cell_id = start_cell_id ! initial setting
    next_cell_id    = start_cell_id ! initial setting 

    ! cartesian coordinates of start cell centre
    cell_cc =  grid%cells%cc_center(start_cell_id)
    ! calculate a measure for the distance to target point
    sp = scal_pro(target_cc_co, cell_cc)
    sp_max = sp

    DO WHILE(searching)
    searching = .false. ! abort condition
     DO nj=1, nvertex_per_cell
       nb_cell_id = grid%cells%neighbor_index(current_cell_id,nj) ! get cell id of neighbour cells
       IF (nb_cell_id > 0 ) THEN ! 0 is the "undefined" value for the cell id
         neighbour_cc = grid%cells%cc_center(nb_cell_id)        ! get cartesian coordinates of neighbour cell
         !sp = cos_arc_length(target_cc_co,neighbour_cc)             ! calculate measure for distance to target point
         sp = scal_pro(target_cc_co,neighbour_cc)             ! calculate measure for distance to target point
         IF (sp > sp_max) THEN              ! if neighbour cell is nearer to target point than the "old" cell
          sp_max = sp                       ! save new distance measure
          next_cell_id = nb_cell_id         ! save cell id
          searching = .true.                ! continue with search loop
         ENDIF
       ENDIF
     ENDDO
     current_cell_id = next_cell_id       ! move one cell toward target point
    ENDDO
    nearest_cell_id = current_cell_id     ! set nearest_cell_id to the cell id 
                                          ! which has smallest distance (i.e. largest sp) to target point

  END SUBROUTINE walk_to_nc

   !> Find the nearest vertex for a given grid cell in the ICON grid 
  !!
  !! Check the distance of the target point to the vertices
  !! give out the id of the nearest vertex
  SUBROUTINE find_nearest_vert(grid,      &
                        target_cc_co,     &
                        cell_id,          &
                        nvertex_per_cell, &
                        nearest_vert_id)
    TYPE(icon_domain), INTENT(IN)               :: grid             !> Data structure with ICON grid
    TYPE(cartesian_coordinates), INTENT(IN)     :: target_cc_co     !>  target coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined
    INTEGER (KIND=i8), INTENT(IN)               :: cell_id    !> id of cell
    INTEGER, INTENT(IN)                         :: nvertex_per_cell        !< number of vertices per cell
    
    INTEGER (KIND=i8), INTENT(OUT)              :: nearest_vert_id  !> id of nearest cell


    ! local variables
    INTEGER (KIND=i8)           :: vert_id_vec(1:nvertex_per_cell) !< indices of cells vertices
    TYPE(cartesian_coordinates) :: vert_cc_vec(1:nvertex_per_cell) !< coordinates of cell vertices in cartesion coordinates
    REAL(KIND=wp)               :: sp_vec(1:nvertex_per_cell)      !< cos arc length of  of geodesic arc 
                                                         !! is used as a distance measure  
   INTEGER :: max_index

   vert_id_vec(:) = grid%cells%vertex_index(cell_id,:) ! get the indices of the cells vertices

   vert_cc_vec(:) = grid%verts%cc_vertex(vert_id_vec)  ! get the coordinates of the cells vertices

   ! calculate a measure for the distance to target point,
   ! the sp value is between [-1,1], with 1 for identical points and -1 for antipodes (on the sphere!)
    sp_vec(:) = scal_pro(target_cc_co, vert_cc_vec(:)) 

    max_index = MAXLOC(sp_vec,DIM=1)  ! the 
    nearest_vert_id = vert_id_vec(max_index)

    
  END SUBROUTINE find_nearest_vert

  !> control with a point in polygon test, if nearest cell contains search point
  !! if not, find the correct the cell id
  SUBROUTINE control_nc(grid,             &
     &                  target_cc_co,     &
     &                  ncells_per_vertex,  &
     &                  nvertex_per_cell, &
     &                  nearest_cell_id)
    TYPE(icon_domain), INTENT(IN)               :: grid              !< Data structure with ICON grid
    TYPE(cartesian_coordinates), INTENT(IN)     :: target_cc_co      !<  target coordinates in cartesian system of point for which the nearest ICON grid cell is to be determined
    INTEGER, INTENT(IN)                         :: ncells_per_vertex !< number of cells per vertex
    INTEGER, INTENT(IN)                         :: nvertex_per_cell  !< number of vertices per cell
    INTEGER (KIND=i8), INTENT(INOUT)            :: nearest_cell_id  !> id of nearest cell

    ! local variables
    TYPE(cartesian_coordinates)  :: cc_vertices(1:nvertex_per_cell) ! cartesian coordinates of vertices of grid element for point in polygon test
    INTEGER  (KIND=i8)           :: ivert            !< counter
    INTEGER  (KIND=i8)           :: vert_index       !< index
    INTEGER                      :: inflag
    TYPE(cartesian_coordinates)  :: vert_cc     !< coordinates of a vertex in cartesian system
    INTEGER (KIND=i8)            :: n_vert_id   !< vertex id
    INTEGER (KIND=i8)            :: nb_cell_id       !< cell id
    INTEGER (KIND=i8)            :: nev         !< counter

    DO ivert=1,nvertex_per_cell
       vert_index = grid%cells%vertex_index(nearest_cell_id,ivert)
       cc_vertices(ivert) = grid%verts%cc_vertex(vert_index)
    ENDDO
    CALL point_in_grid_element(target_cc_co,nvertex_per_cell,cc_vertices,inflag)
            !PRINT *,'--HA debug SUBROUTINE control_nc --'
            !PRINT *,'inflag: ',inflag
            !PRINT *,'--HA debug SUBROUTINE control_nc --'

    IF (inflag > 0) THEN ! point in grid element
      RETURN
    ELSE
      CALL find_nearest_vert(grid, &
             &               target_cc_co,     &
             &               nearest_cell_id,  &
             &               nvertex_per_cell, &
             &               n_vert_id)

      DO nev=1,ncells_per_vertex
         nb_cell_id = grid%verts%cell_index(n_vert_id,nev) ! neighbour cell index
         IF (nb_cell_id > 0) THEN
           DO ivert=1,nvertex_per_cell
             vert_index = grid%cells%vertex_index(nb_cell_id,ivert)
             cc_vertices(ivert) = grid%verts%cc_vertex(vert_index)
           ENDDO
           CALL point_in_grid_element(target_cc_co,nvertex_per_cell,cc_vertices,inflag)
           IF (inflag > 0) THEN ! point in grid element
             nearest_cell_id = nb_cell_id
             RETURN
           ENDIF
         ENDIF
      ENDDO
    ENDIF
  END SUBROUTINE control_nc


END MODULE mo_search_icongrid


