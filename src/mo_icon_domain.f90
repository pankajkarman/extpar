!+ Fortran module to specify data structures for ICON Grid 
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_2         2011/03/25 Hermann Asensio
!  Update documentation
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module to specify data structures for ICON Grid 
!!
!!
MODULE mo_icon_domain

   USE mo_kind,            ONLY: wp, &
                                 i4

   USE mo_exception,       ONLY: message_text, message, finish

   USE mo_base_geometry,   ONLY: vector_product,              &
                                 inter_section,               &
                                 cc2gc,                       &
                                 gc2cc,                       &
                                 cos_arc_length,              &
                                 arc_length,                  &
                                 cartesian_coordinates,       &
                                 coordinates_reference,       &
                                 geographical_coordinates,    &
                                 tangent_vectors

                                 
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: icon_domain
  PUBLIC :: construct_icon_domain
  PUBLIC :: destruct_icon_domain
  PUBLIC :: grid_vertices
  PUBLIC :: grid_cells
            
  PUBLIC :: max_dom

  !> Maximum allowed number of model domains
  INTEGER, PARAMETER :: max_dom = 10
! from mo_grid.f90 of icon_src
!--------------------------------------------------------------
  !> data structure for Icon grid vertices
  TYPE grid_vertices
    INTEGER, ALLOCATABLE  :: idx(:)              ! the index of the vertex
    INTEGER, ALLOCATABLE  :: noOfNeigbors(:)     ! connectivity number = no of neighboring vertices, cells, edges
    INTEGER, ALLOCATABLE  :: neighbor_index(:,:) ! neigbor idx, 1 to noOfNeigbors
    INTEGER, ALLOCATABLE  :: cell_index(:,:)     ! cell idx, 1 to noOfNeigbors
    INTEGER, ALLOCATABLE  :: edge_index(:,:)     ! edge idx, 1 to noOfNeigbors
    INTEGER, ALLOCATABLE  :: edge_orientation(:,:)! +1: from this to neigbor vertex is the same orientation as the edge unit tangent vector
                                             ! -1: opposite orientation
                                              
    REAL(wp), ALLOCATABLE :: dual_area(:)        ! area of dual cell
    TYPE(geographical_coordinates), ALLOCATABLE :: vertex(:)  ! geographical coordinates of the vertices
    TYPE(cartesian_coordinates), ALLOCATABLE :: cc_vertex(:)  ! cartesian coordinats of the vertices
    INTEGER, ALLOCATABLE  :: refin_ctrl(:)       !
    INTEGER, ALLOCATABLE  :: child_id(:)            !
  END TYPE grid_vertices
!--------------------------------------------------------------

!--------------------------------------------------------------
  !> data structure for Icon grid cells
  TYPE grid_cells
    INTEGER, ALLOCATABLE ::  idx(:)           ! the index of the edge
    INTEGER, ALLOCATABLE  :: parent_index(:)  !
    INTEGER, ALLOCATABLE  :: child_index(:,:) !
    INTEGER, ALLOCATABLE  :: child_id(:)      !
    INTEGER, ALLOCATABLE  :: curr_id(:)       ! ID of current domain (needed in
                                          ! mo_gridrefinement only)

    INTEGER, ALLOCATABLE  :: noOfVertices(:)       ! no of cell vertices = no of edges = no of neibgoring cells
    INTEGER, ALLOCATABLE  :: neighbor_index(:,:)   ! neibgoring cells indeces, from 1 to noOfVertices 
    INTEGER, ALLOCATABLE  :: edge_index(:,:)       ! edge indeces, from 1 to noOfVertices 
    INTEGER, ALLOCATABLE  :: vertex_index(:,:)     ! vertex indeces, from 1 to noOfVertices  

    INTEGER, ALLOCATABLE  :: edge_orientation(:,:) ! defined according to Gauss formula 
                                               ! +1 : the normal to the edge is outwards, -1 : inwards
                                               ! for LEFT-HAND coord system the same is true for tangent vectors and Stokes form
                                               ! for RIGHT-HAND coord system it is reveresed for Stokes form: -edge_normal_direction()
                                               ! see TYPE grid,gridOrientation: 
                                               ! the tangent orientation for STOKES is GIVEN BY: edge_normal_direction()*gridOrientation 

    TYPE(geographical_coordinates), ALLOCATABLE  :: center(:) ! geographical coordinates of the geometric center
    TYPE(cartesian_coordinates), ALLOCATABLE  :: cc_center(:) ! cartesian coordinates of the geometric center
    REAL(wp), ALLOCATABLE :: area(:)             ! cell area
    INTEGER, ALLOCATABLE  :: refin_ctrl(:)       !
  END TYPE grid_cells
!--------------------------------------------------------------
! from mo_grid.f90


  !> Data type icon_domain contains basic grid structure for the ICON grid
  !!
  !! this type is a subset of the "patch" type of the grid generator (no edges etc.)
  TYPE icon_domain


    INTEGER :: id                             !< domain ID of current domain

    INTEGER :: parent_id                      !< domain ID of parent domain

    INTEGER, ALLOCATABLE :: child_id(:)           !< list of child domain ID's (dimension max_childdom)

    INTEGER :: max_childdom                   !< maximum number of child domains

    INTEGER :: n_childdom                     !< actual number of child domains

    INTEGER :: grid_root                    !< number of partitions of the icosahedron

    INTEGER :: level                        !< level in grid hierarchy on which icon_domain lives

    INTEGER :: ncells                       !< number of icon_domain items (total no. of cells) 

    INTEGER :: nverts                       !< number of icon_domain items (total no. of vertices) 

    TYPE(grid_cells)    :: cells            !< grid information on the cells

    TYPE(grid_vertices) :: verts            !< grid information on the vertices


  END TYPE icon_domain



 CONTAINS

 !> Allocates arrays for single icon_domain object.
 !! @par Revision History
 !! Developed  by Luca Bonaventura  (2005).
 !! Added maxCellVertices, maxVertexConnect, Leonidas, May 2009
 !! Adopted by Hermann Asensio, DWD, August 2009
 SUBROUTINE construct_icon_domain(p,            &
                            max_childdom,       &
                            n_childdom,         &
                            ncell,              &
                            nvertex,            &
                            nedge,              &
                            ncells_per_edge,    &
                            nvertex_per_cell,   &
                            nedges_per_vertex,  &
                            nchilds_per_cell    &
                            )
   TYPE(icon_domain), INTENT(INOUT) :: p                                   !< icon_domain objects

   INTEGER, INTENT(IN)                      :: max_childdom            !< maximum number of child domains
   INTEGER, INTENT(IN)                      :: n_childdom              !< actual number of child domains
   INTEGER, INTENT(IN)                      :: ncell                   !< number of cells
   INTEGER, INTENT(IN)                      :: nvertex                 !< number of edges
   INTEGER, INTENT(IN)                      :: nedge                   !< number of vertices
   INTEGER, INTENT(IN)                      :: ncells_per_edge         !< number of cells per edge
   INTEGER, INTENT(IN)                      :: nvertex_per_cell        !< number of vertices per cell
   INTEGER, INTENT(IN)                      :: nedges_per_vertex       !< number of edges per vertex
   INTEGER, INTENT(IN)                      :: nchilds_per_cell        !< number of child cells per cell

   ! local variables
   INTEGER :: istat, ist 

  INTEGER :: var_default

   var_default = 0



    p%id              = var_default
    p%parent_id       = var_default
    p%grid_root       = var_default
    p%level           = var_default

    p%max_childdom    = max_childdom
    p%n_childdom      = n_childdom
    p%ncells          = ncell
    p%nverts          = nvertex


    !--------------------------------------------------------------
      ist=0   


      ALLOCATE(p%child_id(max_childdom),STAT=istat)
          ist=ist+istat
      p%child_id(:) = var_default



!--------------------------------------------------------------
      ALLOCATE(p%cells%idx(ncell),STAT=istat)
          ist=ist+istat
      p%cells%idx(:)=0

      ALLOCATE(p%cells%parent_index(ncell),STAT=istat)
          ist=ist+istat
      p%cells%parent_index(:)=0

      ALLOCATE(p%cells%child_index(ncell,nchilds_per_cell),STAT=istat)
          ist=ist+istat
      p%cells%child_index(:,:)=0

      ALLOCATE(p%cells%child_id(ncell),STAT=istat)
          ist=ist+istat
      p%cells%child_id(:)=0

      ALLOCATE(p%cells%curr_id(ncell),STAT=istat)
          ist=ist+istat
      p%cells%curr_id(:)=0

      ALLOCATE(p%cells%neighbor_index(ncell,nvertex_per_cell) ,STAT=istat)  
          ist=ist+istat 
      p%cells%neighbor_index(:,:)=0

      ALLOCATE(p%cells%edge_index (ncell,nvertex_per_cell),STAT=istat)
          ist=ist+istat
      p%cells%edge_index (:,:)=0

      ALLOCATE(p%cells%vertex_index(ncell,nvertex_per_cell),STAT=istat)
          ist=ist+istat
      p%cells%vertex_index(:,:)=0

      ALLOCATE(p%cells%edge_orientation(ncell,nvertex_per_cell),STAT=istat)
          ist=ist+istat
      p%cells%edge_orientation(:,:)=0

      ALLOCATE(p%cells%center(ncell),STAT=istat)
          ist=ist+istat
      p%cells%center(:)%lon=0
      p%cells%center(:)%lat=0

      ALLOCATE(p%cells%cc_center(ncell),STAT=istat)
          ist=ist+istat
      p%cells%cc_center(:)%x(1)=0
      p%cells%cc_center(:)%x(2)=0
      p%cells%cc_center(:)%x(3)=0


      ALLOCATE(p%cells%area(ncell),STAT=istat)
          ist=ist+istat   
      p%cells%area(:)=0
 
      ALLOCATE(p%cells%refin_ctrl(ncell),STAT=istat)
          ist=ist+istat
      p%cells%refin_ctrl(:)=0


      ALLOCATE(p%cells%noOfVertices(ncell),STAT=istat)
          ist=ist+istat
      p%cells%noOfVertices(:)=0

!--------------------------------------------------------------
                                                         ! 
      ALLOCATE(p%verts%idx(nvertex),STAT=istat)
          ist=ist+istat
      p%verts%idx(:)=0

      ALLOCATE(p%verts%noOfNeigbors(nvertex),STAT=istat)
          ist=ist+istat
      p%verts%noOfNeigbors(:)=0

      ALLOCATE(p%verts%neighbor_index(nvertex,nedges_per_vertex) ,STAT=istat)  
          ist=ist+istat 
      p%verts%neighbor_index(:,:)=0

      ALLOCATE(p%verts%cell_index (nvertex,nedges_per_vertex),STAT=istat)
          ist=ist+istat
      p%verts%cell_index (:,:)=0

      ALLOCATE(p%verts%edge_index (nvertex,nedges_per_vertex),STAT=istat)
          ist=ist+istat
      p%verts%edge_index (:,:)=0

      ALLOCATE(p%verts%edge_orientation(nvertex,nedges_per_vertex),STAT=istat)
          ist=ist+istat
      p%verts%edge_orientation(:,:)=0

      ALLOCATE(p%verts%dual_area(nvertex),STAT=istat)
          ist=ist+istat
      p%verts%dual_area(:)=0

      ALLOCATE(p%verts%vertex(nvertex),STAT=istat)
          ist=ist+istat
      p%verts%vertex(:)%lon=0
      p%verts%vertex(:)%lat=0

      
      ALLOCATE(p%verts%cc_vertex(nvertex),STAT=istat)
          ist=ist+istat
      p%verts%cc_vertex(:)%x(1)=0
      p%verts%cc_vertex(:)%x(2)=0
      p%verts%cc_vertex(:)%x(3)=0


      ALLOCATE(p%verts%refin_ctrl(nvertex),STAT=istat)
          ist=ist+istat
      p%verts%refin_ctrl(:)=0

      ALLOCATE(p%verts%child_id(nvertex),STAT=istat)
          ist=ist+istat
      p%verts%child_id(:)=0

    IF (ist>0) THEN   
      WRITE (message_text, '(a,i8,a)') &
           'Generate grid with ', ncell, ' triangles.'
      CALL finish ('construct_icon_domain', TRIM(message_text))
    ENDIF

  END SUBROUTINE construct_icon_domain

!> Deallocates arrays for single grid object.
!! @par REVISION HISTORY:  
!! Developed  by Luca Bonaventura  (2005).
!! Adopted by Hermann Asensio, DWD, 2009
  SUBROUTINE destruct_icon_domain(p)


  INTEGER :: istat, ist  
  TYPE(icon_domain), INTENT(INOUT) :: p
            
          ist=0   
      DEALLOCATE(p%cells%idx,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%parent_index,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%child_index,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%child_id,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%curr_id,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%noOfVertices,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%neighbor_index ,STAT=istat)  
          ist=ist+istat 
      DEALLOCATE(p%cells%edge_index,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%vertex_index,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%edge_orientation,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%center,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%cc_center,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%cells%area,STAT=istat)
          ist=ist+istat    
      DEALLOCATE(p%cells%refin_ctrl,STAT=istat)
          ist=ist+istat

      DEALLOCATE(p%verts%idx,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%noOfNeigbors,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%neighbor_index,STAT=istat)  
          ist=ist+istat 
      DEALLOCATE(p%verts%cell_index,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%edge_index,STAT=istat)

          ist=ist+istat
      DEALLOCATE(p%verts%edge_orientation,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%dual_area,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%vertex,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%cc_vertex,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%refin_ctrl,STAT=istat)
          ist=ist+istat
      DEALLOCATE(p%verts%child_id,STAT=istat)
          ist=ist+istat
    
    IF (ist>0) THEN   
      WRITE (message_text, '(a)') 'Deallocate grid.'
      CALL message ('destruct_icon_domain', 'Deallocate grid.')
    ENDIF

  END SUBROUTINE destruct_icon_domain










END MODULE mo_icon_domain
