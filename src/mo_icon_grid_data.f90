!+ Fortran module for icon grid data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_2         2011/03/25 Hermann Asensio 
!  update to support ICON refinement grids
! V1_3         2011/04/19 Hermann Asensio
!  introduce variables clon, clat, clon_vertices, clat_vertices
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for icon grid data 
!> \author Hermann Asensio
MODULE mo_icon_grid_data

  USE mo_icon_domain,  ONLY: icon_domain
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid
  USE mo_grid_structures, ONLY: icon_grid_def
  USE mo_utilities_extpar, ONLY: abort_extpar
  USE mo_kind,  ONLY: wp, &
    &                  i4, &
    &                  i8


  IMPLICIT NONE

  PRIVATE
  PUBLIC :: icon_grid

  PUBLIC :: icon_grid_region
  PUBLIC :: icon_grid_level
  PUBLIC :: nearest_icon_cells
  ! PUBLIC :: icon_domain_grid
  PUBLIC :: nvertex_dom
  PUBLIC :: ncells_dom

  PUBLIC :: icon_dom_nr
  PUBLIC :: n_dom
  PUBLIC :: nvertex_per_cell

  PUBLIC :: icon_dom_def
  PUBLIC :: init_icon_dom_def

  PUBLIC :: clon, clat
  PUBLIC :: clon_vertices, clat_vertices
  PUBLIC :: allocate_icon_coor

  TYPE(icosahedral_triangular_grid) :: icon_grid  !< structure which contains the definition of the ICON grid
  TYPE(icon_grid_def) :: icon_dom_def !< structure with the definition of the various ICON domains
  TYPE(icon_domain), ALLOCATABLE, TARGET :: icon_grid_region(:) 
  TYPE(icon_domain) , ALLOCATABLE, TARGET :: icon_grid_level(:)
  TYPE(icon_domain) :: icon_domain_grid !< structure for a ICON domain
  INTEGER (KIND=i8), ALLOCATABLE, TARGET ::  nearest_icon_cells(:)
  INTEGER, ALLOCATABLE :: nvertex_dom(:)  !< number of vertices in target domains  nvertex(n_dom)
  INTEGER, ALLOCATABLE :: ncells_dom(:)  !< number of cells in target domains  ncells(n_dom)

  INTEGER (KIND=i4) :: icon_dom_nr !< number of icon domain for target grid
  INTEGER (KIND=i4) :: n_dom       !< total number of model domains
  INTEGER (KIND=i4) :: nvertex_per_cell !< number of verices per Icon grid cell

  REAL (KIND=wp), ALLOCATABLE :: clon(:) !< longitude of Icon grid cells, (1:ncell), in radians
  REAL (KIND=wp), ALLOCATABLE :: clat(:) !< latitude of Icon grid cells, (1:ncell), in radians
  REAL (KIND=wp), ALLOCATABLE :: clon_vertices(:,:) !< longitude of Icon grid vertices, (1:nvertex_per_cell,1:ncell), in radians
  REAL (KIND=wp), ALLOCATABLE :: clat_vertices(:,:) !< latitude of Icon grid vertices, (1:nvertex_per_cell,1:ncell), in radians
  

  CONTAINS
    !> init array in componen icon_dom_def
    SUBROUTINE init_icon_dom_def(n_dom)

      INTEGER, INTENT(IN)  :: n_dom                        !< number of model domains
      ! local variables
      INTEGER :: istat, ist

      ist = 0
      istat = 0

      ALLOCATE(icon_dom_def%nchdom(n_dom),STAT=istat)
      ist = ist+istat

      ALLOCATE(icon_dom_def%ncell(n_dom),STAT=istat)
      ist = ist+istat

      ALLOCATE(icon_dom_def%nvertex(n_dom),STAT=istat)
      ist = ist+istat

      ALLOCATE(icon_dom_def%nedge(n_dom),STAT=istat)
      ist = ist+istat

      ALLOCATE(icon_dom_def%parent_ids(n_dom),STAT=istat)
      ist = ist+istat

      ALLOCATE(icon_dom_def%grid_files(n_dom),STAT=istat)
      ist = ist+istat

      IF (istat /= 0) THEN
        CALL abort_extpar('Error ALLOCATE icon_dom_def)')
      ENDIF

    END SUBROUTINE init_icon_dom_def

    !> alloate icon coordinate arrays
    SUBROUTINE allocate_icon_coor(ncell, nvertex_per_cell)

      INTEGER (KIND=i4), INTENT(IN)  :: ncell            !< number of cells
      INTEGER (KIND=i4), INTENT(IN)  :: nvertex_per_cell !< number of vertices per cell
      ! local variables
      INTEGER :: istat, ist

      ist = 0
      istat = 0

      ALLOCATE(clon(1:ncell),STAT=istat)
      ist = ist+istat

       ALLOCATE(clat(1:ncell),STAT=istat)
      ist = ist+istat

      ALLOCATE(clon_vertices(1:nvertex_per_cell,1:ncell),STAT=istat)
      ist = ist+istat

      ALLOCATE(clat_vertices(1:nvertex_per_cell,1:ncell),STAT=istat)
      ist = ist+istat


      IF (istat /= 0) THEN
        CALL abort_extpar('Error ALLOCATE allocate_icon_coor')
      ENDIF

    END SUBROUTINE allocate_icon_coor


END MODULE mo_icon_grid_data

