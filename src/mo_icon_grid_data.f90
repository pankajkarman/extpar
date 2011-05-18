!+ Fortran module for icon grid data
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
!> Fortran module for icon grid data 
!> \author Hermann Asensio
MODULE mo_icon_grid_data

  USE mo_icon_domain,  ONLY: icon_domain
  USE mo_grid_structures, ONLY: icosahedral_triangular_grid

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: icon_grid
  PUBLIC :: icon_grid_region
  PUBLIC :: icon_grid_level
  PUBLIC :: icon_domain_grid
  PUBLIC :: nvertex_dom
  PUBLIC :: ncells_dom
  TYPE(icosahedral_triangular_grid) :: icon_grid  !< structure which contains the definition of the ICON grid
  TYPE(icon_domain), ALLOCATABLE, TARGET :: icon_grid_region(:) 
  TYPE(icon_domain) , ALLOCATABLE, TARGET :: icon_grid_level(:)
  TYPE(icon_domain) :: icon_domain_grid !< structure for a ICON domain
  INTEGER, ALLOCATABLE :: nvertex_dom(:)  !< number of vertices in target domains  nvertex(n_dom)
  INTEGER, ALLOCATABLE :: ncells_dom(:)  !< number of cells in target domains  ncells(n_dom)

END MODULE mo_icon_grid_data

