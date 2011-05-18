!+ Fortran module with structures to describe various grid types
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
!> Fortran module with structures to describe various grid types
!!
!! data type for the description of
!!  - rotated longitude latitude grid 
!!  - regular longitude latitude grid
!!  - icosahedral triangular grid 
!!  - gme style icosahedral triangular grid
!> \author Hermann Asensio
MODULE mo_grid_structures

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp
USE mo_kind, ONLY: i8
USE mo_kind, ONLY: i4

USE mo_io_units, ONLY: filename_max


IMPLICIT NONE

PRIVATE

PUBLIC :: reg_lonlat_grid
PUBLIC :: rotated_lonlat_grid
PUBLIC :: icosahedral_triangular_grid
PUBLIC :: gme_triangular_grid

PUBLIC :: target_grid_def

PUBLIC :: igrid_icon
PUBLIC :: igrid_cosmo
PUBLIC :: igrid_gme

!> Definition of data type with target grid definition 
TYPE :: target_grid_def
  INTEGER (KIND=i4) :: igrid_type  !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid
  INTEGER (KIND=i8) :: ie !< number of grid elements in first dimension of target grid
  INTEGER (KIND=i8) :: je !< number of grid elements in second dimension of target grid
  INTEGER (KIND=i8) :: ke !< number of grid elements in third dimension of target grid
END TYPE target_grid_def

TYPE(target_grid_def), ALLOCATABLE :: ndom_tg(:) !< structure with target grid description for ndom domains (e.g. Icon grid refinement areas)

INTEGER (KIND=i4), PARAMETER :: igrid_icon = 1 !< parameter to identify ICON grid
INTEGER (KIND=i4), PARAMETER :: igrid_cosmo = 2 !< parameter to identify COSMO grid
INTEGER (KIND=i4), PARAMETER :: igrid_gme = 3 !< parameter to identify GME grid



!> Definition of data type with icon grid definition 
TYPE :: icosahedral_triangular_grid
  INTEGER (KIND=i4) :: n_dom             !< number of icon model domains
  INTEGER (KIND=i4) :: start_lev         !< level of (first) global model domain
  INTEGER (KIND=i4) :: grid_root         !< number of partitions of the icosahedron
  INTEGER (KIND=i4) :: nvertex_per_cell  !< number of vertices per cell
  INTEGER (KIND=i4) :: nchilds_per_cell  !< number of child cells per cell
  INTEGER (KIND=i4) :: ncells_per_edge   !< number of cells per edge
  INTEGER (KIND=i4) :: nedges_per_vertex !< number of edges per vertex
  INTEGER (KIND=i4) :: nchdom            !< maximum number of child domains
  INTEGER (KIND=i4) :: ncell             !< number of cells
  INTEGER (KIND=i4) :: nvertex           !< number of vertices
  INTEGER (KIND=i4) :: nedge             !< number of edges
  CHARACTER (LEN=filename_max) :: nc_grid_file !< filename of grid file which contains the Icon grid coordinates 
END TYPE icosahedral_triangular_grid

!> Definition of data type with icon grid definition 
TYPE :: gme_triangular_grid
  INTEGER (KIND=i4) :: ni   !< resoultion parameter for GME grid, number of intervals on a main triangle side
  INTEGER (KIND=i4) :: ig1s !< first dimension of arrays, start index  (ig1s >= 0)
  INTEGER (KIND=i4) :: ig1e !< first dimension of arrays, end index    (ig1e <= ni)
  INTEGER (KIND=i4) :: ig2s !< second dimension of arrays, start index (ig2s >= 1)
  INTEGER (KIND=i4) :: ig2e !< second dimension of arrays, end index (ig2e >= 1)
  INTEGER (KIND=i4) :: nd   !< number of diamonds      (nd  = 10)
  INTEGER (KIND=i4) :: nip1   !< resoultion parameter for GME grid, number of intervals on a main triangle side + 1
END TYPE gme_triangular_grid
          

          

!> Definition of Data Type to describe a rotated lonlat grid
      TYPE :: rotated_lonlat_grid
          REAL  (KIND=wp) :: pollon !< longitude of the rotated north pole (in degrees, E>0)
          REAL  (KIND=wp) :: pollat !< latitude of the rotated north pole (in degrees, N>0)
          REAL  (KIND=wp) :: polgam !< longitude (in the rotated system) of the geographical north pole
          REAL  (KIND=wp)  :: startlon_rot !< transformed longitude of the lower left grid point of the total domain (in degrees, E>0)
          REAL  (KIND=wp)  :: startlat_rot   !< transformed latitude of the lower left grid point of the total domain (in degrees, N>0)
          REAL  (KIND=wp)  :: dlon_rot !< grid point distance in zonal direction (in degrees, rotated system)
          REAL  (KIND=wp)  :: dlat_rot !< grid point distance in meridional direction (in degrees, rotated system)
          INTEGER  (KIND=i8) :: nlon_rot !< number of grid points in zonal direction (rotated system)
          INTEGER  (KIND=i8) :: nlat_rot !< number of grid points in meridional direction (rotated system)
          INTEGER  (KIND=i8) :: ke_tot !< number of grid points in vertical direction
      END TYPE rotated_lonlat_grid





!> Definition of Data Type to describe a regular lonlat grid 
TYPE :: reg_lonlat_grid
       REAL (KIND=wp) :: start_lon_reg !< start longitude for regular lon-lat grid (usually western boundary)
       REAL (KIND=wp) :: end_lon_reg   !< end longitude for regular lon-lat grid (usually eastern boundary)

       REAL (KIND=wp) :: start_lat_reg !< start latitude for regular lon-lat grid (usually southern boundary, but northern boundary possible of dlat_reg has a negative increment (grid order from north to south)
       REAL (KIND=wp) :: end_lat_reg !< end latitude for regular lon-lat grid  (usually northern boundary, but southern boundary possible of dlat_reg has a negative increment (grid order from north to south)


       REAL (KIND=wp) :: dlon_reg !< grid point distance in zonal direction (in degrees) for regular lon-lat grid
       REAL (KIND=wp) :: dlat_reg !< grid point distance in meridional direction (in degrees) for regular lon-lat grid (negative increment for grid order from north to south)
       INTEGER (KIND=i8) :: nlon_reg !< number of grid elements in zonal direction for regular lon-lat grid
       INTEGER (KIND=i8) :: nlat_reg !< number of grid elements in meridional direction for regular lon-lat grid
END TYPE reg_lonlat_grid



END MODULE mo_grid_structures

