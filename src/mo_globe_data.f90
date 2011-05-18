!+ Fortran module with data fields for GLOBE data
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
!> Fortran module with data fields for GLOBE data
!> \author Hermann Asensio
MODULE mo_GLOBE_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_GRID_structures, ONLY: reg_lonlat_grid
                           
IMPLICIT NONE

PRIVATE

PUBLIC :: globe_tiles_grid, &
          globe_grid,       &
          ntiles_gl,        &
          nc_tot,           &
          nr_tot,           &
          nc_tile,          &
          globe_tiles_lon_min, &
          globe_tiles_lon_max, &
          globe_tiles_lat_min, &
          globe_tiles_lat_max, &
          globe_tiles_ncolumns, &
          globe_tiles_nrows,    &
          raw_globe_line,       &
          h_tile_row,           &
          raw_globe_block,      &
          allocate_raw_globe_fields



! the following parameters are taken from the GLOBE data description, http://www.ngdc.noaa.gov/mgg/topo/globe.html
INTEGER , PARAMETER :: ntiles_gl = 16 !< GLOBE raw data has 16 tiles

REAL (KIND=wp), PARAMETER :: globe_tiles_lon_min(1:ntiles_gl)   = &
      &(/-180.,-90.,0.,90.,-180.,-90.,0.,90.,-180.,-90.,0.,90.,-180.,-90.,0.,90. /)!< western boundary of GLOBE tile 
REAL (KIND=wp), PARAMETER :: globe_tiles_lon_max(1:ntiles_gl)  = &
      &(/-90.,0.,90.,180.,-90.,0.,90.,180.,-90.,0.,90.,180.,-90.,0.,90.,180. /) !< eastern boundary of GLOBE tile
REAL (KIND=wp), PARAMETER:: globe_tiles_lat_min(1:ntiles_gl) = &
      &(/ 50.,50.,50.,50.,0.,0.,0.,0.,-50.,-50.,-50.,-50.,-90.,-90.,-90.,-90. /) !< southern boundary of GLOBE tile
REAL (KIND=wp), PARAMETER :: globe_tiles_lat_max(1:ntiles_gl)  = &
      &(/ 90.,90.,90.,90.,50.,50.,50.,50.,0.,0.,0.,0.,-50.,-50.,-50.,-50. /) !< northern boundary of GLOBE tile
INTEGER , PARAMETER :: globe_tiles_ncolumns(1:ntiles_gl) = &
      & (/10800,10800,10800,10800,10800,10800,10800,10800,10800,10800,10800,10800,10800,10800,10800,10800 /) !< number of columns (nlon) in GLOBE tile
INTEGER , PARAMETER :: globe_tiles_nrows(1:ntiles_gl)   = &
      & (/ 4800, 4800, 4800, 4800, 6000,6000,6000,6000,6000,6000,6000,6000,4800,4800,4800,4800 /) !< number of rows (nlat) in GLOBE tile

INTEGER , PARAMETER :: nc_tot = 43200 !< total number of columns in GLOBE data
INTEGER , PARAMETER :: nr_tot = 21600 !< total number of rows in GLOBE data

INTEGER , PARAMETER :: nc_tile = 10800 !< number of columns in a GLOBE tile


TYPE(reg_lonlat_grid) :: globe_tiles_grid(1:ntiles_gl) !< structure with defenition of the raw data grid for the 16 GLOBE tiles

TYPE(reg_lonlat_grid) :: globe_grid !< structure with defenition of the raw data grid for the whole GLOBE dataset


REAL(KIND=wp)                             :: raw_globe_line(1:nc_tot)  !< one line with GLOBE data
REAL(KIND=wp), ALLOCATABLE                :: raw_globe_block(:,:) !< a block with GLOBE data

INTEGER :: h_tile_row(1:nc_tile)  !< variable for height of GLOBE data for a data row of a tile





CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_globe_fields(nrows,ncolumns)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: nrows !< number of rows
  INTEGER , INTENT(IN) :: ncolumns !< number of columns

  INTEGER :: errorcode !< error status variable


    ALLOCATE (raw_globe_block(1:ncolumns,1:nrows), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array raw_globe_block')
    raw_globe_block = 0.0


  END  SUBROUTINE allocate_raw_globe_fields




END MODULE mo_globe_data
