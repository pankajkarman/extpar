!+ Fortran module with data fields for soil data
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
!> Fortran module with data fields for soil data
!> \author Hermann Asensio
MODULE mo_soil_data

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar

USE mo_GRID_structures, ONLY: reg_lonlat_grid
                           
IMPLICIT NONE

PRIVATE

PUBLIC :: allocate_raw_soil_fields, &
          dsmw_legend, &
          soil_texslo, &
          dsmw_soil_unit, &
          n_unit,         &
          dsmw_grid, &
          lon_soil, &
          lat_soil

PUBLIC :: undef_soiltype, default_soiltype, soiltype_ice, soiltype_water


!> Definition of Data Type to describe the legend for the FAO Digital Soil Map of the World
TYPE :: dsmw_legend
     INTEGER (KIND=i4):: dsmw_code      !< Code number for FAO Digital Soil Map of the World
     ! secondary code legend: 
     ! water = 9000 
     ! glacier, ice = 9001
     ! rock = 9002 
     ! salt = 9003 
     ! histosols (e.g. peat) = 9004 
     ! dunes, shifting sands = 9005 
     ! no data = 9009 
     ! standard classification < 9000
     REAL (KIND=wp)      :: tex_coarse    !< Coarse textured part of the soil unit area
     REAL (KIND=wp)      :: tex_medium    !< Medium textured part of the soil unit area
     REAL (KIND=wp)      :: tex_fine      !< Fine textured part of the soil unit area
     REAL (KIND=wp)      :: part_undefined!< Undefined part of the soil unit area
     REAL (KIND=wp)      :: flat          !< Flat slope part of the soil unit area
     REAL (KIND=wp)      :: hilly         !< Hilly slope part of the soil unit area
     REAL (KIND=wp)      :: steep         !< Steep slope part of the soil unit area

END TYPE dsmw_legend

TYPE(dsmw_legend), ALLOCATABLE :: soil_texslo(:)                  !< legend for DSMW with texture and slope information

INTEGER :: n_unit   !< number of soil units

INTEGER (KIND=i4), ALLOCATABLE :: dsmw_soil_unit(:,:) !< FAO Digital Soil Map of the World, the values represent the soil unit number (see for legend in variable soil_texslo)


TYPE(reg_lonlat_grid) :: dsmw_grid !< structure with defenition of the raw data grid for the FAO Digital Soil Map of the World

REAL (KIND=wp), ALLOCATABLE  :: lon_soil(:)          !< longitide coordinates of the soil grid in the geographical (lonlat) system, dimension (nlon_reg)
REAL (KIND=wp), ALLOCATABLE  :: lat_soil(:)          !< latitude coordinates of the soil grid in the geographical (lonlat) system, dimension (nlat_reg)

INTEGER (KIND=i4) :: undef_soiltype = 0   !< undefined value for soil type (ocean/no data)
INTEGER (KIND=i4) :: default_soiltype = 5 !< default soil type loam (5)
INTEGER (KIND=i4) :: soiltype_ice = 1     !< soiltype for ice
INTEGER (KIND=i4) :: soiltype_water = 9   !< soiltype for water


CONTAINS

  !> allocate raw data fields
  SUBROUTINE allocate_raw_soil_fields(ncolumns,nrows,n_units)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: ncolumns !< number of columns
  INTEGER , INTENT(IN) :: nrows    !< number of rows
  INTEGER , INTENT(IN) :: n_units   !< number of soil units


  INTEGER :: errorcode !< error status variable


   ALLOCATE(dsmw_soil_unit(1:ncolumns,1:nrows), STAT=errorcode) ! allocate dsmw_soil_unit
      IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the field dsmw_soil_unit')
    dsmw_soil_unit = 0 ! _FillValue of the DSMW

      ALLOCATE(soil_texslo(1:n_units), STAT=errorcode) ! allocate soil_texslo
       IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array soil_texslo')

     soil_texslo(:)%dsmw_code = 9009 ! no data flag of FAO

     soil_texslo(:)%tex_coarse = 0.

     soil_texslo(:)%tex_medium = 0.

     soil_texslo(:)%tex_fine = 0.

     soil_texslo(:)%part_undefined = 0.
     
     soil_texslo(:)%flat = 0.

     soil_texslo(:)%hilly = 0.

     soil_texslo(:)%steep = 0.



   ALLOCATE(lon_soil(1:ncolumns), STAT=errorcode) 
      IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the field lon_soil')
    lon_soil = 0. ! _FillValue of the DSMW
 
   ALLOCATE(lat_soil(1:nrows), STAT=errorcode) 
      IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the field lat_soil')
    lat_soil = 0. ! _FillValue of the DSMW





  END  SUBROUTINE allocate_raw_soil_fields




END MODULE mo_soil_data
