!+ Fortran module with routines for the COSMO target grid 
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  small bug fixes accroding to Fortran compiler warnings
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with routines for the COSMO target grid 
!> definition of coordinates, input/ouptut routines
!> \author Hermann Asensio
MODULE mo_cosmo_grid


  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i8
  USE mo_kind, ONLY: i4


  USE mo_grid_structures, ONLY: rotated_lonlat_grid
  USE mo_grid_structures, ONLY: target_grid_def

  USE mo_io_utilities, ONLY: check_netcdf


  USE mo_utilities_extpar, ONLY: abort_extpar



  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_cosmo_domain_namelist
  PUBLIC :: calculate_cosmo_domain_coordinates
  PUBLIC :: get_cosmo_grid_info
  PUBLIC :: cosmo_grid
  PUBLIC :: lon_rot
  PUBLIC :: lat_rot
  PUBLIC :: allocate_cosmo_rc

  PUBLIC :: calculate_cosmo_target_coordinates


  REAL (KIND=wp), ALLOCATABLE  :: lon_rot(:)          !< longitide coordinates of the COSMO grid in the rotated system 
  REAL (KIND=wp), ALLOCATABLE  :: lat_rot(:)          !< latitude coordinates of the COSMO grid in the rotated system

  TYPE (rotated_lonlat_grid) :: cosmo_grid  !< structure which contains the definition of the COSMO grid


  CONTAINS

  !> allocate the variables for the rotated coordinates of the COSMO grid
  SUBROUTINE allocate_cosmo_rc(ie_tot,je_tot)

   INTEGER (KIND=i8), INTENT(IN) :: ie_tot !< number of COSMO grid elements in zonal direction (in the rotated system) 
   INTEGER (KIND=i8), INTENT(IN) :: je_tot !< number of COSMO grid elements in meridional direction (in the rotated system) 
   INTEGER :: errorcode
   
    ALLOCATE (lon_rot(1:ie_tot), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lon_rot')
    lon_rot = 0.0

    ALLOCATE (lat_rot(1:je_tot), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array lat_rot')
    lat_rot = 0.0



END SUBROUTINE allocate_cosmo_rc




!> read namelist with settings for COSMO target grid
!> \author Hermann Asensio
SUBROUTINE read_cosmo_domain_namelist(input_namelist_file, &
                                       COSMO_grid)
  !! read namelist file for the coordinates

  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number
  USE mo_io_units,         ONLY: filename_max
  USE mo_grid_structures,  ONLY: rotated_lonlat_grid ! Definition of Data Type to describe a rotated lonlat grid



    CHARACTER (len=filename_max), INTENT(in) :: input_namelist_file !< file with input namelist with COSMO grid definition
    ! Variables for domain
    TYPE(rotated_lonlat_grid), INTENT(OUT) :: COSMO_grid !< structure which contains the definition of the COSMO grid

     ! Variables for default values
      REAL  (KIND=wp)  :: pollon_d !< longitude of the rotated north pole (in degrees, E>0)
      REAL  (KIND=wp)  :: pollat_d !< latitude of the rotated north pole (in degrees, N>0)
      REAL  (KIND=wp)  :: polgam_d !< angle between the north poles of the systems
      REAL  (KIND=wp)  :: dlon_d !< grid point distance in zonal direction (in degrees)
      REAL  (KIND=wp)  :: dlat_d !< grid point distance in meridional direction (in degrees)
      REAL  (KIND=wp)  :: startlon_tot_d !< transformed longitude of the lower left grid point of the total domain (in degrees, E>0)
      REAL  (KIND=wp)  :: startlat_tot_d   !< transformed latitude of the lower left grid point of the total domain (in degrees, N>0)

      INTEGER  (KIND=i8) :: ie_tot_d !< number of grid points in zonal direction
      INTEGER  (KIND=i8) :: je_tot_d !< number of grid points in meridional direction
      INTEGER  (KIND=i8) :: ke_tot_d !< number of grid points in vertical direction

      ! Variables for domain
      REAL  (KIND=wp)  :: pollon !< longitude of the rotated north pole (in degrees, E>0)
      REAL  (KIND=wp)  :: pollat !< latitude of the rotated north pole (in degrees, N>0)
      REAL  (KIND=wp)  :: polgam !< angle between the north poles of the systems
      REAL  (KIND=wp)  :: dlon !< grid point distance in zonal direction (in degrees)
      REAL  (KIND=wp)  :: dlat !< grid point distance in meridional direction (in degrees)
      REAL  (KIND=wp)  :: startlon_tot !< transformed longitude of the lower left grid point of the total domain (in degrees, E>0)
      REAL  (KIND=wp)  :: startlat_tot   !< transformed latitude of the lower left grid point of the total domain (in degrees, N>0)

      INTEGER  (KIND=i8) :: ie_tot !< number of grid points in zonal direction
      INTEGER  (KIND=i8) :: je_tot !< number of grid points in meridional direction
      INTEGER  (KIND=i8) :: ke_tot !< number of grid points in vertical direction


      INTEGER (KIND=i8) :: ierr !< error flag
      INTEGER                  :: nuin !< unit number

      !> Define the namelist group
      NAMELIST /lmgrid/ pollon, pollat, polgam, dlon, dlat,          &   
                   startlon_tot,startlat_tot, ie_tot, je_tot, ke_tot
      ! Comment HA: namelist input and initialization like in COSMO model, src_setup.f90

      INTEGER :: errorcode !< error status variable

      ! Comment HA: namelist input and initialization like in COSMO model, src_setup.f90
      !------------------------------------------------------------------------------
      !- Section 1: Initialize the default variables
      !------------------------------------------------------------------------------

      pollon_d       = -170.0_wp
      pollat_d       =   32.5_wp
      polgam_d       =    0.0_wp
      dlon_d         =    0.008_wp
      dlat_d         =    0.008_wp
      startlon_tot_d =   -1.252_wp
      startlat_tot_d =   -7.972_wp
      ie_tot_d    = 51
      je_tot_d    = 51
      ke_tot_d    = 0
          
      !------------------------------------------------------------------------------
      !- Section 2: Initialize variables with default
      !------------------------------------------------------------------------------

      pollon       = pollon_d
      pollat       = pollat_d
      polgam       = polgam_d
      dlon         = dlon_d
      dlat         = dlat_d
      startlon_tot = startlon_tot_d
      startlat_tot = startlat_tot_d
      ie_tot    = ie_tot_d 
      je_tot    = je_tot_d 
      ke_tot    = ke_tot_d 


      !------------------------------------------------------------------------------
      !- Section 3: Input of the namelist values
      !------------------------------------------------------------------------------

      nuin = free_un()  ! functioin free_un returns free Fortran unit number
      open(nuin,FILE=input_namelist_file, IOSTAT=ierr)
      !print *, ierr
      read(nuin, NML=lmgrid, IOSTAT=ierr)
      !print *, ierr

      close(nuin)
      ! put values to data structure COSMO_grid
      COSMO_grid%pollon = pollon
      COSMO_grid%pollat = pollat
      COSMO_grid%polgam = polgam
      COSMO_grid%startlon_rot = startlon_tot
      COSMO_grid%startlat_rot = startlat_tot
      COSMO_grid%dlon_rot = dlon
      COSMO_grid%dlat_rot = dlat
      COSMO_grid%nlon_rot = ie_tot
      COSMO_grid%nlat_rot = je_tot
      COSMO_grid%ke_tot = ke_tot


END SUBROUTINE read_cosmo_domain_namelist

!> calculate coordinates of COSMO domain grid
!> \author Hermann Asensio
SUBROUTINE calculate_cosmo_domain_coordinates(tg,COSMO_grid)

   USE mo_utilities_extpar, ONLY: rlarot2rla, &
                               phirot2phi

   USE mo_grid_structures, ONLY: rotated_lonlat_grid ! Definition of Data Type to describe a rotated lonlat grid
   USE mo_target_grid_data, ONLY: lon_geo, &
     &                            lat_geo

   TYPE(target_grid_def), INTENT(IN)      :: tg              !< !< structure with target grid description
   TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid

   ! local variables
   REAL (KIND=wp) :: pollat
   REAL (KIND=wp) :: pollon
   REAL (KIND=wp) :: polgam
   INTEGER :: i,j  ! counters
   INTEGER :: k ! counter

   k = 1 ! for a COSMO grid the third dimension of target arrays is 1


   pollon = COSMO_grid%pollon
   pollat = COSMO_grid%pollat
   polgam = COSMO_grid%polgam

   ! coordinates in the rotated system
   DO i=1, tg%ie
     lon_rot(i) = COSMO_grid%startlon_rot + (i-1) * COSMO_grid%dlon_rot
   ENDDO
   DO j=1, tg%je
     lat_rot(j) =  COSMO_grid%startlat_rot + (j-1) * COSMO_grid%dlat_rot
   ENDDO
      
   ! convert coordinates from the rotated system to the geographical system
   DO j=1, tg%je

   DO i=1, tg%ie
      lon_geo(i,j,k) = rlarot2rla(lat_rot(j),lon_rot(i),pollat,pollon,polgam)
      lat_geo(i,j,k) = phirot2phi(lat_rot(j),lon_rot(i),pollat,pollon,polgam)
   ENDDO
   ENDDO


END SUBROUTINE calculate_cosmo_domain_coordinates

!> calculate coordinates of COSMO target grid
!! determin the coordintas for the rotated system and 
!! transform for each target grid element the coordinates into the geographical system  
!> \author Hermann Asensio
SUBROUTINE calculate_cosmo_target_coordinates(tg,cosmo_grid,lon_geo,lat_geo,lon_rot,lat_rot)

   USE mo_utilities_extpar, ONLY: rlarot2rla
   USE mo_utilities_extpar, ONLY: phirot2phi

   USE mo_grid_structures, ONLY: rotated_lonlat_grid ! Definition of Data Type to describe a rotated lonlat grid

   TYPE(target_grid_def), INTENT(IN)      :: tg              !< !< structure with target grid description
   TYPE(rotated_lonlat_grid), INTENT(IN) :: cosmo_grid !< structure which contains the definition of the COSMO grid
   REAL (KIND=wp), INTENT(INOUT) :: lon_geo(:,:,:)  !< longitude coordinates of the target grid in the geographical system
   REAL (KIND=wp), INTENT(INOUT) :: lat_geo(:,:,:)  !< latitude coordinates of the target grid in the geographical system
   REAL (KIND=wp), INTENT(INOUT) :: lon_rot(:) !< longitide coordinates of the COSMO grid in the rotated system 
   REAL (KIND=wp), INTENT(INOUT) :: lat_rot(:) !< latitude coordinates of the COSMO grid in the rotated system

   ! local variables
   INTEGER :: i,j,k  ! counters

   k = 1 ! for a COSMO grid the third dimension of target arrays is 1
   ! coordinates in the rotated system
   DO i=1, tg%ie
     lon_rot(i) = cosmo_grid%startlon_rot + (i-1) * cosmo_grid%dlon_rot
   ENDDO
   DO j=1, tg%je
     lat_rot(j) =  cosmo_grid%startlat_rot + (j-1) * cosmo_grid%dlat_rot
   ENDDO

   ! convert coordinates from the rotated system to the geographical system
   DO j=1, tg%je
   DO i=1, tg%ie
      lon_geo(i,j,k) = rlarot2rla(lat_rot(j),lon_rot(i), &
        &                         cosmo_grid%pollat,cosmo_grid%pollon,cosmo_grid%polgam)
      lat_geo(i,j,k) = phirot2phi(lat_rot(j),lon_rot(i), &
        &                         cosmo_grid%pollat,cosmo_grid%pollon,cosmo_grid%polgam)
   ENDDO
   ENDDO


END SUBROUTINE calculate_cosmo_target_coordinates


!> get Information for COSMO_grid from namelist INPUT_COSMO_GRID
SUBROUTINE get_cosmo_grid_info(input_namelist_file,tg,cosmo_grid)

   USE mo_io_units,          ONLY: filename_max

   USE mo_exception,         ONLY: message_text, message, finish
   USE mo_grid_structures, ONLY: target_grid_def, rotated_lonlat_grid
   USE mo_grid_structures, ONLY: igrid_cosmo


   IMPLICIT NONE

   CHARACTER (len=filename_max), INTENT(in) :: input_namelist_file !< file with input namelist with COSMO grid definition
  
   TYPE(target_grid_def), INTENT(OUT)      :: tg              !< !< structure with target grid description
   TYPE(rotated_lonlat_grid), INTENT(OUT) :: cosmo_grid !< structure which contains the definition of the COSMO grid


   CALL  read_cosmo_domain_namelist(input_namelist_file,     &
    &                                     cosmo_grid)

   !HA debug
   print *, 'after reading namelist ', TRIM(input_namelist_file)
   print *, 'ie_tot, je_tot:', cosmo_grid%nlon_rot, cosmo_grid%nlat_rot
   print *, 'ke_tot: ', cosmo_grid%ke_tot

   !describe the target grid
   tg%igrid_type = igrid_cosmo ! igrid_cosmo=2 for the COSMO grid
   tg%ie = cosmo_grid%nlon_rot
   tg%je = cosmo_grid%nlat_rot
   tg%ke = 1              ! third dimension with length 1     

      
END SUBROUTINE get_cosmo_grid_info

 
END Module mo_cosmo_grid

