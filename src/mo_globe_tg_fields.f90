!+ Fortran module for GLOBE data on target grid for external parameters 
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_2         2011/03/25 Hermann Asensio
!  Update doxygen documetation (comments)
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module for GLOBE data on target grid for external parameters 
!> \author Hermann Asensio
MODULE mo_globe_tg_fields

  !> kind parameters are defined in MODULE data_parameters
  USE mo_kind, ONLY: wp
  USE mo_kind, ONLY: i4
  USE mo_kind, ONLY: i8


  !> abort_extpar defined in MODULE utilities_extpar
  USE mo_utilities_extpar, ONLY: abort_extpar

  USE mo_grid_structures, ONLY: target_grid_def


  IMPLICIT NONE

  PRIVATE

  PUBLIC :: fr_land_globe, &
    &        hh_globe,            &
    &        stdh_globe,          &
    &        theta_globe,         &
    &        aniso_globe,         &
    &        slope_globe,         &
    &        z0_topo,             &
    &        allocate_globe_target_fields
            
  PUBLIC :: globe_buffer
  PUBLIC :: oro_tg
  PUBLIC :: allocate_oro_tg

  PUBLIC ::   add_parameters_domain, &
    &          vertex_param, &
    &          allocate_additional_hh_param


       
  REAL(KIND=wp), ALLOCATABLE  :: hh_globe(:,:,:)  !< mean height 
  REAL(KIND=wp), ALLOCATABLE  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height

  REAL(KIND=wp), ALLOCATABLE  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
  REAL(KIND=wp), ALLOCATABLE  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
  REAL(KIND=wp), ALLOCATABLE  :: slope_globe(:,:,:) !< sso parameter, mean slope
  REAL(KIND=wp), ALLOCATABLE  :: fr_land_globe(:,:,:) !< fraction land due to GLOBE raw data

  REAL(KIND=wp), ALLOCATABLE  :: z0_topo(:,:,:) !< roughness length due to orography
  
  !> data structure for parameters on vertices of Icon grid
  TYPE add_parameters_domain
     REAL(KIND=wp), ALLOCATABLE     :: hh_vert(:,:,:)   !< height on vertex
     INTEGER (KIND=i8), ALLOCATABLE :: npixel_vert(:,:,:) !< number of raw data pixel corresponding to vertex
  END TYPE add_parameters_domain

  TYPE(add_parameters_domain) :: vertex_param  !< additional external parameters for ICON domain

  !> data structure for orography fields
  TYPE globe_buffer
    REAL(KIND=wp), ALLOCATABLE  :: hh_globe(:,:,:)  !< mean height 
    REAL(KIND=wp), ALLOCATABLE  :: stdh_globe(:,:,:) !< standard deviation of subgrid scale orographic height
    REAL(KIND=wp), ALLOCATABLE  :: theta_globe(:,:,:) !< sso parameter, angle of principal axis
    REAL(KIND=wp), ALLOCATABLE  :: aniso_globe(:,:,:) !< sso parameter, anisotropie factor
    REAL(KIND=wp), ALLOCATABLE  :: slope_globe(:,:,:) !< sso parameter, mean slope
    REAL(KIND=wp), ALLOCATABLE  :: fr_land_globe(:,:,:) !< fraction land due to GLOBE raw data
  END TYPE globe_buffer

  TYPE(globe_buffer) :: oro_tg !< structure for globe buffer 


CONTAINS
  !> allocate fields for GLOBE target data 
  SUBROUTINE allocate_oro_tg(tg)
  USE mo_grid_structures, ONLY : target_grid_def
  TYPE(target_grid_def), INTENT(IN) :: tg

  !local variables
  INTEGER :: ie,je,ke !< indices
  INTEGER :: errorcode !< error status variable

    ie=tg%ie
    je=tg%je
    ke=tg%ke

    ALLOCATE(oro_tg%hh_globe(ie,je,ke), STAT=errorcode)
    IF (errorcode /= 0 ) THEN
      CALL abort_extpar('Can not allocate the structure oro_ntarget')
    ENDIF

    ALLOCATE(oro_tg%stdh_globe(ie,je,ke), STAT=errorcode)
    IF (errorcode /= 0 ) THEN
      CALL abort_extpar('Can not allocate the structure oro_ntarget')
    ENDIF


    ALLOCATE(oro_tg%theta_globe(ie,je,ke), STAT=errorcode)
    IF (errorcode /= 0 ) THEN
      CALL abort_extpar('Can not allocate the structure oro_ntarget')
    ENDIF


    ALLOCATE(oro_tg%aniso_globe(ie,je,ke), STAT=errorcode)
    IF (errorcode /= 0 ) THEN
      CALL abort_extpar('Can not allocate the structure oro_ntarget')
    ENDIF


    ALLOCATE(oro_tg%slope_globe(ie,je,ke), STAT=errorcode)
    IF (errorcode /= 0 ) THEN
      CALL abort_extpar('Can not allocate the structure oro_ntarget')
    ENDIF

    ALLOCATE(oro_tg%fr_land_globe(ie,je,ke), STAT=errorcode)
    IF (errorcode /= 0 ) THEN
      CALL abort_extpar('Can not allocate the structure oro_ntarget')
    ENDIF


  END SUBROUTINE allocate_oro_tg





  !> allocate fields for GLOBE target data 
  SUBROUTINE allocate_globe_target_fields(tg)
  

    IMPLICIT NONE

    TYPE(target_grid_def), INTENT(IN) :: tg  !< structure with target grid description

    INTEGER :: errorcode !< error status variable

      
    ALLOCATE (fr_land_globe(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array fr_land_globe')
    fr_land_globe = 0.0

    ALLOCATE (hh_globe(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array hh_globe')
    hh_globe = 0.0

    ALLOCATE (stdh_globe(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array stdh_globe')
    stdh_globe = 0.0

    ALLOCATE (theta_globe(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array theta_globe')
    theta_globe = 0.0

    ALLOCATE (aniso_globe(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array aniso_globe')
    aniso_globe = 0.0

    ALLOCATE (slope_globe(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array slope_globe')
    slope_globe = 0.0

    
    ALLOCATE (z0_topo(1:tg%ie,1:tg%je,1:tg%ke), STAT=errorcode)
        IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the array z0_topo')
    z0_topo = 0.0



  END SUBROUTINE allocate_globe_target_fields

  !> allocate additional parameters which correspond to the vertex
  !!
  !! the target grid has the dimension nvertex
  !! for future developments (optimizations, other code structure) the target grid is 
  !! defined as a 3-dimensional matrix, but the dimension are set to (nvertex,1,1) in this case
  SUBROUTINE allocate_additional_hh_param(nvertex)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nvertex  !< number of vertices in target domains

    INTEGER, PARAMETER :: je = 1 !< dummy for dimension selection
    INTEGER, PARAMETER :: ke = 1 !< dummy for dimension selection

    INTEGER :: errorcode !< error status variable

      ALLOCATE(vertex_param%hh_vert(1:nvertex,1:je,1:ke), STAT=errorcode)
      IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the vertex_param%hh_vert(nvertex,je,ke')
      vertex_param%hh_vert = 0.0

      ALLOCATE(vertex_param%npixel_vert(1:nvertex,1:je,1:ke), STAT=errorcode)
      IF(errorcode.NE.0) CALL abort_extpar('Cant allocate the vertex_param%npixel_vert(nvertex,je,ke)')
      vertex_param%npixel_vert = 0

  END SUBROUTINE allocate_additional_hh_param



END Module mo_globe_tg_fields

