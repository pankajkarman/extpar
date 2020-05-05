!+ Fortran modules with data fields for CRU temperature data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V2_0         2013/06/04 Martina Messmer
!  introduction of a finer CRU temperature data set (CLM Community)
!  new parameter for the CRU temperature elevation
!  introduction of a deallocate subroutine
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran modules with data fields for CRU temperature data
!> \author Hermann Asensio
!
MODULE mo_cru_data

  USE mo_logging          
  USE mo_kind,                  ONLY: wp, i4
  USE mo_io_units,              ONLY: filename_max  
  
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_namelists_extpar_t_clim

  CONTAINS

  !---------------------------------------------------------------------------
  !> subroutine to read namelist for t_clim data settings for EXTPAR 
  SUBROUTINE read_namelists_extpar_t_clim(namelist_file,            &
       &                                  it_cl_type,               &
       &                                  raw_data_t_clim_path,     &
       &                                  raw_data_t_clim_filename, &
       &                                  t_clim_buffer_file,       &
       &                                  t_clim_output_file)

    CHARACTER (len=*), INTENT(IN)             :: namelist_file !< filename with namelists for for EXTPAR settings
    INTEGER (KIND=i4),      INTENT(OUT)       :: it_cl_type    !< integer switch to choose a land use raw data set
    ! 1 CRU fine (new), 2 CRU coarse (old) temperature climatology
    CHARACTER (len=filename_max), INTENT(OUT) :: raw_data_t_clim_path, &        !< path to raw data
         &                                       raw_data_t_clim_filename, &    !< filename temperature climatology raw data
         &                                       t_clim_buffer_file, & !< name for temperature climatology buffer
         &                                       t_clim_output_file !< name for temperature climatology output file
    

    INTEGER (KIND=i4)                         :: nuin, ierr

    !> namelist with filename for temperature climatlogy data output
    NAMELIST /t_clim_raw_data/ raw_data_t_clim_path, raw_data_t_clim_filename, it_cl_type

    !> namelist with filename for temperature climatlogy data output
    NAMELIST /t_clim_io_extpar/ t_clim_buffer_file, t_clim_output_file

    it_cl_type = -1
    
    raw_data_t_clim_path = ''
    raw_data_t_clim_filename = ''

    t_clim_buffer_file = ''
    t_clim_output_file = ''

    OPEN(NEWUNIT=nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('CRU namelist open error ', __FILE__, __LINE__)
    ENDIF
    READ(nuin, NML=t_clim_raw_data, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      WRITE(0,NML=t_clim_raw_data)
      CALL logging%error('CRU raw data namelist read error ', __FILE__, __LINE__)      
    ENDIF
    READ(nuin, NML=t_clim_io_extpar, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      WRITE(0,NML=t_clim_io_extpar)
      CALL logging%error('CRU io namelist read error ', __FILE__, __LINE__)      
    ENDIF
    CLOSE(nuin)
    
  END SUBROUTINE read_namelists_extpar_t_clim

END MODULE mo_cru_data
