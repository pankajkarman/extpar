!+ Fortran module with EMISS data handling routines
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V2_0_3       2015-02-23 Juergen Helmert, Daniel Luethi
!  Increase working precision in grid size computation         
!  compute raw data lat/lon values (values on file are inaccurate)
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with EMISS data handling routines
!> \author Hermann Asensio
!>
MODULE mo_emiss_routines

  USE mo_logging
  USE mo_kind,                  ONLY:  i4

  USE mo_utilities_extpar,      ONLY: free_un ! function to get free unit number

  USE mo_io_utilities,          ONLY: check_netcdf

  USE mo_io_units,              ONLY: filename_max


  IMPLICIT NONE

  PRIVATE
   
  PUBLIC :: read_namelists_extpar_emiss


  CONTAINS

  !---------------------------------------------------------------------------
  !> subroutine to read namelist for EMISS data settings for EXTPAR 
  SUBROUTINE read_namelists_extpar_emiss(namelist_file, &
       &                                 raw_data_emiss_path, &
       &                                 raw_data_emiss_filename, &
       &                                 emiss_buffer_file, &
       &                                 emiss_output_file)

    CHARACTER (len=*), INTENT(IN)            :: namelist_file !< filename with namelists for for EXTPAR settings

    CHARACTER (len=filename_max),INTENT(OUT) :: raw_data_emiss_path, &         !< path to raw data
         &                                      raw_data_emiss_filename, &  !< filename EMISS raw data
         &                                      emiss_buffer_file, &  !< name for EMISS buffer file
         &                                      emiss_output_file !< name for EMISS output file

    INTEGER(KIND=i4)                         :: nuin, ierr

    !> namelist with filenames for EMISS data input
    NAMELIST /emiss_raw_data/ raw_data_emiss_path, raw_data_emiss_filename
    !> namelist with filenames for EMISS data output
    NAMELIST /emiss_io_extpar/ emiss_buffer_file, emiss_output_file

    nuin = free_un()  ! functioin free_un returns free Fortran unit number
    OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)
    IF (ierr /= 0) THEN
      WRITE(message_text,*)'Cannot open ', TRIM(namelist_file)
      CALL logging%error(message_text,__FILE__, __LINE__) 
    ENDIF

    READ(nuin, NML=emiss_raw_data, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist emiss_raw_data',__FILE__, __LINE__) 
    ENDIF

    READ(nuin, NML=emiss_io_extpar, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist emiss_io_extpar',__FILE__, __LINE__) 
    ENDIF

    CLOSE(nuin)

  END SUBROUTINE read_namelists_extpar_emiss

END MODULE mo_emiss_routines
