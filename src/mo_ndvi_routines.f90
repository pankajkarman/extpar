!+ Fortran module with NDVI data handling routines
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
!> Fortran module with NDVI data handling routines
!> \author Hermann Asensio
!>
MODULE mo_ndvi_routines

  USE mo_logging
  USE mo_kind,                  ONLY: i4

  USE mo_io_units,              ONLY: filename_max

  USE mo_utilities_extpar,      ONLY: free_un ! function to get free unit number

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: read_namelists_extpar_ndvi

  CONTAINS

  !> subroutine to read namelist for NDVI data settings for EXTPAR 
  SUBROUTINE read_namelists_extpar_ndvi(namelist_file, &
       &                                raw_data_ndvi_path, &
       &                                raw_data_ndvi_filename, &
       &                                ndvi_buffer_file, &
       &                                ndvi_output_file)



    CHARACTER (len=*), INTENT(IN) :: namelist_file !< filename with namelists for for EXTPAR settings

    CHARACTER (len=filename_max)             :: raw_data_ndvi_path, &        !< path to raw data
         &                                      raw_data_ndvi_filename, & !< filename NDVI raw data
         &                                      ndvi_buffer_file, & !< name for NDVI buffer file
         &                                      ndvi_output_file !< name for NDVI output file
       
    INTEGER (KIND=i4)                        :: ierr, nuin

    !> namelist with filenames for NDVI data input
    NAMELIST /ndvi_raw_data/ raw_data_ndvi_path, raw_data_ndvi_filename
    !> namelist with filenames for NDVI data output
    NAMELIST /ndvi_io_extpar/ ndvi_buffer_file, ndvi_output_file

    nuin = free_un()  ! functioin free_un returns free Fortran unit number
    OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)
    IF (ierr /= 0) THEN
      WRITE(message_text,*)'Cannot open ', TRIM(namelist_file)
      CALL logging%error(message_text,__FILE__, __LINE__) 
    ENDIF

    READ(nuin, NML=ndvi_raw_data, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist ndvi_raw_data',__FILE__, __LINE__) 
    ENDIF

    READ(nuin, NML=ndvi_io_extpar, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist ndvi_io_extpar',__FILE__, __LINE__) 
    ENDIF

    CLOSE(nuin)

  END SUBROUTINE read_namelists_extpar_ndvi

END MODULE mo_ndvi_routines
