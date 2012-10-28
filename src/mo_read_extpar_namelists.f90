!+ Fortran modules with namelist definitions for the external parameters software extpar
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_0         2010/12/21 Hermann Asensio
!  Initial release
! V1_1         2011/01/20 Hermann Asensio
!  small bug fixes accroding to Fortran compiler warnings
! V1_3         2011/04/19 Hermann Asensio
! introduce Globcover 2009 land use data set for external parameters
! add support for GRIB1 and GRIB2
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran modules with namelist definitions for the external parameters software extpar
!> and input routines
MODULE mo_read_extpar_namelists

 USE mo_kind, ONLY: wp, &
                     i4, &
                     i8
 USE mo_io_units, ONLY: filename_max

 PUBLIC :: read_namelists_extpar_grid_def
 PUBLIC :: read_namelists_extpar_check

CONTAINS

!---------------------------------------------------------------------------
!> subroutine to read namelist for grid settings for EXTPAR
SUBROUTINE read_namelists_extpar_grid_def(namelist_grid_def, &
                                         igrid_type, &
                                         domain_def_namelist, &
                                         domain_refinement_opt)

  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number

  CHARACTER (len=filename_max), INTENT(IN) :: namelist_grid_def !< filename with namelists for grid settings for EXTPAR
 
  INTEGER (KIND=i4), INTENT(OUT)            :: igrid_type       !< target grid type, 1 for ICON, 2 for COSMO, 3 for GME grid
  CHARACTER (len=filename_max), INTENT(OUT) :: domain_def_namelist !< namelist file with domain definition
  CHARACTER (len=filename_max),OPTIONAL, INTENT(OUT) :: domain_refinement_opt   !< namelist file with domain refinement defintion (e.g. for the ICON grid)
  
  ! local variables
  CHARACTER (len=filename_max) :: domain_refinement

  !> namelist with grid defintion
  NAMELIST /grid_def/ igrid_type, domain_def_namelist, domain_refinement


  INTEGER           :: nuin !< unit number
  INTEGER (KIND=i4) :: ierr !< error flag


  nuin = free_un()  ! functioin free_un returns free Fortran unit number
  OPEN(nuin,FILE=TRIM(namelist_grid_def), IOSTAT=ierr)

  READ(nuin, NML=grid_def, IOSTAT=ierr)

  CLOSE(nuin)
  
  ! If optional argument is present for output, copy the value from the local variable to the output argument variable
  IF (PRESENT(domain_refinement_opt)) domain_refinement_opt = TRIM(domain_refinement)

END SUBROUTINE read_namelists_extpar_grid_def

!---------------------------------------------------------------------------
!> subroutine to read namelist for consitency check settings for EXTPAR 
SUBROUTINE read_namelists_extpar_check(namelist_file, &
                                         grib_output_filename, &
                                         grib_sample, &
                                         netcdf_output_filename, &
                                         orography_buffer_file, &
                                         soil_buffer_file, &
                                         lu_buffer_file, &
                                         glcc_buffer_file, &
                                         flake_buffer_file, &
                                         ndvi_buffer_file, &
                                         t_clim_buffer_file, &
                                         aot_buffer_file)

  USE mo_utilities_extpar, ONLY: free_un ! function to get free unit number

  
  CHARACTER (len=filename_max), INTENT(IN) :: namelist_file !< filename with namelists for for EXTPAR settings


   CHARACTER (len=filename_max) :: grib_output_filename  !< name for grib output filename
   CHARACTER (len=filename_max) :: grib_sample  !< name for grib sample  (sample to be found in $GRIB_SAMPLES_PATH)
   CHARACTER (len=filename_max) :: netcdf_output_filename!< name for netcdf output filename
   CHARACTER (len=filename_max) :: orography_buffer_file  !< name for orography buffer file
   CHARACTER (len=filename_max) :: soil_buffer_file !< name for soil buffer file
   CHARACTER (len=filename_max) :: lu_buffer_file  !< name for glc2000 buffer file

   CHARACTER (len=filename_max) :: glcc_buffer_file  !< name for glcc buffer file

   CHARACTER (len=filename_max) :: flake_buffer_file  !< name for flake buffer file

   CHARACTER (len=filename_max) :: ndvi_buffer_file  !< name for ndvi buffer file
   CHARACTER (len=filename_max) :: t_clim_buffer_file  !< name for t_clim buffer file
   CHARACTER (len=filename_max) :: aot_buffer_file  !< name for aot buffer file




   !> namelist with filenames for output of soil data
   NAMELIST /extpar_consistency_check_io/ grib_output_filename, &
                                          grib_sample, &
                                         netcdf_output_filename, &
                                         orography_buffer_file, &
                                         soil_buffer_file, &
                                         lu_buffer_file, &
                                         glcc_buffer_file, &
                                         flake_buffer_file, &
                                         ndvi_buffer_file, &
                                         t_clim_buffer_file, &
                                         aot_buffer_file
                                         

   INTEGER           :: nuin !< unit number
   INTEGER (KIND=i4) :: ierr !< error flag


   nuin = free_un()  ! functioin free_un returns free Fortran unit number
!roa: what the heck is that!?!
!   grib_output_filename = 'external_parameters.grb'
!   grib_sample = 'GRIB2'

   OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)

   READ(nuin, NML=extpar_consistency_check_io, IOSTAT=ierr)

   CLOSE(nuin)
  
!roaprint
print *,"outfn: ",  grib_output_filename
print *,"smple: ",  grib_sample
print *,"nfout: ",                  netcdf_output_filename
print *,"oro: ",                    orography_buffer_file
print *,"soil: ",                    soil_buffer_file
print *,"lu: ",                    lu_buffer_file
print *,"glcc: ",                    glcc_buffer_file
print *,"flake: ",                    flake_buffer_file
print *,"ndvi: ",                    ndvi_buffer_file
print *,"tclm: ",                    t_clim_buffer_file
print *,"aot: ",                    aot_buffer_file

END SUBROUTINE read_namelists_extpar_check
!---------------------------------------------------------------------------


END MODULE mo_read_extpar_namelists


