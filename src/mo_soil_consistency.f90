!+ Fortran module with data fields for soil data
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V2_0         24/4/2013 Juergen Helmert / Martina Messmer
!  Initial release
!  The soiltype is converted from the HWSD world code to the TERRA code.
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module that performs calculation of the soil type and bulk density
!> \author Juergen Helmert / Martina Messmer

MODULE mo_soil_consistency

!> kind parameters are defined in MODULE data_parameters
USE mo_kind, ONLY: wp, &
                   i4, &
                   i8

!> abort_extpar defined in MODULE utilities_extpar
USE mo_utilities_extpar, ONLY: abort_extpar, free_un

USE mo_io_units,         ONLY: filename_max

USE mo_grid_structures,  ONLY: target_grid_def
USE mo_soil_tg_fields,   ONLY: soiltype_fao, soiltype_deep

IMPLICIT NONE

PUBLIC :: calculate_soiltype, &
          read_namelists_extpar_HWSD_index


CONTAINS

  SUBROUTINE calculate_soiltype(tg,            &
     &                          ldeep_soil,    &
     &                          soiltype_fao,  &
     &                          fr_sand,       &
     &                          fr_silt,       &
     &                          fr_clay,       &
     &                          fr_oc,         &
     &                          fr_bd,         &
     &                          fr_dm,         &
     &                          fr_sand_deep,  &
     &                          fr_silt_deep,  &
     &                          fr_clay_deep,  &
     &                          fr_oc_deep,    &
     &                          fr_bd_deep,    &
     &                          fr_dm_deep,    &
     &                          soiltype_deep)

    TYPE(target_grid_def), INTENT(IN)      :: tg
    LOGICAL, INTENT(IN)                    :: ldeep_soil
    INTEGER (KIND=i4), INTENT(INOUT)       :: soiltype_fao(:,:,:)!(1:tg%ie,1:tg%je,1:tg%ke)
   
    REAL(KIND=wp), INTENT(OUT)  :: fr_sand(1:tg%ie,1:tg%je,1:tg%ke) !< fraction sand due to HWSD
    REAL(KIND=wp), INTENT(OUT)  :: fr_silt(1:tg%ie,1:tg%je,1:tg%ke) !< fraction silt due to HWSD
    REAL(KIND=wp), INTENT(OUT)  :: fr_clay(1:tg%ie,1:tg%je,1:tg%ke) !< fraction clay due to HWSD
    REAL(KIND=wp), INTENT(OUT)  :: fr_oc(1:tg%ie,1:tg%je,1:tg%ke) !< fraction oc due to HWSD
    REAL(KIND=wp), INTENT(OUT)  :: fr_bd(1:tg%ie,1:tg%je,1:tg%ke) !< fraction bd due to HWSD
    REAL(KIND=wp), INTENT(OUT)  :: fr_dm(1:tg%ie,1:tg%je,1:tg%ke) !< dummy of HWSD
    INTEGER (KIND=i4), INTENT(INOUT), OPTIONAL:: soiltype_deep(:,:,:)!(1:tg%ie,1:tg%je,1:tg%ke)
    REAL(KIND=wp), INTENT(OUT),  OPTIONAL     :: fr_sand_deep(1:tg%ie,1:tg%je,1:tg%ke) !< fraction sand due to HWSD
    REAL(KIND=wp), INTENT(OUT), OPTIONAL      :: fr_silt_deep(1:tg%ie,1:tg%je,1:tg%ke) !< fraction silt due to HWSD
    REAL(KIND=wp), INTENT(OUT), OPTIONAL      :: fr_clay_deep(1:tg%ie,1:tg%je,1:tg%ke) !< fraction clay due to HWSD
    REAL(KIND=wp), INTENT(OUT), OPTIONAL      :: fr_oc_deep(1:tg%ie,1:tg%je,1:tg%ke) !< fraction oc due to HWSD
    REAL(KIND=wp), INTENT(OUT), OPTIONAL      :: fr_bd_deep(1:tg%ie,1:tg%je,1:tg%ke) !< fraction bd due to HWSD
    REAL(KIND=wp), INTENT(OUT), OPTIONAL      :: fr_dm_deep(1:tg%ie,1:tg%je,1:tg%ke) !< dummy of HWSD

    CHARACTER (len=filename_max) :: namelist_file !< filename with namelists for for EXTPAR settings

    ! HWSD idex files
    CHARACTER (len=filename_max) :: path_HWSD_index_files
    CHARACTER (len=filename_max) :: lookup_table_HWSD   
    CHARACTER (len=filename_max) :: HWSD_data  
    CHARACTER (len=filename_max) :: HWSD_data_deep     
    CHARACTER (len=filename_max) :: HWSD_data_extpar 

    CHARACTER (len=filename_max) :: path_lookup_table_HWSD
    CHARACTER (len=filename_max) :: path_HWSD_data
    CHARACTER (len=filename_max) :: path_HWSD_data_deep
    CHARACTER (len=filename_max) :: path_HWSD_data_extpar

    INTEGER (KIND=i4) :: ic, ic_deep

    INTEGER, PARAMETER:: n_soil=16102
    INTEGER           :: HWSD_ID(n_soil),stype
    REAL (KIND=wp)    :: HWSD_SU(n_soil), HWSD_TERRA(n_soil)

    INTEGER, PARAMETER:: n_soil_db=48148
    INTEGER           :: i_soil_db
    INTEGER           :: HWSD_ID_DB(n_soil_db),HWSD_SU_DB(n_soil_db)
    INTEGER           :: HWSD_SU_DB_S(n_soil_db)
    INTEGER           :: T_SAND(n_soil_db),T_SILT(n_soil_db),T_CLAY(n_soil_db)
    INTEGER           :: S_SAND(n_soil_db),S_SILT(n_soil_db),S_CLAY(n_soil_db)
    INTEGER (KIND=i4) :: nuin
    INTEGER (KIND=i4) :: i,j,k !<counters
    
    REAL(KIND=wp)     :: T_OC(n_soil_db),T_BD(n_soil_db),nfac
    REAL(KIND=wp)     :: S_OC(n_soil_db),S_BD(n_soil_db),nfac_deep
    CHARACTER*10      :: styp_out
    
    REAL(KIND=wp)     :: S,C,OM,D,zsandf,zsiltf,zclayf,zomf, zrog,zrocg, &
         &               zkw,avG,mvG, lnalpha,lnnm1,lnKs, zporv,         &
         &               zpwp, zfcap
    REAL(KIND=wp)     :: S_deep,C_deep,OM_deep,D_deep,                   &
         &               zsandf_deep,zsiltf_deep,zclayf_deep,            &
         &               zomf_deep, zrog_deep,zrocg_deep,                &
         &               zkw_deep,avG_deep,mvG_deep,                     &
         &               lnalpha_deep,lnnm1_deep,lnKs_deep, zporv_deep,  &
         &               zpwp_deep, zfcap_deep

    REAL(KIND=wp),     PARAMETER:: horizon_mid = 65.                    ! horizon mid-point of the subsoil in cm 
    REAL(KIND=wp),     PARAMETER:: minimum     = 0.1
    REAL(KIND=wp),     PARAMETER:: psi_fcap    = 0.1                    !bar
    REAL(KIND=wp),     PARAMETER:: psi_pwp     = 15.                    !bar
    REAL(KIND=wp),     PARAMETER:: zadp        = 0.01
    REAL(KIND=wp),     PARAMETER:: topsoil     = 1.
    REAL(KIND=wp),     PARAMETER:: subsoil     = 0.

      PRINT *,'Open HWSD index files...'

      namelist_file = 'INPUT_SOIL'

      CALL read_namelists_extpar_HWSD_index(namelist_file,        &
                                            path_HWSD_index_files,&
                                            lookup_table_HWSD,    &
                                            HWSD_data,            &
                                            HWSD_data_deep,       &
                                            HWSD_data_extpar)

! TODO: read namelist!!! mes

      nuin = free_un()
      path_lookup_table_HWSD = TRIM(path_HWSD_index_files)//TRIM(lookup_table_HWSD)
      print*, 'path_lookup_table_HWSD: ', TRIM(path_lookup_table_HWSD)
      open(nuin,file=TRIM(path_lookup_table_HWSD), status='old')
      read(nuin,*) !header

      do i=1,n_soil
        read(nuin,*) HWSD_ID(i),HWSD_SU(i),HWSD_TERRA(i)
      end do
      close(nuin)
      
      nuin = free_un()
      path_HWSD_data = TRIM(path_HWSD_index_files)//TRIM(HWSD_data)
      print*, 'path_HWSD_data: ', TRIM(path_HWSD_data)
      open(nuin,file=TRIM(path_HWSD_data), status='old')
      read(nuin,*) !header
      
      do i=1,n_soil_db
        read(nuin,*) HWSD_SU_DB(i),T_SAND(i),T_SILT(i),T_CLAY(i),T_OC(i),T_BD(i)
      end do
      close(nuin)

      IF (ldeep_soil) THEN
        nuin = free_un()
        path_HWSD_data_deep = TRIM(path_HWSD_index_files)//TRIM(HWSD_data_deep)
        print*, 'path_HWSD_data_deep: ', TRIM(path_HWSD_data_deep)
        open(nuin,file=TRIM(path_HWSD_data_deep), status='old')
        read(nuin,*) !header
        do i=1,n_soil_db
          read(nuin,*) HWSD_SU_DB_S(i),S_SAND(i),S_SILT(i),S_CLAY(i),S_OC(i),S_BD(i)
        end do
        close(nuin)
      ENDIF

      nuin = free_un()
      path_HWSD_data_extpar = TRIM(path_HWSD_index_files) // TRIM(HWSD_data_extpar)
      print*, 'path_HWSD_data_extpar: ', TRIM(path_HWSD_data_extpar)
      open(nuin,file=TRIM(path_HWSD_data_extpar), status='unknown')

      DO k=1,tg%ke
        DO j=1,tg%je
          DO i=1,tg%ie
            
            ic=soiltype_fao(i,j,k)
            soiltype_fao(i,j,k)=HWSD_TERRA(ic)
            IF (ldeep_soil) THEN
              ic_deep = soiltype_deep(i,j,k)
              soiltype_deep(i,j,k) = HWSD_TERRA(ic_deep)
            ENDIF
            
            DO i_soil_db=1,n_soil_db
              IF(INT(HWSD_SU(ic))==HWSD_SU_DB(i_soil_db)) THEN
                
                nfac=(100.0-T_OC(i_soil_db))/100.
                
                fr_sand(i,j,k)=T_SAND(i_soil_db)
                fr_silt(i,j,k)=T_SILT(i_soil_db)
                fr_clay(i,j,k)=T_CLAY(i_soil_db)
                fr_oc(i,j,k)=T_OC(i_soil_db)
                fr_bd(i,j,k)=T_BD(i_soil_db)
                
                
              END IF
              IF (ldeep_soil) THEN
                IF(INT(HWSD_SU(ic_deep))==HWSD_SU_DB_S(i_soil_db)) THEN
                
                  nfac_deep=(100.0-S_OC(i_soil_db))/100.
                
                  fr_sand_deep(i,j,k)=S_SAND(i_soil_db)
                  fr_silt_deep(i,j,k)=S_SILT(i_soil_db)
                  fr_clay_deep(i,j,k)=S_CLAY(i_soil_db)
                  fr_oc_deep(i,j,k)=S_OC(i_soil_db)
                  fr_bd_deep(i,j,k)=S_BD(i_soil_db)
                                
                END IF
              ENDIF
              
              IF(INT(HWSD_SU(ic))==HWSD_SU_DB(i_soil_db)) EXIT ! leave loop - only first entry will be considered
              IF (ldeep_soil) THEN
                IF(INT(HWSD_SU(ic_deep))==HWSD_SU_DB(i_soil_db)) EXIT ! leave loop - only first entry will be considered
              ENDIF


            END DO
                        
            
          ENDDO
        ENDDO
      ENDDO
      
      close(nuin)
      
      
      DO k=1,tg%ke
        DO j=1,tg%je
          DO i=1,tg%ie
            
            IF(fr_sand(i,j,k)>0..AND.fr_oc(i,j,k)>0.) THEN
              
              zsandf   = fr_sand(i,j,k)*(100.-fr_oc(i,j,k))/100.
              zsiltf   = fr_silt(i,j,k)*(100.-fr_oc(i,j,k))/100.
              zclayf   = fr_clay(i,j,k)*(100.-fr_oc(i,j,k))/100.
              zomf     = fr_oc(i,j,k)
              
              IF(fr_bd(i,j,k) .le.0.) then
                ! European Journal of Soil Science, February 2012, 63, 96109 doi: 10.1111/j.1365-2389.2011.01412.x
                ! Empirically-derived pedotransfer functions for predicting
                ! bulk density in European soils
                ! J . M. H o l l i sa, J. Hannamb & P. H. Bellamyb
                ! Cultivated topsoils

                S=MAX(minimum,zsandf)
                C=MAX(minimum,zclayf)
                OM=MAX(minimum,zomf)
                
                zrog =0.80806 + (0.823844 * EXP(-0.27993*OM)) & ! g/cm**3
                     + (0.0014065*S) - (0.0010299*C)
                
                zrocg    = (2.E6*0.01*(zsandf+zsiltf+zclayf)+2.5E6*0.01*zomf)*zrog ! C*rho
                
              else
                zrog     = fr_bd(i,j,k)
                zrocg    = (2.E6*0.01*(zsandf+zsiltf+zclayf)+2.5E6*0.01*zomf)*zrog ! C*rho
                
              END IF ! BD undefined
              
              S=MAX(minimum,zsiltf)
              C=MAX(minimum,zclayf)
              OM=MAX(minimum,zomf)
              D=zrog
              
              
              ! print*,mstyp,S,C,OM,D
              
              
              lnalpha  = -14.96+0.03135*C+0.0351*S+0.646*OM+15.29*D-0.192*topsoil &
                   -4.671*D**2.-0.000781*C**2. -0.00687*OM**2.+0.0449*OM**(-1.) &
                   +0.0663*LOG(S)+0.1482*LOG(OM)-0.04546*D*S-0.4852*D*OM+0.00673*topsoil*C
              
              lnnm1      = -25.23-0.02195*C+0.0074*S-0.1940*OM+45.5*D-7.24*D**2.+0.0003658*C**2. &
                   +0.002885*OM**2.-12.81*D**(-1.)-0.1524*S**(-1.)-0.01958*OM**(-1.) &
                   -0.2876*LOG(S)-0.0709*LOG(OM)-44.6*LOG(D)- &
                   0.02264*D*C+0.0896*D*OM+0.00718*topsoil*C
              
              lnKs     = 7.755+0.0352*S+0.93*topsoil-0.967*D**2.-0.000484*C**2. &
                   -0.000322*S**2.+0.001*S**(-1.)-0.0748*OM**(-1.) &
                   -0.643*LOG(S)-0.01398*D*C-0.1673*D*OM+0.02986*topsoil*C-0.03305*topsoil*S
              zkw      = EXP(lnKs)/100./86400. !cm/d -> m/s
              
              
              avG  = EXP(lnalpha)*100. !1/cm -> 1/m
              mvG  = 1.-1./(exp(lnnm1)+1.) ! ln_n = ln(n-1); Woesten et al 1999
              
              zporv= 0.7919+0.001691*C-0.29619*D-0.000001491*S**2. +0.0000821*OM**2. &
                   +0.02427*C**(-1.) +0.01113*S**(-1.)+ 0.01472*LOG(S)-0.0000733*OM*C &
                   -0.000619*D*C-0.001183*D*OM-0.0001664*topsoil*S
                            
              zpwp     = zadp + ((zporv-zadp)/ &
                   (1.+avG* psi_pwp*9.81)**mvG)
              
              zfcap    = zadp + ((zporv-zadp)/ &
                   (1.+avG* psi_fcap*9.81)**mvG)
              

              IF (OM>20..OR.zporv>0.7) THEN ! Treatment of Peat with Histosols
                
                zporv    = 0.766
                zfcap    = 0.663
                zpwp     = 0.267
                zkw      = 0.93E-6
                avG          = 1.3
                
                mvG      = 1.-1./1.20
                
              END IF

              fr_dm(i,j,k)=zkw*86400.*100.
              
            ELSE

              fr_dm(i,j,k)=-1.

            END IF


            IF (ldeep_soil) THEN

              IF(fr_sand_deep(i,j,k)>0..AND.fr_oc_deep(i,j,k)>0.) THEN
              
                zsandf_deep   = fr_sand_deep(i,j,k)*(100.-fr_oc_deep(i,j,k))/100.
                zsiltf_deep   = fr_silt_deep(i,j,k)*(100.-fr_oc_deep(i,j,k))/100.
                zclayf_deep   = fr_clay_deep(i,j,k)*(100.-fr_oc_deep(i,j,k))/100.
                zomf_deep     = fr_oc_deep(i,j,k)
                
                IF(fr_bd_deep(i,j,k) .le.0.) then
                  ! European Journal of Soil Science, February 2012, 63, 96109 doi: 10.1111/j.1365-2389.2011.01412.x
                  ! Empirically-derived pedotransfer functions for predicting
                  ! bulk density in European soils
                  ! J . M. H o l l i sa, J. Hannamb & P. H. Bellamyb
                  ! Compact subsoils

                  S_deep=MAX(minimum,zsandf_deep)
                  C_deep=MAX(minimum,zclayf_deep)
                  OM_deep=MAX(minimum,zomf_deep)
                
                  zrog_deep = 1.1257 - (0.1140245 * LOG(OM_deep)) + (0.0555 * LOG(horizon_mid)) & ! g/cm**3
                       + (0.002248 * S_deep)
                  
                  zrocg_deep    = (2.E6*0.01*(zsandf_deep+zsiltf_deep+zclayf_deep)+2.5E6*0.01*zomf_deep)*zrog_deep ! C*rho
                
                else
                  zrog_deep     = fr_bd_deep(i,j,k)
                  zrocg_deep    = (2.E6*0.01*(zsandf_deep+zsiltf_deep+zclayf_deep)+2.5E6*0.01*zomf_deep)*zrog_deep ! C*rho
                
                END IF ! BD undefined
              
                S_deep=MAX(minimum,zsiltf_deep)
                C_deep=MAX(minimum,zclayf_deep)
                OM_deep=MAX(minimum,zomf_deep)
                D_deep=zrog_deep
              
              
                ! print*,mstyp,S,C,OM,D
              
              
                lnalpha_deep = -14.96+0.03135*C_deep+0.0351*S_deep+0.646*OM_deep+15.29*D_deep-0.192*subsoil &
                     -4.671*D_deep**2.-0.000781*C_deep**2. -0.00687*OM_deep**2.+0.0449*OM_deep**(-1.) &
                     +0.0663*LOG(S_deep)+0.1482*LOG(OM_deep)-0.04546*D_deep*S_deep-0.4852*D_deep*OM_deep+0.00673*subsoil*C_deep
              
                lnnm1_deep   = -25.23-0.02195*C_deep+0.0074*S_deep-0.1940*OM_deep+45.5*D_deep-7.24*D_deep**2.+0.0003658*C_deep**2. &
                     +0.002885*OM_deep**2.-12.81*D_deep**(-1.)-0.1524*S_deep**(-1.)-0.01958*OM_deep**(-1.) &
                     -0.2876*LOG(S_deep)-0.0709*LOG(OM_deep)-44.6*LOG(D_deep)- &
                     0.02264*D_deep*C_deep+0.0896*D_deep*OM_deep+0.00718*subsoil*C_deep
              
                lnKs_deep    = 7.755+0.0352*S_deep+0.93*subsoil-0.967*D_deep**2.-0.000484*C_deep**2. &
                     -0.000322*S_deep**2.+0.001*S_deep**(-1.)-0.0748*OM_deep**(-1.) &
                     -0.643*LOG(S_deep)-0.01398*D_deep*C_deep-0.1673*D_deep*OM_deep+0.02986*subsoil*C_deep-0.03305*subsoil*S_deep

                zkw_deep     = EXP(lnKs_deep)/100./86400. !cm/d -> m/s              
              
                avG_deep     = EXP(lnalpha_deep)*100. !1/cm -> 1/m
                mvG_deep     = 1.-1./(exp(lnnm1_deep)+1.) ! ln_n = ln(n-1); Woesten et al 1999
              
                zporv_deep   = 0.7919+0.001691*C_deep-0.29619*D_deep-0.000001491*S_deep**2. +0.0000821*OM_deep**2. &
                     +0.02427*C_deep**(-1.) +0.01113*S_deep**(-1.)+ 0.01472*LOG(S_deep)-0.0000733*OM_deep*C_deep &
                     -0.000619*D_deep*C_deep-0.001183*D_deep*OM_deep-0.0001664*subsoil*S_deep              
             
                zpwp_deep    = zadp + ((zporv-zadp)/ &
                     (1.+avG* psi_pwp*9.81)**mvG)
                
                zfcap_deep   = zadp + ((zporv-zadp)/ &
                     (1.+avG* psi_fcap*9.81)**mvG)
              

                IF (OM_deep>20..OR.zporv_deep>0.7) THEN ! Treatment of Peat with Histosols
                  
                  zporv_deep    = 0.766
                  zfcap_deep    = 0.663
                  zpwp_deep     = 0.267
                  zkw_deep      = 0.93E-6
                  avG_deep      = 1.3
                
                  mvG_deep      = 1.-1./1.20
                
                ENDIF

                fr_dm_deep(i,j,k)=zkw_deep*86400.*100.
                
              ELSE

                fr_dm_deep(i,j,k)=-1.

              ENDIF

              ! Where the fractions of all 4 parameters are undefined 
              ! it is assumed that the soil depth is less than 30 cm
              ! thus the soiltype is set to rock (2).

              IF (fr_sand_deep(i,j,k)< 0 .and. fr_silt_deep(i,j,k)< 0 .and. &
                  fr_clay_deep(i,j,k)< 0 .and. fr_oc_deep(i,j,k)< 0 .and. &
                  soiltype_deep(i,j,k) == 0) THEN
                soiltype_deep(i,j,k) = 2
              ENDIF

            ENDIF

          ENDDO
        ENDDO
      ENDDO
      
    END SUBROUTINE calculate_soiltype

    !-----------------------------------------------------------------------------------------------------------------------!

    SUBROUTINE read_namelists_extpar_HWSD_index(namelist_file,        &
                                                path_HWSD_index_files,&
                                                lookup_table_HWSD,   &
                                                HWSD_data,            &
                                                HWSD_data_deep,       &
                                                HWSD_data_extpar)

      CHARACTER (len=filename_max), INTENT(IN) :: namelist_file !< filename with namelists for for EXTPAR settings

      ! HWSD idex files
      CHARACTER (len=filename_max) :: path_HWSD_index_files
      CHARACTER (len=filename_max) :: lookup_table_HWSD   
      CHARACTER (len=filename_max) :: HWSD_data   
      CHARACTER (len=filename_max) :: HWSD_data_deep    
      CHARACTER (len=filename_max) :: HWSD_data_extpar    

      !>Define the namelist group for soil raw data
      NAMELIST /HWSD_index_files/ path_HWSD_index_files, lookup_table_HWSD, HWSD_data, HWSD_data_deep, HWSD_data_extpar

      INTEGER           :: nuin !< unit number
      INTEGER (KIND=i4) :: ierr !< error flag


      nuin = free_un()  ! functioin free_un returns free Fortran unit number
      OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)
      
      READ(nuin, NML=HWSD_index_files, IOSTAT=ierr)

      CLOSE(nuin)
  

 END SUBROUTINE read_namelists_extpar_HWSD_index


END MODULE mo_soil_consistency
