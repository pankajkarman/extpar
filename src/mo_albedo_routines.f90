!+ Fortran module with Albedo data handling routines
!
! History:
! Version      Date       Name
! ------------ ---------- ----
! V1_8         2013-03-12 Frank Brenner
!  introduced MODIS albedo dataset(s) as new external parameter(s)
! V1_11        2013/04/16 Juergen Helmert
!  Adaptions for using external land-sea mask  
! V1_12        2013-04-24 Frank Brenner
!  bug fix regarding old file paths         
! V1_13        2013-05-29 Frank Brenner
!  missing values fixed         
! V2_0_3       2015-01-12 Juergen Helmert
!  Bugfix in ICON part
!
! Code Description:
! Language: Fortran 2003.
!=======================================================================
!> Fortran module with Albedo data handling routines
!> \author Hermann Asensio, Frank Brenner
!>
MODULE mo_albedo_routines

  USE mo_logging
  USE mo_kind,                    ONLY: wp, i4
  USE mo_utilities_extpar,        ONLY: free_un ! function to get free unit number
  USE mo_io_units,                ONLY: filename_max

  USE netcdf,                     ONLY:   &
    &                                   nf90_open,              &
    &                                   nf90_close,             &
    &                                   nf90_nowrite

  USE mo_io_utilities,            ONLY: check_netcdf

  USE mo_albedo_tg_fields,        ONLY: alb_interpol
  USE mo_soil_tg_fields,          ONLY: soiltype_fao
  USE mo_target_grid_data,        ONLY: tg, &
    &                                   lon_geo,lat_geo

  USE mo_bilinterpol,             ONLY: calc_weight_bilinear_interpol, &
    &                                   calc_value_bilinear_interpol

  USE mo_albedo_data,             ONLY: zalso
  USE  mo_icon_grid_data,         ONLY: icon_grid_region

  IMPLICIT NONE

  PRIVATE

  PUBLIC    open_netcdf_ALB_data, &
    &       read_namelists_extpar_alb, &
    &       const_check_interpol_alb

  CONTAINS

  !---------------------------------------------------------------------------
  !> subroutine to read namelist including albedo data settings for EXTPAR 
  SUBROUTINE read_namelists_extpar_alb(namelist_file, &
      &                                  raw_data_alb_path, &
      &                                  raw_data_alb_filename, &
      &                                  raw_data_alnid_filename, &
      &                                  raw_data_aluvd_filename, &
      &                                  ialb_type,         &
      &                                  alb_buffer_file, &
      &                                  alb_output_file, &
      &                                  alb_source, &
      &                                  alnid_source, &
      &                                  aluvd_source)



    
    CHARACTER (len=*), INTENT(IN)             :: namelist_file !< filename with namelists for for EXTPAR settings

  ! NDVI
    CHARACTER (len=filename_max)              :: raw_data_alb_path, &        !< path to raw data
      &                                          raw_data_alb_filename, & !< filenames albedo raw data
      &                                          raw_data_alnid_filename, &
      &                                          raw_data_aluvd_filename, &
      &                                          alb_buffer_file, & !< name for albedo buffer file
      &                                          alb_output_file, & !< name for albedo output file
      &                                          alb_source, &
      &                                          alnid_source, &
      &                                          aluvd_source

    INTEGER (KIND=i4)                         :: ialb_type, nuin, ierr

  !> namelist with filenames for albedo data input
    NAMELIST /alb_raw_data/ raw_data_alb_path, raw_data_alb_filename, ialb_type
    NAMELIST /alnid_raw_data/ raw_data_alb_path, raw_data_alnid_filename
    NAMELIST /aluvd_raw_data/ raw_data_alb_path, raw_data_aluvd_filename
  !> namelist with filenames for albedo data output
    NAMELIST /alb_io_extpar/ alb_buffer_file, alb_output_file
    NAMELIST /alb_source_file/ alb_source, alnid_source, aluvd_source

    nuin = free_un()  ! functioin free_un returns free Fortran unit number

    OPEN(nuin,FILE=TRIM(namelist_file), IOSTAT=ierr)
    IF (ierr /= 0) THEN
      WRITE(message_text,*)'Cannot open ', TRIM(namelist_file)
      CALL logging%error(message_text,__FILE__, __LINE__) 
    ENDIF

    READ(nuin, NML=alb_raw_data, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist alb_raw_data',__FILE__, __LINE__) 
    ENDIF

    IF ((ialb_type < 1).OR.(ialb_type > 3)) THEN
      WRITE(message_text,*) 'ialb_type must be in the range 1-3. It is now:',ialb_type
      CALL logging%error(message_text,__FILE__,__LINE__)
    ENDIF

    IF ((ialb_type /= 2).AND.(ialb_type /= 3)) THEN
      READ(nuin, NML=alnid_raw_data, IOSTAT=ierr)
      IF (ierr /= 0) THEN
        CALL logging%error('Cannot read in namelist alnid_raw_data',__FILE__, __LINE__) 
      ENDIF

      READ(nuin, NML=aluvd_raw_data, IOSTAT=ierr)
      IF (ierr /= 0) THEN
        CALL logging%error('Cannot read in namelist aluvd_raw_data',__FILE__, __LINE__) 
      ENDIF

    ENDIF
    READ(nuin, NML=alb_io_extpar, IOSTAT=ierr)
    IF (ierr /= 0) THEN
      CALL logging%error('Cannot read in namelist alb_io_extpar',__FILE__, __LINE__) 
    ENDIF

    IF (ialb_type /= 2) THEN
      READ(nuin, NML=alb_source_file, IOSTAT=ierr)
      IF (ierr /= 0) THEN
        CALL logging%error('Cannot read in namelist alb_source_file',__FILE__, __LINE__) 
      ENDIF
    ENDIF
    CLOSE(nuin)
    
  END SUBROUTINE read_namelists_extpar_alb
  !---------------------------------------------------------------------------

  !> open netcdf-file and get netcdf unit file number
  SUBROUTINE open_netcdf_ALB_data(path_alb_file, &
                                          ncid)
    CHARACTER (len=*), INTENT(IN) :: path_alb_file     !< filename with path for albedo raw data
    INTEGER, INTENT(OUT)          :: ncid                       !< netcdf unit file number
    !! open netcdf file 
    CALL check_netcdf( nf90_open(TRIM(path_alb_file),NF90_NOWRITE, ncid))

  END SUBROUTINE open_netcdf_ALB_data

  SUBROUTINE const_check_interpol_alb(alb_field_mom_d,fr_land_lu,alb_min)


    REAL(KIND=wp), INTENT(INOUT) :: alb_field_mom_d(:,:,:,:)

    REAL(KIND=wp), INTENT(IN)    :: fr_land_lu(:,:,:), &
         &                          alb_min

    INTEGER (KIND=i4), PARAMETER :: mpy=12     !< month per year
    
    INTEGER (KIND=i4)            :: i_miss, &
         &                          t,k,j,i,i2,j2, &
         &                          igrid_type 

    REAL (KIND=wp)               :: lon_geo_w,lon_geo_e,lat_geo_n,lat_geo_s, &
         &                          alb_sw,alb_nw,alb_se,alb_ne, &
         &                          bwlon,bwlat

    igrid_type = tg%igrid_type

    !preparing albedo values for interpolation, original data is copied to
    !alb_interpol to easier remove the procedure if necessary.
    !at the end alb_interpol is written in alb_field_mom

    DO t=1, mpy
      DO k=1,tg%ke
        DO j=1,tg%je
          DO i=1,tg%ie
            alb_interpol(i,j,k,t) = alb_field_mom_d(i,j,k,t)
          ENDDO
        ENDDO
      ENDDO
    ENDDO

    DO t=1, mpy
      DO k=1,tg%ke
        DO j=1,tg%je
          DO i=1,tg%ie
            IF (igrid_type.eq.2) THEN !COSMO interpolation
              !albedo < 0.07 and no water point
              IF ((alb_interpol(i,j,k,t).lt.alb_min).AND.(fr_land_lu(i,j,k).ge.0.5)) THEN 
                  !avoiding 'out of range' errors at the border
                j2 = j
                i2 = i
                IF (j.EQ.(tg%je)) THEN
                  j2 = j-1
                ENDIF
                IF (j.EQ.1) THEN
                  j2 = j+1
                ENDIF
                IF (i.EQ.(tg%ie)) THEN
                  i2 = i-1
                ENDIF
                IF (i.EQ.1) THEN
                  i2 = i+1
                ENDIF

                !coordinates and albedo values for neighbor points

                lon_geo_w = lon_geo(i2-1,j2,k)
                lon_geo_e = lon_geo(i2+1,j2,k)
                lat_geo_n = lat_geo(i2,j2+1,k)
                lat_geo_s = lat_geo(i2,j2-1,k)

                alb_sw = alb_interpol(i2-1,j2-1,k,t)
                alb_se = alb_interpol(i2+1,j2-1,k,t)
                alb_nw = alb_interpol(i2-1,j2+1,k,t)
                alb_ne = alb_interpol(i2+1,j2+1,k,t)

                !calculate weights for interpolation
                !these weights might give wrong results, if there are no 4 valid
                !points surrounding the interpolated point(i,j)
                CALL calc_weight_bilinear_interpol(lon_geo(i,j,k), &
                                                    lat_geo(i,j,k), &
                                                    lon_geo_w,      &
                                                    lon_geo_e,      &
                                                    lat_geo_n,     &
                                                    lat_geo_s,     &
                                                    bwlon,         &
                                                    bwlat)
                 
                i_miss = 0

                IF (fr_land_lu(i2-1,j2-1,k).LT.0.5) THEN
                  alb_sw = 0.
                  i_miss = i_miss + 1
                ENDIF
                IF (fr_land_lu(i2+1,j2-1,k).LT.0.5) THEN
                  alb_se = 0.
                  i_miss = i_miss + 1
                ENDIF
                IF (fr_land_lu(i2-1,j2+1,k).LT.0.5) THEN
                  alb_nw = 0.
                  i_miss = i_miss + 1
                ENDIF
                IF (fr_land_lu(i2+1,j2+1,k).LT.0.5) THEN
                  alb_ne = 0.
                  i_miss = i_miss + 1
                ENDIF


                IF (i_miss.EQ.4) THEN
                !    if there are no valid interpolation values, the next step is skipped  
                  i_miss = 5
                  GOTO 100
                ENDIF
                 
                alb_interpol(i,j,k,t) = calc_value_bilinear_interpol(bwlon, bwlat, &
                                        alb_sw, alb_se, alb_ne, alb_nw)*(4/(4-i_miss))

    100         IF (alb_interpol(i,j,k,t).LT.alb_min.and. soiltype_fao(i,j,k).LE.9 .AND. soiltype_fao(i,j,k).GE.0) THEN
                  !values that are still too small, will receive a soiltype dependent albedo
                  alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)*fr_land_lu(i,j,k) + &
                                            alb_min*(1.-fr_land_lu(i,j,k))
                ELSEIF (alb_interpol(i,j,k,t).LT.alb_min .AND. (soiltype_fao(i,j,k).GT.9 &
                        & .OR. soiltype_fao(i,j,k).EQ.0)) THEN
                  alb_interpol(i,j,k,t) = alb_min
                ENDIF
  !override wrong values due to bad interpolation (occurs only at some borderpoints)
                IF (alb_interpol(i,j,k,t).GT.0.7) THEN
                  alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)*fr_land_lu(i,j,k) + &
                                            alb_min*(1.-fr_land_lu(i,j,k))
                ENDIF

                !water points:
              ELSEIF ((alb_interpol(i,j,k,t).LT.alb_min).AND.(fr_land_lu(i,j,k).LT.0.5)) THEN
                alb_interpol(i,j,k,t) = alb_min
              ELSEIF ((alb_interpol(i,j,k,t).GE.alb_min).AND.(fr_land_lu(i,j,k).LT.0.5)) THEN
                alb_interpol(i,j,k,t) = alb_min
              ENDIF

            ELSEIF (igrid_type.EQ.1) THEN !ICON interpolation 

              IF (fr_land_lu(i,j,k).LT.0.01) THEN
                !water point
                alb_interpol(i,j,k,t) = 0.07
              ELSEIF (alb_interpol(i,j,k,t).LE.0.) THEN
                !land point with albedo = 0
   
                alb_se = alb_interpol(icon_grid_region%cells%neighbor_index(i,1),j,k,t)
                alb_nw = alb_interpol(icon_grid_region%cells%neighbor_index(i,2),j,k,t)
                alb_ne = alb_interpol(icon_grid_region%cells%neighbor_index(i,3),j,k,t)

                IF (alb_se.GT.0.AND.alb_nw.GT.0.AND.alb_ne.GT.0) THEN
                  alb_interpol(i,j,k,t) = (alb_se+alb_nw+alb_ne)/3
                ELSE IF (alb_se.GT.0.AND.alb_nw.GT.0) THEN
                  alb_interpol(i,j,k,t) = (alb_se+alb_nw)/2
                ELSE IF (alb_nw.GT.0.AND.alb_ne.GT.0) THEN
                  alb_interpol(i,j,k,t) = (alb_nw+alb_ne)/2
                ELSE IF (alb_ne.GT.0.AND.alb_se.GT.0) THEN
                  alb_interpol(i,j,k,t) = (alb_ne+alb_se)/2
                ELSE
                  alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)*fr_land_lu(i,j,k) + &
                                            0.07*(1.-fr_land_lu(i,j,k))
                ENDIF

              ENDIF     
            ENDIF  !ICON interpolation

             !Gletscher
            IF ((soiltype_fao(i,j,k).EQ.1).AND.(fr_land_lu(i,j,k).GE.0.01)) THEN
              alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)
            ENDIF
   
            !catching false values (occurs only at the borders)
            IF (alb_interpol(i,j,k,t).gt.0.7) THEN
              alb_interpol(i,j,k,t) = zalso(soiltype_fao(i,j,k),t)*fr_land_lu(i,j,k) + &
                                     0.07*(1.-fr_land_lu(i,j,k))
            ENDIF

          ENDDO !i
        ENDDO !j
      ENDDO !k
    ENDDO !t           


    DO t=1, mpy
      DO k=1,tg%ke
        DO j=1,tg%je
          DO i=1,tg%ie
            alb_field_mom_d(i,j,k,t) = 100*alb_interpol(i,j,k,t)
          ENDDO
        ENDDO
      ENDDO
    ENDDO
     
  END SUBROUTINE const_check_interpol_alb

END MODULE mo_albedo_routines
