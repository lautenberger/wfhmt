! *****************************************************************************
PROGRAM FIRE_DYNAMICS
! *****************************************************************************
! This program calculates fire dynamics across across the landscape using 
! a standard surface fire spread methodology (Rothermel). Crown fire initiation
! and type (passive/active) is calculated from the Van Wagner criteria.

USE VARS
USE IO
USE SUBS
USE SURFACE_SPREAD_VARS, ONLY : FUEL_MODEL_TABLE_2D
USE FIRE_DYNAMICS_MODULE
USE FIRE_DYNAMICS_IO, ONLY : READ_FUEL_MODEL_TABLE

IMPLICIT NONE

INTEGER :: I, IX, IY, ILH, IOS, IBAND, NBANDS, NX, NY
CHARACTER(400) :: NAMELIST_FN, FN
REAL, POINTER, DIMENSION(:,:) :: ADJ2D, SLP2D
INTEGER*2, POINTER, DIMENSION(:,:) :: FBFM2D
LOGICAL :: DUMP_FLAME_LENGTH, DUMP_FLIN, DUMP_VELOCITY_DMS, DUMP_HPUA, DUMP_IR, &
           USE_CONSTANT_FBFM, USE_CONSTANT_SLP, USE_CONSTANT_WAF, USE_CONSTANT_ADJ, &
           USE_CONSTANT_LH, USE_CONSTANT_LW, WS_AT_10M, WS_IN_MPS
INTEGER*2 :: CONSTANT_FBFM, CONSTANT_SLP
REAL :: CONSTANT_WAF, CONSTANT_ADJ

NAMELIST /FIRE_DYNAMICS_INPUTS/ FUELS_AND_TOPOGRAPHY_DIRECTORY, WEATHER_DIRECTORY, FIRE_DYNAMICS_DIRECTORY, MISCELLANEOUS_INPUTS_DIRECTORY, FUEL_MODEL_FILE, &
         ADJ_FILENAME, CBD_FILENAME, CBH_FILENAME, CC_FILENAME, CH_FILENAME, FBFM_FILENAME, WAF_FILENAME, &
         ASP_FILENAME, SLP_FILENAME, DEM_FILENAME, &
         WS_FILENAME, WD_FILENAME, TMP_FILENAME, RH_FILENAME, & 
         MEQ_FILENAME, M1_FILENAME, M10_FILENAME, M100_FILENAME, M1_MINUS_MEQ, M10_MINUS_MEQ, M100_MINUS_MEQ, &
         USE_CONSTANT_LH, MLH_FILENAME, LH_MOISTURE_CONTENT, &
         USE_CONSTANT_LW, MLW_FILENAME, LW_MOISTURE_CONTENT, &
         FLAME_LENGTH_FILENAME, FLIN_FILENAME, VELOCITY_DMS_FILENAME, HPUA_FILENAME, IR_FILENAME, &
         CBD_TIMES_100, CBH_TIMES_10, CC_IN_PERCENT, CH_TIMES_10, &
         PATH_TO_GDAL, SCRATCH, &
         DUMP_FLAME_LENGTH, DUMP_FLIN, DUMP_VELOCITY_DMS, DUMP_HPUA, DUMP_IR, &
         USE_CONSTANT_FBFM, CONSTANT_FBFM, USE_CONSTANT_SLP, CONSTANT_SLP, USE_CONSTANT_WAF, CONSTANT_WAF, USE_CONSTANT_ADJ, CONSTANT_ADJ, &
         WS_AT_10M, WS_IN_MPS

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &FIRE_DYNAMICS_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Set defaults:
CALL SET_NAMELIST_DEFAULTS
DUMP_FLAME_LENGTH = .TRUE.
DUMP_FLIN         = .TRUE.
DUMP_VELOCITY_DMS = .TRUE.
DUMP_HPUA         = .TRUE.
DUMP_IR           = .TRUE.
USE_CONSTANT_FBFM = .FALSE.
USE_CONSTANT_SLP  = .FALSE.
USE_CONSTANT_WAF  = .FALSE.
USE_CONSTANT_ADJ  = .TRUE.
WS_AT_10M         = .FALSE.
WS_IN_MPS         = .FALSE.

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=FIRE_DYNAMICS_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &FIRE_DYNAMICS_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

!Get operating system (linux or windows/dos)
CALL GET_OPERATING_SYSTEM

! Check for errors
CALL GENERAL_ERROR_CHECKING
CALL FIRE_DYNAMICS_ERROR_CHECKING

! Read fuel model table
CALL READ_FUEL_MODEL_TABLE

! Get coordinate system string
FN=TRIM(WEATHER_DIRECTORY) // TRIM(WS_FILENAME)
CALL GET_COORDINATE_SYSTEM(FN)

! Set tan(slp^2)
DO I=0,90
   TANSLP2(I) = TAN(REAL(I)*PIO180)
   COSSLP (I) = COS(REAL(I)*PIO180)
ENDDO
TANSLP2(:)=TANSLP2(:)*TANSLP2(:)

! Begin reading inputs

! Wind and weather:
CALL READ_BSQ_RASTER(WS   , WEATHER_DIRECTORY, WS_FILENAME )
!CALL READ_BSQ_RASTER(WD   , WEATHER_DIRECTORY, WD_FILENAME )
!CALL READ_BSQ_RASTER(TMP  , WEATHER_DIRECTORY, TMP_FILENAME )
!CALL READ_BSQ_RASTER(RH   , WEATHER_DIRECTORY, RH_FILENAME )

NBANDS = WS%NBANDS
NX     = WS%NCOLS
NY     = WS%NROWS

! Topography:
!CALL READ_BSQ_RASTER(ASP  , FUELS_AND_TOPOGRAPHY_DIRECTORY, ASP_FILENAME )
IF (USE_CONSTANT_SLP) THEN
   CALL ALLOCATE_EMPTY_RASTER(SLP,NX,NY,1,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,2,'SIGNEDINT ')
   SLP%I2ZT(1,:,:) = CONSTANT_SLP
ELSE
   CALL READ_BSQ_RASTER(SLP  , FUELS_AND_TOPOGRAPHY_DIRECTORY, SLP_FILENAME )
ENDIF
!CALL READ_BSQ_RASTER(DEM  , FUELS_AND_TOPOGRAPHY_DIRECTORY, DEM_FILENAME )

! Fuels:
IF (USE_CONSTANT_ADJ) THEN
   CALL ALLOCATE_EMPTY_RASTER(ADJ,NX,NY,1,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,2,'FLOAT     ')   
   ADJ%RZT(1,:,:) = CONSTANT_ADJ
ELSE
   CALL READ_BSQ_RASTER(ADJ  , FUELS_AND_TOPOGRAPHY_DIRECTORY, ADJ_FILENAME  )
ENDIF

!CALL READ_BSQ_RASTER(CBD  , FUELS_AND_TOPOGRAPHY_DIRECTORY, CBD_FILENAME  )
!CALL READ_BSQ_RASTER(CBH  , FUELS_AND_TOPOGRAPHY_DIRECTORY, CBH_FILENAME  )
!CALL READ_BSQ_RASTER(CC   , FUELS_AND_TOPOGRAPHY_DIRECTORY, CC_FILENAME   )
!CALL READ_BSQ_RASTER(CH   , FUELS_AND_TOPOGRAPHY_DIRECTORY, CH_FILENAME   )

IF (USE_CONSTANT_FBFM) THEN
   CALL ALLOCATE_EMPTY_RASTER(FBFM,NX,NY,1,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,2,'SIGNEDINT ')
   FBFM%I2ZT(1,:,:) = CONSTANT_FBFM
ELSE
   CALL READ_BSQ_RASTER(FBFM , FUELS_AND_TOPOGRAPHY_DIRECTORY, FBFM_FILENAME )
ENDIF

IF (USE_CONSTANT_WAF) THEN
   CALL ALLOCATE_EMPTY_RASTER(WAF,NX,NY,1,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,2,'FLOAT     ')   
   WAF%RZT(1,:,:) = CONSTANT_WAF
ELSE
   CALL READ_BSQ_RASTER(WAF  , FUELS_AND_TOPOGRAPHY_DIRECTORY, WAF_FILENAME  )
ENDIF

! Moisture:
IF (M1_MINUS_MEQ .GT. 0 .OR. M10_MINUS_MEQ .GT. 0 .OR. M100_MINUS_MEQ .GT. 0) THEN
   CALL READ_BSQ_RASTER(MEQ  , WEATHER_DIRECTORY, MEQ_FILENAME )
ELSE
   CALL READ_BSQ_RASTER(M1   , WEATHER_DIRECTORY, M1_FILENAME )
   CALL READ_BSQ_RASTER(M10  , WEATHER_DIRECTORY, M10_FILENAME )
   CALL READ_BSQ_RASTER(M100 , WEATHER_DIRECTORY, M100_FILENAME )
ENDIF

IF (M1_MINUS_MEQ .GT. 0 .OR. M10_MINUS_MEQ .GT. 0 .OR. M100_MINUS_MEQ .GT. 0) THEN
   CALL ALLOCATE_EMPTY_RASTER(M1  ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')   
   CALL ALLOCATE_EMPTY_RASTER(M10 ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')   
   CALL ALLOCATE_EMPTY_RASTER(M100,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')   
ENDIF

IF (USE_CONSTANT_LH) THEN
   CALL ALLOCATE_EMPTY_RASTER(MLH,NX,NY,1,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,2,'FLOAT     ')   
   MLH%RZT(1,:,:) = LH_MOISTURE_CONTENT
ELSE
   CALL READ_BSQ_RASTER(MLH, FUELS_AND_TOPOGRAPHY_DIRECTORY, MLH_FILENAME  )
ENDIF
! Need to adjust this for variable LH / LW
WHERE(MLH%RZT(:,:,:) .NE. MLH%NODATA_VALUE) MLH%RZT(:,:,:) = 0.01 * MLH%RZT(:,:,:) 

IF (USE_CONSTANT_LW) THEN
   CALL ALLOCATE_EMPTY_RASTER(MLW,NX,NY,1,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,2,'FLOAT     ')   
   MLW%RZT(1,:,:) = LW_MOISTURE_CONTENT
ELSE
   CALL READ_BSQ_RASTER(MLW, FUELS_AND_TOPOGRAPHY_DIRECTORY, MLW_FILENAME  )
ENDIF
WHERE(MLW%RZT(:,:,:) .NE. MLW%NODATA_VALUE) MLW%RZT(:,:,:) = 0.01 * MLW%RZT(:,:,:) 

! Allocate empty output rasters:
CALL ALLOCATE_EMPTY_RASTER(VS0            ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')   
CALL ALLOCATE_EMPTY_RASTER(FLIN           ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(FLAME_LENGTH   ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(HPUA           ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(IR             ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(VELOCITY_DMS   ,NX,NY,NBANDS,WAF%XLLCORNER,WAF%YLLCORNER,WAF%XDIM,WAF%YDIM,WAF%NODATA_VALUE,2,'FLOAT     ')

! Need to allocate arrays that are not read in:
ALLOCATE(PHIS    (1:NX,1:NY))
ALLOCATE(PHIW    (1:NX,1:NY))
ALLOCATE(M1_NOW  (1:NX,1:NY))
ALLOCATE(M10_NOW (1:NX,1:NY))
ALLOCATE(M100_NOW(1:NX,1:NY))
ALLOCATE(WSMF    (1:NX,1:NY))
ALLOCATE(FBFM2D  (1:NX,1:NY))
ALLOCATE(SLP2D   (1:NX,1:NY))
ALLOCATE(ADJ2D   (1:NX,1:NY))

!! CC, CH, CBH, and CBD are normally read in as integers, but need to be converted to real
!IF (ALLOCATED(CC%I2ZT)) THEN
!   ALLOCATE(CC%RZT(1,1:CC%NCOLS,1:CC%NROWS))
!   IF (CC_IN_PERCENT) THEN
!      WHERE(CC%RZT(:,:,:) .NE. CC%NODATA_VALUE) CC%RZT(:,:,:) = 0.01*REAL(CC%I2ZT(:,:,:))
!   ELSE
!      CC%RZT(:,:,:) = REAL(CC%I2ZT(:,:,:))
!   ENDIF
!   DEALLOCATE(CC%I2ZT)
!ELSE
!   IF (CC_IN_PERCENT) WHERE(CC%RZT(:,:,:) .NE. CC%NODATA_VALUE) CC%RZT(:,:,:) = 0.01 * CC%RZT(:,:,:) 
!ENDIF
!
!IF (ALLOCATED(CH%I2ZT)) THEN
!   ALLOCATE(CH%RZT(1,1:CH%NCOLS,1:CH%NROWS))
!   IF (CH_TIMES_10) THEN
!      WHERE (CH%RZT(:,:,:) .NE. CH%NODATA_VALUE) CH%RZT(:,:,:) = 0.1*REAL(CH%I2ZT(:,:,:))
!   ELSE
!      CH%RZT(:,:,:) = REAL(CH%I2ZT(:,:,:))
!   ENDIF
!   DEALLOCATE(CH%I2ZT)
!ELSE
!   IF (CH_TIMES_10) WHERE(CH%RZT(:,:,:) .NE. CH%NODATA_VALUE) CH%RZT(:,:,:) = 0.1 * CH%RZT(:,:,:) 
!ENDIF
!
!IF (ALLOCATED(CBH%I2ZT)) THEN
!   ALLOCATE(CBH%RZT(1,1:CBH%NCOLS,1:CBH%NROWS))
!   IF (CBH_TIMES_10) THEN
!      WHERE (CBH%RZT(:,:,:) .NE. CBH%NODATA_VALUE) CBH%RZT(:,:,:) = 0.1*REAL(CBH%I2ZT(:,:,:))
!   ELSE
!      CBH%RZT(:,:,:) = REAL(CBH%I2ZT(:,:,:))
!   ENDIF
!   DEALLOCATE(CBH%I2ZT)
!ELSE
!   IF (CBH_TIMES_10) WHERE(CBH%RZT(:,:,:) .NE. CBH%NODATA_VALUE) CBH%RZT(:,:,:) = 0.1 * CBH%RZT(:,:,:) 
!ENDIF
!
!IF (ALLOCATED(CBD%I2ZT)) THEN
!   ALLOCATE(CBD%RZT(1,1:CBD%NCOLS,1:CBD%NROWS))
!   IF (CBD_TIMES_100) THEN
!      WHERE (CBD%RZT(:,:,:) .NE. CBD%NODATA_VALUE) CBD%RZT(:,:,:) = 0.01*REAL(CBD%I2ZT(:,:,:))
!   ELSE
!      CBD%RZT(:,:,:) = REAL(CBD%I2ZT(:,:,:))
!   ENDIF
!   DEALLOCATE(CBD%I2ZT)
!ELSE
!   IF (CBD_TIMES_100) WHERE(CBD%RZT(:,:,:) .NE. CBD%NODATA_VALUE) CBD%RZT(:,:,:) = 0.01 * CBD%RZT(:,:,:) 
!ENDIF

IF (M1_MINUS_MEQ .GT. 0 .OR. M10_MINUS_MEQ .GT. 0 .OR. M100_MINUS_MEQ .GT. 0) THEN
   DO IBAND = 1, NBANDS
      WHERE(MEQ%RZT (IBAND,:,: ) .NE. MEQ %NODATA_VALUE ) M1   %RZT (IBAND,:,:) = 0.01*(MEQ %RZT(IBAND,:,:) + M1_MINUS_MEQ)
      WHERE(MEQ%RZT (IBAND,:,: ) .NE. MEQ %NODATA_VALUE ) M10  %RZT (IBAND,:,:) = 0.01*(MEQ %RZT(IBAND,:,:) + M10_MINUS_MEQ)
      WHERE(MEQ%RZT (IBAND,:,: ) .NE. MEQ %NODATA_VALUE ) M100 %RZT (IBAND,:,:) = 0.01*(MEQ %RZT(IBAND,:,:) + M100_MINUS_MEQ)
   ENDDO
   DEALLOCATE(MEQ%RZT)
ELSE
   DO IBAND = 1, NBANDS
      WHERE(M1%RZT   (IBAND,:,: ) .NE. M1  %NODATA_VALUE ) M1  %RZT (IBAND,:,:) = 0.01 * M1  %RZT(IBAND,:,:)
      WHERE(M10%RZT  (IBAND,:,: ) .NE. M10 %NODATA_VALUE ) M10 %RZT (IBAND,:,:) = 0.01 * M10 %RZT(IBAND,:,:)
      WHERE(M100%RZT (IBAND,:,: ) .NE. M100%NODATA_VALUE ) M100%RZT (IBAND,:,:) = 0.01 * M100%RZT(IBAND,:,:)
   ENDDO
ENDIF

IF (WS_AT_10M) WHERE(WS%RZT(:,:,: ) .NE. WS%NODATA_VALUE) WS%RZT(:,:,:) = 0.87*WS%RZT(:,:,:)
IF (WS_IN_MPS) WHERE(WS%RZT(:,:,: ) .NE. WS%NODATA_VALUE) WS%RZT(:,:,:) = WS%RZT(:,:,:) / 0.447

FBFM2D(:,:) = FBFM%I2ZT(1,:,:)
SLP2D (:,:) = REAL(SLP%I2ZT (1,:,:))
ADJ2D (:,:) = ADJ%RZT (1,:,:)

DO IBAND = 1, NBANDS

   M1_NOW  (:,:) = M1  %RZT(IBAND,:,:)
   M10_NOW (:,:) = M10 %RZT(IBAND,:,:)
   M100_NOW(:,:) = M100%RZT(IBAND,:,:)
   WSMF    (:,:) = WAF%RZT(1,:,:)*WS%RZT(IBAND,:,:)*5280./60. !Convert mph to ft/min

   CALL SURFACE_SPREAD_RATE(NX,NY,FBFM2D,SLP2D,PHIS,PHIW,ADJ2D,IBAND)

   WHERE (VS0%RZT(IBAND,:,:) .NE. VS0%NODATA_VALUE) VELOCITY_DMS%RZT(IBAND,:,:) = VS0%RZT(IBAND,:,:) * (1. + PHIS(:,:) + PHIW(:,:)) !ft/min

!$omp PARALLEL DO SCHEDULE(STATIC) PRIVATE(IX,IY,ILH) &
!$omp SHARED(HPUA,FLIN,FLAME_LENGTH,NX,NY,VS0,IR,FUEL_MODEL_TABLE_2D,VELOCITY_DMS)
   DO IY = 1, NY
   DO IX = 1, NX
      IF (VS0%RZT(IBAND,IX,IY) .NE. VS0%NODATA_VALUE) THEN 
         ILH = MAX(MIN(NINT(100.*MLH%RZT(1,IX,IY)),120),30)
         HPUA%RZT(IBAND,IX,IY) = IR%RZT(IBAND,IX,IY) * FUEL_MODEL_TABLE_2D(FBFM%I2ZT(1,IX,IY),ILH)%TR * 60. ! kJ/m2
         FLIN%RZT(IBAND,IX,IY) = FUEL_MODEL_TABLE_2D(FBFM%I2ZT(1,IX,IY),ILH)%TR * IR%RZT(IBAND,IX,IY) * VELOCITY_DMS%RZT(IBAND,IX,IY) * 0.3048 ! kW/m
         FLAME_LENGTH%RZT(IBAND,IX,IY) = (0.0775 / 0.3048) * FLIN%RZT(IBAND,IX,IY) ** 0.46  !ft
      ENDIF
   ENDDO
   ENDDO
!$omp END PARALLEL DO

ENDDO

! Now write to disk:
IF (DUMP_FLAME_LENGTH) CALL WRITE_BIL_RASTER(FLAME_LENGTH , FIRE_DYNAMICS_DIRECTORY, FLAME_LENGTH_FILENAME, .TRUE., .TRUE. ) !ft
IF (DUMP_FLIN        ) CALL WRITE_BIL_RASTER(FLIN         , FIRE_DYNAMICS_DIRECTORY, FLIN_FILENAME        , .TRUE., .TRUE. ) !kW/m
IF (DUMP_VELOCITY_DMS) CALL WRITE_BIL_RASTER(VELOCITY_DMS , FIRE_DYNAMICS_DIRECTORY, VELOCITY_DMS_FILENAME, .TRUE., .TRUE. ) !ft/min
IF (DUMP_HPUA        ) CALL WRITE_BIL_RASTER(HPUA         , FIRE_DYNAMICS_DIRECTORY, HPUA_FILENAME        , .TRUE., .TRUE. ) !kJ/m2
IF (DUMP_IR          ) CALL WRITE_BIL_RASTER(IR           , FIRE_DYNAMICS_DIRECTORY, IR_FILENAME          , .TRUE., .TRUE. ) !kW/m2

STOP

! *****************************************************************************
END PROGRAM FIRE_DYNAMICS
! *****************************************************************************