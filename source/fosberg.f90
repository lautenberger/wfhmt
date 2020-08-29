! *****************************************************************************
PROGRAM FOSBERG
! *****************************************************************************
! This program calculates equilibrium moisture content and Fosberg fire weather 
! from (potentially) "stacked" rasters of temperature, humidity, and wind speed. 
! Also calculated is a "modified" Fosberg index that is standard Fosberg index 
! multiplied by the Schroeder ember ignition probability

USE VARS
USE IO
USE SUBS

IMPLICIT NONE

INTEGER :: IBAND, IOS, IROW, ICOL
REAL :: MO30, ETA, PROBIGN
CHARACTER(400) :: FN, NAMELIST_FN
LOGICAL :: CALC_FFWI_FROM_M1, M1_IN_PERCENT

NAMELIST /FOSBERG_INPUTS/ INPUT_DIRECTORY, OUTPUT_DIRECTORY, A_SRS, WS_FILENAME, RH_FILENAME, TMP_FILENAME, &
                          MEQ_FILENAME, FFWI_FILENAME, MFFWI_FILENAME, WS_IN_METERS_PER_SECOND, TMP_IN_CELSIUS, &
                          LOWPASS_TEMP, PATH_TO_GDAL, SCRATCH, CONVERT_TO_GEOTIFF, COMPRESS, CALC_FFWI_FROM_M1, &
                          M1_IN_PERCENT

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &FOSBERG_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Set defaults:
CALL SET_NAMELIST_DEFAULTS
CALC_FFWI_FROM_M1=.FALSE.
M1_IN_PERCENT=.FALSE.

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=FOSBERG_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &FOSBERG_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

!Get operating system (linux or windows/dos)
CALL GET_OPERATING_SYSTEM

! Get coordinate system string
IF (TRIM(A_SRS) .EQ. 'null') THEN
   FN=TRIM(INPUT_DIRECTORY) // TRIM(WS_FILENAME)
   CALL GET_COORDINATE_SYSTEM(FN)
ENDIF

! Read input rasters:
FN=TRIM(INPUT_DIRECTORY) // TRIM(WS_FILENAME)
CALL READ_BIL_RASTER(WS,FN)

IF (CALC_FFWI_FROM_M1) THEN
   FN=TRIM(INPUT_DIRECTORY) // TRIM(MEQ_FILENAME)
   CALL READ_BIL_RASTER(MEQ,FN)
   IF (.NOT. M1_IN_PERCENT) WHERE(MEQ%RZT(:,:,:) .GT. 0.) MEQ%RZT(:,:,:) = MEQ%RZT(:,:,:) * 100.0
ELSE
   FN=TRIM(INPUT_DIRECTORY) // TRIM(RH_FILENAME)
   CALL READ_BIL_RASTER(RH,FN)

   FN=TRIM(INPUT_DIRECTORY) // TRIM(TMP_FILENAME)
   CALL READ_BIL_RASTER(TMP,FN)

   CALL ALLOCATE_EMPTY_RASTER(MEQ  ,RH%NCOLS,RH%NROWS,RH%NBANDS,RH%XLLCORNER,RH%YLLCORNER,RH%XDIM,RH%YDIM,RH%NODATA_VALUE,1,'FLOAT     ')
ENDIF

! Adjust units to mph and deg F, if appropriate:
IF (WS_IN_METERS_PER_SECOND) WS %RZT(:,:,:) = WS %RZT(:,:,:) / 0.447
IF (TMP_IN_CELSIUS .AND. (.NOT. CALC_FFWI_FROM_M1) ) TMP%RZT(:,:,:) = TMP%RZT(:,:,:) * 9./5. + 32.

! Allocate Fosberg rasters  
CALL ALLOCATE_EMPTY_RASTER(FFWI ,WS%NCOLS,WS%NROWS,WS%NBANDS,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,RH%YDIM,WS%NODATA_VALUE,1,'FLOAT     ')
IF (.NOT. CALC_FFWI_FROM_M1) CALL ALLOCATE_EMPTY_RASTER(MFFWI,WS%NCOLS,WS%NROWS,WS%NBANDS,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,1,'FLOAT     ')

IF (CALC_FFWI_FROM_M1) THEN

   DO IBAND = 1, WS%NBANDS
      DO IROW = 1, WS%NROWS
      DO ICOL = 1, WS%NCOLS 
         IF (ABS( MEQ%RZT(IBAND,ICOL,IROW) -  MEQ%NODATA_VALUE) .GT. 0.1 .AND. &
             ABS(  WS%RZT(IBAND,ICOL,IROW) -   WS%NODATA_VALUE) .GT. 0.1) THEN
            MO30 = MEQ%RZT(IBAND,ICOL,IROW) / 30.0
            ETA  = 1.0 - 2.0*MO30 + 1.5*MO30*MO30 - 0.5*MO30*MO30*MO30
            FFWI%RZT(IBAND,ICOL,IROW) = ETA*SQRT(1. + WS%RZT(IBAND,ICOL,IROW)*WS%RZT(IBAND,ICOL,IROW)) / 0.3002
         ELSE
            FFWI%RZT(IBAND,ICOL,IROW)  = FFWI%NODATA_VALUE
         ENDIF
      ENDDO !ICOL
      ENDDO !IROW
   ENDDO !IBAND

ELSE

! Now calculate MEQ, FFWI, and MFFWI
   DO IBAND = 1, RH%NBANDS
      DO IROW = 1, RH%NROWS
      DO ICOL = 1, RH%NCOLS 
         IF (ABS( TMP%RZT(IBAND,ICOL,IROW) -  TMP%NODATA_VALUE) .GT. 0.1 .AND. &
             ABS(  RH%RZT(IBAND,ICOL,IROW) -   RH%NODATA_VALUE) .GT. 0.1 .AND. &
             ABS(  WS%RZT(IBAND,ICOL,IROW) -   WS%NODATA_VALUE) .GT. 0.1) THEN
            CALL GET_MEQ(TMP%RZT(IBAND,ICOL,IROW),RH%RZT(IBAND,ICOL,IROW),ADD_TO_MEQ,MEQ%RZT(IBAND,ICOL,IROW))
            MO30 = MEQ%RZT(IBAND,ICOL,IROW) / 30.0
            ETA  = 1.0 - 2.0*MO30 + 1.5*MO30*MO30 - 0.5*MO30*MO30*MO30

            IF (TMP%RZT(IBAND,ICOL,IROW) .LT. LOWPASS_TEMP) THEN
               FFWI%RZT(IBAND,ICOL,IROW)  = 0.
               MFFWI%RZT(IBAND,ICOL,IROW) = 0.
            ELSE
               FFWI%RZT(IBAND,ICOL,IROW) = ETA*SQRT(1. + WS%RZT(IBAND,ICOL,IROW)*WS%RZT(IBAND,ICOL,IROW)) / 0.3002
! Calculate modified Fosberg index, i.e. multiply FFWI by ignition probability:
               CALL GET_PIGN(TMP%RZT(IBAND,ICOL,IROW),MEQ%RZT(IBAND,ICOL,IROW),PROBIGN)
               MFFWI%RZT(IBAND,ICOL,IROW) = FFWI%RZT(IBAND,ICOL,IROW) * PROBIGN
            ENDIF
         ELSE
            FFWI%RZT(IBAND,ICOL,IROW)  = FFWI%NODATA_VALUE
            MFFWI%RZT(IBAND,ICOL,IROW) = MFFWI%NODATA_VALUE         
         ENDIF
      ENDDO !ICOL
      ENDDO !IROW
   ENDDO !IBAND

ENDIF

! Now write Meq and Fosberg indices to disk:
CALL WRITE_BIL_RASTER(FFWI , OUTPUT_DIRECTORY, FFWI_FILENAME  , CONVERT_TO_GEOTIFF, COMPRESS )
IF (.NOT. CALC_FFWI_FROM_M1) THEN
   CALL WRITE_BIL_RASTER(MEQ  , OUTPUT_DIRECTORY, MEQ_FILENAME   , CONVERT_TO_GEOTIFF, COMPRESS )
   CALL WRITE_BIL_RASTER(MFFWI, OUTPUT_DIRECTORY, MFFWI_FILENAME , CONVERT_TO_GEOTIFF, COMPRESS )
ENDIF

! *****************************************************************************
END PROGRAM FOSBERG
! *****************************************************************************
