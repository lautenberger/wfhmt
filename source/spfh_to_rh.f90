! *****************************************************************************
PROGRAM SPECIFIC_TO_RELATIVE_HUMIDITY
! *****************************************************************************
! This program relative humidity from specific humidity

USE VARS
USE IO
USE SUBS

IMPLICIT NONE

INTEGER :: IBAND, IOS, IROW, ICOL
REAL :: UX, UY, WDNOW, TMPK, NUMER, DENOM, EXPTERM
CHARACTER(400) :: FN, NAMELIST_FN

NAMELIST /SPECIFIC_TO_RELATIVE_HUMIDITY_INPUTS/ INPUT_DIRECTORY, OUTPUT_DIRECTORY, SPFH_FILENAME, TMPF_FILENAME, PRES_FILENAME, RH_FILENAME, &
                                                PATH_TO_GDAL, SCRATCH, CONVERT_TO_GEOTIFF, COMPRESS

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &SPECIFIC_TO_RELATIVE_HUMIDITY_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Set defaults:
CALL SET_NAMELIST_DEFAULTS

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=SPECIFIC_TO_RELATIVE_HUMIDITY_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &SPECIFIC_TO_RELATIVE_HUMIDITY_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

!Get operating system (linux or windows/dos)
CALL GET_OPERATING_SYSTEM

!Get coordinate system string
FN=TRIM(INPUT_DIRECTORY) // TRIM(SPFH_FILENAME)
CALL GET_COORDINATE_SYSTEM(FN)

FN=TRIM(INPUT_DIRECTORY) // TRIM(SPFH_FILENAME)
CALL READ_BIL_RASTER(SPFH,FN)

FN=TRIM(INPUT_DIRECTORY) // TRIM(TMPF_FILENAME)
CALL READ_BIL_RASTER(TMPF,FN)

FN=TRIM(INPUT_DIRECTORY) // TRIM(PRES_FILENAME)
CALL READ_BIL_RASTER(PRES,FN)

! Allocate rasters
CALL ALLOCATE_EMPTY_RASTER(RH   ,SPFH%NCOLS,SPFH%NROWS,SPFH%NBANDS,SPFH%XLLCORNER,SPFH%YLLCORNER,SPFH%XDIM,SPFH%YDIM,SPFH%NODATA_VALUE,1,'FLOAT     ')

! Now calculate rh
DO IBAND = 1, SPFH%NBANDS
   DO IROW = 1, SPFH%NROWS
   DO ICOL = 1, SPFH%NCOLS 
      IF (ABS(SPFH%RZT(IBAND,ICOL,IROW) - SPFH%NODATA_VALUE) .GT. 0.1 ) THEN
          TMPK = (TMPF%RZT(IBAND,ICOL,IROW) - 32.0) * (5./9.) + 273.15
          NUMER = 17.67 * (TMPK - 273.15)
          DENOM = TMPK - 29.65
          EXPTERM = NUMER / DENOM
          DENOM = EXP(EXPTERM)
          NUMER = 0.263 * PRES%RZT(IBAND,ICOL,IROW) * SPFH%RZT(IBAND,ICOL,IROW)
          RH%RZT(IBAND,ICOL,IROW) = NUMER / DENOM
          CONTINUE
      ELSE
          RH%RZT(IBAND,ICOL,IROW) = SPFH%NODATA_VALUE
      ENDIF
   ENDDO !ICOL
   ENDDO !IROW
ENDDO !IBAND

! Now write raster to disk:
CALL WRITE_BIL_RASTER(RH, OUTPUT_DIRECTORY, RH_FILENAME, CONVERT_TO_GEOTIFF, COMPRESS)

! *****************************************************************************
END PROGRAM SPECIFIC_TO_RELATIVE_HUMIDITY
! *****************************************************************************
