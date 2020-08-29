! *****************************************************************************
PROGRAM WRITE_RASTER_HOURS
! *****************************************************************************
! This program writes files containing the hour number to be sorted later 

USE VARS, ONLY : RASTER_TYPE, INPUT_DIRECTORY, OUTPUT_DIRECTORY, PATH_TO_GDAL, SCRATCH
USE SUBS
USE IO

IMPLICIT NONE

INTEGER :: IOS, IHR, IDAY, ICOUNT, NDAYS
CHARACTER(400) :: FN, NAMELIST_FN, FNOUT, DUMMY_FILENAME, OUTPUT_STUB
CHARACTER(4) :: FOUR
TYPE(RASTER_TYPE) :: HOURS

NAMELIST /RASTER_HOURS_INPUTS/ INPUT_DIRECTORY, OUTPUT_DIRECTORY, DUMMY_FILENAME, OUTPUT_STUB, NDAYS, &
                               PATH_TO_GDAL, SCRATCH

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &RASTER_HOURS_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Set defaults:
CALL SET_NAMELIST_DEFAULTS
DUMMY_FILENAME = 'null'
OUTPUT_STUB    = 'hours'
NDAYS          = -1

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=RASTER_HOURS_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &RASTER_HOURS_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

!Get operating system (linux or windows/dos)
CALL GET_OPERATING_SYSTEM

!Get coordinate system string
FN=TRIM(INPUT_DIRECTORY) // TRIM(DUMMY_FILENAME)
CALL GET_COORDINATE_SYSTEM(FN)

! Read dummy raster into hours
CALL READ_BSQ_RASTER(HOURS, INPUT_DIRECTORY, DUMMY_FILENAME)

! Count and write one file for every day:
ICOUNT = 0 
DO IDAY = 1, NDAYS
   DO IHR = 1, 24
      ICOUNT = ICOUNT + 1
      HOURS%RZT(IHR,:,:) = REAL(ICOUNT)
   ENDDO
   WRITE(FOUR, '(I4.4)') IDAY
   FNOUT = FOUR // "_" // TRIM(OUTPUT_STUB)
   CALL WRITE_BIL_RASTER(HOURS, OUTPUT_DIRECTORY, FNOUT, .TRUE., .TRUE.)
ENDDO

STOP

! *****************************************************************************
END PROGRAM WRITE_RASTER_HOURS
! *****************************************************************************
