! *****************************************************************************
PROGRAM WIND_ADJUSTMENT_FACTOR_CALC
! *****************************************************************************
! This program calculates the wind adustment factor from canopy cover 
! and canopy height rasters using the Albini and Baughman method

USE VARS
USE IO
USE SUBS
USE FIRE_DYNAMICS_IO

IMPLICIT NONE

INTEGER :: IOS, ICC, ICH
REAL :: CC1,CH1
CHARACTER(400) :: FN,NAMELIST_FN

NAMELIST /WAF_INPUTS/ INPUT_DIRECTORY, OUTPUT_DIRECTORY, CC_FILENAME, CH_FILENAME, FBFM_FILENAME, WAF_FILENAME, &
                      PATH_TO_GDAL, SCRATCH, CBD_TIMES_100, CBH_TIMES_10, CC_IN_PERCENT, CH_TIMES_10, &
                      MISCELLANEOUS_INPUTS_DIRECTORY, FUEL_MODEL_FILE

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &WAF_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Set defaults:
CALL SET_NAMELIST_DEFAULTS

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=WAF_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &WAF_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

!Get operating system (linux or windows/dos)
CALL GET_OPERATING_SYSTEM

!Get coordinate system string
FN=TRIM(INPUT_DIRECTORY) // TRIM(CC_FILENAME)
CALL GET_COORDINATE_SYSTEM(FN)

!calculate WAF
CALL READ_BSQ_RASTER(CC  ,INPUT_DIRECTORY, CC_FILENAME)
CALL READ_BSQ_RASTER(CH  ,INPUT_DIRECTORY, CH_FILENAME)
CALL READ_BSQ_RASTER(FBFM,INPUT_DIRECTORY, FBFM_FILENAME)   

CALL READ_FUEL_MODEL_TABLE

! CC and CH are normally read in as integers, but need to be converted to real
IF (ALLOCATED(CC%I2ZT)) THEN
   ALLOCATE(CC%RZT(1,1:CC%NCOLS,1:CC%NROWS))
   IF (CC_IN_PERCENT) THEN
      WHERE(CC%RZT(:,:,:) .NE. CC%NODATA_VALUE) CC%RZT(:,:,:) = 0.01*REAL(CC%I2ZT(:,:,:))
   ELSE
      CC%RZT(:,:,:) = REAL(CC%I2ZT(:,:,:))
   ENDIF
   DEALLOCATE(CC%I2ZT)
ELSE
   IF (CC_IN_PERCENT) WHERE(CC%RZT(:,:,:) .NE. CC%NODATA_VALUE) CC%RZT(:,:,:) = 0.01 * CC%RZT(:,:,:) 
ENDIF

IF (ALLOCATED(CH%I2ZT)) THEN
   ALLOCATE(CH%RZT(1,1:CH%NCOLS,1:CH%NROWS))
   IF (CH_TIMES_10) THEN
      WHERE (CH%RZT(:,:,:) .NE. CH%NODATA_VALUE) CH%RZT(:,:,:) = 0.1*REAL(CH%I2ZT(:,:,:))
   ELSE
      CH%RZT(:,:,:) = REAL(CH%I2ZT(:,:,:))
   ENDIF
   DEALLOCATE(CH%I2ZT)
ELSE
   IF (CH_TIMES_10) WHERE(CH%RZT(:,:,:) .NE. CH%NODATA_VALUE) CH%RZT(:,:,:) = 0.1 * CH%RZT(:,:,:) 
ENDIF

! Allocate wind adjustment factor raster 
CALL ALLOCATE_EMPTY_RASTER(WAF,CC%NCOLS,CC%NROWS,1,CC%XLLCORNER,CC%YLLCORNER,CC%XDIM,CC%YDIM,CC%NODATA_VALUE,1,'FLOAT     ')

! Set up shelterd waf table:
DO ICC = 0, 20
   CC1=REAL(ICC)*0.05
   DO ICH = 0, 50
      CH1=0.5+REAL(ICH)
      SHELTERED_WAF_TABLE(ICC,ICH) = WIND_ADJUSTMENT_FACTOR(CC1, CH1, 0.)
   ENDDO
ENDDO

CALL CALC_WIND_ADJUSTMENT_FACTOR(CC, CH, FBFM, WAF)
! Now write wind adjustment factor to disk:
CALL WRITE_BIL_RASTER(WAF,OUTPUT_DIRECTORY,WAF_FILENAME,.TRUE.,.TRUE.)

CONTAINS

! *****************************************************************************
SUBROUTINE CALC_WIND_ADJUSTMENT_FACTOR(CC, CH, FBFM, WAF)
! *****************************************************************************
! This does the whole array

USE VARS, ONLY : RASTER_TYPE, SHELTERED_WAF_TABLE
USE SURFACE_SPREAD_VARS, ONLY : FUEL_MODEL_TABLE_2D

IMPLICIT NONE

TYPE(RASTER_TYPE), INTENT(IN) :: CC, CH, FBFM
TYPE(RASTER_TYPE), INTENT(INOUT) :: WAF

INTEGER :: IROW, ICOL, ICC, ICH

DO IROW = 1, WAF%NROWS
DO ICOL = 1, WAF%NCOLS
   IF (CC%RZT(1,ICOL,IROW) .LT. 0.) THEN
      WAF%RZT(1,ICOL,IROW) = 0.
   ELSE
      IF (CC%RZT(1,ICOL,IROW) .GT. 1E-4 .AND. CH%RZT(1,ICOL,IROW) .GT. 1E-4) THEN !Canopy is present
         ICC=NINT(CC%RZT(1,ICOL,IROW)*20.)
         ICH=NINT(CH%RZT(1,ICOL,IROW)-0.5)
         WAF%RZT(1,ICOL,IROW)=SHELTERED_WAF_TABLE(ICC,ICH)
      ELSE !Canopy is not present
         WAF%RZT(1,ICOL,IROW) = FUEL_MODEL_TABLE_2D(MAX(FBFM%I2ZT(1,ICOL,IROW),0),30)%UNSHELTERED_WAF
      ENDIF
   ENDIF
ENDDO
ENDDO

! *****************************************************************************
END SUBROUTINE CALC_WIND_ADJUSTMENT_FACTOR
! *****************************************************************************

! *****************************************************************************
REAL FUNCTION WIND_ADJUSTMENT_FACTOR(CC, CH, FUEL_BED_HEIGHT)
! *****************************************************************************

IMPLICIT NONE

REAL, INTENT(IN) :: CC, CH, FUEL_BED_HEIGHT

REAL :: HFT, NUMER, DENOM, UHOU20PH, F, UCOUH, HFOH, TERM1, TERM2

IF (CC .LT. 0.) THEN
   WIND_ADJUSTMENT_FACTOR = 0.
ELSE
   IF (CC .GT. 1E-4 .AND. CH .GT. 1E-4) THEN !Canopy is present
      HFT = CH / 0.3048 
      NUMER = 20. + 0.36*HFT
      DENOM = 0.13 * HFT
      UHOU20PH = 1. / LOG(NUMER/DENOM)
      F = CC / 3. !Same as BEHAVE
      UCOUH = 0.555 / SQRT(F * HFT)
      WIND_ADJUSTMENT_FACTOR = UHOU20PH * UCOUH
   ELSE !Canopy is not present
      IF (FUEL_BED_HEIGHT .GT. 1E-4) THEN
         HFOH = 1.0 ! Same as BEHAVE and FARSITE
         HFT = FUEL_BED_HEIGHT
         NUMER = 20. + 0.36*HFT
         DENOM = 0.13 * HFT
         TERM1 = (1. + 0.36/HFOH) / LOG(NUMER/DENOM)
         NUMER = HFOH + 0.36
         TERM2 = LOG(NUMER/0.13) - 1.
         WIND_ADJUSTMENT_FACTOR = TERM1 * TERM2       
      ELSE
         WIND_ADJUSTMENT_FACTOR = 0.
      ENDIF
   ENDIF
ENDIF

! *****************************************************************************
END FUNCTION WIND_ADJUSTMENT_FACTOR
! *****************************************************************************

! *****************************************************************************
END PROGRAM WIND_ADJUSTMENT_FACTOR_CALC
! *****************************************************************************
