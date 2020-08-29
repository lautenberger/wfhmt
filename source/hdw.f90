! *****************************************************************************
PROGRAM HOT_DRY_WINDY
! *****************************************************************************
! This program calculates vapor pressure deficit and hot dry windy index

USE VARS
USE IO
USE SUBS

IMPLICIT NONE

INTEGER :: IBAND, IOS, IROW, ICOL
CHARACTER(400) :: FN, NAMELIST_FN
REAL, PARAMETER :: E0  = 611E0 !Pa
REAL, PARAMETER :: MV  = 0.018 !kg/mol
REAL, PARAMETER :: MA  = 0.029 !kg/mol
REAL, PARAMETER :: DHV = 2.5E6 ! J/kg
REAL, PARAMETER :: R0  = 8.314 ! J/mol-K
REAL, PARAMETER :: T0  = 273.15
REAL :: PSAT, DENOM1, DENOM2, MBAR, XV, PVAP

NAMELIST /HDW_INPUTS/ INPUT_DIRECTORY, OUTPUT_DIRECTORY, WS_FILENAME, P0_FILENAME, SH_FILENAME, TMP_FILENAME, &
                      WS_IN_METERS_PER_SECOND, PATH_TO_GDAL, SCRATCH, TMP_UNITS, VPD_FILENAME, HDW_FILENAME

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &HDW_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Set defaults:
CALL SET_NAMELIST_DEFAULTS

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=HDW_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &HDW_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

!Get operating system (linux or windows/dos)
CALL GET_OPERATING_SYSTEM

! Get coordinate system string
FN=TRIM(INPUT_DIRECTORY) // TRIM(WS_FILENAME)
CALL GET_COORDINATE_SYSTEM(FN)

! Read input rasters:
CALL READ_BSQ_RASTER(WS  ,INPUT_DIRECTORY, WS_FILENAME)
CALL READ_BSQ_RASTER(SH  ,INPUT_DIRECTORY, SH_FILENAME)
CALL READ_BSQ_RASTER(TMP ,INPUT_DIRECTORY, TMP_FILENAME)   
CALL READ_BSQ_RASTER(P0  ,INPUT_DIRECTORY, P0_FILENAME)   

! Adjust units to m/s if appropriate:
IF (.NOT. WS_IN_METERS_PER_SECOND) WS %RZT(:,:,:) = WS %RZT(:,:,:) * 0.447

! Convert TMP to K if necessary
SELECT CASE (TMP_UNITS)
   CASE ('C')
      TMP%RZT(:,:,:) = TMP%RZT(:,:,:) + 273.15
   CASE ('F')
      TMP%RZT(:,:,:) = (TMP%RZT(:,:,:) - 32.0) * (5./9.) + 273.15
   CASE ('K')
      CONTINUE
   CASE DEFAULT
      WRITE (*,*) 'Error:  TMP_UNITS must be one of C, F, or K'
END SELECT
   
! Allocate hot dry windy and vapor pressure deficit:
CALL ALLOCATE_EMPTY_RASTER(HDW  ,WS%NCOLS,WS%NROWS,WS%NBANDS,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,1,'FLOAT     ')
CALL ALLOCATE_EMPTY_RASTER(VPD  ,WS%NCOLS,WS%NROWS,WS%NBANDS,WS%XLLCORNER,WS%YLLCORNER,WS%XDIM,WS%YDIM,WS%NODATA_VALUE,1,'FLOAT     ')

! Now calculate VPD and HDW
!$omp PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(IBAND,IROW,ICOL,PSAT,DENOM1,DENOM2,MBAR,XV,PVAP) & 
!$omp SHARED(SH,TMP,P0,WS,VPD,HDW)
DO IBAND = 1, SH%NBANDS
   DO IROW = 1, SH%NROWS
   DO ICOL = 1, SH%NCOLS 
      IF (ABS( TMP%RZT(IBAND,ICOL,IROW) -  TMP%NODATA_VALUE) .GT. 0.1 .AND. &
         ABS(  SH%RZT(IBAND,ICOL,IROW) -   SH%NODATA_VALUE) .GT. 0.1 .AND. &
         ABS(  P0%RZT(IBAND,ICOL,IROW) -   P0%NODATA_VALUE) .GT. 0.1) THEN

! Saturation vapor pressure:
         PSAT = E0 * EXP( (MV * DHV / R0) * (1/T0 - 1/TMP%RZT(IBAND,ICOL,IROW) ) )

! Now do vapor pressure:
         DENOM1 = SH%RZT(IBAND,ICOL,IROW) / MV
         DENOM2 = (1. - SH%RZT(IBAND,ICOL,IROW) ) / MA
         MBAR   = 1. / (DENOM1 + DENOM2)
         XV     = SH%RZT(IBAND,ICOL,IROW) * MBAR / MV 
         PVAP   = XV * P0%RZT(IBAND,ICOL,IROW)
         VPD%RZT(IBAND,ICOL,IROW) = 0.01 * (PSAT - PVAP) !hPa
         HDW%RZT(IBAND,ICOL,IROW) = WS%RZT(IBAND,ICOL,IROW) * VPD%RZT(IBAND,ICOL,IROW)
      ELSE
         VPD%RZT(IBAND,ICOL,IROW) = VPD%NODATA_VALUE
         HDW%RZT(IBAND,ICOL,IROW) = HDW%NODATA_VALUE
      ENDIF
   ENDDO !ICOL
   ENDDO !IROW
ENDDO !IBAND
!$omp END PARALLEL DO 

! Now write VPD and HDW to disk:
CALL WRITE_BIL_RASTER(VPD , OUTPUT_DIRECTORY, VPD_FILENAME , .TRUE., .TRUE.)
CALL WRITE_BIL_RASTER(HDW , OUTPUT_DIRECTORY, HDW_FILENAME , .TRUE., .TRUE.)

! *****************************************************************************
END PROGRAM HOT_DRY_WINDY
! *****************************************************************************
