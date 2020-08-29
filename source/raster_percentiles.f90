! *****************************************************************************
PROGRAM CALC_FILENAME_PERCENTILES
! *****************************************************************************
! This calculates percentile values for single band rasters

USE VARS
USE SORT
USE SUBS
USE IO

IMPLICIT NONE

INTEGER :: I, IOS, IROW, ICOL, ICOUNT, NVALS
REAL, ALLOCATABLE, DIMENSION(:) :: Z
CHARACTER(400) :: FN, NAMELIST_FN
INTEGER, DIMENSION(1:NMAX) :: ILOC
REAL, DIMENSION(1:NMAX) :: PERCENTILES_VALS
TYPE (RASTER_TYPE) :: RASTER_ONE, RASTER_TWO

NAMELIST /RASTER_PERCENTILES_INPUTS/ INPUT_DIRECTORY, OUTPUT_DIRECTORY, PERCENTILE_INPUT_FILENAME, PERCENTILE_OUTPUT_FILENAME, &
                                     A_SRS, COMPRESS, NPERCENTILES, PERCENTILES, PATH_TO_GDAL, SCRATCH

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &RASTER_PERCENTILES_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Set defaults:
CALL SET_NAMELIST_DEFAULTS

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=RASTER_PERCENTILES_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &RASTER_PERCENTILES_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

!Get operating system (linux or windows/dos)
CALL GET_OPERATING_SYSTEM

!Get coordinate system string
FN=TRIM(INPUT_DIRECTORY) // TRIM(PERCENTILE_INPUT_FILENAME)
CALL GET_COORDINATE_SYSTEM(FN)

FN=TRIM(INPUT_DIRECTORY) // TRIM(PERCENTILE_INPUT_FILENAME)
CALL READ_BIL_RASTER(RASTER_ONE,FN)

! Allocate empty raster
WRITE(*,*) 'ALLOCATEing empty raster'
CALL ALLOCATE_EMPTY_RASTER(RASTER_TWO ,RASTER_ONE%NCOLS,RASTER_ONE%NROWS,RASTER_ONE%NBANDS,RASTER_ONE%XLLCORNER,RASTER_ONE%YLLCORNER,RASTER_ONE%XDIM,RASTER_ONE%YDIM,RASTER_ONE%NODATA_VALUE,1,'FLOAT     ')

! Count up non NODATA values
WRITE(*,*) 'Counting up NODATA values'
ICOUNT = 0
DO IROW = 1, RASTER_ONE%NROWS
DO ICOL = 1, RASTER_ONE%NCOLS
   IF (RASTER_ONE%RZT(1,ICOL,IROW) .EQ. RASTER_ONE%RZT(1,ICOL,IROW)) THEN 
      IF (ABS(RASTER_ONE%RZT(1,ICOL,IROW) - RASTER_ONE%NODATA_VALUE) .GT. 0.001) ICOUNT = ICOUNT + 1
   ENDIF
ENDDO
ENDDO

! Allocate arrays for sorting:
NVALS = ICOUNT
ALLOCATE(Z(1:NVALS))

! Now store values in 1D array Z:
WRITE(*,*) 'Looping over raster'
ICOUNT = 0
DO IROW = 1, RASTER_ONE%NROWS
DO ICOL = 1, RASTER_ONE%NCOLS
   IF (RASTER_ONE%RZT(1,ICOL,IROW) .EQ. RASTER_ONE%RZT(1,ICOL,IROW)) THEN 
      IF (ABS(RASTER_ONE%RZT(1,ICOL,IROW) - RASTER_ONE%NODATA_VALUE) .GT. 0.001) THEN
         ICOUNT = ICOUNT + 1
         Z(ICOUNT) = RASTER_ONE%RZT(1,ICOL,IROW)
      ENDIF
   ENDIF
ENDDO
ENDDO

!! Sort Z from low to high
!WRITE(*,*) 'DSORTing NVALS: ', NVALS 
!CALL DSORT (Z(:), Z(:), NVALS, 1)

WRITE(*,*) 'quicksorting NVALS: ', NVALS 
CALL QUICKSORT (Z(:), 1, NVALS)

! Write sorted raster to .csv file:
WRITE(*,*) 'Writing .csv file'
FN=TRIM(OUTPUT_DIRECTORY) // TRIM(PERCENTILE_INPUT_FILENAME)
OPEN(LUOUTPUT,FILE=TRIM(FN) // '.csv',FORM='FORMATTED',STATUS='REPLACE',IOSTAT=IOS) 
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening', TRIM(FN)//'.csv'
   STOP
ENDIF
DO I = 1, NVALS, 1
   WRITE(LUOUTPUT,200) Z(I)
ENDDO
CLOSE(LUOUTPUT)
200 FORMAT(F12.4)

! Now determine percentiles
WRITE(*,*) 'Determining percentiles'
DO I = 1, NPERCENTILES
   ILOC(I) = NINT(0.01*PERCENTILES(I) * REAL(NVALS)) 
   PERCENTILES_VALS(I) = Z(ILOC(I))
ENDDO

! Loop over percentiles and dump rasters
WRITE(*,*) 'Looping over percentiles and dumping rasters'
DO I = 1, NPERCENTILES
   RASTER_TWO%RZT(:,:,:) = 0D0
   WHERE(RASTER_ONE%RZT(:,:,:) .GE. PERCENTILES_VALS(I)) RASTER_TWO%RZT(:,:,:) = 1.
   CALL WRITE_BIL_RASTER(RASTER_TWO,OUTPUT_DIRECTORY,PERCENTILE_OUTPUT_FILENAME(I),.TRUE.,COMPRESS)
ENDDO

STOP

CONTAINS

recursive subroutine quicksort(a, first, last)
  implicit none
  real  a(*), x, t
  integer first, last
  integer i, j

  x = a( (first+last) / 2 )
  i = first
  j = last
  do
     do while (a(i) < x)
        i=i+1
     end do
     do while (x < a(j))
        j=j-1
     end do
     if (i >= j) exit
     t = a(i);  a(i) = a(j);  a(j) = t
     i=i+1
     j=j-1
  end do
  if (first < i-1) call quicksort(a, first, i-1)
  if (j+1 < last)  call quicksort(a, j+1, last)
end subroutine quicksort

! *****************************************************************************
END PROGRAM CALC_FILENAME_PERCENTILES
! *****************************************************************************
