! *****************************************************************************
PROGRAM CSV_PERCENTILES
! *****************************************************************************

IMPLICIT NONE

INTEGER :: I, J, IOS, LUINPUT=10, LUOUTPUT=20
INTEGER :: N, NUM_DATA, NUM_CONVERSION
REAL, ALLOCATABLE, DIMENSION(:) :: RAW, PERCENTILED
REAL, ALLOCATABLE, DIMENSION(:,:) :: CONVERSION
CHARACTER(400) :: FN, NAMELIST_FN, INPUT_DIRECTORY, OUTPUT_DIRECTORY, RAW_DATA_FILENAME, PERCENTILED_DATA_FILENAME, &
                  CONVERSION_FILENAME 

NAMELIST /CSV_PERCENTILES_INPUTS/ INPUT_DIRECTORY, OUTPUT_DIRECTORY, RAW_DATA_FILENAME, PERCENTILED_DATA_FILENAME, &
                                  CONVERSION_FILENAME

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group:
WRITE(*,*) 'Reading &CSV_PERCENTILES_INPUTS namelist group from ', TRIM(NAMELIST_FN)

! Open input file and read in namelist group
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=CSV_PERCENTILES_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &CSV_PERCENTILES_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

! Start by reading raw data
FN=TRIM(INPUT_DIRECTORY) // TRIM(RAW_DATA_FILENAME)
OPEN(LUINPUT,FILE=TRIM(FN), FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS) 
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening raw data filename', TRIM(FN)
   STOP
ENDIF
IOS=0
N=0
DO WHILE (IOS .EQ. 0)
   READ (LUINPUT,*,IOSTAT=IOS)
   N = N + 1
ENDDO
NUM_DATA = N - 1
ALLOCATE(RAW        (1:NUM_DATA)) 
ALLOCATE(PERCENTILED(1:NUM_DATA))
REWIND(LUINPUT)
DO I = 1, NUM_DATA
   READ(LUINPUT,*) RAW(I)
ENDDO
CLOSE(LUINPUT)

! Now read conversion data
FN=TRIM(INPUT_DIRECTORY) // TRIM(CONVERSION_FILENAME)
OPEN(LUINPUT,FILE=TRIM(FN), FORM='FORMATTED', ACCESS='SEQUENTIAL', IOSTAT=IOS) 
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening conversion filename', TRIM(FN)
   STOP
ENDIF
IOS=0
N=0
DO WHILE (IOS .EQ. 0)
   READ (LUINPUT,*,IOSTAT=IOS)
   N = N + 1
ENDDO
NUM_CONVERSION = N - 1
ALLOCATE(CONVERSION(1:2,1:NUM_CONVERSION)) 
CONVERSION(:,:) = 0.
CLOSE(LUINPUT)
OPEN(LUINPUT,FILE=TRIM(FN), FORM='FORMATTED', ACCESS='SEQUENTIAL', IOSTAT=IOS) 
DO I = 1, NUM_CONVERSION
   READ(LUINPUT,*,IOSTAT=IOS) CONVERSION(1,I), CONVERSION(2,I)
ENDDO

CLOSE(LUINPUT)

! Now convert to percentiled data
DO I = 1, NUM_DATA
   CALL LOCATE(CONVERSION(1,:), NUM_CONVERSION, RAW(I), J)
   IF (J .EQ. NUM_DATA) THEN
      PERCENTILED(I) = CONVERSION(2,J)
   ELSE
      PERCENTILED(I) = LINTERP(RAW(I), CONVERSION(1,J), CONVERSION(1,J+1), CONVERSION(2,J), CONVERSION(2,J+1) )
   ENDIF
ENDDO

! Now write percentiled data
FN=TRIM(OUTPUT_DIRECTORY) // TRIM(PERCENTILED_DATA_FILENAME)
OPEN(LUOUTPUT,FILE=TRIM(FN), FORM='FORMATTED',STATUS='REPLACE',IOSTAT=IOS) 
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening output filename', TRIM(FN)
   STOP
ENDIF
 
DO I = 1, NUM_DATA
   WRITE(LUOUTPUT,200) PERCENTILED(I)
ENDDO
CLOSE(LUOUTPUT)

200 FORMAT (F6.2)

CONTAINS

!******************************************************************************
REAL FUNCTION LINTERP(X,XL,XH,FXL,FXH)
!******************************************************************************

REAL, INTENT(IN) :: X, XL,XH,FXL,FXH
LINTERP = FXL + (FXH-FXL)*(X-XL)/(XH-XL)

! *****************************************************************************
END FUNCTION LINTERP
!******************************************************************************

! *****************************************************************************
SUBROUTINE locate(xx,n,x,j)
! *****************************************************************************
! Given an array xx(1:n), and given a value x, returns a value j such that x is between
! xx(j) and xx(j+1). xx(1:n) must be monotonic, either increasing or decreasing. j=0
! or j=n is returned to indicate that x is out of range.

INTEGER j,n
REAL :: x,xx(n)
INTEGER jl,jm,ju
jl=0 !Initialize lower
ju=n+1 !and upper limits.
 10 if(ju-jl.gt.1)then !If we are not yet done,
       jm=(ju+jl)/2 !compute a midpoint,
       if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
          jl=jm !and replace either the lower limit
       else
          ju=jm !or the upper limit, as appropriate.
       endif
       goto 10 !Repeat until
    endif !the test condition 10 is satisfied.
      
if(x.eq.xx(1))then !Then set the output
   j=1
else if(x.eq.xx(n))then
   j=n-1
else
   j=jl
endif

return !and return.

! *****************************************************************************
END SUBROUTINE LOCATE
! *****************************************************************************

! *****************************************************************************
END PROGRAM CSV_PERCENTILES
! *****************************************************************************
