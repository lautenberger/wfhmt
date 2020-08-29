! *****************************************************************************
PROGRAM ISH_TO_MET
! *****************************************************************************
! Converts integrated surface hourly (ISH / DS3505) data to ASCII format
! required by MET

IMPLICIT NONE

REAL, PARAMETER :: PI = 3.14159
INTEGER :: IYEAR_ISH, IMONTH_ISH, IDAY_ISH, IHOUR_ISH

REAL :: PWSTINF, PWSTD, A, M, TN, UWIND, VWIND
INTEGER :: IYEAR, IMONTH, IDAY
REAL :: TMP, DEWP, PSFC, WD, WS, RH

INTEGER, PARAMETER :: NBLOCKMAX=100
CHARACTER(400) :: FN, NAMELIST_FN, ISH_DIRECTORY, ISH_STATION_LIST_FILENAME, OUTPUT_DIRECTORY

CHARACTER(400), DIMENSION(1:1000) :: ISH_FN, ISH_DESCRIPTION
REAL, DIMENSION(1:1000) :: ISH_LAT, ISH_LON, ISH_ELEV

CHARACTER(4) :: CYEAR
CHARACTER(2) :: CMONTH, CDAY, CHOUR

INTEGER :: NSTATIONS, IOS, ISTATION
CHARACTER(15) :: CTIME

INTEGER, PARAMETER :: LUINPUT = 100, LUOUTPUT = 110

NAMELIST /ISH_TO_MET_INPUTS/ ISH_DIRECTORY, ISH_STATION_LIST_FILENAME, OUTPUT_DIRECTORY, IYEAR, IMONTH, IDAY

A = 6.1162
M = 7.5892
TN = 240.71

!Begin by getting input file name:
CALL GETARG(1,NAMELIST_FN)
IF (NAMELIST_FN(1:1)==' ') THEN
   WRITE(*,*) "Error, no input file specified."
   WRITE(*,*) "Hit Enter to continue."
   READ(5,*)
   STOP
ENDIF

!Now read NAMELIST group

WRITE(*,*) 'Reading &ISH_TO_MET_INPUTS namelist group'

! Open the input file and read in namelist groups
OPEN(LUINPUT,FILE=TRIM(NAMELIST_FN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening input file ', TRIM(NAMELIST_FN)
   STOP
ENDIF

READ(LUINPUT,NML=ISH_TO_MET_INPUTS,END=100,IOSTAT=IOS)
 100  IF (IOS > 0) THEN
         WRITE(*,*) 'Error: Problem with namelist group &ISH_TO_MET_INPUTS.'
         STOP
      ENDIF
CLOSE(LUINPUT)

! Open output file 
WRITE(CYEAR , '(I4.4)') IYEAR
WRITE(CMONTH, '(I2.2)') IMONTH
WRITE(CDAY  , '(I2.2)') IDAY

FN=TRIM(OUTPUT_DIRECTORY) // CYEAR // "-" // CMONTH // "-" // CDAY // "_point_obs.txt"
OPEN (LUOUTPUT,FILE=TRIM(FN), FORM='FORMATTED', STATUS='REPLACE',IOSTAT=IOS)

! Get station info:
FN=TRIM(ISH_DIRECTORY) // TRIM(ISH_STATION_LIST_FILENAME)
OPEN (LUINPUT,FILE=TRIM(FN), FORM='FORMATTED', STATUS='OLD',IOSTAT=IOS)

IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening station list file ', TRIM(FN)
   STOP
ENDIF

! Read in ISH station data:
IOS = 0
ISTATION = 0
DO WHILE (IOS .EQ. 0)
   ISTATION = ISTATION + 1
   READ(LUINPUT,*,IOSTAT=IOS) ISH_FN(ISTATION), ISH_DESCRIPTION(ISTATION), ISH_LAT(ISTATION), ISH_LON(ISTATION), ISH_ELEV(ISTATION)
ENDDO
NSTATIONS = ISTATION - 1

! Loop over stations and write MET data
DO ISTATION = 1, NSTATIONS
   FN=TRIM(ISH_DIRECTORY) // TRIM(ISH_FN(ISTATION)) // ".txt"
   OPEN (LUINPUT,FILE=TRIM(FN), FORM='FORMATTED', STATUS='OLD',IOSTAT=IOS)

   IF (IOS .GT. 0) THEN
      WRITE(*,*) 'Problem opening ISH file ', TRIM(FN)
      CONTINUE
   ENDIF

   DO WHILE (IOS .EQ. 0) 
      READ(LUINPUT,*,IOSTAT=IOS) IYEAR_ISH, IMONTH_ISH, IDAY_ISH, IHOUR_ISH, TMP, DEWP, PSFC, WD, WS

      IF (IOS        .NE. 0     ) CYCLE      
      IF (IYEAR_ISH  .NE. IYEAR ) CYCLE
      IF (IMONTH_ISH .NE. IMONTH) CYCLE
      IF (IDAY_ISH   .NE. IDAY  ) CYCLE

!    Message_Type
!    Station_ID
!    Valid_Time in YYYYMMDD_HHMMSS format
!    Lat in degrees North
!    Lon in degrees East
!    Elevation in meters above sea level
!    Grib_Code corresponding to this observation type
!    Level as the pressure level in hPa or accumulation interval in hours
!    Height in meters above sea level or above ground level
!    Observation_Value in the units prescribed for the grib code
      
      WRITE(CHOUR , '(I2.2)') IHOUR_ISH

      CTIME = CYEAR // CMONTH // CDAY // "_" // CHOUR // "0000"

      TMP  = (TMP  / 10.) + 273.15 ! C*10 to K
      DEWP = (DEWP / 10.) + 273.15 ! C*10 to K
      WD   = WD                    ! No conversion necessary
      WS   = (WS / 10.)            ! m/s*10 to m/s (this should already be at 10 m height)
         
      PWSTINF = A * 10.0 **((M * (TMP  - 273.15)) / (TMP  - 273.15 + TN))
      PWSTD   = A * 10.0 **((M * (DEWP - 273.15)) / (DEWP - 273.15 + TN))
          
      RH = 100.*PWSTD/PWSTINF
      RH = MAX(RH,0.)
      RH = MIN(RH,100.)

      WRITE(LUOUTPUT,200) 'ADPSFC ', TRIM(ISH_FN(ISTATION)), CTIME, ISH_LAT(ISTATION), ISH_LON(ISTATION), ISH_ELEV(ISTATION), 11, -9999, -9999, TMP
      WRITE(LUOUTPUT,200) 'ADPSFC ', TRIM(ISH_FN(ISTATION)), CTIME, ISH_LAT(ISTATION), ISH_LON(ISTATION), ISH_ELEV(ISTATION), 31, -9999, -9999, WD
      WRITE(LUOUTPUT,200) 'ADPSFC ', TRIM(ISH_FN(ISTATION)), CTIME, ISH_LAT(ISTATION), ISH_LON(ISTATION), ISH_ELEV(ISTATION), 32, -9999, -9999, WS
      WRITE(LUOUTPUT,200) 'ADPSFC ', TRIM(ISH_FN(ISTATION)), CTIME, ISH_LAT(ISTATION), ISH_LON(ISTATION), ISH_ELEV(ISTATION), 52, -9999, -9999, RH

      WD = WD + 180. 
      IF (WD .GT. 360) WD = WD - 360.
      WD = WD*PI/180.
      UWIND = SIN(WD)*WS
      VWIND = COS(WD)*WS
      
      WRITE(LUOUTPUT,200) 'ADPSFC ', TRIM(ISH_FN(ISTATION)), CTIME, ISH_LAT(ISTATION), ISH_LON(ISTATION), ISH_ELEV(ISTATION), 33, -9999, -9999, UWIND
      WRITE(LUOUTPUT,200) 'ADPSFC ', TRIM(ISH_FN(ISTATION)), CTIME, ISH_LAT(ISTATION), ISH_LON(ISTATION), ISH_ELEV(ISTATION), 34, -9999, -9999, VWIND
            
   ENDDO

   CLOSE(LUINPUT)

ENDDO

CLOSE(LUOUTPUT)
   
200 FORMAT(A, " ", A, " ", A, " ", F10.5, " ", F10.5, " ", F10.5, " ", I3, " ", I5, " ", I5, " " F8.2)

! *****************************************************************************
END PROGRAM ISH_TO_MET
! *****************************************************************************
