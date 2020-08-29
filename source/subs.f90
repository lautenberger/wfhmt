! *****************************************************************************
MODULE SUBS
! *****************************************************************************

USE VARS

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE GET_OPERATING_SYSTEM
! *****************************************************************************

CHARACTER(2000) :: PATH

CALL GET_ENVIRONMENT_VARIABLE('PATH',PATH)

IF (PATH(1:1) .EQ. '/') THEN 
   OPERATING_SYSTEM = 'linux  '
   PATH_SEPARATOR   = '/'
   DELETECOMMAND    = 'rm -f '
ELSE
   OPERATING_SYSTEM = 'windows'
   PATH_SEPARATOR   = '\'
   DELETECOMMAND    = 'del   '
ENDIF

! *****************************************************************************
END SUBROUTINE GET_OPERATING_SYSTEM
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GENERAL_ERROR_CHECKING
! *****************************************************************************

IF (TRIM(FUELS_AND_TOPOGRAPHY_DIRECTORY       ) .EQ. 'null') THEN
   WRITE(*,*) 'Specify FUELS_AND_TOPOGRAPHY_DIRECTORY in &FIRE_DYNAMICS_INPUTS and rerun.' 
   STOP
ENDIF

IF (TRIM(WEATHER_DIRECTORY             ) .EQ. 'null') THEN
   WRITE(*,*) 'Specify WEATHER_DIRECTORY in &FIRE_DYNAMICS_INPUTS and rerun.' 
   STOP
ENDIF

IF (TRIM(FIRE_DYNAMICS_DIRECTORY       ) .EQ. 'null') THEN
   WRITE(*,*) 'Specify FIRE_DYNAMICS_DIRECTORY in &FIRE_DYNAMICS_INPUTS and rerun.' 
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE GENERAL_ERROR_CHECKING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE FIRE_DYNAMICS_ERROR_CHECKING
! *****************************************************************************

IF (TRIM(MISCELLANEOUS_INPUTS_DIRECTORY) .EQ. 'null') THEN
   WRITE(*,*) 'Specify MISCELLANEOUS_INPUTS_DIRECTORY in &FIRE_DYNAMICS_INPUTS and rerun.' 
   STOP
ENDIF

!IF (TRIM(CBD_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify CBD_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF

!IF (TRIM(CBH_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify CBH_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF

!IF (TRIM(CC_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify CC_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF

!IF (TRIM(FBFM_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify FBFM_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(SLP_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify SLP_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(WAF_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify WAF_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(WS_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify WS_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(FLAME_LENGTH_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify FLAME_LENGTH_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(FLIN_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify FLIN_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(VELOCITY_DMS_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify VELOCITY_DMS_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(HPUA_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify HPUA_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (TRIM(IR_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) 'Specify IR_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun.' 
!   STOP
!ENDIF
!
!IF (M1_MINUS_MEQ .LT. 0. .AND. TRIM(M1_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) '1-hour moisture content is undefined. Specify either M1_MINUS_MEQ'
!   WRITE(*,*) 'or M1_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun. Units are %.'
!   STOP
!ENDIF
!
!IF (M1_MINUS_MEQ .GT. 0. .AND. TRIM(M1_FILENAME) .NE. 'null') THEN
!   WRITE(*,*) '1-hour moisture content specified twice. Specify either'
!   WRITE(*,*) 'M1_MINUS_MEQ or M1_FILENAME, but not both. Units are %.'
!   STOP
!ENDIF
!
!IF (M10_MINUS_MEQ .LT. 0. .AND. TRIM(M10_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) '10-hour moisture content is undefined. Specify either M10_MINUS_MEQ'
!   WRITE(*,*) 'or M10_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun. Units are %.'
!   STOP
!ENDIF
!
!IF (M10_MINUS_MEQ .GT. 0. .AND. TRIM(M10_FILENAME) .NE. 'null') THEN
!   WRITE(*,*) '10-hour moisture content specified twice. Specify either'
!   WRITE(*,*) 'M10_MINUS_MEQ or M10_FILENAME, but not both. Units are %.'
!   STOP
!ENDIF
!
!IF (M100_MINUS_MEQ .LT. 0. .AND. TRIM(M100_FILENAME) .EQ. 'null') THEN
!   WRITE(*,*) '100-hour moisture content is undefined. Specify either M100_MINUS_MEQ'
!   WRITE(*,*) 'or M100_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun. Units are %.'
!   STOP
!ENDIF
!
!IF (M100_MINUS_MEQ .GT. 0. .AND. TRIM(M100_FILENAME) .NE. 'null') THEN
!   WRITE(*,*) '100-hour moisture content specified twice. Specify either'
!   WRITE(*,*) 'M100_MINUS_MEQ or M100_FILENAME, but not both. Units are %.'
!   STOP
!ENDIF

IF (LH_MOISTURE_CONTENT .LT. 0. .AND. TRIM(MLH_FILENAME) .EQ. 'null') THEN
   WRITE(*,*) 'Live herbaceous moisture content not defined. Specify either'
   WRITE(*,*) 'LH_MOISTURE_CONTENT or MLH_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun'
   STOP
ENDIF

IF (LH_MOISTURE_CONTENT .GT. 0. .AND. TRIM(MLH_FILENAME) .NE. 'null') THEN
   WRITE(*,*) 'Live herbaceous moisture content specified twice. Specify'
   WRITE(*,*) 'either LH_MOISTURE_CONTENT or LH_FILENAME, but not both, and rerun'
   STOP
ENDIF

IF (LW_MOISTURE_CONTENT .LT. 0. .AND. TRIM(MLW_FILENAME) .EQ. 'null') THEN
   WRITE(*,*) 'Live woody moisture content not defined. Specify either'
   WRITE(*,*) 'LW_MOISTURE_CONTENT or MLW_FILENAME in &FIRE_DYNAMICS_INPUTS and rerun'
   STOP
ENDIF

IF (LW_MOISTURE_CONTENT .GT. 0. .AND. TRIM(MLW_FILENAME) .NE. 'null') THEN
   WRITE(*,*) 'Live woody moisture content specified twice. Specify'
   WRITE(*,*) 'either LW_MOISTURE_CONTENT or MLW_FILENAME, but not both, and rerun'
   STOP
ENDIF

! *****************************************************************************
END SUBROUTINE FIRE_DYNAMICS_ERROR_CHECKING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE MAP_GENERATOR_ERROR_CHECKING
! *****************************************************************************

INTEGER :: ICOMP, ICAT, ITHRESH, I, J, K

SELECT CASE (TRIM(DISCRETE_OR_CONTINUOUS))
   CASE ('DISCRETE')
      WRITE(*,*) 'Using discrete categories for hazard ranking'
   CASE ('CONTINUOUS')
      WRITE(*,*) 'Using continuous scale for hazard ranking'
   CASE ('DEFAULT')
      WRITE(*,*) "Set DISCRETE_OR_CONTINUOUS to 'DISCRETE' or 'CONTINUOUS'"
      STOP
END SELECT

SELECT CASE (TRIM(ADDITIVE_OR_MULTIPLICATIVE))
   CASE ('ADDITIVE')
      WRITE(*,*) 'Using additive hazard ranking'
   CASE ('MULTIPLICATIVE')
      WRITE(*,*) 'Using multiplicative hazard ranking'
   CASE ('DEFAULT')
      WRITE(*,*) "Set ADDITIVE_OR_MULTIPLICATIVE to 'ADDITIVE' or 'MULTIPLICATIVE'"
      STOP
END SELECT

IF (NUMBER_OF_COMPONENTS .LT. 1) THEN
   WRITE(*,*) 'Specify NUMBER_OF_COMPONENTS and rerun'
   STOP
ENDIF

DO ICOMP = 1, NUMBER_OF_COMPONENTS
   IF (TRIM(INPUT_DIRECTORY_FOR_COMPONENT(ICOMP)) .EQ. 'null') THEN
      WRITE(*,*) 'Specify INPUT_DIRECTORY_FOR_COMPONENT # ', ICOMP
      STOP
   ENDIF
   
   IF (TRIM(FILENAME_OF_COMPONENT(ICOMP)) .EQ. 'null') THEN
      WRITE(*,*) 'Specify FILENAME_OF_COMPONENT # ', ICOMP
      STOP
   ENDIF

   IF (TRIM(DISCRETE_OR_CONTINUOUS) .EQ. 'DISCRETE' .AND. NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP) .LT. 1) THEN
      WRITE(*,*) 'Specify NUMBER_OF_CATEGORIES_FOR_COMPONENT # ', ICOMP
      STOP
   ENDIF
   
   IF (TRIM(OUTPUT_FILENAME_OF_CATEGORIZED_COMPONENT(ICOMP)) .EQ. 'null') THEN
      WRITE(*,*) 'Specify OUTPUT_FILENAME_OF_CATEGORIZED_COMPONENT # ', ICOMP
      STOP
   ENDIF

! Check category thresholds:
   DO ITHRESH = 1, NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP) - 1
      IF (CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRESH) .LT. -8E9) THEN
         WRITE(*,*) 'For component # ', ICOMP
         WRITE(*,*) 'and threshold ', ITHRESH
         WRITE(*,*) 'Specify CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRESH)'
         STOP      
      ENDIF
   ENDDO

   IF (NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP) .GT. 0) THEN
   DO ITHRESH = NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP), 100
      IF (CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRESH) .GT. -9E9) THEN
         WRITE(*,*) 'For component # ', ICOMP
         WRITE(*,*) 'and threshold ', ITHRESH
         WRITE(*,*) 'there is no need to specify CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRESH)'
         STOP      
      ENDIF
   ENDDO
   ENDIF

! Check category scores:
   DO ICAT = 1, NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP)
      IF (CATEGORY_SCORES_FOR_COMPONENT(ICOMP,ICAT) .LT. -8E9) THEN
         WRITE(*,*) 'For component # ', ICOMP
         WRITE(*,*) 'and category ', ICAT
         WRITE(*,*) 'Specify CATEGORY_SCORES_FOR_COMPONENT(ICOMP,ICAT)'
         STOP      
      ENDIF
   ENDDO

   IF (NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP) .GT. 0) THEN
   DO ICAT = NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP) + 1, 100
      IF (CATEGORY_SCORES_FOR_COMPONENT(ICOMP,ICAT) .GT. -9E9) THEN
         WRITE(*,*) 'For component # ', ICOMP
         WRITE(*,*) 'and category ', ICAT
         WRITE(*,*) 'there is no need to specify CATEGORY_SCORES_FOR_COMPONENT(ICOMP,ICAT)'
         STOP      
      ENDIF
   ENDDO
   ENDIF

ENDDO

IF (TRIM(DISCRETE_OR_CONTINUOUS) .EQ. 'DISCRETE' .AND. NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE .LT. 1) THEN
   WRITE(*,*) 'Specify NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE '
   STOP
ENDIF
   
IF (TRIM(OUTPUT_FILENAME_OF_FINAL_HAZARD_SCALE) .EQ. 'null') THEN
   WRITE(*,*) 'Specify OUTPUT_FILENAME_OF_FINAL_HAZARD_SCALE'
   STOP
ENDIF

IF (TRIM(OUTPUT_DIRECTORY) .EQ. 'null') THEN
   WRITE(*,*) 'Specify OUTPUT_DIRECTORY'
   STOP
ENDIF

! Check category thresholds: 
DO ITHRESH = 1, NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE - 1
   IF (CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE(ITHRESH) .LT. -8E9) THEN
      WRITE(*,*) 'For final hazard scale threshold ', ITHRESH
      WRITE(*,*) 'Specify CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE'
      STOP      
   ENDIF
ENDDO

IF (NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE .GT. 1) THEN
DO ITHRESH = NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE , 100
   IF (CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE(ITHRESH) .GT. -8E9) THEN
      WRITE(*,*) 'For final hazard scale there is no need to '
      WRITE(*,*) 'specify CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE for ITHRESH', ITHRESH
      STOP      
   ENDIF
ENDDO
ENDIF

! Check category scores:
IF (NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE .GT. 1) THEN
DO ICAT = 1, NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE
   IF (CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE(ICAT) .LT. -8E9) THEN
      WRITE(*,*) 'Specify CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE for ICAT ', ICAT
      STOP
   ENDIF
ENDDO
ENDIF

IF (NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE .GT. 1) THEN
DO ICAT = NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE + 1, 100
   IF (CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE(ICAT) .GT. -8E9) THEN
      WRITE(*,*) 'Specify CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE for ICAT ', ICAT
      STOP
   ENDIF
ENDDO
ENDIF

IF (USE_3BY3_SCORING) THEN

   IF (NUMBER_OF_COMPONENTS .NE. 3) THEN
      WRITE(*,*) 'In order to USE_3BY3_SCORING, set NUMBER_OF_COMPONENTS to 3'
      STOP
   ENDIF
   
   DO ICOMP = 1, NUMBER_OF_COMPONENTS
      DO ITHRESH = 1, 2
         IF (CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRESH) .LT. -8E9) THEN
            WRITE(*,*) 'In order to USE_3BY3_SCORING, specify '
            WRITE(*,*) 'CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRES)', ICOMP, ITHRESH
            STOP      
         ENDIF
      ENDDO
      DO ICAT = 1, 3
         IF (CATEGORY_SCORES_FOR_COMPONENT(ICOMP,ICAT) .NE. 1E0 .AND. &
             CATEGORY_SCORES_FOR_COMPONENT(ICOMP,ICAT) .NE. 2E0 .AND. &
             CATEGORY_SCORES_FOR_COMPONENT(ICOMP,ICAT) .NE. 3E0) THEN
            WRITE(*,*) 'In order to USE_3BY3_SCORING, specify '
            WRITE(*,*) 'CATEGORY_VALUES_FOR_COMPONENT(ICOMP,ICAT) ', ICOMP, ITHRESH
            WRITE(*,*) 'to be 1, 2, or 3.'
            STOP      
         ENDIF
      ENDDO
   ENDDO

   WRITE(*,*) 'Using three by three scoring'
   WRITE(*,*)
      
   DO I = 1, 3
   DO J = 1, 3
   DO K = 1, 3
      IF (THREE_BY_THREE_SCORING(I,J,K) .LE. -8E9) THEN
         WRITE(*,*) 'Specify THREE_BY_THREE_SCORING(I,J,K)' 
         STOP
      ENDIF
      WRITE(*,*) '3 by 3 scoring: ', I, J, K, ' value: ', THREE_BY_THREE_SCORING(I,J,K)
   ENDDO
   ENDDO
   ENDDO

   DO ICOMP = 1, NUMBER_OF_COMPONENTS
      WRITE(*,*) 'Component ', ICOMP, ' thresholds: ', CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,1), CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,2), CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,3)
   ENDDO 
   WRITE(*,*)

ELSE

! Print info/diagnostics abour scoring system:
   IF (TRIM(DISCRETE_OR_CONTINUOUS) .EQ. 'DISCRETE') THEN
      WRITE(*,*) 'The following scoring system is in use: '

      DO ICOMP = 1, NUMBER_OF_COMPONENTS
         WRITE(*,*)
         WRITE(*,*) 'For component ', ICOMP
         WRITE(*,*) 'Scores < ', CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,1), ' : ', CATEGORY_SCORES_FOR_COMPONENT(ICOMP,1)
         DO ITHRESH = 2, NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP)-1
             ICAT = ITHRESH
             WRITE(*,*) 'Scores between ', CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRESH-1), ' and ', CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,ITHRESH), ' : ', CATEGORY_SCORES_FOR_COMPONENT(ICOMP, ICAT)
         ENDDO
         WRITE(*,*) 'Scores > ', CATEGORY_THRESHOLDS_FOR_COMPONENT(ICOMP,NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP)-1), ' : ', CATEGORY_SCORES_FOR_COMPONENT(ICOMP,NUMBER_OF_CATEGORIES_FOR_COMPONENT(ICOMP))
      ENDDO

      WRITE(*,*)
      WRITE(*,*) 'For final hazard scale '
      WRITE(*,*) 'Scores < ', CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE(1), ' : ', CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE(1)
      DO ITHRESH = 2, NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE - 1
         ICAT = ITHRESH
         WRITE(*,*) 'Scores between ', CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE(ITHRESH-1), ' and ', CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE(ITHRESH), ' : ', CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE(ICAT)
      ENDDO
      WRITE(*,*) 'Scores > ', CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE(NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE-1), ' : ', CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE(NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE)

      WRITE(*,*)
   ENDIF

ENDIF

! *****************************************************************************
END SUBROUTINE MAP_GENERATOR_ERROR_CHECKING
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_MEQ(TMPF,RH,ADD_TO_MEQ,MEQ)
! *****************************************************************************

REAL, INTENT(IN) :: TMPF, RH, ADD_TO_MEQ
REAL, INTENT(OUT):: MEQ

IF (RH .LT. 10.) THEN
   MEQ = 0.03229 + 0.281073*RH - 0.000578 * RH * TMPF
ELSEIF (RH .LT. 50.) THEN
   MEQ = 2.22749 + 0.160107 * RH - 0.01478*TMPF
ELSE
   MEQ = 21.0606 + 0.005565*RH*RH - 0.00035*RH*TMPF - 0.483199*RH
ENDIF

MEQ = MEQ + ADD_TO_MEQ

! *****************************************************************************
END SUBROUTINE GET_MEQ
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_PIGN(TMPF,MEQ,PIGN)
! *****************************************************************************

REAL, INTENT(IN) :: TMPF, MEQ
REAL, INTENT(OUT):: PIGN

LOGICAL, PARAMETER :: OLDWAY = .FALSE. 
REAL :: F1, F2, SL, SH
INTEGER :: IT, IM
INTEGER, PARAMETER :: NT = 15
INTEGER, PARAMETER :: NM = 16
REAL, DIMENSION(1:NT), SAVE :: TS
REAL, DIMENSION(1:NM), SAVE :: MS 
REAL, DIMENSION(1:NT,1:NM), SAVE :: S
LOGICAL, SAVE :: FIRSTCALL = .TRUE. 

IF (FIRSTCALL .AND. (.NOT. OLDWAY) ) THEN 
   TS(:) = (/ -100., 35., 45., 55., 65., 75., 85., 95., 105., 115., 125., 135., 145., 155., 200. /)
   MS(:) = (/ 1.5, 2.0, 2.5, 3.0,  4.0, 5.0, 6.0, 7.5, 9.5, 11.5, 14.5, 18.5, 23.0, 28.0, 30.0, 100.0 /)

   S( 1,:) = (/  87.,  80.,  74.,  69., 59., 51., 43., 34., 25., 17., 10.,  4., 1., 0., 0., 0. /)
   S( 2,:) = (/  87.,  80.,  74.,  69., 59., 51., 43., 34., 25., 17., 10.,  4., 1., 0., 0., 0. /)
   S( 3,:) = (/  89.,  83.,  77.,  71., 61., 53., 45., 36., 26., 18., 11.,  5., 1., 0., 0., 0. /)
   S( 4,:) = (/  92.,  85.,  79.,  73., 63., 54., 47., 37., 27., 20., 11.,  5., 2., 0., 0., 0. /)
   S( 5,:) = (/  94.,  88.,  81.,  76., 65., 56., 49., 39., 29., 21., 12.,  6., 2., 0., 0., 0. /)
   S( 6,:) = (/  97.,  90.,  84.,  78., 68., 59., 51., 41., 30., 22., 13.,  6., 2., 0., 0., 0. /)
   S( 7,:) = (/ 100.,  93.,  87.,  81., 70., 61., 53., 42., 31., 23., 14.,  7., 2., 1., 0., 0. /)
   S( 8,:) = (/ 100.,  96.,  90.,  84., 73., 63., 55., 44., 33., 24., 15.,  7., 3., 1., 0., 0. /)
   S( 9,:) = (/ 100.,  99.,  93.,  86., 75., 66., 57., 46., 35., 26., 16.,  8., 3., 1., 0., 0. /)
   S(10,:) = (/ 100., 100.,  96.,  89., 78., 68., 59., 48., 36., 27., 17.,  9., 3., 1., 0., 0. /)
   S(11,:) = (/ 100., 100.,  99.,  93., 81., 71., 62., 51., 38., 29., 18.,  9., 4., 1., 0., 0. /)
   S(12,:) = (/ 100., 100., 100.,  96., 84., 74., 65., 53., 40., 30., 20., 10., 4., 1., 0., 0. /)
   S(13,:) = (/ 100., 100., 100.,  99., 87., 77., 67., 55., 42., 32., 21., 11., 5., 2., 0., 0. /)
   S(14,:) = (/ 100., 100., 100., 100., 90., 80., 70., 58., 45., 34., 22., 12., 5., 2., 0., 0. /)
   S(15,:) = (/ 100., 100., 100., 100., 90., 80., 70., 58., 45., 34., 22., 12., 5., 2., 0., 0. /)
   FIRSTCALL = .FALSE. 
ENDIF

IF (OLDWAY) THEN
   F1   = 107. / (1. + 0.028 * MEQ**1.94 )
   F2   = 0.677 + 0.00322 * TMPF
   PIGN = 0.01 * MAX(MIN(F1*F2,100.),0.1)
ELSE
! Figure out where we are in the ignition probability table:
   CALL LOCATE(MS(:), NM, MEQ , IM)
   CALL LOCATE(TS(:), NT, TMPF, IT)

!First, interpolate in x direction to get SL, SH 
   SL =  LINTERP(MEQ, MS(IM), MS(IM+1), S(IT  ,IM), S(IT  ,IM+1) )
   SH =  LINTERP(MEQ, MS(IM), MS(IM+1), S(IT+1,IM), S(IT+1,IM+1) )

! Now linearly interpolate in y direction:
   PIGN =  0.01 * LINTERP(TMPF,TS(IT),TS(IT+1),SL,SH)

!! Dummy code to compare old way to new way
!   F1   = 107. / (1. + 0.028 * MEQ**1.94 )
!   F2   = 0.677 + 0.00322 * TMPF
!   PIGNOLD = 0.01 * MAX(MIN(F1*F2,100.),0.1)
!   WRITE(*,*) PIGNOLD, PIGN

ENDIF

! *****************************************************************************
END SUBROUTINE GET_PIGN
! *****************************************************************************

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

!******************************************************************************
REAL FUNCTION LINTERP(X,XL,XH,FXL,FXH)
!******************************************************************************

REAL, INTENT(IN) :: X, XL,XH,FXL,FXH
LINTERP = FXL + (FXH-FXL)*(X-XL)/(XH-XL)

! *****************************************************************************
END FUNCTION LINTERP
!******************************************************************************

! *****************************************************************************
SUBROUTINE SETUP_FIRE_SPREAD
! *****************************************************************************

INTEGER :: IANGLE

! Set angle to 8 adjacent cells   
DO IANGLE = 1, 8
   THETA(IANGLE) = REAL(IANGLE-1)*PI/4. 
   SINTHETA(IANGLE) = SIN(THETA(IANGLE)) 
   COSTHETA(IANGLE) = COS(THETA(IANGLE)) 
ENDDO

!SINTHETA(2) = 0. ; COSTHETA(2) = 0.
!SINTHETA(4) = 0. ; COSTHETA(4) = 0.
!SINTHETA(6) = 0. ; COSTHETA(6) = 0.
!SINTHETA(8) = 0. ; COSTHETA(8) = 0.

! Set column and row offset values from current cells
COLOFFSET(1) = 0; ROWOFFSET(1) =  1  
COLOFFSET(2) = 1; ROWOFFSET(2) =  1  
COLOFFSET(3) = 1; ROWOFFSET(3) =  0  
COLOFFSET(4) = 1; ROWOFFSET(4) = -1  
COLOFFSET(5) = 0; ROWOFFSET(5) = -1  
COLOFFSET(6) =-1; ROWOFFSET(6) = -1  
COLOFFSET(7) =-1; ROWOFFSET(7) =  0  
COLOFFSET(8) =-1; ROWOFFSET(8) =  1  

! Set distance travelled in adjacent 
DIST(:,:) = 0.5*FBFM%XDIM
DIST(2,:) = SQRT((0.5*FBFM%XDIM)**2 + (0.5*FBFM%XDIM)**2)
DIST(4,:) = SQRT((0.5*FBFM%XDIM)**2 + (0.5*FBFM%XDIM)**2)
DIST(6,:) = SQRT((0.5*FBFM%XDIM)**2 + (0.5*FBFM%XDIM)**2)
DIST(8,:) = SQRT((0.5*FBFM%XDIM)**2 + (0.5*FBFM%XDIM)**2)

! *****************************************************************************
END SUBROUTINE SETUP_FIRE_SPREAD
! *****************************************************************************

! *****************************************************************************
SUBROUTINE GET_COORDINATE_SYSTEM(FN)
! *****************************************************************************

CHARACTER(400), INTENT(IN) :: FN
CHARACTER(400) :: SRS_STRING
INTEGER :: IOS

! First get NCOLS, NROWS from file FN
WRITE(SHELLSTR,500) 'gdalsrsinfo ' // TRIM(FN) // '.tif > gdalsrsinfo.txt'
WRITE(*,200) TRIM(SHELLSTR); CALL SYSTEM(TRIM(SHELLSTR))

! Use gdalinfo to get file size
OPEN(LUINPUT,FILE='gdalsrsinfo.txt',FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
READ(LUINPUT,*)
READ(LUINPUT,200) SRS_STRING
CLOSE(LUINPUT)

SHELLSTR = TRIM(DELETECOMMAND) // ' gdalsrsinfo.txt'; WRITE(*,200) TRIM(SHELLSTR); CALL SYSTEM(TRIM(SHELLSTR))

A_SRS = TRIM(SRS_STRING(9:))

200 FORMAT(A)
500 FORMAT(A, 4(I6, ' '), 4(A))

! *****************************************************************************
END SUBROUTINE GET_COORDINATE_SYSTEM
! *****************************************************************************

! *****************************************************************************
END MODULE SUBS
! *****************************************************************************
