! *****************************************************************************
MODULE FIRE_DYNAMICS_IO
! *****************************************************************************

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE READ_FUEL_MODEL_TABLE
! *****************************************************************************

USE SURFACE_SPREAD_VARS, ONLY : FUEL_MODEL_TABLE_TYPE, FUEL_MODEL_TABLE_2D
USE VARS, ONLY : MISCELLANEOUS_INPUTS_DIRECTORY, FUEL_MODEL_FILE, LUINPUT

IMPLICIT NONE

CHARACTER(400) :: FNINPUT
INTEGER :: INUM, IOS, ILH
REAL :: LIVEFRAC, DEADFRAC, LH
TYPE(FUEL_MODEL_TABLE_TYPE) :: FM, FUEL_MODEL_TABLE(0:256)

FUEL_MODEL_TABLE(:)%SHORTNAME='NULL' !Initialize fuel model names

FNINPUT = TRIM(MISCELLANEOUS_INPUTS_DIRECTORY) // TRIM(FUEL_MODEL_FILE)

!Attempt to open fuel model table file:
OPEN(LUINPUT,FILE=TRIM(FNINPUT),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
IF (IOS .GT. 0) THEN
   WRITE(*,*) 'Problem opening fuel model table file ', TRIM(FNINPUT)
   STOP
ENDIF

!Read fuel models and store in FUEL_MODEL_TABLE
IOS = 0
DO WHILE (IOS .EQ. 0)
   READ(LUINPUT,*,IOSTAT=IOS) INUM,FM%SHORTNAME,FM%DYNAMIC,FM%W0(1),FM%W0(2),FM%W0(3),FM%W0(5), &
                                FM%W0(6),FM%SIG(1),FM%SIG(5),FM%SIG(6),FM%DELTA,FM%MEX_DEAD,FM%HOC
   FM%MEX_DEAD = FM%MEX_DEAD / 100.
   FM%SIG(2) = 109.  !  10-hour surface area to volume ratio, 1/ft
   FM%SIG(3) =  30.  ! 100-hour surface area to volume ratio, 1/ft
   FM%RHOP   =  32.  ! Particle density
   FM%ST     =   0.055
   FM%SE     =   0.01
   FM%ETAS   = 0.174/(FM%SE**0.19) !Mineral damping coefficient, dimensionless

   IF (IOS .EQ. 0) FUEL_MODEL_TABLE(INUM) = FM
ENDDO
CLOSE(LUINPUT)

DO INUM = 0, 256
   IF ( TRIM(FUEL_MODEL_TABLE(INUM)%SHORTNAME) .EQ. 'NULL' ) CYCLE
   FUEL_MODEL_TABLE_2D(INUM,:) = FUEL_MODEL_TABLE(INUM)

   DO ILH = 30, 120
      LH = REAL(ILH)
      FM = FUEL_MODEL_TABLE_2D(INUM,ILH)

      IF (FM%DYNAMIC) THEN
         LIVEFRAC  = MIN( MAX( (LH - 30. ) / (120.  - 30. ) , 0.), 1.)
         DEADFRAC  = 1. - LIVEFRAC
         FM%W0 (4) = DEADFRAC * FM%W0(5)
         FM%W0 (5) = LIVEFRAC * FM%W0(5)
         FM%SIG(4) = FM%SIG(5)
         FM%SIG(1) = (FM%SIG(1)*FM%SIG(1)*FM%W0(1) + FM%SIG(4)*FM%SIG(4)*FM%W0(4)) / ( FM%SIG(1)*FM%W0(1) + FM%SIG(4)*FM%W0(4) )
         FM%W0 (1) = FM%W0(1) + FM%W0(4)
         FM%W0 (4) = 0.
         FM%SIG(4) = 9999.
      ELSE
         FM%W0 (4) = 0.0
         FM%SIG(4) = 9999.
      ENDIF
   
      FM%A(:) = FM%SIG(:)*FM%W0(:) / FM%RHOP

      FM%A_DEAD = MAX(SUM(FM%A(1:4)),1D-9)
      FM%A_LIVE = MAX(SUM(FM%A(5:6)),1D-9)
      FM%A_OVERALL = FM%A_DEAD + FM%A_LIVE

      FM%F(1:4) = FM%A(1:4) / FM%A_DEAD 
      FM%F(5:6) = FM%A(5:6) / FM%A_LIVE 
   
      FM%F_DEAD = FM%A_DEAD / FM%A_OVERALL
      FM%F_LIVE = FM%A_LIVE / FM%A_OVERALL

      FM%FW0(:) = FM%F(:) * FM%W0(:)
   
      FM%FSIG(:) = FM%F(:) * FM%SIG(:)

      FM%EPS(:) = EXP(-138. / FM%SIG(:)) 
       
      FM%WPRIMENUMER(1:4) = FM%W0(1:4) * FM%EPS(1:4)
      FM%WPRIMEDENOM(5:6) = FM%W0(5:6) * EXP(-500./FM%SIG(5:6))

      FM%MPRIMEDENOM(1:4) = FM%W0(1:4) * FM%EPS(1:4)
   
      FM%W0_DEAD = SUM(FM%FW0(1:4))
      FM%W0_LIVE = SUM(FM%FW0(5:6))
   
      FM%WN_DEAD = FM%W0_DEAD * (1. - FM%ST)
      FM%WN_LIVE = FM%W0_LIVE * (1. - FM%ST)
      
      FM%SIG_DEAD = SUM(FM%FSIG(1:4))
      FM%SIG_LIVE = SUM(FM%FSIG(5:6))
  
      FM%SIG_OVERALL = FM%F_DEAD * FM%SIG_DEAD + FM%F_LIVE * FM%SIG_LIVE
      FM%BETA        = SUM(FM%W0(1:6)) / (FM%DELTA * FM%RHOP)
      FM%BETAOP      = 3.348/(FM%SIG_OVERALL**0.8189)
      FM%RHOB        = SUM(FM%W0(1:6)) / FM%DELTA
   
      FM%XI = EXP((0.792 + 0.681*SQRT(FM%SIG_OVERALL))*(0.1+FM%BETA)) / (192. + 0.2595*FM%SIG_OVERALL)
   
      FM%A_COEFF = 133./(FM%SIG_OVERALL**0.7913)
      FM%B_COEFF = 0.02526*FM%SIG_OVERALL**0.54
      FM%C_COEFF = 7.47*EXP(-0.133*FM%SIG_OVERALL**0.55)
      FM%E_COEFF = 0.715*(EXP(-0.000359*FM%SIG_OVERALL))
   
      FM%GAMMAPRIMEPEAK = FM%SIG_OVERALL**1.5 / (495. + 0.0594*FM%SIG_OVERALL**1.5)
      FM%GAMMAPRIME = FM%GAMMAPRIMEPEAK*(FM%BETA/FM%BETAOP)**FM%A_COEFF*EXP(FM%A_COEFF*(1.-FM%BETA/FM%BETAOP))
   
      FM%TR = 384. / FM%SIG_OVERALL

      FM%GP_WND_EMD_ES_HOC = FM%GAMMAPRIME * FM%WN_DEAD * FM%ETAS * FM%HOC
      FM%GP_WNL_EML_ES_HOC = FM%GAMMAPRIME * FM%WN_LIVE * FM%ETAS * FM%HOC

      FM%PHISTERM=5.275 * FM%BETA**(-0.3)
      FM%PHIWTERM = FM%C_COEFF * (FM%BETA / FM%BETAOP)**(-FM%E_COEFF)

      FM%WPRIMEDENOM56SUM = SUM(FM%WPRIMEDENOM(5:6))
      FM%WPRIMENUMER14SUM = SUM(FM%WPRIMENUMER(1:4))
      FM%MPRIMEDENOM14SUM = SUM(FM%MPRIMEDENOM(1:4))

      FM%UNSHELTERED_WAF = WIND_ADJUSTMENT_FACTOR(0., 0., FM%DELTA)

      FUEL_MODEL_TABLE_2D(INUM,ILH) = FM

   ENDDO

ENDDO

!Set any unused fuel models to 256 (NB)
DO INUM = 0, 256
DO ILH = 30, 120
   IF ( TRIM(FUEL_MODEL_TABLE_2D(INUM,ILH)%SHORTNAME) .EQ. 'NULL' ) FUEL_MODEL_TABLE_2D(INUM,ILH) = FUEL_MODEL_TABLE(256)
ENDDO
ENDDO

CONTAINS

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
END SUBROUTINE READ_FUEL_MODEL_TABLE
! *****************************************************************************

! *****************************************************************************
END MODULE FIRE_DYNAMICS_IO
! *****************************************************************************
