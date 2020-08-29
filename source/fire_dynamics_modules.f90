! *****************************************************************************
MODULE FIRE_DYNAMICS_MODULE
! *****************************************************************************

IMPLICIT NONE

CONTAINS

! *****************************************************************************
SUBROUTINE SURFACE_SPREAD_RATE(NX,NY,FBFM,SLP,PHIS,PHIW,ADJ,IBAND)
! *****************************************************************************
! Applies Rothermel suface fire spread model to calculate surface fire rate
! of spread, heat per unit area, fireline intensity, flame length, and 
! reaction intensity

USE SURFACE_SPREAD_VARS, ONLY : PI,FUEL_MODEL_TABLE_2D, QIG, FEPSQIG, FMC, FMEX, MPRIMENUMER, MEX_LIVE, M_DEAD, M_LIVE, &
                                ETAM_DEAD, ETAM_LIVE, RHOBEPSQIG_DEAD, RHOBEPSQIG_LIVE, RHOBEPSQIG, IR_DEAD, IR_LIVE, &
                                MOMEX, M

USE VARS, ONLY : TANSLP2, PIO180, M1_NOW, M10_NOW, M100_NOW, WSMF, IR, VS0, MLH, MLW

IMPLICIT NONE

INTEGER, INTENT(IN) :: NX, NY, IBAND
INTEGER*2, POINTER, DIMENSION(:,:), INTENT(IN) :: FBFM
REAL, POINTER, DIMENSION(:,:), INTENT(IN) :: SLP,ADJ
REAL, DIMENSION(:,:), INTENT(OUT) :: PHIS,PHIW

!Local variables:
INTEGER :: IX, IY, ISLP, ILH
REAL :: WS_LIMIT, WSMF_LIMITED, PHIS_MAX, MOMEX2, MOMEX3
REAL, PARAMETER :: BTUPFT2MIN_TO_KWPM2 = 1.055/(60. * 0.3048 * 0.3048)

! Only need to do this once - check if QIG is ALLOCATED
IF (.NOT. ALLOCATED(QIG)) THEN 
   ALLOCATE (QIG            (1:6,1:NX,1:NY)) ! Heat of preignition, Btu/lb
   ALLOCATE (FEPSQIG        (1:6,1:NX,1:NY)) ! f*epsilon*Qig
   ALLOCATE (FMC            (1:6,1:NX,1:NY)) ! f*moisture content
   ALLOCATE (FMEX           (1:6,1:NX,1:NY)) ! f*Mex
   ALLOCATE (MPRIMENUMER    (1:6,1:NX,1:NY)) ! Numerator in M'
   ALLOCATE (MEX_LIVE       (1:NX,1:NY    )) ! Live fuel moisture of extinction
   ALLOCATE (M_DEAD         (1:NX,1:NY    )) ! Dead fuel moisture content
   ALLOCATE (M_LIVE         (1:NX,1:NY    )) ! Live fuel moisture content
   ALLOCATE (ETAM_DEAD      (1:NX,1:NY    )) ! Dead fuel moisture damping coefficient
   ALLOCATE (ETAM_LIVE      (1:NX,1:NY    )) ! Live fuel moisture damping coefficient
   ALLOCATE (RHOBEPSQIG_DEAD(1:NX,1:NY    )) ! Dead fuel bulk density * epsilon * Qig
   ALLOCATE (RHOBEPSQIG_LIVE(1:NX,1:NY    )) ! Live fuel bulk density * epsilon * Qig
   ALLOCATE (RHOBEPSQIG     (1:NX,1:NY    )) ! Overall bulk density * epsilon * Qig
   ALLOCATE (IR_DEAD        (1:NX,1:NY    )) ! Reaction intensity of dead fuels, Btu/ft2-min
   ALLOCATE (IR_LIVE        (1:NX,1:NY    )) ! Reaction intensity of live fuels, Btu/ft2-min
   ALLOCATE (MOMEX          (1:NX,1:NY    )) ! M / Mex
   ALLOCATE (M              (1:6,1:NX,1:NY)) ! Fuel moisture content array, dimensionless
ENDIF

!$omp PARALLEL DO SCHEDULE(STATIC) PRIVATE(IX,IY,ILH,MOMEX2,MOMEX3,WS_LIMIT,WSMF_LIMITED,PHIS_MAX,ISLP)
DO IY = 1, NY
DO IX = 1, NX
   IF (FBFM(IX,IY) .EQ. 91 .OR. FBFM(IX,IY) .EQ. 93 .OR. FBFM(IX,IY) .EQ. 98 .OR. FBFM(IX,IY) .EQ. 99 &
       .OR. FBFM(IX,IY) .EQ. 256 .OR. FBFM(IX,IY) .LE. 0 .OR. SLP(IX,IY) .LT. 0.) CYCLE

   M(1,IX,IY) = M1_NOW(IX,IY)
   M(2,IX,IY) = M10_NOW(IX,IY)
   M(3,IX,IY) = M100_NOW(IX,IY)
   M(4,IX,IY) = M1_NOW(IX,IY) !Set dynamic dead to m1
   M(5,IX,IY) = MLH%RZT(1,IX,IY)
   M(6,IX,IY) = MLW%RZT(1,IX,IY)

   ILH = MAX(MIN(NINT(100.*M(5,IX,IY)),120),30)

!Calculate live fuel moisture of extinction:
   MPRIMENUMER(1:4,IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%W0(1:4) * M(1:4,IX,IY) * FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%EPS(1:4)

   IF (FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%WPRIMEDENOM56SUM .GT. 1E-6) THEN
      MEX_LIVE(IX,IY) = 2.9 * FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%WPRIMENUMER14SUM / FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%WPRIMEDENOM56SUM
   ELSE
      MEX_LIVE(IX,IY) = 100.0
   ENDIF

   MEX_LIVE(IX,IY) = MEX_LIVE(IX,IY) * (1. - SUM(MPRIMENUMER(1:4,IX,IY)) / (FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%MPRIMEDENOM14SUM*FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%MEX_DEAD)) - 0.226
   MEX_LIVE(IX,IY) = MAX(MEX_LIVE(IX,IY), FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%MEX_DEAD)

   FMEX(1:4,IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%F(1:4) * FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%MEX_DEAD
   FMEX(5:6,IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%F(5:6) * MEX_LIVE(IX,IY)

   FMC(:,IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%F(:) * M(:,IX,IY)

   QIG(:,IX,IY) = 250. + 1116.*M(:,IX,IY)

   FEPSQIG(:,IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%F(:) * FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%EPS(:) * QIG(:,IX,IY)

   RHOBEPSQIG_DEAD(IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%RHOB*SUM(FEPSQIG(1:4,IX,IY))
   RHOBEPSQIG_LIVE(IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%RHOB*SUM(FEPSQIG(5:6,IX,IY))
   RHOBEPSQIG(IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%F_DEAD*RHOBEPSQIG_DEAD(IX,IY) + FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%F_LIVE*RHOBEPSQIG_LIVE(IX,IY)

   M_DEAD(IX,IY) = SUM(FMC(1:4,IX,IY))
   M_LIVE(IX,IY) = SUM(FMC(5:6,IX,IY))

   MOMEX(IX,IY) = M_DEAD(IX,IY) / FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%MEX_DEAD
   MOMEX2=MOMEX(IX,IY)*MOMEX(IX,IY)
   MOMEX3=MOMEX2*MOMEX(IX,IY)

   ETAM_DEAD(IX,IY) = 1.0 - 2.59*MOMEX(IX,IY) + 5.11*MOMEX2 - 3.52*MOMEX3
   ETAM_DEAD(IX,IY) = MAX(0.,MIN(ETAM_DEAD(IX,IY),1.))

   MOMEX(IX,IY) = M_LIVE(IX,IY) / MEX_LIVE(IX,IY)
   MOMEX2=MOMEX(IX,IY)*MOMEX(IX,IY)
   MOMEX3=MOMEX2*MOMEX(IX,IY)
   ETAM_LIVE(IX,IY) = 1.0 - 2.59*MOMEX(IX,IY) + 5.11*MOMEX2 - 3.52*MOMEX3
   ETAM_LIVE(IX,IY) = MAX(0.,MIN(ETAM_LIVE(IX,IY),1.))

   IR_DEAD(IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%GP_WND_EMD_ES_HOC * ETAM_DEAD(IX,IY)
   IR_LIVE(IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%GP_WNL_EML_ES_HOC * ETAM_LIVE(IX,IY)

   IR%RZT(IBAND,IX,IY) = IR_DEAD(IX,IY) + IR_LIVE(IX,IY) !Btu/(ft^2-min)

!   WS_LIMIT = 96.8*IR(IX,IY)**0.3333333 !Andrews, Cruz, and Rothermel (2013) limit
   WS_LIMIT = 0.9*IR%RZT(IBAND,IX,IY) !Original limit
   WSMF_LIMITED = MIN(WSMF(IX,IY), WS_LIMIT)
   PHIW(IX,IY) = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%PHIWTERM * WSMF_LIMITED**FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%B_COEFF

! Max slope factor is equal to max wind factor:
   PHIS_MAX = FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%PHIWTERM * WS_LIMIT**FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%B_COEFF
   ISLP=MIN(MAX(NINT(SLP(IX,IY)),0),90)
   PHIS(IX,IY) = MIN(FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%PHISTERM * TANSLP2(ISLP), PHIS_MAX)

   VS0%RZT(IBAND,IX,IY) = ADJ(IX,IY) * IR%RZT(IBAND,IX,IY) * FUEL_MODEL_TABLE_2D(FBFM(IX,IY),ILH)%XI / RHOBEPSQIG(IX,IY) !ft/min

! Convert reaction intensity to SI:
   IR%RZT(IBAND,IX,IY) = IR%RZT(IBAND,IX,IY) * BTUPFT2MIN_TO_KWPM2 ! kW/m2

ENDDO
ENDDO
!$omp END PARALLEL DO

! *****************************************************************************
END SUBROUTINE SURFACE_SPREAD_RATE
! *****************************************************************************

! *****************************************************************************
END MODULE FIRE_DYNAMICS_MODULE
! *****************************************************************************