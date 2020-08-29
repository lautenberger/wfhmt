! *****************************************************************************
MODULE SURFACE_SPREAD_VARS
! *****************************************************************************

IMPLICIT NONE

TYPE :: FUEL_MODEL_TABLE_TYPE
   CHARACTER(400) :: SHORTNAME ! Short character string describing fuel model
   LOGICAL        :: DYNAMIC   ! Is this a dynamic fuel model?
   REAL           :: W0 (1:6)  ! Total fuel loading, lb/ft2
   REAL           :: WN (1:6)  ! Net   fuel loading, lb/ft2
   REAL           :: SIG(1:6)  ! Surface area to volume ratio, 1/ft
   REAL           :: DELTA     ! Fuel bed thickness, ft
   REAL           :: MEX_DEAD  ! Dead fuel moisture of extinction 
   REAL           :: HOC       ! Heat of combustion, Btu/lb
   REAL           :: RHOB      ! Bulk density, lb/ft3
   REAL           :: RHOP      ! Particle density, lb/ft3
   REAL           :: ST        ! Mineral content, lb minerals / lb ovendry mass
   REAL           :: SE        ! Factor in ETAS
   REAL           :: ETAS      ! Mineral damping coefficient, dimensionless
   REAL           :: BETA      ! Packing ratio, dimensionless
   REAL           :: BETAOP    ! Optimal packing ratio, dimensionless
   REAL           :: XI        ! Propagating flux ratio, dimensionless
   
   REAL           :: A_COEFF ! "A" coefficient in Rothermel model
   REAL           :: B_COEFF ! "B" coefficient in Rothermel model
   REAL           :: C_COEFF ! "C" coefficient in Rothermel model
   REAL           :: E_COEFF ! "E" coefficient in Rothermel model

   REAL           :: GAMMAPRIME     ! Reaction velocity, 1/min
   REAL           :: GAMMAPRIMEPEAK ! Peak reaction velocity, 1/min 

   REAL           :: A_DEAD    ! Area factor for dead fuels
   REAL           :: A_LIVE    ! Area factor for live fuels
   REAL           :: A_OVERALL ! Area factor (overall)

   REAL           :: F_DEAD ! f factor for dead fuels
   REAL           :: F_LIVE ! f factor for live fuels

   REAL           :: W0_DEAD ! Total fuel loading for dead fuels, lb/ft2
   REAL           :: W0_LIVE ! Total fuel loading for live fuels, lb/ft2

   REAL           :: WN_DEAD ! Net fuel loading for dead fuels, lb/ft2
   REAL           :: WN_LIVE ! Net fuel loading for live fuels, lb/ft2
   
   REAL           :: SIG_DEAD    ! Surface area to volume ratio for dead fuels, 1ft
   REAL           :: SIG_LIVE    ! Surface area to volume ratio for live fuels, 1/ft
   REAL           :: SIG_OVERALL ! Surface area to volume ratio overall, 1/ft
   
   REAL           :: TR          ! Residence time, min

   REAL, DIMENSION (1:6) :: A           ! Area factor 
   REAL, DIMENSION (1:6) :: F           ! f factor
   REAL, DIMENSION (1:6) :: FW0         ! f * w0
   REAL, DIMENSION (1:6) :: FSIG        ! f * sigma
   REAL, DIMENSION (1:6) :: EPS         ! epsilon (Surface heating number)
   REAL, DIMENSION (1:6) :: WPRIMENUMER ! Numerator of W'
   REAL, DIMENSION (1:6) :: WPRIMEDENOM ! Denominator of W'
   REAL, DIMENSION (1:6) :: MPRIMEDENOM ! Denominator of M'

! These are for performance optimizations:
   REAL :: GP_WND_EMD_ES_HOC ! = FM%GAMMAPRIME * FM%WN_DEAD * FM%ETAS * FM%HOC
   REAL :: GP_WNL_EML_ES_HOC ! = FM%GAMMAPRIME * FM%WN_LIVE * FM%ETAS * FM%HOC
   REAL :: PHISTERM          ! = (5.275 / FUEL_MODEL_TABLE(FBFM(IX,IY))%BETA**0.3)
   REAL :: PHIWTERM          ! = FM%C_COEFF * (FM%BETA / FM%BETAOP)**(-FM%E_COEFF)

   REAL :: WPRIMEDENOM56SUM ! = SUM(FM%WPRIMEDENOM(5:6))
   REAL :: WPRIMENUMER14SUM ! = SUM(FM%WPRIMENUMER(1:4))
   REAL :: MPRIMEDENOM14SUM ! = SUM(FM%MPRIMEDENOM(1:4))

   REAL :: UNSHELTERED_WAF !Wind adjustment factor when unsheltered

END TYPE

TYPE(FUEL_MODEL_TABLE_TYPE), DIMENSION(0:256,30:120) :: FUEL_MODEL_TABLE_2D !Table for holding fuel models

! Rothermel arrays:
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: QIG, FEPSQIG, FMC, FMEX, MPRIMENUMER, M
REAL, ALLOCATABLE, DIMENSION(:,:) :: MEX_LIVE, M_DEAD, M_LIVE, ETAM_DEAD, ETAM_LIVE, RHOBEPSQIG, RHOBEPSQIG_DEAD, &
                                     RHOBEPSQIG_LIVE, IR_DEAD, IR_LIVE, MOMEX, TANSLPPIO180
!Constants:
REAL, PARAMETER :: PI = 3.1415926535897932384626433832795
REAL, PARAMETER :: HUGE = 9D9
REAL, PARAMETER :: TINY = 1D-9

! *****************************************************************************
END MODULE SURFACE_SPREAD_VARS
! *****************************************************************************

