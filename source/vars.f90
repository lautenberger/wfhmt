! *****************************************************************************
MODULE VARS
! *****************************************************************************
IMPLICIT NONE

CHARACTER(400) :: A_SRS
CHARACTER(6) :: DELETECOMMAND = 'del ' 
CHARACTER(1) :: PATH_SEPARATOR
CHARACTER(7) :: OPERATING_SYSTEM

!Constants
REAL, PARAMETER :: PI     = 3.14159265358979323846264338327950288419716
REAL, PARAMETER :: TWOPI  = 2.0 * PI
REAL, PARAMETER :: PIO2 = 0.5 * PI 
REAL, PARAMETER :: PIO180 = PI / 180.
REAL, PARAMETER :: SIGMA = 5.67D-8

TYPE :: RASTER_TYPE
   CHARACTER     :: BYTEORDER
   CHARACTER(3)  :: LAYOUT
   INTEGER       :: NROWS
   INTEGER       :: NCOLS
   INTEGER       :: NBANDS   
   INTEGER       :: NBITS
   INTEGER       :: BANDROWBYTES
   INTEGER       :: TOTALROWBYTES
   CHARACTER(10) :: PIXELTYPE
   REAL          :: ULXMAP
   REAL          :: ULYMAP
   REAL          :: XDIM
   REAL          :: YDIM
   REAL          :: NODATA_VALUE

!   REAL          :: CELLSIZE
   REAL          :: XLLCORNER
   REAL          :: YLLCORNER

!   REAL,POINTER, DIMENSION(:,:,:) :: RZT !=>NULL()   
!   INTEGER*2,POINTER, DIMENSION(:,:,:) :: I2ZT !=>NULL()   

   REAL,ALLOCATABLE, DIMENSION(:,:,:) :: RZT !=>NULL()   
   INTEGER*2,ALLOCATABLE, DIMENSION(:,:,:) :: I2ZT !=>NULL()   

END TYPE

INTEGER, PARAMETER :: LUINPUT = 20, LUOUTPUT = 10

CHARACTER(5000) :: SHELLSTR

CHARACTER(400)   :: FUEL_MODEL_FILE, MISCELLANEOUS_INPUTS_DIRECTORY, PATH_TO_GDAL, SCRATCH

CHARACTER(400) :: ADJ_FILENAME, ASP_FILENAME, CBD_FILENAME, CBH_FILENAME, CC_FILENAME, CH_FILENAME, DEM_FILENAME, FBFM_FILENAME, MFRI_FILENAME, &
                  SLP_FILENAME, WAF_FILENAME, WS_FILENAME, WD_FILENAME, M1_FILENAME, M10_FILENAME, M100_FILENAME, MLH_FILENAME, MLW_FILENAME, &
                  FLAME_LENGTH_FILENAME, FLIN_FILENAME, VELOCITY_DMS_FILENAME, HPUA_FILENAME, IR_FILENAME, MEQ_FILENAME, RH_FILENAME, TMP_FILENAME, FFWI_FILENAME, &
                  MFFWI_FILENAME, UWIND_FILENAME, VWIND_FILENAME, &
                  TARR_FILENAME, NORMANGLE_FILENAME, PIGN_FILENAME, PIGN_AVG_FILENAME, LBF_FILENAME, LBF_AVG_FILENAME, PIGN_LBF_FILENAME, PIGN_LBF_AVG_FILENAME, &
                  PIGN_WS_FILENAME, PIGN_WS_AVG_FILENAME, PCT_WS_ABOVE_THRESHOLD_FILENAME, PIGN_PCT_WS_ABOVE_THRESHOLD_FILENAME, &
                  MEQ_AVG_FILENAME, WS_AVG_FILENAME, RH_AVG_FILENAME, TMP_AVG_FILENAME, FFWI_AVG_FILENAME, PRES_FILENAME, SPFH_FILENAME, TMPF_FILENAME

TYPE(RASTER_TYPE) :: ADJ, ASP, CBD, CBH, CC, CH, DEM, FBFM, MFRI, SLP, WAF, WS, WD, MEQ, M1, M10, M100, MLH, MLW, FLAME_LENGTH, FLIN, VELOCITY_DMS, VS0, HPUA, IR, &
                     RH, TMP, FFWI, MFFWI, UWIND, VWIND, TARR, VELANGLE, &
                     PIGN, PIGN_AVG, LBF, LBF_AVG, PIGN_LBF, PIGN_LBF_AVG, PIGN_WS, PIGN_WS_AVG, PCT_WS_ABOVE_THRESHOLD, &
                     PIGN_PCT_WS_ABOVE_THRESHOLD, WS_AVG, TMP_AVG, FFWI_AVG, RH_AVG, MEQ_AVG, SPFH, TMPF, PRES

REAL, ALLOCATABLE, DIMENSION(:,:) :: WSMF, M1_NOW, M10_NOW, M100_NOW, PHIS, PHIW

CHARACTER(400) :: INPUT_DIRECTORY, OUTPUT_DIRECTORY, FUELS_AND_TOPOGRAPHY_DIRECTORY, WEATHER_DIRECTORY, FIRE_DYNAMICS_DIRECTORY

LOGICAL :: CONVERT_TO_GEOTIFF, COMPRESS

! Map generator variables:
INTEGER :: NUMBER_OF_COMPONENTS
INTEGER, DIMENSION(100) :: NUMBER_OF_CATEGORIES_FOR_COMPONENT
REAL, DIMENSION(100,100) :: CATEGORY_THRESHOLDS_FOR_COMPONENT,CATEGORY_SCORES_FOR_COMPONENT
REAL, DIMENSION(100) :: WEIGHTING_FACTOR_FOR_COMPONENT,THRESHOLDS_FOR_FINAL_HAZARD_RANKING
CHARACTER(60) :: DISCRETE_OR_CONTINUOUS, ADDITIVE_OR_MULTIPLICATIVE
CHARACTER(400), DIMENSION(0:100) :: FILENAME_OF_COMPONENT, INPUT_DIRECTORY_FOR_COMPONENT, OUTPUT_FILENAME_OF_CATEGORIZED_COMPONENT, &
                                    FNINPUTCOMP, FNOUTPUTCOMP, COLOR_TABLE_FOR_COMPONENT
CHARACTER(400) :: OUTPUT_FILENAME_OF_FINAL_HAZARD_SCALE, COLOR_TABLE_FOR_FINAL_HAZARD_SCALE

TYPE(RASTER_TYPE) :: INPUTCOMP(0:100), OUTPUTCOMP(0:100)

INTEGER :: NUMBER_OF_CATEGORIES_FOR_FINAL_HAZARD_SCALE
REAL, DIMENSION(1:100) :: CATEGORY_THRESHOLDS_FOR_FINAL_HAZARD_SCALE, CATEGORY_SCORES_FOR_FINAL_HAZARD_SCALE
LOGICAL :: USE_3BY3_SCORING
REAL, DIMENSION(1:3, 1:3, 1:3) :: THREE_BY_THREE_SCORING

! Fire dynamics variables:
REAL :: M1_MINUS_MEQ, M10_MINUS_MEQ, M100_MINUS_MEQ, LH_MOISTURE_CONTENT, LW_MOISTURE_CONTENT
REAL, DIMENSION(1:8) :: THETA, SINTHETA, COSTHETA
REAL, DIMENSION(1:8,1:2) :: DIST
INTEGER, DIMENSION(1:8) :: COLOFFSET, ROWOFFSET

LOGICAL, ALLOCATABLE, DIMENSION(:,:) :: PMASK, QMASK

! Units:
LOGICAL :: WS_IN_METERS_PER_SECOND, TMP_IN_CELSIUS

! Variables for RASTER_PERCENTILES:
INTEGER, PARAMETER :: NMAX = 100
INTEGER :: NPERCENTILES
REAL, DIMENSION(1:NMAX) :: PERCENTILES
CHARACTER(400) :: PERCENTILE_INPUT_FILENAME, PERCENTILE_OUTPUT_FILENAME(1:NMAX)

! Variables for FOCAL_STATS
CHARACTER(400) :: FOCAL_STATS_INPUT_FILENAME, FOCAL_STATS_OUTPUT_FILENAME

! Ignition axis:
INTEGER :: NBANDS_FOR_IGNITION_AXIS
REAL :: WS_THRESHOLD, ADD_TO_MEQ
LOGICAL :: NORMALIZE_PIGN

! Misc Fosberg
REAL :: LOWPASS_TEMP

! WAF optimization:
REAL, DIMENSION(0:20,0:50) :: SHELTERED_WAF_TABLE

! Optimization:
REAL, DIMENSION(0:90) :: TANSLP2=0., COSSLP=0
!REAL, DIMENSION(-1:360) :: ABSSINASP=0., ABSCOSASP=0., SINASPM180=0., COSASPM180=0.

! Misc
LOGICAL :: CBD_TIMES_100, CBH_TIMES_10, CC_IN_PERCENT, CH_TIMES_10

! Moving average
INTEGER :: NBANDS_TO_AVERAGE
CHARACTER(400) :: INPUT_RASTER_FILENAME, OUTPUT_RASTER_FILENAME
TYPE(RASTER_TYPE) :: INPUT_RASTER, OUTPUT_RASTER

! HDW 
CHARACTER(400) :: SH_FILENAME, P0_FILENAME, VPD_FILENAME, HDW_FILENAME
CHARACTER(1) :: TMP_UNITS
TYPE(RASTER_TYPE) :: SH, P0, HDW, VPD

! KBDI
INTEGER :: NUM_INTERVALS_PER_DAY
TYPE(RASTER_TYPE) :: KBDI, KBDI0, FAF, PRECIP, PRECIP_AVG
CHARACTER(400) :: PRECIP_FILENAME, PRECIP_AVG_FILENAME, KBDI0_FILENAME, KBDI_FILENAME, FAF_FILENAME
REAL :: PRECIP_MULT

! Wind speed and direction
LOGICAL :: ROTATE_WINDS

! *****************************************************************************
END MODULE VARS
! *****************************************************************************
