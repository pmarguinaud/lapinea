MODULE YOMGSGEOM

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Grid point horizontal geometry: structure TGSGEOM

! RCORI      : Coriolis parameter "f = 2 Omega sin(theta)".
! RCORIC     : 2 Omega cos(theta).
! GEMU       : sine of geographic latitude "sin(theta)".
! GSQM2      : cosine of geographic latitude "cos(theta)".
! GELAM      : geographic longitude "lambda".
! GELAT      : geographic latitude "theta".
! GECLO      : cosine of geographic longitude "cos(lambda)".
! GESLO      : sine of geographic longitude "sin(lambda)".
! GM         : mapping factor "M".
! GMAPPA     : approximation of M**2 or RSTRET**2 according to model geometry.
! GOMVRL     : zonal component of vector "2 vec(Omega) wedge a vec(k)".
! GOMVRM     : meridian component of vector "2 vec(Omega) wedge a vec(k)".
! GNORDL     : zonal component "gnordl" of unit vector directed towards the geographic Northern pole.
! GNORDM     : meridian component "gnordm" of unit vector directed towards the geographic Northern pole.
! GNORDLCL   : zonal component of vector "grad(gnordl)".
! GNORDMCL   : zonal component of vector "grad(gnordm)".
! GNORDMCM   : meridian component of vector "grad(gnordm)".
! GAW        : Gaussian weight.
! NGPLAT     : DM-global number of the Gaussian grid latitude.
! NUNIQUEGP  : pointer array (see computation in sugem2.F90).

TYPE TGSGEOM
  REAL(KIND=JPRB),    POINTER :: RCORI(:)     => NULL()
  REAL(KIND=JPRB),    POINTER :: RCORIC(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GEMU(:)      => NULL()
  REAL(KIND=JPRB),    POINTER :: GSQM2(:)     => NULL()
  REAL(KIND=JPRB),    POINTER :: GELAM(:)     => NULL()
  REAL(KIND=JPRB),    POINTER :: GELAT(:)     => NULL()
  REAL(KIND=JPRB),    POINTER :: GECLO(:)     => NULL()
  REAL(KIND=JPRB),    POINTER :: GESLO(:)     => NULL()
  REAL(KIND=JPRB),    POINTER :: GM(:)        => NULL()
  REAL(KIND=JPRB),    POINTER :: GMAPPA(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GOMVRL(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GOMVRM(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GNORDL(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GNORDM(:)    => NULL()
  REAL(KIND=JPRB),    POINTER :: GNORDLCL(:)  => NULL()
  REAL(KIND=JPRB),    POINTER :: GNORDMCL(:)  => NULL()
  REAL(KIND=JPRB),    POINTER :: GNORDMCM(:)  => NULL()
  REAL(KIND=JPRB),    POINTER :: GAW(:)       => NULL()
  INTEGER(KIND=JPIM), POINTER :: NGPLAT(:)    => NULL()
  INTEGER(KIND=JPIM), POINTER :: NUNIQUEGP(:) => NULL()
END TYPE TGSGEOM
  
! define blocked and non-blocked (_NB) structures
! note that the blocked structure YRGSGEOM will be initialised to point into 
! the non-blocked structure YRGSGEOM_NB
!!TYPE(TGSGEOM), POINTER :: YRGSGEOM(:)  => NULL()
!!TYPE(TGSGEOM), POINTER :: YRGSGEOM_NB  => NULL()

! ------------------------------------------------------------------
END MODULE YOMGSGEOM
