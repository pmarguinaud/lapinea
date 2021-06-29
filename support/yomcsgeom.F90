MODULE YOMCSGEOM

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    * Computational sphere horizontal geometry

!     RCOLON(NGPTOT) cosine of longitude on transformed sphere
!     RSILON(NGPTOT)   sine        "             "         "
!     RINDX (NGPTOT) Longitude index
!     RINDY (NGPTOT) Latitude index
!     RATATH(NGPTOT) RA*TAN(THETA) on real sphere
!     RATATX(NGPTOT) Curvature term for LAM (for u eq.)

TYPE TCSGEOM
  REAL(KIND=JPRB), POINTER :: RCOLON(:) => NULL()
  REAL(KIND=JPRB), POINTER :: RSILON(:) => NULL()
  REAL(KIND=JPRB), POINTER :: RINDX (:) => NULL()
  REAL(KIND=JPRB), POINTER :: RINDY (:) => NULL()
  REAL(KIND=JPRB), POINTER :: RATATH(:) => NULL()
  REAL(KIND=JPRB), POINTER :: RATATX(:) => NULL()
END TYPE TCSGEOM

! define blocked and non-blocked (_NB) structures
! note that the blocked structure YRCSGEOM will be initialised to point into 
! the non-blocked structure YRCSGEOM_NB
!!TYPE(TCSGEOM), POINTER :: YRCSGEOM(:) => NULL()
!!TYPE(TCSGEOM), POINTER :: YRCSGEOM_NB => NULL()

! ------------------------------------------------------------------
END MODULE YOMCSGEOM
