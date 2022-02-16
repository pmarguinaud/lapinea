MODULE YEMLBC_GEO

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Defining specific geometry variables for LAM model: those linked with LBC and I+E zones.
!     These variables are set-up in SUEGEOLBC and should not be modified elsewhere.

!     ------------------------------------------------------------------

TYPE :: TELBC_GEO

  ! NEDLST         : Nb. of points in E+I area (DM-local var.)
  ! NIND_LIST,NIND_LEN: help arrays for memory transfers between
  !  (NPROMA,NGPBLKS)-dimensioned arrays and NEDLST-dimensioned arrays.

  INTEGER(KIND=JPIM) :: NEDLST
  INTEGER(KIND=JPIM), ALLOCATABLE :: NIND_LIST(:,:)
  INTEGER(KIND=JPIM), ALLOCATABLE :: NIND_LEN(:)

END TYPE TELBC_GEO

!TYPE(TELBC_GEO), POINTER :: YRGEOLBC => NULL()   ! moved to type_geometry.F90

!     ------------------------------------------------------------------
END MODULE YEMLBC_GEO
