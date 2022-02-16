MODULE YOMLUN


#include "create.h"

USE PARKIND1  ,ONLY : JPIM
USE YOMLUN_IFSAUX, ONLY : NULOUT, NULERR

IMPLICIT NONE

SAVE

PRIVATE
PUBLIC :: NULERR, NULSTAT, NULNAM, NSCRTCH, NULFP10, NULCL1, NULFP11, &
        & NULCL2, NULFP09, NULFP08, NULFP07, NULASE, NULASS, &
        & NULFP06, NULFP05, NULOUT, NULRAD, NULRTL, NULFP04, &
        & NULFP12, NULFP13, NULFP14, NULFP15, NSCATAB, NSCASIG, &
        & NSCASPE, NEGASH, NULFP03, NULDILA, NULCONT, NULFP02, NPOSSH, &
        & NPODDH, NULFP01, NULCO, NTIDE, NTRJSH, NULTRAJHR, &
        & NEFLSS, NULFPOS, NEFLS, NINMSH, NINISH, &
        & NINIGG, NFGISH, NULTRAJBG, NBIAS, NPPPSH, NPDIRL, NULHWF, &
        & NULRCF, NULUSR1, NULUSR2, NULUSR3, NULUSR4, NULUSR5, &
        & NULBDESH, NULBDEGP, &
        & NPRT0,  NPRT1,  NPRT2,  NPRT3,  NPRT4,&
        & NUIO_SERV_LOG

!     ------------------------------------------------------------------

!*    Logical units used by code

! NB: Please use the RESERVE_LUN/FREE_LUN mechanism where possible.

!     NULNAM :   unit number for namelist
!     NULCL1 :   unit number for climatological fields (month before)
!     NULCL2 :   unit number for climatological fields (month after)
!     NTRJSH :   unit number for trajectory spectral data          WRTRA
!     NINMSH :   unit number for initial point of the minimization SUVAZX
!     NINISH :   unit number for initial spectral data             SUSPEC
!     NINIGG :   unit number for initial grid-point data           SUSPEC
!     NFGISH :   unit number for first-guess spectral data
!     NPOSSH :   output unit number (spectral fields)              CPREP1
!     NTIDE  :   unit number for the LFI file containing the total tendencies
!     NPDIRL :   unit number for post-processing directory listing
!     NPPPSH :   unit number for post-processed spherical harmonics WRPLPP

!     NULDILA:   unit number for dilatation matrix (SUDIL,DILAT,SPDILA)
!     NULCONT:   unit number for contraction matrix (SUDIL,DILAT,SPDILA)

!     NULCO  :   unit number for coupled fields (ICMCO)
!     NPODDH :   unit number for mask diagnostic files (DDH)
!     NULRCF :   unit number for restart control file
!     NULHWF :   unit number for history witness file
!     NBIAS  :   unit number for bias (dig. filt. guess - guess)

!     NEFLS  :   unit number for coupling ALADIN file
!     NEFLSS :   unit number for coupling ALADIN file (initialisation)

!     NULUSR1:   unit numbers for user defined files
!     NULSTAT:   unit number for status  file

!     NULASE :   unit number for CANARI statistics (forecast error s.d.)
!     NULASS :   unit number for CANARI statistics (analysis error s.d.)

!     NULUSR2
!     NULUSR3
!     NULUSR4
!     NULUSR5

!     NULFPxx    unit numbers for Full-POS output files
!     NSCRTCH:   unit number for Full-POS scratch file (for in-line post-proc.)
!     NULFPOS:   unit number for Full-POS control file (end of post-processing
!                in conf. 001 ; auxilary namelist file in conf. 927)
!     NULRAD :   unit number for writing radiation diagnostics
!     NULRTL :   unit number for reading RTLIMB coefficient files
!     NSCATAB    SCAT. SIGMA0 TABLE
!     NSCASIG    SCAT. SIGMA0 BIAS CORRECTION
!     NSCASPE    SCAT. SPEED BIAS CORRECTION
!     NEGASH     UNIT NUMBER FOR JK INPUT

!     NULTRAJHR: unit number for high resolution trajectory (option LTRAJHR)
!     NULTRAJBG: unit number for background (option LBACKGR)
!     NUIO_SERV_LOG: unit number of IO server log

! NB:
!   NULERR is initialised to zero in YOMLUN_IFSAUX
!   NULOUT is initialised to 6 in YOMLUN_IFSAUX, and is reset to 20 in SULUN.
!   NULDRHACK is initialised to 999 in YOMLUN_IFSAUX (see dr_hook_util.F90 for more infos)

! JP_RESERVE_FIRST = Lowest unit number that can be reserved by RESERVE_LUN
! JP_RESERVE_LAST  = Highest unit number that can be reserved by RESERVE_LUN

! NULBDESH  = offset for boundary pert spectral space
! NULBDEGP  = offset for boundary pert grid-point space


! Modifications:
!   2011-03-04 M. Fisher   : Unit numbers are now parameters.
!                            Added RESERVE_LUN and FREE_LUN.
!     ------------------------------------------------------------------

! --- Reserved for NULERR ----------------  0
INTEGER(KIND=JPIM), PARAMETER :: NULSTAT =  1
create (NULSTAT)
!                                           2
!                                           3
INTEGER(KIND=JPIM), PARAMETER :: NULNAM  =  4
create (NULNAM)
!                                           5
! --- Reserved by Fortran                   6
INTEGER(KIND=JPIM), PARAMETER :: NULFP11 =  7
create (NULFP11)
INTEGER(KIND=JPIM), PARAMETER :: NSCRTCH =  8
create (NSCRTCH)
INTEGER(KIND=JPIM), PARAMETER :: NULFP10 =  9
create (NULFP10)
INTEGER(KIND=JPIM), PARAMETER :: NULCL1  = 10
create (NULCL1)
INTEGER(KIND=JPIM), PARAMETER :: NULCL2  = 11
create (NULCL2)
INTEGER(KIND=JPIM), PARAMETER :: NULFP09 = 12
create (NULFP09)
INTEGER(KIND=JPIM), PARAMETER :: NULFP08 = 13
create (NULFP08)
INTEGER(KIND=JPIM), PARAMETER :: NULFP07 = 14
create (NULFP07)
INTEGER(KIND=JPIM), PARAMETER :: NULASE  = 16
create (NULASE)
INTEGER(KIND=JPIM), PARAMETER :: NULASS  = 17
create (NULASS)
INTEGER(KIND=JPIM), PARAMETER :: NULFP06 = 18
create (NULFP06)
INTEGER(KIND=JPIM), PARAMETER :: NULFP05 = 19
create (NULFP05)
! --- Reserved for NULOUT ---------------- 20
INTEGER(KIND=JPIM), PARAMETER :: NULRAD  = 25
create (NULRAD)
INTEGER(KIND=JPIM), PARAMETER :: NULRTL  = 26
create (NULRTL)
INTEGER(KIND=JPIM), PARAMETER :: NULFP04 = 30
create (NULFP04)
!                                          31
INTEGER(KIND=JPIM), PARAMETER :: NULFP12 = 32
create (NULFP12)
INTEGER(KIND=JPIM), PARAMETER :: NULFP13 = 33
create (NULFP13)
INTEGER(KIND=JPIM), PARAMETER :: NULFP14 = 34
create (NULFP14)
INTEGER(KIND=JPIM), PARAMETER :: NULFP15 = 35
create (NULFP15)
INTEGER(KIND=JPIM), PARAMETER :: NSCATAB = 36
create (NSCATAB)
INTEGER(KIND=JPIM), PARAMETER :: NSCASIG = 37
create (NSCASIG)
INTEGER(KIND=JPIM), PARAMETER :: NSCASPE = 38
create (NSCASPE)
INTEGER(KIND=JPIM), PARAMETER :: NEGASH  = 39
create (NEGASH)
!                                          40
!                                          41
!                                          42
INTEGER(KIND=JPIM), PARAMETER :: NULFP03 = 43
create (NULFP03)
!                                          44
!                                          45
INTEGER(KIND=JPIM), PARAMETER :: NULDILA = 46
create (NULDILA)
INTEGER(KIND=JPIM), PARAMETER :: NULCONT = 47
create (NULCONT)
!                                          48
!                                          49
INTEGER(KIND=JPIM), PARAMETER :: NULFP02 = 50
create (NULFP02)
INTEGER(KIND=JPIM), PARAMETER :: NPOSSH  = 51
create (NPOSSH)
!                                          52
INTEGER(KIND=JPIM), PARAMETER :: NPODDH  = 53
create (NPODDH)
INTEGER(KIND=JPIM), PARAMETER :: NULFP01 = 54
create (NULFP01)
INTEGER(KIND=JPIM), PARAMETER :: NULCO   = 55
create (NULCO)
INTEGER(KIND=JPIM), PARAMETER :: NTIDE   = 56
create (NTIDE)
! --- Reserved for RESERVE_LUN ----------- 57 
! --- Reserved for RESERVE_LUN ----------- 58
! --- Reserved for RESERVE_LUN ----------- 59
! --- Reserved for RESERVE_LUN ----------- 60
! --- Reserved for RESERVE_LUN ----------- 61
! --- Reserved for RESERVE_LUN ----------- 62
! --- Reserved for RESERVE_LUN ----------- 63
! --- Reserved for RESERVE_LUN ----------- 64
! --- Reserved for RESERVE_LUN ----------- 65
! --- Reserved for RESERVE_LUN ----------- 66
INTEGER(KIND=JPIM), PARAMETER :: NUIO_SERV_LOG = 67
create (NUIO_SERV_LOG)
INTEGER(KIND=JPIM), PARAMETER :: NTRJSH  = 71

create (NTRJSH)
! NB: NULTRAJHR is not a parameter because read_surfgrid_traj_fromfa resets it.
INTEGER(KIND=JPIM)            :: NULTRAJHR = 72

create (NULTRAJHR)
! NB: NEFLSS is not a parameter because ELSAC (in ALD) resets it.
INTEGER(KIND=JPIM)            :: NEFLSS  = 73

create (NEFLSS)
INTEGER(KIND=JPIM), PARAMETER :: NULFPOS = 74
create (NULFPOS)
!                                          75
!                                          76
!                                          77
INTEGER(KIND=JPIM), PARAMETER :: NEFLS   = 78
create (NEFLS)
INTEGER(KIND=JPIM), PARAMETER :: NINMSH  = 79
create (NINMSH)
!                                          80
INTEGER(KIND=JPIM), PARAMETER :: NINISH  = 81
create (NINISH)
INTEGER(KIND=JPIM), PARAMETER :: NINIGG  = 82
create (NINIGG)
INTEGER(KIND=JPIM), PARAMETER :: NFGISH  = 83
create (NFGISH)
INTEGER(KIND=JPIM), PARAMETER :: NULTRAJBG = NFGISH ! -- Dangerous!!
create (NULTRAJBG)
INTEGER(KIND=JPIM), PARAMETER :: NPRT0   = 85
create (NPRT0)
INTEGER(KIND=JPIM), PARAMETER :: NPRT1   = 86
create (NPRT1)
INTEGER(KIND=JPIM), PARAMETER :: NPRT2   = 87
create (NPRT2)
INTEGER(KIND=JPIM), PARAMETER :: NPRT3   = 88
create (NPRT3)
INTEGER(KIND=JPIM), PARAMETER :: NPRT4   = 89
create (NPRT4)
INTEGER(KIND=JPIM), PARAMETER :: NBIAS   = 90
create (NBIAS)
INTEGER(KIND=JPIM), PARAMETER :: NPPPSH  = 91
create (NPPPSH)
INTEGER(KIND=JPIM), PARAMETER :: NPDIRL  = 92
create (NPDIRL)
INTEGER(KIND=JPIM), PARAMETER :: NULHWF  = 93
create (NULHWF)
INTEGER(KIND=JPIM), PARAMETER :: NULRCF  = 94
create (NULRCF)
INTEGER(KIND=JPIM), PARAMETER :: NULUSR1 = 95
create (NULUSR1)
INTEGER(KIND=JPIM), PARAMETER :: NULUSR2 = 96
create (NULUSR2)
INTEGER(KIND=JPIM), PARAMETER :: NULUSR3 = 97
create (NULUSR3)
INTEGER(KIND=JPIM), PARAMETER :: NULUSR4 = 98
create (NULUSR4)
INTEGER(KIND=JPIM), PARAMETER :: NULUSR5 = 99
create (NULUSR5)
INTEGER(KIND=JPIM), PARAMETER :: NULBDEGP = 100
create (NULBDEGP)
INTEGER(KIND=JPIM), PARAMETER :: NULBDESH = 200
create (NULBDESH)
! --- Reserved for NULDRHACK ----------- 999 defined in yomlun_ifsaux.F90
INTEGER(KIND=JPIM), PARAMETER :: JP_RESERVE_FIRST=57
create (JP_RESERVE_FIRST)
INTEGER(KIND=JPIM), PARAMETER :: JP_RESERVE_LAST =70

create (JP_RESERVE_LAST)
INTEGER(KIND=JPIM) :: I
create (I)
LOGICAL :: LRESERVED(JP_RESERVE_FIRST:JP_RESERVE_LAST) = &
        & (/ (.FALSE.,I=JP_RESERVE_FIRST,JP_RESERVE_LAST) /)
create (LRESERVED)
!     ------------------------------------------------------------------

END MODULE YOMLUN

