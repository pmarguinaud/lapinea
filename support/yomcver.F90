MODULE YOMCVER


#include "create.h"

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMLUN   , ONLY : NULOUT   ,NULNAM   
USE YOMCT0   , ONLY : LR2D     ,LECMWF   ,LNHDYN   ,LSLAG

IMPLICIT NONE

SAVE

! =============================================================================

! * Variables related to vertical discretisation in finite elements:

! NVSCH         : type of basis if the finite element vertical discretisation is used.
!               (1=>linear functions, 3=>Hermite cubic functions)
! NVFE_TYPE     : Type of spline basis used for finite element vertical discretisation.
!               (1 = linear, 3 = cubic)
! NVFE_ORDER    : Order of spline used in VFE; NVFE_ORDER=NVFE_TYPE+1
! NVFE_INTBC/DERBC : Boundary conditions used for integrals/derivatives
!               0 - no bc applied (RINTE/RDERI/RDDERI used)
!               1 - old LVFE_INTB/DERIB (as in cy45)
!               2 - implicit definition through basis
!               3 - explicit definition
! NVFE_INTERNALS: number of internals knots
! NVFE_BC       : integer that determines the way the boundary knots are defined
!               0 - full levels used
!               1 - regular distribution between last internal knot and boundary

! LVERTFE       : .T./.F. Finite element/conventional vertical discretisation.
! LVFE_LAPL     : VFE for vertical Laplacian term (NH model)
! LVFE_LAPL_BC  : VFE for boundary cond. in vert. Laplacian term (NH model)
! LVFE_LAPL_TBC/BBC: VFE for top/bottom boundary cond. in vert. Laplacian term (NH model)
!               : if inner domain is purely in VFE manner
! LVFE_LAPL2PI  : simpler formula for vertical Laplacian in VFE
! RLAPL2PI      : parameter used for simpler Laplacian in VFE
! LVFE_X_TERM   : VFE X-term (NH model)
! LVFE_Z_TERM   : VFE Z-term (w on full levels in NH model)
! LVFE_GW       : VFE for vertical velocity (NH model); in this case
!                 vertical velocity is at full levels.
! LVFE_DELNHPRE : VFE to compute [Delta pre] at full levels.
! LVFE_GWMPA    : VFE for AROME physics vertical velocity
!                 (NH model with AROME physics)
! LVFE_CENTRI   : Centripetal method for full levels eta_vfe calculation.
! RVFE_CENTRI   : Exponent in function computing eta_vfe if LVFE_CENTRI=T
! LVFE_APPROX   : Approximation (or interpolation) used to represent a function.
! LVFE_ECMWF    : T if original ECMWF way to compute vertical integral and derivative
! LVFE_LAPL_HALF: Vertical Laplacian uses derivative operators full->half->full
! LVFE_FIX_ORDER: T/F - VFE operators defined with fixed order splines/fixed knot sequence
!                 (BCs are included by changing order of splines)
! LVFE_GW_HALF  : T - GW on HALF levels under key LGWADV 
! LVFE_MAXIMAS  : T/F - full levels at maximas of spline basis functions/full
!                 levels - Greville abscissa - Variation diminishing approach
! LVFE_VERBOSE  : print several diagnostics or not
! RMINDETA      : minimum distance between knots; for smaller intervals knots
!                 are considered to be equal (multiple knots)

! ----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: NVSCH
create (NVSCH)
INTEGER(KIND=JPIM) :: NVFE_TYPE
create (NVFE_TYPE)
INTEGER(KIND=JPIM) :: NVFE_ORDER
create (NVFE_ORDER)
INTEGER(KIND=JPIM) :: NVFE_INTBC
create (NVFE_INTBC)
INTEGER(KIND=JPIM) :: NVFE_DERBC
create (NVFE_DERBC)
INTEGER(KIND=JPIM) :: NVFE_INTERNALS
create (NVFE_INTERNALS)
INTEGER(KIND=JPIM) :: NVFE_BC
create (NVFE_BC)
LOGICAL :: LVERTFE
create (LVERTFE)
LOGICAL :: LVFE_LAPL
create (LVFE_LAPL)
LOGICAL :: LVFE_LAPL_BC
create (LVFE_LAPL_BC)
LOGICAL :: LVFE_LAPL_TBC
create (LVFE_LAPL_TBC)
LOGICAL :: LVFE_LAPL_BBC
create (LVFE_LAPL_BBC)
LOGICAL :: LVFE_LAPL2PI
create (LVFE_LAPL2PI)
REAL(KIND=JPRB) :: RLAPL2PI = 0._JPRB
create (RLAPL2PI)
LOGICAL :: LVFE_X_TERM
create (LVFE_X_TERM)
LOGICAL :: LVFE_Z_TERM
create (LVFE_Z_TERM)
LOGICAL :: LVFE_GW
create (LVFE_GW)
LOGICAL :: LVFE_DELNHPRE
create (LVFE_DELNHPRE)
LOGICAL :: LVFE_GWMPA
create (LVFE_GWMPA)
LOGICAL :: LVFE_CENTRI
create (LVFE_CENTRI)
REAL(KIND=JPRB) :: RVFE_CENTRI
create (RVFE_CENTRI)
LOGICAL :: LVFE_APPROX
create (LVFE_APPROX)
LOGICAL :: LVFE_ECMWF
create (LVFE_ECMWF)
LOGICAL :: LVFE_LAPL_HALF
create (LVFE_LAPL_HALF)
LOGICAL :: LVFE_FIX_ORDER
create (LVFE_FIX_ORDER)
LOGICAL :: LVFE_GW_HALF
create (LVFE_GW_HALF)
LOGICAL :: LVFE_MAXIMAS
create (LVFE_MAXIMAS)
LOGICAL :: LVFE_VERBOSE
create (LVFE_VERBOSE)
REAL(KIND=JPRB) :: RMINDETA

create (RMINDETA)
! =============================================================================

NAMELIST/NAMCVER/NVSCH,NVFE_TYPE,NVFE_INTBC,NVFE_DERBC,NVFE_BC, &
      & LVERTFE,LVFE_LAPL,LVFE_LAPL_BC,LVFE_LAPL_TBC,LVFE_LAPL_BBC, &
      & LVFE_LAPL2PI,RLAPL2PI,LVFE_X_TERM,LVFE_Z_TERM,LVFE_GW, &
      & LVFE_DELNHPRE,LVFE_GWMPA,LVFE_CENTRI,RVFE_CENTRI,LVFE_APPROX, &
      & LVFE_ECMWF,LVFE_LAPL_HALF,LVFE_FIX_ORDER,LVFE_GW_HALF, &
      & LVFE_MAXIMAS,LVFE_VERBOSE,RMINDETA

! =============================================================================


END MODULE YOMCVER
