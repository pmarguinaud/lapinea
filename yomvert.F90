MODULE YOMVERT

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMCT0   , ONLY : LNHDYN
USE YOMCVER  , ONLY : LVERTFE, LVFE_ECMWF, LVFE_GW, LVFE_GW_HALF, LVFE_LAPL, &
 &                    LVFE_LAPL_HALF, LVFE_DELNHPRE, NVFE_TYPE, &
 &                    NVFE_DERBC, NVFE_INTBC
USE YOMDYNA  , ONLY : LGWADV

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    * DEFINING THE VERTICAL COORDINATE: A and B

!     VP00  : REFERENCE PRESSURE FOR DEFINING VERTICAL COORDINATE
!     VALH  : (0:NFLEVG)
!     VBH   : (0:NFLEVG) : B of the vertical coordinate
!     VAH   : (0:NFLEVG) ;  =VALH*VP00
!     VC    : (NFLEVG)   ;  =VAH(J)*VBH(J-1)-VAH(J-1)*VBH(J)
!     VDELB : (NFLEVG)   ;  =VBH(J)-VBH(J-1)
!     VDELA : (NFLEVG)   ;  =VAH(J)-VAH(J-1)
!     VAF   : like VAH but at full levels.
!     VBF   : like VBH but at full levels.
!     TOPPRES: REFERENCE "EVANESCENT" PRESSURE
!              TOPPRES allows to solve some calculations of singularities
!              when the top pressure of the model is zero (for ex. in
!              GPPREF, GPXYB, SUNHBMAT).

!     WE HAVE THEN FOR THE HALF LEVEL PRESSURE : VAH + VBH*(SURFACE PRESSURE)


!! scalar reference values moved out from type TVAB
REAL(KIND=JPRB)         :: VP00
REAL(KIND=JPRB)         :: TOPPRES

TYPE TVAB
  REAL(KIND=JPRB), ALLOCATABLE :: VALH(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VBH(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VAH(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VC(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VAF(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VBF(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VDELA(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VDELB(:)
END TYPE TVAB

!     ------------------------------------------------------------------

!*    * DEFINING THE VERTICAL COORDINATE: eta

!     VETAH : (0:NFLEVG) ; VERTICAL COORDINATE = VALH+VBH
!     VFE_ETAH : version of VETAH used for VFE operators.
!     VETAF : (0:NFLEVG+1) ; VERTICAL COORDINATE ON LAYERS.
!     VFE_ETAF : version of VETAF used for VFE operators.
!     VFE_RDETAH: VFE_RDETAH(jlev)=1/(VFE_ETAH(jlev)-VFE_ETAH(jlev-1))

TYPE TVETA
  REAL(KIND=JPRB), ALLOCATABLE :: VETAH(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VFE_ETAH(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VETAF(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VFE_ETAF(:)
  REAL(KIND=JPRB), ALLOCATABLE :: VFE_RDETAH(:)
END TYPE TVETA

! -----------------------------------------------------------------------------

! * Finite element vertical discretisation 
! ----------------------------------------
! VFE_KNOT       : internal knots

! * Matricial operators related to vertical discretisation in finite elements
!   (to compute integrals or derivatives):
! -----------------------------------------------------------------------------
! * Integral operators on full levels
! RINTE    : without boundary conditions defined
! RINTBF00 : with explicitely given top and bottom BC;
!            input boundary conditions: X_0=0, X_{L+1}=0
!            output boundary conditions: KX_0=0
! RINTBF11 : with explicitely given top and bottom BC;
!            input boundary conditions: (dX/deta)_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: KX_0=0

! * First derivative operators on full levels
! RDERI    : without boundary conditions defined
! RDERB    = RDERBF00
! RDERBF00 : with explicitely given top and bottom BC;
!            input boundary conditions: X_0=0, X_{L+1}=X_L
!            output boundary conditions: none
! RDERBF01 : with explicitely given top and bottom BC;
!            input boundary conditions: X_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: DX_{L+1}=0
! RDERBF10 : with explicitely given top and bottom BC;
!            input boundary conditions: X_{L+1}=0, (dX/deta)_0=0
!            output boundary conditions: DX_0=0
! RDERBF11 : with explicitely given top and bottom BC;
!            input boundary conditions: (dX/deta)_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: DX_{L+1}=0

! * First derivative operators from full levels to half levels
! RDERBH00 : with explicitely given top and bottom BC;
!            input boundary conditions: X_0=0, X_L+1=X_L
!            output boundary conditions: none
! RDERBH01 : with explicitely given top and bottom BC;
!            input boundary conditions: X_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: none

! * Second derivative operators on full levels
! RDDERI   : without boundary conditions
! RDDERBF01: with explicitely given top and bottom BC;
!            input boundary conditions: X_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: DDX_{L+1}=0
! RDDERBF11: with explicitely given top and bottom BC;
!            input boundary conditions: X_0=0, (dX/deta)_{L+1}=0
!            output boundary conditions: DDX_0=0, DDX_{L+1}=0

! * Matricial operators with implicitly defined boundary conditions
!   analogicly to their explicit versions
! RINTBF11_IMPL
! RDERBF01_IMPL
! RDERBF10_IMPL
! RDERBF11_IMPL
! RDERBH01_IMPL
! RDDERBF11_IMPL

! * Invertible matricial operators for transformations gw<->d
! RDERGW   : derivative operator 
! RINTGW   : integral operator 

! -----------------------------------------------------------------------------

TYPE TVFE
  REAL(KIND=JPRB),ALLOCATABLE :: VFE_KNOT(:)
  REAL(KIND=JPRB),ALLOCATABLE :: RINTE(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RINTBF00 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RINTBF11 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERI(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERB(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBF00 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBF01 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBF10 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBF11 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBH00 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBH01 (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDDERI(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDDERBF01(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDDERBF11(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RINTBF11_IMPL (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBF01_IMPL (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBF10_IMPL (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBF11_IMPL (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERBH01_IMPL (:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDDERBF01_IMPL(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDDERBF10_IMPL(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDDERBF11_IMPL(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RINTGW(:,:)
  REAL(KIND=JPRB),ALLOCATABLE :: RDERGW(:,:)
END TYPE TVFE

TYPE TVERTICAL_GEOM
  TYPE(TVAB) :: YRVAB 
  TYPE(TVETA):: YRVETA
  TYPE(TVFE) :: YRVFE 
END TYPE TVERTICAL_GEOM

END MODULE YOMVERT
