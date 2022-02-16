#ifdef RS6K
@PROCESS NOCHECK
#endif
!option! -O nomove
!$acc routine (ELARMES) vector
SUBROUTINE ELARMES(YDGEOMETRY,YDML_DYN,YDRIP,KST,KPROF,YDSL,KSTABUF,PGMDTX,PGMDTY,PB1,PB2,&
 & PLSDEPI,KIBL,&
 & KVSEPC,KVSEPL,&
 & PSCO,PLEV,PCCO,PUF,PVF,&
 & KL0,KLH0,KLEV,PLSCAW,PRSCAW, YDSTACK)  


!     ------------------------------------------------------------------
!**** *ELARMES - semi-LAgrangian scheme:
!                Research of the origin point on the Sphere.

!     Purpose.
!     --------

!      The computation of the location of the interpolation point of
!     the lagrangian trajectory is performed by an iterative
!     method described by Robert and adapted to the sphere by M. Rochas.
!     Trajectories are great circles for spherical geometry.
!      In ELARMES the trajectories are straight lines on the plane.
!     Finally we find the departure (origin) point "O".

!**   Interface.
!     ----------
!        *CALL* *ELARMES(...)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST      - first element of arrays where computations are performed.
!          KPROF    - depth of work.
!          YDSL     - SL_STRUCT definition.
!          KSTABUF  - for a latitude IGL, KSTABUF(IGL) is the
!                     address of the element corresponding to
!                     (ILON=1,IGL) in the NPROMA arrays.
!          PGMDTX   - m * DELTA t / DELTA x.
!          PGMDTY   - m * DELTA t / DELTA y.
!          PB1      - SLBUF1-buffer for interpolations.
!          PB2      - SLBUF2-buf to communicate info from non lag. to lag. dyn.
!          PLSDEPI  - (Number of points by latitude) / (2 * PI) .
!          KIBL     - index into YRCSGEOM/YRGSGEOM instances in YDGEOMETRY

!        INPUT/OUTPUT:
!          KVSEPC   - vertical separation (used in S/L adjoint, cubic interp.)
!          KVSEPL   - vertical separation (used in S/L adjoint, linear interp.)

!        OUTPUT:
!          PSCO     - information about geographic position of interpol. point.
!          PLEV     - vertical coordinate of the interpolation point.
!          PCCO     - information about comput. space position of interpol. point.
!          PUF      - U-comp of wind necessary to
!                     find the position of the origin point,
!                     in a local repere linked to computational sphere.
!          PVF      - V-comp of wind necessary to
!                     find the position of the origin point,
!                     in a local repere linked to computational sphere.
!          KL0      - index of the four western points
!                     of the 16 points interpolation grid.
!          KLH0     - second value of index of the four western points
!                     of the 16 points interpolation grid if needed.
!          KLEV     - lower level of the vertical interpolation
!                     grid needed for vertical interpolations.
!          PLSCAW   - linear weights (distances) for interpolations.
!          PRSCAW   - non-linear weights for interpolations.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation about semi-Lagrangian scheme.

!     Externals.
!     ----------
!        Calls  LARCINA.
!        Is called by LAPINEA (3D model)

!     Reference.
!     ----------

!     Author.
!     -------
!        R. Bubnova after LARMES (by Karim Yessad) and developments
!        by Martin Janousek      CNRM/GMAP/EXT
!        Original : JANUARY 1995

!     Modifications.
!     --------------
!        K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!        K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!        K. Yessad (Nov 2009): keep LLO.OR.LELTRA=T only in SL2TL.
!        K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!        F. Vana      : 23-Feb-2011  LARCINA arguments update
!        G. Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!        G. Mozdzynski (Feb 2011): OOPS cleaning, use of derived types TGSGEOM and TCSGEOM
!        K. Yessad (Nov 2011): introduce LRALTVDISP; do same printings as in LARMES.
!        S. Malardel and D. Ricard (Nov 2013): COMAD weights for SL interpolations
!        B. Bochenek (Apr 2015): Phasing: move some variables.
!        O. Marsden: June 2015 CY42 YRGMV, YRGFL, YRSURF, YRGMV5, and YRGFL5 are now passed by argument
!        K. Yessad (Feb 2018): remove deep-layer formulations.
! End Modifications
!     ------------------------------------------------------------------

USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE
USE GEOMETRY_MOD , ONLY : GEOMETRY
USE PARKIND1  , ONLY : JPIM, JPRB

USE YOMCST    , ONLY : RPI
USE YOMCT0    , ONLY : NCONF, LTWOTL
USE YOMDYNA   , ONLY : LELTRA, LRALTVDISP, LRPRSLTRJ
USE YOMLUN    , ONLY : NULERR
USE YOMRIP    , ONLY : TRIP
USE EINT_MOD  , ONLY : SL_STRUCT
USE REDUCTION , ONLY : REDUCE, IREDUCTION_POW2, &
                     & IREDUCTION_MAX, IREDUCTION_SUM, &
                     & IREDUCTION_NONE
USE STACK_MOD
#include "stack.h"

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY), INTENT(IN)       :: YDGEOMETRY
TYPE(MODEL_DYNAMICS_TYPE),INTENT(IN):: YDML_DYN
TYPE(TRIP)     ,INTENT(IN)       :: YDRIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF
TYPE(SL_STRUCT),   INTENT(INOUT) :: YDSL
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTABUF(YDSL%NDGSAH:YDSL%NDGENH)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMDTX(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGMDTY(YDGEOMETRY%YRDIM%NPROMA)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PB1(YDSL%NASLB1,YDML_DYN%YRPTRSLB1%NFLDSLB1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PB2(YDGEOMETRY%YRDIM%NPROMA,YDML_DYN%YRPTRSLB2%NFLDSLB2)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSDEPI(YDSL%NDGSAH:YDSL%NDGENH)
INTEGER(KIND=JPIM),INTENT(IN)    :: KIBL
INTEGER(KIND=JPIM),INTENT(INOUT) :: KVSEPC
INTEGER(KIND=JPIM),INTENT(INOUT) :: KVSEPL
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTSCO%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTCCO%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PUF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVF(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KL0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KLH0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTLSCAW%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTRSCAW%NDIM)
TYPE(STACK) :: YDSTACK, YLSTACK

!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: IHVI
INTEGER(KIND=JPIM) :: ILEVEXP
INTEGER(KIND=JPIM) :: IROFEXP
INTEGER(KIND=JPIM) :: IROT

temp (INTEGER(KIND=JPIM), ISTESB, (YDGEOMETRY%YRDIM%NPROMA))
INTEGER(KIND=JPIM) :: ISTESB_TOT

temp (INTEGER(KIND=JPIM), ISTEST, (YDGEOMETRY%YRDIM%NPROMA))
INTEGER(KIND=JPIM) :: ISTEST_TOT

INTEGER(KIND=JPIM) :: ITIP
INTEGER(KIND=JPIM) :: JITER
INTEGER(KIND=JPIM) :: JLEV
INTEGER(KIND=JPIM) :: JROF

temp (INTEGER(KIND=JPIM), IL0A, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3))


temp (INTEGER(KIND=JPIM), IDEP, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
LOGICAL :: LLINTV
LOGICAL :: LLO
LOGICAL :: LLSLHD, LLSLHDQUAD

REAL(KIND=JPRB) :: ZDGUN, ZDGUX, ZDLUN, ZDLUX
REAL(KIND=JPRB) :: ZEW, ZEWX
REAL(KIND=JPRB) :: ZINDX, ZINDY, ZINEZ
temp (REAL(KIND=JPRB), ZINEZV, (YDGEOMETRY%YRDIM%NPROMA))
REAL(KIND=JPRB) :: ZLEVB, ZLEVO, ZLEVT
REAL(KIND=JPRB) :: ZNOR, ZNORX
REAL(KIND=JPRB) :: ZPU, ZPV
REAL(KIND=JPRB) :: ZTXO, ZTYO
REAL(KIND=JPRB) :: ZVMAX1, ZVMAX2
REAL(KIND=JPRB) :: ZEPS
REAL(KIND=JPRB) :: ZVETAON, ZVETAOX


temp (REAL(KIND=JPRB), ZWF, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZWFSM, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZWFASM, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZSCO, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTSCO%NDIM))


temp (REAL(KIND=JPRB), ZPLEV, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
! unused arguments in call to LARCINA:
! a) input arrays - will not be used since LSLHD was set to .FALSE.
!    in LAPINEA before call to LARMES/ELARMES => dimensions can be
!    contracted
temp (REAL(KIND=JPRB), ZPROLEVDUM, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

REAL(KIND=JPRB) :: ZVDISP_1,ZZ,ZVDISP_2,ZVDISP

temp (REAL(KIND=JPRB), ZWO_2, (YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG))
temp (REAL(KIND=JPRB), ZWF_2, (YDGEOMETRY%YRDIM%NPROMA, YDGEOMETRY%YRDIMV%NFLEVG))
REAL(KIND=JPRB) :: ZHVETAON,ZHVETAOX

!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "larcina.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS AND TESTS.
!              --------------------------------------

!*       1.1   Test that wind is not too strong.



YLSTACK=YDSTACK

alloc (ISTESB)
alloc (ISTEST)
alloc (IL0A)
alloc (IDEP)
alloc (ZINEZV)
alloc (ZWF)
alloc (ZWFSM)
alloc (ZWFASM)
alloc (ZSCO)
alloc (ZPLEV)
alloc (ZPROLEVDUM)
alloc (ZWO_2)
alloc (ZWF_2)
ZNOR=0.0_JPRB
ZEW=0.0_JPRB
ZDLUN=REAL(YDSL%NDLUNG,JPRB)
ZDLUX=REAL(YDSL%NDLUXG,JPRB)
ZDGUN=REAL(YDSL%NDGUNG,JPRB)
ZDGUX=REAL(YDSL%NDGUXG,JPRB)
ZVMAX1=YDML_DYN%YRDYN%VMAX1*YDML_DYN%YRDYN%VMAX1
ZVMAX2=YDML_DYN%YRDYN%VMAX2*YDML_DYN%YRDYN%VMAX2
ZEPS=1.E-6_JPRB
!$acc loop vector
DO JROF=KST,KPROF
  ZINEZV(JROF)=MAX(0.0_JPRB,SIGN(1.0_JPRB,(YDGEOMETRY%YRCSGEOM(KIBL)%RINDX(JROF)-ZDLUN)*&
   & (ZDLUX-YDGEOMETRY%YRCSGEOM(KIBL)%RINDX(JROF))))*&
   & MAX(0.0_JPRB,SIGN(1.0_JPRB,(YDGEOMETRY%YRCSGEOM(KIBL)%RINDY(JROF)-ZDGUN)*(ZDGUX-YDGEOMETRY%YRCSGEOM(KIBL)%RINDY(JROF))))  
ENDDO

!$acc loop vector
DO JROF = KST, KPROF
DO JLEV=1,YDGEOMETRY%YRDIMV%NFLEVG
  
    PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQX)=1.0_JPRB
    PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQY)=1.0_JPRB
  
ENDDO
ENDDO


CALL REDUCE (PB2(KST:KPROF,YDML_DYN%YRPTRSLB2%MSLB2VRL:YDML_DYN%YRPTRSLB2%MSLB2VRL+YDGEOMETRY%YRDIMV%NFLEVG-1), &
           & ZNOR, IREDUCTION_MAX, IREDUCTION_POW2)
CALL REDUCE (PB2(KST:KPROF,YDML_DYN%YRPTRSLB2%MSLB2URL:YDML_DYN%YRPTRSLB2%MSLB2URL+YDGEOMETRY%YRDIMV%NFLEVG-1), &
           & ZEW,  IREDUCTION_MAX, IREDUCTION_POW2)

IF (ZNOR > ZVMAX1) THEN
  WRITE(NULERR,*) ' MAX V WIND=',SQRT(ZNOR)
ENDIF
IF (ZEW > ZVMAX1) THEN
  WRITE(NULERR,*) ' MAX U WIND=',SQRT(ZEW)
ENDIF
IF (ZNOR > ZVMAX2) THEN
  ZNOR=0.0_JPRB
  !$acc loop vector
  DO JROF = KST, KPROF
  DO JLEV=1,YDGEOMETRY%YRDIMV%NFLEVG
    
      ZNORX=PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2VRL+JLEV-1)*PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2VRL+JLEV-1)
      ZNOR=MAX(ZNORX,ZNOR)
      IF(ZNOR == ZNORX) THEN
        ILEVEXP=JLEV
        IROFEXP=JROF
      ENDIF
    
  ENDDO
  ENDDO

  WRITE(NULERR,*) ' V WIND =',SQRT(ZNOR),' IS TOO STRONG, EXPLOSION.'
  WRITE(NULERR,*) ' LEVEL= ',ILEVEXP,' POINT= ',IROFEXP
  WRITE(NULERR,*) ' LON  = ',ACOS(YDGEOMETRY%YRCSGEOM(KIBL)%RCOLON(IROFEXP))*180._JPRB/RPI,' degrees'
  WRITE(NULERR,*) ' LAT  = ',ASIN(YDGEOMETRY%YRGSGEOM(KIBL)%GEMU(IROFEXP))*180._JPRB/RPI,' degrees'
  CALL ABOR1(' !V WIND TOO STRONG, EXPLOSION!!!')
ENDIF
IF (ZEW > ZVMAX2) THEN
  ZEW=0.0_JPRB
  !$acc loop vector
  DO JROF = KST, KPROF
  DO JLEV=1,YDGEOMETRY%YRDIMV%NFLEVG
    
      ZEWX=PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2URL+JLEV-1)*PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2URL+JLEV-1)
      ZEW=MAX(ZEWX,ZEW)
      IF(ZEW == ZEWX) THEN
        ILEVEXP=JLEV
        IROFEXP=JROF
      ENDIF
    
  ENDDO
  ENDDO

  WRITE(NULERR,*) ' U WIND =',SQRT(ZEW),' IS TOO STRONG, EXPLOSION.'
  WRITE(NULERR,*) ' LEVEL= ',ILEVEXP,' POINT= ',IROFEXP
  WRITE(NULERR,*) ' LON  = ',ACOS(YDGEOMETRY%YRCSGEOM(KIBL)%RCOLON(IROFEXP))*180._JPRB/RPI,' degrees'
  WRITE(NULERR,*) ' LAT  = ',ASIN(YDGEOMETRY%YRGSGEOM(KIBL)%GEMU(IROFEXP))*180._JPRB/RPI,' degrees'
  CALL ABOR1(' !U WIND TOO STRONG, EXPLOSION!!!')
ENDIF

!*       1.2   Miscellaneous preliminary initialisations.

! in practical LLO.OR.LELTRA should now be always T for SL2TL.
IF (LTWOTL) THEN
  LLO=.NOT.LELTRA
ELSE
  LLO=.FALSE.
ENDIF

! deactivate computation of SLHD weights
LLSLHD     =.FALSE.
LLSLHDQUAD =.FALSE.

!*       1.3   Computation of weights for smooth interpolation at arrival point.

IF(YDML_DYN%YRDYN%LSVTSM) THEN
   CALL ABOR1 ('UNEXPECTED CASE')
ENDIF

!     ------------------------------------------------------------------

!*       2.    ITERATIONS.
!              -----------

DO JITER=1,YDML_DYN%YRDYN%NITMP

  !*       2.1   DETERMINATION OF THE MEDIUM POINT "M" OR THE ORIGIN POINT "O".

  ! Computation of the coordinates of the medium or origin point.
  ! If (LLO=T or LELTRA=T) the origin point "O" is computed
  ! instead of the medium point "M".

  ZLEVT=YDGEOMETRY%YRVETA%VETAF(0)
  ZLEVB=YDGEOMETRY%YRVETA%VETAF(YDGEOMETRY%YRDIMV%NFLEVG+1)
  ZVETAON=(1.0_JPRB-YDML_DYN%YRDYN%VETAON)*YDGEOMETRY%YRVETA%VETAH(0)+YDML_DYN%YRDYN%VETAON*YDGEOMETRY%YRVETA%VETAF(1)
  ZVETAOX=(1.0_JPRB-YDML_DYN%YRDYN%VETAOX)*YDGEOMETRY%YRVETA%VETAH(YDGEOMETRY%YRDIMV%NFLEVG)+YDML_DYN%YRDYN%VETAOX*YDGEOMETRY%YRVETA%VETAF(YDGEOMETRY%YRDIMV%NFLEVG)
  IF (LRALTVDISP) THEN
    ZHVETAON=0.5_JPRB*ZVETAON
    ZHVETAOX=1.0_JPRB-0.5_JPRB*(1.0_JPRB-ZVETAOX)
  ENDIF
  !$acc loop vector
  DO JROF = KST, KPROF
    ISTEST(JROF)=0
  ENDDO

  !$acc loop vector
  DO JROF = KST, KPROF
    ISTESB(JROF)=0
  ENDDO


  IF (JITER == 1) THEN

    !$acc loop vector
    DO JROF = KST, KPROF
    DO JLEV=1,YDGEOMETRY%YRDIMV%NFLEVG
      

        ZINEZ=ZINEZV(JROF)

        ! * computations on horizontal plans.

        ZINDX=YDGEOMETRY%YRCSGEOM(KIBL)%RINDX(JROF)
        ZINDY=YDGEOMETRY%YRCSGEOM(KIBL)%RINDY(JROF)

        !   - Compute the relative coordinates of departure point of trajectory

        PUF(JROF,JLEV)=PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2URL+JLEV-1)
        PVF(JROF,JLEV)=PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2VRL+JLEV-1)
        ZTXO = ZINDX-2.0_JPRB*PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2URL+JLEV-1)*PGMDTX(JROF)
        ZTYO = ZINDY-2.0_JPRB*PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2VRL+JLEV-1)*PGMDTY(JROF)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON) = ZTXO*ZINEZ +ZINDX*(1.0_JPRB-ZINEZ)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT) = ZTYO*ZINEZ +ZINDY*(1.0_JPRB-ZINEZ)

        !   - Return back the departure point if it is out of the C+I zone

        ZTXO = MIN(MAX(ZTXO,ZDLUN),ZDLUX)
        ZTYO = MIN(MAX(ZTYO,ZDGUN),ZDGUX)

        !   - Fill the array elements by coordinates of the origin point
        !     Set the origin point to the arrival point if it is left or right
        !     out of C+I zone

        IF(LLO.OR.LELTRA) THEN
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO) = ZTXO*ZINEZ+ZINDX*(1.0_JPRB-ZINEZ)
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO) = ZTYO*ZINEZ+ZINDY*(1.0_JPRB-ZINEZ)
        ELSE
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO) = 0.5_JPRB*(ZTXO+ZINDX)*ZINEZ&
           & + ZINDX*(1.0_JPRB-ZINEZ)
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO) = 0.5_JPRB*(ZTYO+ZINDY)*ZINEZ&
           & + ZINDY*(1.0_JPRB-ZINEZ)
        ENDIF

        ! * computations on a vertical.

        IF(YDGEOMETRY%YRSTA%STPREH(JLEV) > YDML_DYN%YRDYN%RPRES_SVTSM .OR. (NCONF /= 1 .AND. NCONF /= 302) .OR..NOT.YDML_DYN%YRDYN%LSVTSM) THEN
          ZWF(JROF,JLEV)=PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2WRL+JLEV-1)
        ELSE
          ZWF(JROF,JLEV)=ZWFASM(JROF,JLEV)
        ENDIF
        IF (LRALTVDISP) THEN
          ZVDISP_1=-YDRIP%RTDT*ZINEZ*ZWF(JROF,JLEV)
          ZZ=EXP(-YDRIP%RTDT*ZINEZ*ZWF(JROF,JLEV)*(1.0_JPRB/(YDGEOMETRY%YRVETA%VETAF(JLEV)-ZHVETAON)&
           & +1.0_JPRB/(ZHVETAOX-YDGEOMETRY%YRVETA%VETAF(JLEV))))&
           & *(YDGEOMETRY%YRVETA%VETAF(JLEV)-ZHVETAON)/(ZHVETAOX-YDGEOMETRY%YRVETA%VETAF(JLEV))
          ZVDISP_2=(ZHVETAON+ZHVETAOX*ZZ)/(1.0_JPRB+ZZ)-YDGEOMETRY%YRVETA%VETAF(JLEV)
          ZVDISP=SIGN(1.0_JPRB,ZVDISP_1)*MIN(ABS(ZVDISP_1),ABS(ZVDISP_2))
          ZLEVO=YDGEOMETRY%YRVETA%VETAF(JLEV)+ZVDISP
        ELSE
          ZLEVO=YDGEOMETRY%YRVETA%VETAF(JLEV)-YDRIP%RTDT*ZINEZ*ZWF(JROF,JLEV)
        ENDIF
        ISTEST(JROF)=ISTEST(JROF)-MIN(0,MAX(-1,NINT(ZLEVO-ZLEVT-0.5_JPRB)))
        ISTESB(JROF)=ISTESB(JROF)-MIN(0,MAX(-1,NINT(ZLEVB-ZLEVO-0.5_JPRB)))
        ZLEVO=MIN(ZVETAOX,MAX(ZVETAON,ZLEVO))
        IF(LLO.OR.LELTRA) THEN
          PLEV(JROF,JLEV)=ZLEVO
        ELSE
          PLEV(JROF,JLEV)=0.5_JPRB*(ZLEVO+YDGEOMETRY%YRVETA%VETAF(JLEV))
        ENDIF

      
    ENDDO
    ENDDO


  ELSE

    !$acc loop vector
    DO JROF = KST, KPROF
    DO JLEV=1,YDGEOMETRY%YRDIMV%NFLEVG
      

        ZINEZ=ZINEZV(JROF)

        ! * computations on horizontal plans.

        ZINDX=YDGEOMETRY%YRCSGEOM(KIBL)%RINDX(JROF)
        ZINDY=YDGEOMETRY%YRCSGEOM(KIBL)%RINDY(JROF)

        !   - Compute the relative coordinates of departure point of trajectory
        !     ZPU,ZPV are the coordinates of VM in the local repere related to F

        IF(LELTRA) THEN
          ZPU=PUF(JROF,JLEV)
          ZPV=PVF(JROF,JLEV)
        ELSEIF(LLO) THEN
          ZPU=0.5_JPRB*(PUF(JROF,JLEV)+PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2URL+JLEV-1))
          ZPV=0.5_JPRB*(PVF(JROF,JLEV)+PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2VRL+JLEV-1))
        ELSE
          ZPU=PUF(JROF,JLEV)
          ZPV=PVF(JROF,JLEV)
        ENDIF

        ZTXO = ZINDX-2.0_JPRB*ZPU*PGMDTX(JROF)
        ZTYO = ZINDY-2.0_JPRB*ZPV*PGMDTY(JROF)

        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON) = ZTXO*ZINEZ +ZINDX*(1.0_JPRB-ZINEZ)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT) = ZTYO*ZINEZ +ZINDY*(1.0_JPRB-ZINEZ)

        !   - Return back the departure point if it is out of the C+I zone

        ZTXO = MIN(MAX(ZTXO,ZDLUN),ZDLUX)
        ZTYO = MIN(MAX(ZTYO,ZDGUN),ZDGUX)

        !   - Fill the array elements by coordinates of the origin point
        !     Set the origin point to the arrival point if it is left or right
        !     out of C+I zone

        IF(LLO.OR.LELTRA) THEN
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO) = ZTXO*ZINEZ+ZINDX*(1.0_JPRB-ZINEZ)
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO) = ZTYO*ZINEZ+ZINDY*(1.0_JPRB-ZINEZ)
        ELSE
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO) = 0.5_JPRB*(ZTXO+ZINDX)*ZINEZ&
           & + ZINDX*(1.0_JPRB-ZINEZ)
          PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO) = 0.5_JPRB*(ZTYO+ZINDY)*ZINEZ&
           & + ZINDY*(1.0_JPRB-ZINEZ)
        ENDIF
        IF(JITER == YDML_DYN%YRDYN%NITMP) THEN
          ! * save (zpu,zpv) in (puf,pvf) with a rotation equal to identity
          !   in plane geometry.
          PUF(JROF,JLEV)=ZPU
          PVF(JROF,JLEV)=ZPV
        ENDIF

        ! * computations on a vertical.

        IF(LLO) THEN
          IF(YDGEOMETRY%YRSTA%STPREH(JLEV) > YDML_DYN%YRDYN%RPRES_SVTSM .OR. (NCONF /= 1 .AND. NCONF /= 302) .OR..NOT.YDML_DYN%YRDYN%LSVTSM) THEN
            IF (LRALTVDISP) THEN
              ZWF_2(JROF,JLEV)=ZWF(JROF,JLEV)
              ZWO_2(JROF,JLEV)=PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2WRL+JLEV-1)
            ENDIF
            ZWF(JROF,JLEV)=0.5_JPRB*(ZWF(JROF,JLEV)+PB2(JROF,YDML_DYN%YRPTRSLB2%MSLB2WRL+JLEV-1)*ZINEZ)
          ELSE
            IF (LRALTVDISP) THEN
              ZWF_2(JROF,JLEV)=ZWFASM(JROF,JLEV)
              ZWO_2(JROF,JLEV)=ZWFSM(JROF,JLEV)
            ENDIF
            ZWF(JROF,JLEV)=0.5_JPRB*(ZWFSM(JROF,JLEV)+ZWFASM(JROF,JLEV)*ZINEZ)
          ENDIF
        ENDIF

        IF (LRALTVDISP) THEN
          ZVDISP_1=-YDRIP%RTDT*ZINEZ*ZWF(JROF,JLEV)
          IF (LLO) THEN
            ZZ=EXP(-0.5_JPRB*YDRIP%RTDT*ZINEZ*ZWF_2(JROF,JLEV)*(1.0_JPRB/(YDGEOMETRY%YRVETA%VETAF(JLEV)-ZHVETAON)&
             & +1.0_JPRB/(ZHVETAOX-YDGEOMETRY%YRVETA%VETAF(JLEV)))&
             & -0.5_JPRB*YDRIP%RTDT*ZINEZ*ZWO_2(JROF,JLEV)*(1.0_JPRB/(PLEV(JROF,JLEV)-ZHVETAON)&
             & +1.0_JPRB/(ZHVETAOX-PLEV(JROF,JLEV))))&
             & *(YDGEOMETRY%YRVETA%VETAF(JLEV)-ZHVETAON)/(ZHVETAOX-YDGEOMETRY%YRVETA%VETAF(JLEV))
          ELSE
            ZZ=EXP(-YDRIP%RTDT*ZINEZ*ZWF(JROF,JLEV)*(1.0_JPRB/(PLEV(JROF,JLEV)-ZHVETAON)&
             & +1.0_JPRB/(ZHVETAOX-PLEV(JROF,JLEV))))&
             & *(YDGEOMETRY%YRVETA%VETAF(JLEV)-ZHVETAON)/(ZHVETAOX-YDGEOMETRY%YRVETA%VETAF(JLEV))
          ENDIF
          ZVDISP_2=(ZHVETAON+ZHVETAOX*ZZ)/(1.0_JPRB+ZZ)-YDGEOMETRY%YRVETA%VETAF(JLEV)
          ZVDISP=SIGN(1.0_JPRB,ZVDISP_1)*MIN(ABS(ZVDISP_1),ABS(ZVDISP_2))
          ZLEVO=YDGEOMETRY%YRVETA%VETAF(JLEV)+ZVDISP
        ELSE
          ZLEVO=YDGEOMETRY%YRVETA%VETAF(JLEV)-YDRIP%RTDT*ZINEZ*ZWF(JROF,JLEV)
        ENDIF
        ISTEST(JROF)=ISTEST(JROF)-MIN(0,MAX(-1,NINT(ZLEVO-ZLEVT-0.5_JPRB)))
        ISTESB(JROF)=ISTESB(JROF)-MIN(0,MAX(-1,NINT(ZLEVB-ZLEVO-0.5_JPRB)))
        ZLEVO=MIN(ZVETAOX,MAX(ZVETAON,ZLEVO))
        IF(LLO.OR.LELTRA) THEN
          PLEV(JROF,JLEV)=ZLEVO
        ELSE
          PLEV(JROF,JLEV)=0.5_JPRB*(ZLEVO+YDGEOMETRY%YRVETA%VETAF(JLEV))
        ENDIF

        !later IF( LSLDIA.AND.(JITER==NITMP) ) THEN
        !later   ! trajectory vertical velocity
        !later   PWF(JROF,JLEV)=ZWF(JROF,JLEV)
        !later ENDIF

      
    ENDDO
    ENDDO


  ENDIF

  CALL REDUCE (ISTEST (KST:KPROF), ISTEST_TOT, IREDUCTION_SUM, IREDUCTION_NONE)

  IF (ISTEST_TOT > 0 .AND. JITER == YDML_DYN%YRDYN%NITMP) THEN
    WRITE(NULERR,*) ' SMILAG TRAJECTORY OUT OF ATM ',ISTEST_TOT,' TIMES.'
    ! print statistics
    IF (LRPRSLTRJ) THEN
      !$acc loop vector
      DO JROF= KST, KPROF
        IF (ISTEST(JROF) /= 0) THEN
          WRITE (NULERR,*) ' POINT= ',JROF,' MAX ETADOT VERTICAL VEL.= ',MAXVAL(ZWF(JROF,:))
          WRITE (NULERR,*) ' LON  = ',ACOS(YDGEOMETRY%YRCSGEOM(KIBL)%RCOLON(JROF))*180._JPRB/RPI,' degrees'
          WRITE (NULERR,*) ' LAT  = ',ASIN(YDGEOMETRY%YRGSGEOM(KIBL)%GEMU(JROF))*180._JPRB/RPI,' degrees'
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  CALL REDUCE (ISTESB (KST:KPROF), ISTESB_TOT, IREDUCTION_SUM, IREDUCTION_NONE)

  IF (ISTESB_TOT > 0 .AND. JITER == YDML_DYN%YRDYN%NITMP) THEN
    WRITE(NULERR,*) ' SMILAG TRAJECTORY UNDERGROUND ',ISTESB_TOT,' TIMES.'
    ! print statistics
    IF (LRPRSLTRJ) THEN
      !$acc loop vector
      DO JROF = KST, KPROF
        IF (ISTESB(JROF) /= 0) THEN
          WRITE (NULERR,*) ' POINT= ',JROF,' MAX ETADOT VERTICAL VEL.= ',MAXVAL(ZWF(JROF,:))
          WRITE (NULERR,*) ' LON  = ',ACOS(YDGEOMETRY%YRCSGEOM(KIBL)%RCOLON(JROF))*180._JPRB/RPI,' degrees'
          WRITE (NULERR,*) ' LAT  = ',ASIN(YDGEOMETRY%YRGSGEOM(KIBL)%GEMU(JROF))*180._JPRB/RPI,' degrees'
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  !*       2.2   DETERMINATION OF THE wind AT "M" OR "O".

  IF(JITER /= YDML_DYN%YRDYN%NITMP) THEN

    ! If (LLO=T or LELTRA=T)
    ! and JITER < NITMP wind is interpolated at
    ! the origin point "O" instead of at the medium point "M".
    ! In the other cases the wind is interpolated at "M".

    IHVI=0
    IROT=0
    ITIP=1
    LLINTV=.TRUE.

    CALL LARCINA(YDGEOMETRY,YDML_DYN,KST,KPROF,YDSL,IHVI,KSTABUF,YDML_DYN%YRDYN%LFINDVSEP,LLSLHD,LLSLHDQUAD,LLINTV,&
     & ITIP,IROT,.TRUE.,PLSDEPI,&
     & KIBL,PSCO,PLEV,&
     & ZPROLEVDUM, ZPROLEVDUM, ZPROLEVDUM, ZPROLEVDUM, &
     & PB2(:,YDML_DYN%YRPTRSLB2%MSLB2STDDISU:YDML_DYN%YRPTRSLB2%MSLB2STDDISU+YDGEOMETRY%YRDIMV%NFLEVG-1),PB2(:,YDML_DYN%YRPTRSLB2%MSLB2STDDISV:YDML_DYN%YRPTRSLB2%MSLB2STDDISV+YDGEOMETRY%YRDIMV%NFLEVG-1),&
     & PB2(:,YDML_DYN%YRPTRSLB2%MSLB2STDDISW:YDML_DYN%YRPTRSLB2%MSLB2STDDISW+YDGEOMETRY%YRDIMV%NFLEVG-1),&
     & PB1(:,YDML_DYN%YRPTRSLB1%MSLB1UR0),PB1(:,YDML_DYN%YRPTRSLB1%MSLB1VR0),PB1(:,YDML_DYN%YRPTRSLB1%MSLB1WR0),&
     & KVSEPC,KVSEPL,PCCO,&
     & PUF,PVF,ZWF,ZWFSM,&
     & KL0,KLH0,KLEV,PLSCAW,PRSCAW,IDEP, YLSTACK)

  ENDIF

ENDDO

!     ------------------------------------------------------------------

!*       3.    COMPUTATION OF THE ORIGIN POINT "O" COORDINATES.
!              ------------------------------------------------

IF (.NOT.(LLO.OR.LELTRA)) THEN
  ! this case may occur only for SL3TL scheme.
  IF (LTWOTL) CALL ABOR1(' ELARMES 3 ')

  ZVETAON=(1.0_JPRB-YDML_DYN%YRDYN%VETAON)*YDGEOMETRY%YRVETA%VETAH(0)+YDML_DYN%YRDYN%VETAON*YDGEOMETRY%YRVETA%VETAF(1)
  ZVETAOX=(1.0_JPRB-YDML_DYN%YRDYN%VETAOX)*YDGEOMETRY%YRVETA%VETAH(YDGEOMETRY%YRDIMV%NFLEVG)+YDML_DYN%YRDYN%VETAOX*YDGEOMETRY%YRVETA%VETAF(YDGEOMETRY%YRDIMV%NFLEVG)

  !$acc loop vector
  DO JROF = KST, KPROF
  DO JLEV=1,YDGEOMETRY%YRDIMV%NFLEVG
    

      ! computations on horizontal plans.
      PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)=2.0_JPRB*PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)-YDGEOMETRY%YRCSGEOM(KIBL)%RINDY(JROF)
      PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)=2.0_JPRB*PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)-YDGEOMETRY%YRCSGEOM(KIBL)%RINDX(JROF)

      ! computations on a vertical.

      PLEV(JROF,JLEV)=2.0_JPRB*PLEV(JROF,JLEV)-YDGEOMETRY%YRVETA%VETAF(JLEV)
      PLEV(JROF,JLEV)=MAX(ZVETAON,MIN(ZVETAOX,PLEV(JROF,JLEV)))

    
  ENDDO
  ENDDO


ENDIF

!     ------------------------------------------------------------------


END SUBROUTINE ELARMES
