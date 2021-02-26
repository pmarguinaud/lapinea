!option! -O extendreorder
SUBROUTINE ELASCAW(&
 ! --- INPUT -------------------------------------------------
 & YDVSPLIP,LDSLHDHEAT,YDSL,KPROMB,KDIMK,KST,KPROF,KFLEV,&
 & KFLDN,KSTABUF,KWIS,KHOR,KHVI,&
 & LDSLHD,LDSLHDQUAD,LDSLHD_OLD,LD3DTURB,&
 & LDCOMAD,LDCOMADH,LDCOMADV,KSPLTHOI,&
 & PLON,PLAT,PLEV,&
 & PVETA,KVAUT,&
 & PVCUICO,PVSLD,PVSLDW,KRLEVX,PVRLEVX,&
 & PKAPPA,PKAPPAT,PKAPPAM,PKAPPAH,&
 & PSTDDISU,PSTDDISV,PSTDDISW,&
 ! --- OUTPUT ------------------------------------------------
 & PDLAT,PDLAMAD,PCLA,PCLASLD,PCLASLT,PCLAMAD,&
 & PDLO ,PDLOMAD,PCLO,PCLOSLD,PCLOSLT,PCLOMAD,&
 & KL0,KLH0,KLEV,&
 & PDVER,PDVERMAD,PVINTW,PVINTWSLD,PVINTWSLT,PVINTWMAD,PVINTWS,&
 & PVDERW,PHVW,KDEP,KSTPT,KSTSZ,PSTACK)


#include "temp.h"

!**** *ELASCAW  -  Externalisable interpolator:
!                 Storage of Coordinates And Weights.
!                 Plane geometry version

!     Purpose.
!     --------
!       Determines the interpolation grid:
!       - computation of the coordinates of the
!         point situated at the upper left corner of the 16 points
!         square, and of the interpolation point.
!       - computation of weights.
!       Storage of coordinates and weights.

!       Note that this routine should not know if levels are half levels or full levels;
!       this information must remain in the caller.

!**   Interface.
!     ----------
!        *CALL* *ELASCAW( ... )

!        Explicit arguments :
!        --------------------

!        INPUT:
!          YDSL    - SL_STRUCT definition
!          YDDYN   - structure containing dynamics.
!          KPROMB  - horizontal dimension for interpolation point quantities.
!          KDIMK   - last dimension for some non-linear weights.
!          KST     - first element of arrays where computations are performed.
!          KPROF   - depth of work.
!          KFLEV   - vertical dimension.
!          KFLDN   - number of the first field.
!          KSTABUF - for a latitude IGL, KSTABUF(IGL) is the
!                    address of the element corresponding to
!                    (ILON=1,IGL) in the KPROMB arrays.
!          KWIS    - kind of interpolation.
!          KHOR    - 0: Horizontal interpolation for each level
!                       of a 3D variable.
!                    1: Interpolation for all origin points corresponding
!                       to a final point of a 2D variable.
!          KHVI    - 1/0: filling weights arrays PVDERW and PHVW is necessary/not necessary.
!          LDSLHD  - key activating SLHD weights precomputation
!          LDSLHDQUAD - key activating quadratic weights precomputation
!          LDSLHD_OLD - use old SLHD interpolator
!          LD3DTURB- key activating 3D turbulence weights precomputation
!          LDCOMAD -  key activating COMAD weight computation
!          LDCOMADH-  key activating hor. COMAD
!          LDCOMADV-  key activating ver. COMAD
!          KSPLTHOI- controls additional weights precomputation
!          PLON    - x-coordinate of the interpolation point
!                    (in the fractional system <NDLUNG;NDLON>)
!          PLAT    - y-coordinate of the interpolation point
!                    (in the fractional system <NDGSAG;NDGENG>)
!          PLEV    - vertical coordinate of the interpolation point.
!          PVETA   - Values of ETA.
!          KVAUT   - Help table for vertical box search: gives the number
!                    of the layer immediately above eta.
!          PVCUICO - Denominators of the vertical cubic interpolation coefficients
!          PVSLD   - auxiliary quantities for vertical SLHD interpolation
!          PVSLDW  - weights for SLHD vertical Laplacian smoother
!          KRLEVX  - Dimension of KVAUT
!          PVRLEVX - REAL(KRLEVX).
!          PKAPPA  - kappa function ("coefficient of SLHD") based on the
!                    rescaled horizontal deformation of the flow evaluated
!                    at instant "t" for the final point F
!          PKAPPAT - PKAPPA for heat variable
!          PKAPPAM - horizontal exchange coefficient for momentum in 3D turb. 
!          PKAPPAH - horizontal exchange coefficient for heat in 3D turb.
!          PSTDDISU- COMAD correction coefficient based on estimated flow deformation
!                    along zonal direction of the trajectory but computed
!                    with wind derivatives at instant "t" at the final point F
!          PSTDDISV- COMAD correction coefficient based on estimated flow deformation
!                    along meridional direction of the trajectory but computed
!                    with wind derivatives at instant "t" at the final point F
!          PSTDDISW- COMAD correction coefficient based on estimated flow deformation
!                    along vertical direction of the trajectory but computed
!                    with wind derivatives at instant "t" at the final point F
!        OUTPUT:
!          PDLAT     - distance for horizontal linear interpolations in latitude
!          PDLAMAD   - PDLAT, COMAD case
!          PCLA      - weights for horizontal cubic interpolations in latitude
!          PCLASLD   - weights for horizontal cubic interpolations in latitude, SLHD case
!          PCLAMAD   - PCLA, COMAD case
!          PCLASLT   - weights for horizontal cubic interpolations in latitude, SLHD case on T
!          PDLO      - distances for horizontal linear interpolations
!                      in longitude (latitude rows 0, 1, 2, 3)
!          PDLOMAD   - PDLO, COMAD case
!          PCLO      - weights for horizontal cubic interpolations in
!                      longitude (latitude rows 1, 2)
!          PCLOSLD   - weights for horizontal cubic interpolations in
!                      longitude, SLHD case (latitude rows 1, 2)
!          PCLOMAD   - PCLO, COMAD case
!          PCLOSLT   - weights for horizontal cubic interpolations in
!                      longitude, SLHD case (latitude rows 1, 2) on T
!          KL0       - index of the four western points
!                      of the 16 points interpolation grid.
!          KLH0      - second value of index of the four western points
!                      of the 16 points interpolation grid if needed.
!          KLEV      - lower level of the vertical interpolation
!                      grid needed for vertical interpolations.
!          PDVER     - distance for vertical linear interpolation
!          PDVERMAD  - PDVER, COMAD case
!          PVINTW    - vertical cubic interpolation weights
!          PVINTWSLD - vertical cubic interpolation weights, SLHD case
!          PVINTWMAD - PVINTW, COMAD case
!          PVINTWSLT - vertical cubic interpolation weights, SLHD case on T
!          PVINTWS   - Vertical spline interpolation weights.
!          PVDERW    - weights to compute vertical derivatives necessary for
!                      Hermite cubic vertical interpolation.
!          PHVW      - Hermite vertical cubic interpolation weights.
!          KDEP      - indication of the interpolation stencil latitudial
!                      dependences (used for LVECADIN=.T. option in adjoint)

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------

!     Author.
!     -------
!        M. JANOUSEK, after the subroutine LASCAW written by K. YESSAD
!        METEO-FRANCE, CNRM/GMAP/EXT
!        Original : MARCH 1994.

!     Modifications.
!     --------------
!        Modified 28-Aug-2007 F. Vana : removing distances for 4p-splines
!        07-Nov-2007 J. Masek   New weights for SLHD interpolators.
!        Modified 02-Jun-2008 A. Bogatchev - use YOMLASCAW et c.
!        F. Vana  23-Jul-2008 support for vectorization
!        K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!        K. Yessad (Feb 2009): split loops, rewrite in a shorter way.
!        R. El Khatib 07-08-2009 Optimisation directive for NEC
!        F. Vana  23-Feb-2011: Horizontal turbulence + diff of phys. tend.
!        G.Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!        Y. Seity  20-Jan-2012: activation of SLHD in Preditor in case LPC_CHEAP
!        S. Malardel and D. Ricard (Nov 2013): COMAD weights for SL interpolations
!        B. Bochenek (Apr 2015): Phasing: move some variables and update
!        K. Yessad (March 2017): simplify level numbering.
! End Modifications
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

! arp/ifs dependencies to be solved later.
USE YOMDYNA  , ONLY : LSLHD    ,LSLHDQUAD
USE YOMMP0   , ONLY : LOPT_SCALAR,NPROC

USE EINT_MOD , ONLY : SL_STRUCT,JPDUP
USE YOMVSPLIP , ONLY : TVSPLIP

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(TVSPLIP)     ,INTENT(IN)    :: YDVSPLIP
LOGICAL           ,INTENT(IN)    :: LDSLHDHEAT
TYPE(SL_STRUCT)   ,INTENT(INOUT) :: YDSL
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMB
INTEGER(KIND=JPIM),INTENT(IN)    :: KDIMK
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTABUF(YDSL%NDGSAH:YDSL%NDGENH)
INTEGER(KIND=JPIM),INTENT(IN)    :: KWIS
INTEGER(KIND=JPIM),INTENT(IN)    :: KHOR
INTEGER(KIND=JPIM),INTENT(IN)    :: KHVI
LOGICAL           ,INTENT(IN)    :: LDSLHD
LOGICAL           ,INTENT(IN)    :: LDSLHDQUAD
LOGICAL           ,INTENT(IN)    :: LDSLHD_OLD
LOGICAL           ,INTENT(IN)    :: LD3DTURB
LOGICAL           ,INTENT(IN)    :: LDCOMAD
LOGICAL           ,INTENT(IN)    :: LDCOMADH
LOGICAL           ,INTENT(IN)    :: LDCOMADV
INTEGER(KIND=JPIM),INTENT(IN)    :: KSPLTHOI
INTEGER(KIND=JPIM),INTENT(IN)    :: KRLEVX
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLON(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAT(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLEV(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVETA(0:KFLEV+1)
INTEGER(KIND=JPIM),INTENT(IN)    :: KVAUT(0:KRLEVX)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCUICO(4,0:KFLEV-1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVSLD(3,0:KFLEV-1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVSLDW(3,3,0:KFLEV-1)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVRLEVX
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPA(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAT(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAM(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAH(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISU(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISV(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISW(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDLAT(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDLAMAD(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLA(KPROMB,KFLEV,3,KDIMK)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLASLD(KPROMB,KFLEV,3,KDIMK)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLASLT(KPROMB,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLAMAD(KPROMB,KFLEV,3,KDIMK)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDLO(KPROMB,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDLOMAD(KPROMB,KFLEV,0:3)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLO(KPROMB,KFLEV,3,2,KDIMK)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLOSLD(KPROMB,KFLEV,3,2,KDIMK)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLOSLT(KPROMB,KFLEV,3,2)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCLOMAD(KPROMB,KFLEV,3,2,KDIMK)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KL0(KPROMB,KFLEV,0:3)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KLH0(KPROMB,KFLEV,0:3)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KLEV(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDVER(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDVERMAD(KPROMB,KFLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVINTW(KPROMB,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVINTWSLD(KPROMB,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVINTWMAD(KPROMB,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVINTWSLT(KPROMB,KFLEV,3)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVINTWS(KPROMB,KFLEV,1:4)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVDERW(KPROMB,KFLEV,2*KHVI,2*KHVI)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PHVW(KPROMB,KFLEV,4*KHVI)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KDEP(KPROMB,KFLEV)

INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL   (KIND=JPRB),INTENT(INOUT) :: PSTACK (KSTSZ)
!     ------------------------------------------------------------------

INTEGER(KIND=JPIM) :: IADDR(YDSL%NDGSAH:YDSL%NDGENH)

INTEGER(KIND=JPIM) :: IDLUN1, IDLUX1, IFLVM2, ILA, ILA1, ILA2, ILA3,&
 & ILAG, ILEV, ILEVV, ILO, JLAT, JLEV, JROF, JJ, IJ_, J_

REAL(KIND=JPRB) :: PD, ZD2, ZDEN1, ZDEN2, ZDVER, ZFAC, ZNUM


temp (REAL(KIND=JPRB), ZKHTURB, (KPROMB,KFLEV,KDIMK))
LOGICAL         :: LLT_SLHD(4),LLT_PHYS(4),LLSLHD,LLSLHDQUAD,LLSLHD_OLD,LL3DTURB
LOGICAL         :: LLCOMAD,LLCOMADH,LLCOMADV
 
temp (REAL(KIND=JPRB), ZCLA, (KPROMB,KFLEV,3))


temp (REAL(KIND=JPRB), ZCLO, (KPROMB,KFLEV,3,2))
! Duplicata of some dummy or local arrays for optimisation on NEC platform.
INTEGER(KIND=JPIM) :: IADDR_(JPDUP,YDSL%NDGSAH:YDSL%NDGENH)
REAL(KIND=JPRB) :: ZVETA_(JPDUP,0:KFLEV+1)
REAL(KIND=JPRB) :: ZVCUICO_(JPDUP,4,0:KFLEV-1)
REAL(KIND=JPRB) :: ZVSLD_(JPDUP,3,0:KFLEV-1)
REAL(KIND=JPRB) :: ZVSLDW_(JPDUP,3,3,0:KFLEV-1)


!     ------------------------------------------------------------------
! functions

REAL(KIND=JPRB) :: FHLO1, FHLO2, FHLO3, FHLO4

! auxiliary functions for Hermite cubic interpolation

FHLO1(PD)= (1.0_JPRB-PD)*(1.0_JPRB-PD)*(1.0_JPRB+2.0_JPRB*PD)
FHLO2(PD)= PD*PD*(3._JPRB-2.0_JPRB*PD)
FHLO3(PD)= PD*(1.0_JPRB-PD)*(1.0_JPRB-PD)
FHLO4(PD)=-PD*PD*(1.0_JPRB-PD)

!     ------------------------------------------------------------------

#include "lascaw_clo.intfb.h"
#include "lascaw_vintw.intfb.h"

!     ------------------------------------------------------------------

init_stack ()

alloc (ZKHTURB)
alloc (ZCLA)
alloc (ZCLO)


ASSOCIATE(RFVV=>YDVSPLIP%RFVV)
!     ------------------------------------------------------------------

!*       0.    PRELIMINARY INITIALISATIONS.
!              ----------------------------

! cases relevant for SLHD scheme (switches LDSLHD, LDSLHDQUAD are
! deactivated during computation of medium points in LAPINEA)
LLSLHD=LDSLHD.AND.(KWIS==103.OR.KWIS==104.OR.KWIS==105.OR.KWIS==203)
LLSLHDQUAD=LDSLHDQUAD.AND.(KWIS==103.OR.KWIS==104.OR.KWIS==105.OR.KWIS==203)
LL3DTURB=LD3DTURB.AND.(KWIS==103.OR.KWIS==104.OR.KWIS==105)

! switch for old SLHD scheme
LLSLHD_OLD=LLSLHD.AND.LDSLHD_OLD

LLT_SLHD(1)=LLSLHD
LLT_SLHD(2)=LLSLHDQUAD
LLT_SLHD(3)=LLSLHD_OLD
LLT_SLHD(4)=.FALSE.

! cases relevant for COMAD scheme (switches LDCOMADH and LDCOMADV  are
! deactivated during computation of interpolation points in LAPINEA)
LLCOMAD =LDCOMAD.AND.(KWIS==103.OR.KWIS==104.OR.KWIS==105.OR.KWIS==203)
LLCOMADH=LLCOMAD.AND.LDCOMADH
LLCOMADV=LLCOMAD.AND.LDCOMADV

! switches for interpolation of physics 
! It holds the same value for every iteration step (in ICI scheme). 
LLT_PHYS(1)=LSLHD
LLT_PHYS(2)=LSLHDQUAD
LLT_PHYS(3)=LDSLHD_OLD
LLT_PHYS(4)=.FALSE.

DO JLAT=YDSL%NDGSAH,YDSL%NDGENH
  IADDR(JLAT)=KSTABUF(JLAT)+YDSL%NASLB1*(0-KFLDN)
ENDDO

DO J_ = 1, JPDUP
  IADDR_(J_,YDSL%NDGSAH:YDSL%NDGENH)=IADDR(YDSL%NDGSAH:YDSL%NDGENH)
ENDDO
DO J_ = 1, JPDUP
  ZVETA_ (J_,  0:KFLEV+1)=PVETA (0:KFLEV+1)
  IF (LOPT_SCALAR) THEN
    ZVCUICO_(J_,1:4,0:KFLEV-1)=PVCUICO(1:4,0:KFLEV-1) 
  ELSE
!CDIR NOUNROLL
    DO JLEV=1,4*KFLEV
      ZVCUICO_(J_,JLEV,0)=PVCUICO(JLEV,0)
    ENDDO
  ENDIF
ENDDO
IF (LLSLHDQUAD) THEN
  DO J_ = 1, JPDUP
    IF (LOPT_SCALAR) THEN
      ZVSLD_(J_,1:3,0:KFLEV-1)=PVSLD(1:3,0:KFLEV-1)
    ELSE
!CDIR NOUNROLL
      DO JLEV=1,3*KFLEV
        ZVSLD_(J_,JLEV,0)=PVSLD(JLEV,0)
      ENDDO
    ENDIF
  ENDDO
ENDIF
IF (LLSLHD) THEN
  DO J_ = 1, JPDUP
    IF (LOPT_SCALAR) THEN
      ZVSLDW_(J_,1:3,1:3,0:KFLEV-1)=PVSLDW(1:3,1:3,0:KFLEV-1)
    ELSE
!CDIR NOUNROLL
      DO JLEV=1,9*KFLEV
        ZVSLDW_(J_,JLEV,1,0)=PVSLDW(JLEV,1,0)
      ENDDO
    ENDIF
  ENDDO
ENDIF
IF (LL3DTURB) THEN
  ! copy from kappa
  ZKHTURB(KST:KPROF,1:KFLEV,2)=PKAPPAM(KST:KPROF,1:KFLEV)
  ZKHTURB(KST:KPROF,1:KFLEV,3)=PKAPPAH(KST:KPROF,1:KFLEV)
ENDIF

IF (KSPLTHOI == 1) THEN
  ! In this case the ZKHTURB is set to static mode with maximum diffusion.
  ZKHTURB(KST:KPROF,1:KFLEV,KDIMK)=1._JPRB
ENDIF

IFLVM2=KFLEV-2

!     ------------------------------------------------------------------

!*       1.    3D MODEL.
!              ---------

IDLUN1=YDSL%NDLUNG-1
IDLUX1=YDSL%NDLUXG-1

!        1.01  Coordinates and weights for trilinear interpolations.

IF (KWIS == 101) THEN

  ZFAC=PVRLEVX/(PVETA(KFLEV+1)-PVETA(0))

  DO JLEV=1,KFLEV

    ! * Calculation of linear weights, KL0.
!CDIR NODEP
!DIR$ PREFERVECTOR
    DO JROF=KST,KPROF

      IJ_ = MOD(JROF+1-KST,JPDUP)+1
      ILAG=INT(PLAT(JROF,JLEV))-1
      ILA=ILAG - YDSL%NFRSTLOFF
      PDLAT(JROF,JLEV)=PLAT(JROF,JLEV)-REAL(ILAG+1,JPRB)
      ILO=INT(PLON(JROF,JLEV))-1
      PDLO(JROF,JLEV,1)=PLON(JROF,JLEV)-REAL(ILO+1,JPRB)
      PDLO(JROF,JLEV,2)=PDLO(JROF,JLEV,1)
      PDLO(JROF,JLEV,3)=PDLO(JROF,JLEV,1)
      PDLO(JROF,JLEV,0)=PDLO(JROF,JLEV,1)

      ILEV  =KVAUT(INT(PLEV(JROF,JLEV)*ZFAC))-1
      IF(ILEV < IFLVM2.AND.&
       & (PLEV(JROF,JLEV)-PVETA(ILEV+2)) > 0.0_JPRB) ILEV=ILEV+1  

      KLEV(JROF,JLEV)=ILEV

      PDVER(JROF,JLEV)=(PLEV(JROF,JLEV)-PVETA(ILEV+1))/&
       & (PVETA(ILEV+2)-PVETA(ILEV+1))  

      ILA1=ILA+1
      ILA1=MIN(MAX(ILA1,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA2=ILA+2
      ILA2=MIN(MAX(ILA2,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILO =MIN(MAX(ILO,IDLUN1),IDLUX1)
      KL0(JROF,JLEV,1)=IADDR_(IJ_,ILA1)+YDSL%NSLEXT(ILO,ILA1)
      KL0(JROF,JLEV,2)=IADDR_(IJ_,ILA2)+YDSL%NSLEXT(ILO,ILA2)
      KL0(JROF,JLEV,0)=IADDR_(IJ_,ILA1)+YDSL%NSLEXT(ILO,ILA1)
      KL0(JROF,JLEV,3)=IADDR_(IJ_,ILA2)+YDSL%NSLEXT(ILO,ILA2)
      KDEP(JROF,JLEV)= (ILA1 -ILA2 +1)*7
    ENDDO

    ! * Mask calculation for on-demand communications:
    IF(NPROC > 1.AND.YDSL%LSLONDEM_ACTIVE)THEN
!CDIR NODEP
!DIR$ PREFERVECTOR
!DIR IVDEP
      DO JROF=KST,KPROF
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+3)=1
      ENDDO
    ENDIF

    DO JROF=KST,KPROF
      KL0(JROF,JLEV,0)=KL0(JROF,JLEV,0)+YDSL%NASLB1*KLEV(JROF,JLEV)
      KL0(JROF,JLEV,1)=KL0(JROF,JLEV,1)+YDSL%NASLB1*KLEV(JROF,JLEV)
      KL0(JROF,JLEV,2)=KL0(JROF,JLEV,2)+YDSL%NASLB1*KLEV(JROF,JLEV)
      KL0(JROF,JLEV,3)=KL0(JROF,JLEV,3)+YDSL%NASLB1*KLEV(JROF,JLEV)
    ENDDO

  ENDDO

ENDIF

!        1.03  Coordinates and weights for ( horizontal 12 points
!              + vertical cubic + 32 points interpolations ) or
!              ( horizontal 12 points + 32 points interpolations ).
!              Optionally, Hermite cubic or cubic B-spline vertical interpolations weights are computed.

IF (KWIS == 103 .OR. KWIS == 104 .OR. KWIS == 105) THEN

  ZFAC=PVRLEVX/(PVETA(KFLEV+1)-PVETA(0))

  DO JLEV=1,KFLEV
    IF (KHOR == 0) ILEV=JLEV
    IF (KHOR == 1) ILEV=KFLDN

    ! * Calculation of linear weights, KL0, KLH0.
!CDIR NODEP
!DIR$ PREFERVECTOR
    DO JROF=KST,KPROF

      IJ_ = MOD(JROF+1-KST,JPDUP)+1
      ILAG=INT(PLAT(JROF,JLEV))-1
      ILA=ILAG - YDSL%NFRSTLOFF

      ! meridional interpolation: linear weights (regular grid)
      ! general case 
      PDLAT(JROF,JLEV)=PLAT(JROF,JLEV)-REAL(ILAG+1,JPRB)

      ! COMAD meridional interpolation 
      IF (LLCOMADH) THEN
        PDLAMAD(JROF,JLEV)=PDLAT(JROF,JLEV)*PSTDDISV(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISV(JROF,JLEV))
      ELSE
        PDLAMAD(JROF,JLEV)=PDLAT(JROF,JLEV)
      ENDIF

      ! zonal interpolation: linear weights for 4 lat. lines
      ! as the grid is regular in the zonal direction,
      ! the cubic weight computation does not need 
      ! other input than linear weights (LASCAW_CLO)
      ! general case
      ILO=INT(PLON(JROF,JLEV))-1
      PDLO(JROF,JLEV,0)=PLON(JROF,JLEV)-REAL(ILO+1,JPRB)
      PDLO(JROF,JLEV,1)=PDLO(JROF,JLEV,0)
      PDLO(JROF,JLEV,2)=PDLO(JROF,JLEV,0)
      PDLO(JROF,JLEV,3)=PDLO(JROF,JLEV,0)

      ! COMAD zonal interpolation 
      IF (LLCOMADH) THEN
        PDLOMAD(JROF,JLEV,0)=PDLO(JROF,JLEV,0)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
        PDLOMAD(JROF,JLEV,1)=PDLO(JROF,JLEV,1)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
        PDLOMAD(JROF,JLEV,2)=PDLO(JROF,JLEV,2)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
        PDLOMAD(JROF,JLEV,3)=PDLO(JROF,JLEV,3)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
      ELSE
        PDLOMAD(JROF,JLEV,0)=PDLO(JROF,JLEV,0)
        PDLOMAD(JROF,JLEV,1)=PDLO(JROF,JLEV,1)
        PDLOMAD(JROF,JLEV,2)=PDLO(JROF,JLEV,2)
        PDLOMAD(JROF,JLEV,3)=PDLO(JROF,JLEV,3)
      ENDIF

      ! vertical interpolation: linear weights
      ! the cubic weight computation are done in 
      ! LASCAW_VINTW (including terms for grid irregularity)
      ILEVV=KVAUT(INT(PLEV(JROF,JLEV)*ZFAC))-1
      IF(ILEVV < IFLVM2.AND.&
       & (PLEV(JROF,JLEV)-PVETA(ILEVV+2)) > 0.0_JPRB) ILEVV=ILEVV+1  
      KLEV(JROF,JLEV)=ILEVV
      ! general case
      PDVER(JROF,JLEV)=(PLEV(JROF,JLEV)-PVETA(ILEVV+1))/&
       & (PVETA(ILEVV+2)-PVETA(ILEVV+1))  
      ! COMAD vertical interpolation 
      IF (LLCOMADV) THEN
        PDVERMAD(JROF,JLEV)=PDVER(JROF,JLEV)*PSTDDISW(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISW(JROF,JLEV))
      ELSE
        PDVERMAD(JROF,JLEV)=PDVER(JROF,JLEV)
      ENDIF

      ILA1=ILA+1
      ILA1=MIN(MAX(ILA1,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA2=ILA+2
      ILA2=MIN(MAX(ILA2,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA3=ILA+3
      ILA3=MIN(MAX(ILA3,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA =MIN(MAX(ILA ,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILO =MIN(MAX(ILO,IDLUN1),IDLUX1)
      KL0(JROF,JLEV,0)=IADDR_(IJ_,ILA )+YDSL%NSLEXT(ILO,ILA)
      KL0(JROF,JLEV,1)=IADDR_(IJ_,ILA1)+YDSL%NSLEXT(ILO,ILA1)
      KL0(JROF,JLEV,2)=IADDR_(IJ_,ILA2)+YDSL%NSLEXT(ILO,ILA2)
      KL0(JROF,JLEV,3)=IADDR_(IJ_,ILA3)+YDSL%NSLEXT(ILO,ILA3)
      KDEP(JROF,JLEV)=(ILA  -ILA1 +1) + (ILA1 -ILA2 +1)*2&
       & + (ILA2 -ILA3 +1)*4

      KLH0(JROF,JLEV,0)=IADDR_(IJ_,ILA )+YDSL%NSLEXT(ILO,ILA)+YDSL%NASLB1*ILEV
      KLH0(JROF,JLEV,1)=IADDR_(IJ_,ILA1)+YDSL%NSLEXT(ILO,ILA1)+YDSL%NASLB1*ILEV
      KLH0(JROF,JLEV,2)=IADDR_(IJ_,ILA2)+YDSL%NSLEXT(ILO,ILA2)+YDSL%NASLB1*ILEV
      KLH0(JROF,JLEV,3)=IADDR_(IJ_,ILA3)+YDSL%NSLEXT(ILO,ILA3)+YDSL%NASLB1*ILEV
    ENDDO

    ! * Mask calculation for on-demand communications:
    IF(NPROC > 1.AND.YDSL%LSLONDEM_ACTIVE)THEN
!CDIR NODEP
!DIR$ PREFERVECTOR
      DO JROF=KST,KPROF
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+3)=1
      ENDDO
    ENDIF

    DO JROF=KST,KPROF
      KL0(JROF,JLEV,0)=KL0(JROF,JLEV,0)+YDSL%NASLB1*KLEV(JROF,JLEV)
      KL0(JROF,JLEV,1)=KL0(JROF,JLEV,1)+YDSL%NASLB1*KLEV(JROF,JLEV)
      KL0(JROF,JLEV,2)=KL0(JROF,JLEV,2)+YDSL%NASLB1*KLEV(JROF,JLEV)
      KL0(JROF,JLEV,3)=KL0(JROF,JLEV,3)+YDSL%NASLB1*KLEV(JROF,JLEV)
    ENDDO

  ENDDO

  IF (LLSLHD.AND.LDSLHDHEAT) THEN
    ! Computes the weights for heat fields affected by SLHD
    !  all the rest is recomputed once again bellow.
    LLT_SLHD(4)=.FALSE.

    ! * Calculation of PCLA and PCLASLD:
    CALL LASCAW_CLO(KFLEV,&
       & KPROMB,KST,KPROF,LLT_SLHD,PDLAT,PDLAMAD,PKAPPAT,ZKHTURB(:,:,1),&
       & PCLA,PCLAMAD(:,:,:,1),PCLASLT(:,:,:),ISTPT,KSTSZ,PSTACK)

    ! * Calculation of PCLO and PCLOSLD:
    CALL LASCAW_CLO(KFLEV,&
     & KPROMB,KST,KPROF,LLT_SLHD,PDLO(:,:,1),PDLOMAD(:,:,1),&
     & PKAPPAT,ZKHTURB(:,:,1),&
     & PCLO(:,:,:,1:1,1),PCLOMAD(:,:,:,1:1,1),PCLOSLT(:,:,:,1:1),ISTPT,KSTSZ,PSTACK)
    CALL LASCAW_CLO(KFLEV,&
     & KPROMB,KST,KPROF,LLT_SLHD,PDLO(:,:,2),PDLOMAD(:,:,2),&
     & PKAPPAT,ZKHTURB(:,:,1),&
     & PCLO(:,:,:,2:2,1),PCLOMAD(:,:,:,2:2,1),PCLOSLT(:,:,:,2:2),ISTPT,KSTSZ,PSTACK)

  ENDIF

  ! Loop over all horiz. weights for 3Dturb (computing in addition
  !                         two sets with and without SLHD)
  DO JJ=1,KDIMK
    IF ((KSPLTHOI == 1).AND.(JJ == KDIMK)) THEN
      ! Bit specific case computing diffusive weights for physical tendencies.
      ! In this case SLHD weights are of no use.

      ! * Calculation of PCLA and PCLASLD:
      CALL LASCAW_CLO(KFLEV,&
       & KPROMB,KST,KPROF,LLT_PHYS,PDLAT,PDLAMAD,ZKHTURB(:,:,JJ),ZKHTURB(:,:,JJ),&
       & ZCLA,PCLAMAD(:,:,:,JJ),PCLA(:,:,:,JJ),ISTPT,KSTSZ,PSTACK)

      ! * Calculation of PCLO and PCLOSLD:
      CALL LASCAW_CLO(KFLEV,&
       & KPROMB,KST,KPROF,LLT_PHYS,PDLO(:,:,1),PDLOMAD(:,:,1),&
       & ZKHTURB(:,:,JJ),ZKHTURB(:,:,JJ),&
       & ZCLO(:,:,:,1),PCLOMAD(:,:,:,1:1,JJ),PCLO(:,:,:,1:1,JJ),ISTPT,KSTSZ,PSTACK)
      CALL LASCAW_CLO(KFLEV,&
       & KPROMB,KST,KPROF,LLT_PHYS,PDLO(:,:,2),PDLOMAD(:,:,2),&
       & ZKHTURB(:,:,JJ),ZKHTURB(:,:,JJ),&
       & ZCLO(:,:,:,2),PCLOMAD(:,:,:,2:2,JJ),PCLO(:,:,:,2:2,JJ),ISTPT,KSTSZ,PSTACK)

    ELSE

      IF (JJ == 1) THEN
        LLT_SLHD(4)=.FALSE.
      ELSE
        LLT_SLHD(4)=LL3DTURB
      ENDIF

      ! * Calculation of PCLA, PCLAMAD and PCLASLD:
      CALL LASCAW_CLO(KFLEV,&
       & KPROMB,KST,KPROF,LLT_SLHD,PDLAT,PDLAMAD,&
       & PKAPPA,ZKHTURB(:,:,JJ),&
       & PCLA(:,:,:,JJ),PCLAMAD(:,:,:,JJ),PCLASLD(:,:,:,JJ),ISTPT,KSTSZ,PSTACK)

      ! * Calculation of PCLO and PCLOSLD for central lat 1 and 2
      ! (linear int. only for lat 0 and 3)
      CALL LASCAW_CLO(KFLEV,&
       & KPROMB,KST,KPROF,LLT_SLHD,PDLO(:,:,1),PDLOMAD(:,:,1),&
       & PKAPPA,ZKHTURB(:,:,JJ),&
       & PCLO(:,:,:,1:1,JJ),PCLOMAD(:,:,:,1:1,JJ),PCLOSLD(:,:,:,1:1,JJ),ISTPT,KSTSZ,PSTACK)
      CALL LASCAW_CLO(KFLEV,&
       & KPROMB,KST,KPROF,LLT_SLHD,PDLO(:,:,2),PDLOMAD(:,:,2),&
       & PKAPPA,ZKHTURB(:,:,JJ),&
       & PCLO(:,:,:,2:2,JJ),PCLOMAD(:,:,:,2:2,JJ),PCLOSLD(:,:,:,2:2,JJ),ISTPT,KSTSZ,PSTACK)

    ENDIF

  ENDDO

  ! * Calculation of PVINTW and PVINTWSLD:
  CALL LASCAW_VINTW(LDSLHDHEAT,&
   & KPROMB,KFLEV,KST,KPROF,LLCOMADV,LLT_SLHD(1:3),KLEV,&
   & PLEV,PDVER,PDVERMAD,PSTDDISW,PKAPPA,PKAPPAT,PVETA,ZVCUICO_,ZVSLD_,ZVSLDW_,&
   & PVINTW,PVINTWMAD,PVINTWSLD,PVINTWSLT,ISTPT,KSTSZ,PSTACK)

  IF (KWIS == 104) THEN
    DO JLEV=1,KFLEV
      ! * Calculation of PHVW:
      DO JROF=KST,KPROF
        ZDVER=PDVER(JROF,JLEV)
        PHVW(JROF,JLEV,1)=FHLO1(ZDVER)
        PHVW(JROF,JLEV,2)=FHLO2(ZDVER)
        PHVW(JROF,JLEV,3)=FHLO3(ZDVER)
        PHVW(JROF,JLEV,4)=FHLO4(ZDVER)
      ENDDO
      ! * Calculation of PVDERW:
      DO JROF=KST,KPROF
        IJ_ = MOD(JROF+1-KST,JPDUP)+1
        ILEVV=KLEV(JROF,JLEV)
        ZNUM=ZVETA_(IJ_,ILEVV+2)-ZVETA_(IJ_,ILEVV+1)
        ZDEN1=0.5_JPRB*(ZVETA_(IJ_,ILEVV+2)-ZVETA_(IJ_,ILEVV))
        ZDEN2=0.5_JPRB*(ZVETA_(IJ_,ILEVV+3)-ZVETA_(IJ_,ILEVV+1))
        IF(ILEVV >= 1.AND.ILEVV <= KFLEV-3) THEN
          PVDERW(JROF,JLEV,1,1)=0.5_JPRB*ZNUM/ZDEN1
          PVDERW(JROF,JLEV,2,1)=0.5_JPRB*ZNUM/ZDEN1
          PVDERW(JROF,JLEV,1,2)=0.5_JPRB*ZNUM/ZDEN2
          PVDERW(JROF,JLEV,2,2)=0.5_JPRB*ZNUM/ZDEN2
        ELSEIF (ILEVV == 0) THEN
          PVDERW(JROF,JLEV,1,1)=0.0_JPRB
          PVDERW(JROF,JLEV,2,1)=ZNUM/ZDEN1
          PVDERW(JROF,JLEV,1,2)=0.5_JPRB*ZNUM/ZDEN2
          PVDERW(JROF,JLEV,2,2)=0.5_JPRB*ZNUM/ZDEN2
        ELSEIF (ILEVV == KFLEV-2) THEN
          PVDERW(JROF,JLEV,1,1)=0.5_JPRB*ZNUM/ZDEN1
          PVDERW(JROF,JLEV,2,1)=0.5_JPRB*ZNUM/ZDEN1
          PVDERW(JROF,JLEV,1,2)=ZNUM/ZDEN2
          PVDERW(JROF,JLEV,2,2)=0.0_JPRB
        ENDIF
      ENDDO
    ENDDO
  ENDIF

  IF (KWIS == 105) THEN
    ! * Calculation of PVINTWS (weights for cubic spline interpolation).
    DO JLEV=1,KFLEV
      DO JROF=KST,KPROF
        IJ_ = MOD(JROF+1-KST,JPDUP)+1
        ILEVV=KLEV(JROF,JLEV)
        ZD2=PLEV(JROF,JLEV)-ZVETA_(IJ_,ILEVV+1)
        PVINTWS(JROF,JLEV,1)=RFVV(4,ILEVV  ,1)+ZD2*( RFVV(4,ILEVV  ,2) +&
         & ZD2*(RFVV(4,ILEVV   ,3) + ZD2*RFVV(4,ILEVV  ,4) ) )  
        PVINTWS(JROF,JLEV,2)=RFVV(3,ILEVV+1,1)+ZD2*( RFVV(3,ILEVV+1,2) +&
         & ZD2*( RFVV(3,ILEVV+1,3) + ZD2*RFVV(3,ILEVV+1,4) ) )  
        PVINTWS(JROF,JLEV,3)=RFVV(2,ILEVV+2,1)+ZD2*( RFVV(2,ILEVV+2,2) +&
         & ZD2*( RFVV(2,ILEVV+2,3) + ZD2*RFVV(2,ILEVV+2,4) ) )  
        PVINTWS(JROF,JLEV,4)=RFVV(1,ILEVV+3,1)+ZD2*( RFVV(1,ILEVV+3,2) +&
         & ZD2*( RFVV(1,ILEVV+3,3) + ZD2*RFVV(1,ILEVV+3,4) ) )  
      ENDDO
    ENDDO
  ENDIF

ENDIF

!     ----------------------------------------------------------------

!*       2.    2D MODEL AND CASES IN THE 3D MODEL WHERE ONLY
!              2D INTERPOLATIONS ARE NEEDED.
!              ---------------------------------------------

!        2.01  Coordinates and weights for bilinear interpolations.

IF (KWIS == 201) THEN

  DO JLEV=1,KFLEV
    IF (KHOR == 0) ILEV=JLEV
    IF (KHOR == 1) ILEV=KFLDN

    ! * Calculation of linear weights, KL0.
!CDIR NODEP
!DIR$ PREFERVECTOR
    DO JROF=KST,KPROF

      IJ_ = MOD(JROF+1-KST,JPDUP)+1

      ILAG=INT(PLAT(JROF,JLEV))-1
      ILA=ILAG - YDSL%NFRSTLOFF
      PDLAT(JROF,JLEV)=PLAT(JROF,JLEV)-REAL(ILAG+1,JPRB)
      ILO=INT(PLON(JROF,JLEV))-1
      PDLO(JROF,JLEV,1)=PLON(JROF,JLEV)-REAL(ILO+1,JPRB)
      PDLO(JROF,JLEV,2)=PDLO(JROF,JLEV,1)

      ILA1=ILA+1
      ILA1=MIN(MAX(ILA1,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA2=ILA+2
      ILA2=MIN(MAX(ILA2,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILO =MIN(MAX(ILO,IDLUN1),IDLUX1)
      KL0(JROF,JLEV,1)=IADDR_(IJ_,ILA1)+YDSL%NSLEXT(ILO,ILA1)+YDSL%NASLB1*ILEV
      KL0(JROF,JLEV,2)=IADDR_(IJ_,ILA2)+YDSL%NSLEXT(ILO,ILA2)+YDSL%NASLB1*ILEV
      KDEP(JROF,JLEV)= (ILA1 -ILA2 +1)*7
    ENDDO

    ! * Mask calculation for on-demand communications:
    IF(NPROC > 1.AND.YDSL%LSLONDEM_ACTIVE)THEN
!CDIR NODEP
!DIR$ PREFERVECTOR
      DO JROF=KST,KPROF
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+3)=1
      ENDDO
    ENDIF

  ENDDO

ENDIF

!        2.03  Coordinates and weights for 12 points interpolations.

IF (KWIS == 203) THEN

  DO JLEV=1,KFLEV
    IF (KHOR == 0) ILEV=JLEV
    IF (KHOR == 1) ILEV=KFLDN

    ! * Calculation of linear weights, KL0.
!CDIR NODEP
!DIR$ PREFERVECTOR
    DO JROF=KST,KPROF

      IJ_ = MOD(JROF+1-KST,JPDUP)+1

      ILAG=INT(PLAT(JROF,JLEV))-1
      ILA=ILAG - YDSL%NFRSTLOFF

      ! meridional interpolation: linear weights (regular grid)
      ! general case 
      PDLAT(JROF,JLEV)=PLAT(JROF,JLEV)-REAL(ILAG+1,JPRB)

      ! COMAD meridional interpolation 
      IF (LLCOMADH) THEN
        PDLAMAD(JROF,JLEV)=PDLAT(JROF,JLEV)*PSTDDISV(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISV(JROF,JLEV))
      ELSE
        PDLAMAD(JROF,JLEV)=PDLAT(JROF,JLEV)
      ENDIF

      ! zonal interpolation: linear weights for 4 lat. lines
      ! as the grid is regular in the zonal direction,
      ! the cubic weight computation does not need 
      ! other input than linear weights (LASCAW_CLO)
      ! general case
      ILO =INT(PLON(JROF,JLEV))-1
      PDLO(JROF,JLEV,0)=PLON(JROF,JLEV)-REAL(ILO+1,JPRB)
      PDLO(JROF,JLEV,1)=PDLO(JROF,JLEV,0)
      PDLO(JROF,JLEV,2)=PDLO(JROF,JLEV,0)
      PDLO(JROF,JLEV,3)=PDLO(JROF,JLEV,0)

      ! COMAD zonal interpolation 
      IF (LLCOMADH) THEN
        PDLOMAD(JROF,JLEV,0)=PDLO(JROF,JLEV,0)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
        PDLOMAD(JROF,JLEV,1)=PDLO(JROF,JLEV,1)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
        PDLOMAD(JROF,JLEV,2)=PDLO(JROF,JLEV,2)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
        PDLOMAD(JROF,JLEV,3)=PDLO(JROF,JLEV,3)*PSTDDISU(JROF,JLEV) + 0.5_JPRB * (1._JPRB-PSTDDISU(JROF,JLEV))
      ELSE
        PDLOMAD(JROF,JLEV,0)= PDLO(JROF,JLEV,0)
        PDLOMAD(JROF,JLEV,1)= PDLO(JROF,JLEV,1)
        PDLOMAD(JROF,JLEV,2)= PDLO(JROF,JLEV,2)
        PDLOMAD(JROF,JLEV,3)= PDLO(JROF,JLEV,3)
      ENDIF

      ILA1=ILA+1
      ILA1=MIN(MAX(ILA1,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA2=ILA+2
      ILA2=MIN(MAX(ILA2,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA3=ILA+3
      ILA3=MIN(MAX(ILA3,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILA =MIN(MAX(ILA ,YDSL%NDGSAH),YDSL%NDGUXG-YDSL%NFRSTLOFF)
      ILO =MIN(MAX(ILO,IDLUN1),IDLUX1)
      KL0(JROF,JLEV,0)=IADDR_(IJ_,ILA )+YDSL%NSLEXT(ILO,ILA)
      KL0(JROF,JLEV,1)=IADDR_(IJ_,ILA1)+YDSL%NSLEXT(ILO,ILA1)
      KL0(JROF,JLEV,2)=IADDR_(IJ_,ILA2)+YDSL%NSLEXT(ILO,ILA2)
      KL0(JROF,JLEV,3)=IADDR_(IJ_,ILA3)+YDSL%NSLEXT(ILO,ILA3)
      KDEP(JROF,JLEV)=(ILA  -ILA1 +1) + (ILA1 -ILA2 +1)*2&
       &              + (ILA2 -ILA3 +1)*4

      KL0(JROF,JLEV,0)=KL0(JROF,JLEV,0)+YDSL%NASLB1*ILEV
      KL0(JROF,JLEV,1)=KL0(JROF,JLEV,1)+YDSL%NASLB1*ILEV
      KL0(JROF,JLEV,2)=KL0(JROF,JLEV,2)+YDSL%NASLB1*ILEV
      KL0(JROF,JLEV,3)=KL0(JROF,JLEV,3)+YDSL%NASLB1*ILEV
    ENDDO

    ! * Mask calculation for on-demand communications:
    IF(NPROC > 1.AND.YDSL%LSLONDEM_ACTIVE)THEN
!CDIR NODEP
!DIR$ PREFERVECTOR
      DO JROF=KST,KPROF
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,0)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,1)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,2)+3)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)  )=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+1)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+2)=1
        YDSL%MASK_SL2(KL0(JROF,JLEV,3)+3)=1
      ENDDO
    ENDIF

  ENDDO

  ! * Calculation of PCLA and PCLASLD:
  CALL LASCAW_CLO(KFLEV,&
   & KPROMB,KST,KPROF,LLT_SLHD,PDLAT,PDLAMAD,&
   & PKAPPA,ZKHTURB(:,:,1),&
   & PCLA(:,:,:,1),PCLAMAD(:,:,:,1),PCLASLD(:,:,:,1),ISTPT,KSTSZ,PSTACK)

  ! * Calculation of PCLO and PCLOSLD:
  CALL LASCAW_CLO(KFLEV,&
   & KPROMB,KST,KPROF,LLT_SLHD,PDLO(:,:,1),PDLOMAD(:,:,1),&
   & PKAPPA,ZKHTURB(:,:,1),&
   & PCLO(:,:,:,1:1,1),PCLOMAD(:,:,:,1:1,1),PCLOSLD(:,:,:,1:1,1),ISTPT,KSTSZ,PSTACK)
  CALL LASCAW_CLO(KFLEV,&
   & KPROMB,KST,KPROF,LLT_SLHD,PDLO(:,:,2),PDLOMAD(:,:,2),&
   & PKAPPA,ZKHTURB(:,:,1),&
   & PCLO(:,:,:,2:2,1),PCLOMAD(:,:,:,2:2,1),PCLOSLD(:,:,:,2:2,1),ISTPT,KSTSZ,PSTACK)

ENDIF

!     ------------------------------------------------------------------
END ASSOCIATE
END SUBROUTINE ELASCAW
