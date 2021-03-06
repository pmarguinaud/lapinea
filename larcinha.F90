!$acc routine(LARCINHA) seq
SUBROUTINE LARCINHA(&
 ! --- INPUT ---------------------------------------------------------
 & YDGEOMETRY, YDML_DYN,KST,KPROF,YDSL,KHVI,KSTABUF,KTIP,KROT,PLSDEPI,&
 & KIBL,&
 & PSCO,PLEV,PKAPPA,PKAPPAT,PKAPPAM,PKAPPAH,&
 & PSTDDISU,PSTDDISV,PSTDDISW,&
 ! --- OUTPUT --------------------------------------------------------
 & KL0H,PLSCAWH,PRSCAWH,KSTPT,KSTSZ,PSTACK)


#include "temp.h"

!**** *LARCINHA  -  semi-LAgrangian scheme:(Trajectory)
!                 Research of the Coordinates (of the medium or origin
!                 point) and INterpolations.

!     Purpose.
!     --------
!       Computes the longitude and latitude of the interpolation
!       point from its cartesian coordinates.
!       Then computes the vector displacement matrix
!                        I po pq I
!                        I       I
!                        I-pq po I
!       from the interpolation point to the grid point.
!       At last determines the interpolation grid:
!       - computation of the latitude and the longitude of the
!         point situated at the upper left corner of the 16 points
!         square, and of the interpolation point.

!       Version of LARCINA used for half-level quantities.

!**   Interface.
!     ----------
!        *CALL* *LARCINHA(......)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST     - first element of arrays where computations are performed.
!          KPROF   - depth of work.
!          YDSL    - SL_STRUCT definition
!          KHVI    - 1/0: Cubic Hermite vertical interpolations are needed/not needed.
!          KSTABUF - for a latitude IGL, KSTABUF(IGL) is the
!                    address of the element corresponding to
!                    (ILON=1,IGL) in the NPROMA arrays.
!          KTIP    - 1: interpolation grid for wind used in the traj research.
!                    2: interpolation grid for origin point (RHS of eqns).
!                    3: interpolation grid for origin point, U and V only
!                       (used in adjoint of semi-Lagrangian scheme). 
!          KROT    - KROT=1: computation of the elements po and pq
!                    of the wind displacement matrix.
!                    KROT=0: no computation.
!          PLSDEPI - (Number of points by latitude) / (2 * PI) .
!          KIBL    - index into YRGSGEOM/YRCSGEOM instances in YDGEOMETRY
!          PSCO    - information about geographic position of interpol. point.
!          PLEV    - vertical coordinate of the interpolation point at full levels.
!          PKAPPA  - kappa function ("coefficient of SLHD") based on the
!                    rescaled horizontal deformation of the flow evaluated
!                    at instant "t" for the final point F (full level)
!          PKAPPAT - kappa function for temperature (full level)
!          PKAPPAM - horizontal exchange coefficient for momentum in 3D turb.
!          PKAPPAH - horizontal exchange coefficient for heat in 3D turb.
!          PSTDDISU- zonal correction coef. for COMAD
!          PSTDDISV- meridional correction coef. for COMAD
!          PSTDDISW- vertical correction coef. for COMAD

!        OUTPUT:
!          KL0H      - index of the four western points
!                      of the 16 points interpolation grid.
!          PLSCAWH   - linear weights (distances) for interpolations.
!          PRSCAWH   - non-linear weights for interpolations.

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
!        C. SMITH, mirrors the LAIDEP -> ELASCAW calling sequence.
!        K. YESSAD, after the subroutine LAGINT0 written by Maurice IMBARD
!        Alain CRAPLET and Michel ROCHAS  METEO FRANCE/EERM/CRMD

!     Modifications.
!     --------------
!  Original : AUGUST 2002.
!  Modifications:
!   Modified 03-05-01 by A. Bogatchev: add arg in call elascaw
!   M.Hamrud      01-Oct-2003 CY28 Cleaning
!   F. Vana  2004-Nov: new argument PVINTDSH
!   K. Yessad and J. Vivoda (Mar 2005): move calculation of
!    ZSINCOH, ZCOSCOH, ZLEVH from LAIDEPH, and adapt the code
!    for spherical geometry also.
!   F.Vana  09-Jan-2007 new argument to ELASCAW
!   K. Yessad 07-03-2007: Remove useless (gw)_surf interpol. in NH+LGWADV.
!   F. Vana 28-Aug-2007 removing argument PVINTDSH
!   N. Wedi and K. Yessad (Jan 2008): different dev for NH model and PC scheme
!   30-Jun-2008 J. Masek   Dataflow for new SLHD interpolators.
!   K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!   K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!   K. Yessad (Aug 2009): use RIPI, RSLD
!   K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!   F. Vana 21-Feb-2011: horizontal turbulence
!   G.Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!   G.Mozdzynski (Feb 2011): OOPS cleaning, use of derived types TGSGEOM, TCSGEOM and TCSGLEG
!   G. Mozdzynski (May 2012): further cleaning
!   F. Vana  13-Feb-2014  Distinguish between heat and momentum SLHD.
!   T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!   S. Malardel (Nov 2013): COMAD weights for SL interpolations
!   B. Bochenek (Apr 2015): Phasing: update
!   K. Yessad (March 2017): simplify level numbering in interpolator.
!     ------------------------------------------------------------------

USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE
USE GEOMETRY_MOD       , ONLY : GEOMETRY
USE PARKIND1           , ONLY : JPIM, JPRB

USE YOMCST             , ONLY : RPI
USE YOMCT0             , ONLY : LRPLANE
USE YOMDYNA            , ONLY : LSLHD, LSLHDQUAD, LSLHD_OLD, L3DTURB, LCOMAD, LCOMADH, LCOMADV
USE YOMLUN             , ONLY : NULOUT
USE EINT_MOD           , ONLY : SL_STRUCT

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(MODEL_DYNAMICS_TYPE),INTENT(IN):: YDML_DYN
INTEGER(KIND=JPIM),INTENT(IN)    :: KST 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
TYPE(SL_STRUCT)   ,INTENT(INOUT) :: YDSL
INTEGER(KIND=JPIM),INTENT(IN)    :: KHVI 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTABUF(YDSL%NDGSAH:YDSL%NDGENH) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTIP 
INTEGER(KIND=JPIM),INTENT(IN)    :: KROT 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSDEPI(YDSL%NDGSAH:YDSL%NDGENH)
INTEGER(KIND=JPIM),INTENT(IN)    :: KIBL
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTSCO%NDIM) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPA(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAT(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAM(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAH(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISU(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KL0H(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLSCAWH(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTLSCAWH%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRSCAWH(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTRSCAWH%NDIM)

INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL   (KIND=JPRB),INTENT(INOUT) :: PSTACK (KSTSZ)
!     ------------------------------------------------------------------

! Half levels lbar=0 to nflevg-1 are numbered 1 to nflevg in these Z.._H arrays, for (E)LASCAW.

temp (REAL(KIND=JPRB), ZKAPPA_H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZKAPPAT_H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZKAPPAM_H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZKAPPAH_H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZSTDDISU_H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZSTDDISV_H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (REAL(KIND=JPRB), ZSTDDISW_H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
REAL(KIND=JPRB)    :: ZVETA_H(0:YDGEOMETRY%YRDIMV%NFLEVG+1)


temp (INTEGER(KIND=JPIM), ILEVH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))

temp (INTEGER(KIND=JPIM), ILH0H, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3))


temp (INTEGER(KIND=JPIM), IDEP, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
INTEGER(KIND=JPIM) :: ISPLTHOI, IDIMK
INTEGER(KIND=JPIM) :: IHOR, IWIS
INTEGER(KIND=JPIM) :: JLEV, JROF

LOGICAL :: LL3DTURB


temp (REAL(KIND=JPRB), ZVDERWH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,2*KHVI,2*KHVI))

temp (REAL(KIND=JPRB), ZPHVWH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,4*KHVI))

temp (REAL(KIND=JPRB), ZVINTWS, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,1:4))
 
temp (REAL(KIND=JPRB), ZCOSCOH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
 
temp (REAL(KIND=JPRB), ZSINCOH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
 
temp (REAL(KIND=JPRB), ZLONH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
 
temp (REAL(KIND=JPRB), ZLATH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
 
temp (REAL(KIND=JPRB), ZLEVH, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG))
REAL(KIND=JPRB)    :: ZWEI(YDGEOMETRY%YRDIMV%NFLEVG) 
REAL(KIND=JPRB)    :: ZVETAON,ZVETAOX,ZDEL_LON
REAL(KIND=JPRB)    :: ZDSTRET,ZDEPI,ZPIS2
 


temp (REAL(KIND=JPRB), ZCCO, (YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTCCO%NDIM))
!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "elascaw.intfb.h"

!     ------------------------------------------------------------------

init_stack ()

alloc (ZKAPPA_H)
alloc (ZKAPPAT_H)
alloc (ZKAPPAM_H)
alloc (ZKAPPAH_H)
alloc (ZSTDDISU_H)
alloc (ZSTDDISV_H)
alloc (ZSTDDISW_H)
alloc (ILEVH)
alloc (ILH0H)
alloc (IDEP)
alloc (ZVDERWH)
alloc (ZPHVWH)
alloc (ZVINTWS)
alloc (ZCOSCOH)
alloc (ZSINCOH)
alloc (ZLONH)
alloc (ZLATH)
alloc (ZLEVH)
alloc (ZCCO)



ASSOCIATE(YDDIM=>YDGEOMETRY%YRDIM, &
 & YDDIMV=>YDGEOMETRY%YRDIMV,YDGEM=>YDGEOMETRY%YRGEM, YDMP=>YDGEOMETRY%YRMP,  YDHSLMER=>YDGEOMETRY%YRHSLMER, &
 & YDCSGLEG=>YDGEOMETRY%YRCSGLEG, YDVAB=>YDGEOMETRY%YRVAB, YDVETA=>YDGEOMETRY%YRVETA,  YDVFE=>YDGEOMETRY%YRVFE,  &
 & YDVSLETA=>YDGEOMETRY%YRVSLETA, YDVSPLIP=>YDGEOMETRY%YRVSPLIP, YDSTA=>YDGEOMETRY%YRSTA, &
 & YDLAP=>YDGEOMETRY%YRLAP, &
 & YDCSGEOM=>YDGEOMETRY%YRCSGEOM(KIBL), YDGSGEOM=>YDGEOMETRY%YRGSGEOM(KIBL),  YDSPGEOM=>YDGEOMETRY%YSPGEOM,  &
 & YDDYN=>YDML_DYN%YRDYN, YDTCCO=>YDML_DYN%YYTCCO, YDTSCO=>YDML_DYN%YYTSCO, YDTLSCAWH=>YDML_DYN%YYTLSCAWH, &
 & YDTRSCAWH=>YDML_DYN%YYTRSCAWH)
ASSOCIATE(NPROMA=>YDDIM%NPROMA, &
 & NFLEN=>YDDIMV%NFLEN, NFLEVG=>YDDIMV%NFLEVG, NFLSA=>YDDIMV%NFLSA, &
 & NSLDIMK=>YDDYN%NSLDIMK, NSPLTHOI=>YDDYN%NSPLTHOI, LSLHDHEAT=>YDDYN%LSLHDHEAT,&
 & NSTTYP=>YDGEM%NSTTYP, R4JP=>YDGEM%R4JP, RC2M1=>YDGEM%RC2M1, &
 & RC2P1=>YDGEM%RC2P1, RLOCEN=>YDGEM%RLOCEN, RMUCEN=>YDGEM%RMUCEN, &
 & RSTRET=>YDGEM%RSTRET, &
 & R3DTW=>YDHSLMER%R3DTW, RIPI=>YDHSLMER%RIPI, RSLD=>YDHSLMER%RSLD, &
 & RSLDW=>YDHSLMER%RSLDW, &
 & NRLEVX=>YDVSLETA%NRLEVX, NVAUTH=>YDVSLETA%NVAUTH, VCUICOH=>YDVSLETA%VCUICOH, &
 & VRLEVX=>YDVSLETA%VRLEVX, VSLDH=>YDVSLETA%VSLDH, VSLDWH=>YDVSLETA%VSLDWH)

!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS.
!              ----------------------------

IHOR=1

ZDSTRET=2.0_JPRB*RSTRET
ZDEPI=2.0_JPRB*RPI
ZPIS2=0.5_JPRB*RPI

! * Input variable IWIS for LASCAW.
IF (KTIP == 1) THEN
! * trajectory research.
  CALL ABOR1('LARCINHA: interpolation for trajectory search not coded.')
  IWIS=101
ELSEIF (KTIP == 2) THEN
! * origin point interpolations.
  IWIS=103+KHVI
ELSEIF (KTIP == 3) THEN
  IWIS=103
ENDIF

IF ( IWIS /= 103 ) THEN
!  CALL ABOR1('LARCINHA: only cubic Lagrange interpolation is coded.')
  WRITE(NULOUT,*) 'IWIS = ', IWIS
  IWIS = 103
ENDIF

ZVETAON=YDVETA%VETAH(0)
ZVETAOX=YDVETA%VETAH(NFLEVG)

ISPLTHOI=NSPLTHOI
LL3DTURB=L3DTURB
IDIMK=NSLDIMK

!     ------------------------------------------------------------------

!*       2.    COMPUTE INTERPOLATION WEIGHTS AND GRID.
!              ---------------------------------------

! This part requires calculation of some half-level quantities,
! with a specific half-level numbering consistent with the (E)LASCAW one:
! in these Z.._H arrays, jlev=1 stands for the top value.
! Numbering is 1 to nflevg (and not the "standard" half-level numbering 0 to nflevg-1).
! This is due to the fact that interpolations are required at the top but not at the bottom.

! * compute half-level KAPPA from full-level ones:
IF (LSLHD) THEN
  ZKAPPA_H(KST:KPROF,1)=PKAPPA(KST:KPROF,1)
  DO JLEV=2,NFLEVG
    DO JROF=KST,KPROF
      ZKAPPA_H(JROF,JLEV)=0.5_JPRB*(ABS(PKAPPA(JROF,JLEV-1))+ABS(PKAPPA(JROF,JLEV)))
      ZKAPPA_H(JROF,JLEV)=SIGN(ZKAPPA_H(JROF,JLEV),PKAPPA(JROF,JLEV))
    ENDDO
  ENDDO
ENDIF
! * compute half-level KAPPAT from full-level ones:
IF (LSLHD.AND.LSLHDHEAT) THEN
  ZKAPPAT_H(KST:KPROF,1)=PKAPPAT(KST:KPROF,1)
  DO JLEV=2,NFLEVG
    DO JROF=KST,KPROF
      ZKAPPAT_H(JROF,JLEV)=0.5_JPRB*(ABS(PKAPPAT(JROF,JLEV-1))+ABS(PKAPPAT(JROF,JLEV)))
      ZKAPPAT_H(JROF,JLEV)=SIGN(ZKAPPAT_H(JROF,JLEV),PKAPPAT(JROF,JLEV))
    ENDDO
  ENDDO
ENDIF
! * compute half-level KAPPAM and KAPPAH from full-level ones:
IF (LL3DTURB) THEN
  ZKAPPAM_H(KST:KPROF,1)=PKAPPAM(KST:KPROF,1)
  ZKAPPAH_H(KST:KPROF,1)=PKAPPAH(KST:KPROF,1)
  DO JLEV=2,NFLEVG
    DO JROF=KST,KPROF
      ZKAPPAM_H(JROF,JLEV)=0.5_JPRB*(PKAPPAM(JROF,JLEV-1)+PKAPPAM(JROF,JLEV))
      ZKAPPAH_H(JROF,JLEV)=0.5_JPRB*(PKAPPAH(JROF,JLEV-1)+PKAPPAH(JROF,JLEV))
    ENDDO
  ENDDO
ENDIF
! * compute half-level STDDIS(U,V,W) from full-level ones:
IF (LCOMAD) THEN
  ZSTDDISU_H(KST:KPROF,1)=PSTDDISU(KST:KPROF,1)
  ZSTDDISV_H(KST:KPROF,1)=PSTDDISV(KST:KPROF,1)
  ZSTDDISW_H(KST:KPROF,1)=PSTDDISW(KST:KPROF,1)
  DO JLEV=2,NFLEVG
    DO JROF=KST,KPROF
      ZSTDDISU_H(JROF,JLEV)=0.5_JPRB*(PSTDDISU(JROF,JLEV-1)+PSTDDISU(JROF,JLEV))
      ZSTDDISV_H(JROF,JLEV)=0.5_JPRB*(PSTDDISV(JROF,JLEV-1)+PSTDDISV(JROF,JLEV))
      ZSTDDISW_H(JROF,JLEV)=0.5_JPRB*(PSTDDISW(JROF,JLEV-1)+PSTDDISW(JROF,JLEV))
    ENDDO
  ENDDO
ENDIF
! * store half-level ETA in ZVETA_H:
ZVETA_H(0)=0.0_JPRB
ZVETA_H(NFLEVG+1)=1.0_JPRB
DO JLEV=1,NFLEVG
  ZVETA_H(JLEV)=YDVETA%VETAH(JLEV-1)
ENDDO

IF (LRPLANE) THEN

  IF (KROT == 1) THEN
    CALL ABOR1('LARCINHA: ELARCHE not coded for LGWADV=.T.')
    ! CALL ELARCHE(......)
  ENDIF

  ! * compute half level ZCOSCOH, ZSINCOH, ZLEVH:

  ! Obtain the departure points for half-level grid-points from the coordinates
  ! of full-level departure points.
  ! Notes:
  ! a) On the upper and lower boundaries there is no vertical displacement:
  !    that is, a particle initially on the upper or lower boundary remains
  !    within that boundary for all time.
  !    The horizontal displacement within these boundary surfaces is 
  !    obtained by vertical extrapolation of displacements at full-levels.
  !    The safest extrapolation is used: constant extrapolation.

  ! b) For interior half-levels, the departure point coordinates are
  !    obtained by averaging of the departure point coordinates on the
  !    two neighbouring full-levels. This averaging could be done using
  !    linear interpolation in eta or any other valid vertical coordinate.
  !    Logarithmic pressure thickness may offer desirable conservation
  !    properties if used for this purpose.
  !    Currently this average is done using a linear interpolation in eta.
  DO JLEV=2,NFLEVG
    ZWEI(JLEV)=(YDVETA%VETAH(JLEV-1)-YDVETA%VETAF(JLEV-1))/(YDVETA%VETAF(JLEV)-YDVETA%VETAF(JLEV-1))
  ENDDO

  DO JROF=KST,KPROF
    ! Horizontal coordinates of departure points for grid-points on upper
    ! and lower boundaries.
    ZSINCOH(JROF,1) = PSCO(JROF,1,YDTSCO%M_SINCO)
    ZCOSCOH(JROF,1) = PSCO(JROF,1,YDTSCO%M_COSCO)
    ! Vertical coordinates reflecting zero normal displacement at boundaries
    ZLEVH(JROF,1) = 0.0_JPRB
  ENDDO

  DO JLEV=2,NFLEVG
    DO JROF=KST,KPROF

      ! * computations on horizontal plans.
      ZSINCOH(JROF,JLEV) = PSCO(JROF,JLEV-1,YDTSCO%M_SINCO)&
       & + ZWEI(JLEV) * ( PSCO(JROF,JLEV,YDTSCO%M_SINCO) - PSCO(JROF,JLEV-1,YDTSCO%M_SINCO) )  
      ZCOSCOH(JROF,JLEV) = PSCO(JROF,JLEV-1,YDTSCO%M_COSCO)&
       & + ZWEI(JLEV) * ( PSCO(JROF,JLEV,YDTSCO%M_COSCO) - PSCO(JROF,JLEV-1,YDTSCO%M_COSCO) )  

      ! * computations on a vertical.
      ZLEVH(JROF,JLEV) = PLEV(JROF,JLEV-1)&
       & + ZWEI(JLEV) * ( PLEV(JROF,JLEV) - PLEV(JROF,JLEV-1) )
      ZLEVH(JROF,JLEV)=MAX(ZVETAON,MIN(ZVETAOX,ZLEVH(JROF,JLEV)))

    ENDDO
  ENDDO

  CALL ELASCAW(&
   ! --- INPUT ----------------------------------------------------------------
   & YDVSPLIP,YDDYN%LSLHDHEAT,YDSL,NPROMA,IDIMK,KST,KPROF,NFLEVG,&
   & NFLSA,KSTABUF,IWIS,IHOR,KHVI,&
   & LSLHD,LSLHDQUAD,LSLHD_OLD,LL3DTURB,&
   & LCOMAD,LCOMADH,LCOMADV,ISPLTHOI,&
   & ZCOSCOH,ZSINCOH,ZLEVH,&
   & ZVETA_H,NVAUTH,&
   & VCUICOH,VSLDH,VSLDWH,NRLEVX,VRLEVX,&
   & ZKAPPA_H,ZKAPPAT_H,ZKAPPAM_H,ZKAPPAH_H,ZSTDDISU_H,ZSTDDISV_H,ZSTDDISW_H,&
   ! --- OUTPUT ---------------------------------------------------------------
   & PLSCAWH(1,1,YDTLSCAWH%M_WDLAT),PLSCAWH(1,1,YDTLSCAWH%M_WDLAMAD),&
   & PRSCAWH(1,1,YDTRSCAWH%M_WCLA(1)),PRSCAWH(1,1,YDTRSCAWH%M_WCLASLD(1)),PRSCAWH(1,1,YDTRSCAWH%M_WCLASLT),&
   & PRSCAWH(1,1,YDTRSCAWH%M_WCLAMAD(1)),&
   & PLSCAWH(1,1,YDTLSCAWH%M_WDLO),PLSCAWH(1,1,YDTLSCAWH%M_WDLOMAD),&
   & PRSCAWH(1,1,YDTRSCAWH%M_WCLO(1)),PRSCAWH(1,1,YDTRSCAWH%M_WCLOSLD(1)),PRSCAWH(1,1,YDTRSCAWH%M_WCLOSLT),&
   & PRSCAWH(1,1,YDTRSCAWH%M_WCLOMAD(1)),&
   & KL0H,ILH0H,ILEVH,&
   & PLSCAWH(1,1,YDTLSCAWH%M_WDVER),PLSCAWH(1,1,YDTLSCAWH%M_WDVERMAD),&
   & PRSCAWH(1,1,YDTRSCAWH%M_WVINTW),PRSCAWH(1,1,YDTRSCAWH%M_WVINTWSLD),PRSCAWH(1,1,YDTRSCAWH%M_WVINTWSLT),&
   & PRSCAWH(1,1,YDTRSCAWH%M_WVINTWMAD),ZVINTWS,&
   & ZVDERWH,ZPHVWH,&
   & IDEP,ISTPT,KSTSZ,PSTACK)

ELSE

   CALL ABOR1 ('UNEXPECTED CASE')

ENDIF

!     ------------------------------------------------------------------

END ASSOCIATE
END ASSOCIATE
END SUBROUTINE LARCINHA

