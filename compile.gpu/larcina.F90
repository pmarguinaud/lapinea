#ifdef RS6K
@PROCESS NOCHECK
#endif
!$acc routine (LARCINA) vector
SUBROUTINE LARCINA(&
 ! --- INPUT ---------------------------------------------------------
 & YDGEOMETRY, YDML_DYN,KST,KPROF,YDSL,KHVI,KSTABUF,LDFINDVSEP,LDSLHD,LDSLHDQUAD,LDINTV,&
 & KTIP,KROT,LDPLANE,PLSDEPI,&
 & KIBL,&
 & PSCO,PLEV,&
 & PKAPPA,PKAPPAT,PKAPPAM,PKAPPAH,PSTDDISU,PSTDDISV,PSTDDISW,PURL0,PVRL0,PWRL0,&
 ! --- INPUT/OUTPUT --------------------------------------------------
 & KVSEPC,KVSEPL,&
 ! --- OUTPUT --------------------------------------------------------
 & PCCO,PUF0,PVF0,PWF0,PWFSM,KL0,KLH0,KLEV,PLSCAW,PRSCAW,KDEP, YDSTACK)


!**** *LARCINA  -  semi-LAgrangian scheme:(Trajectory)
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
!       - optionally, interpolates "(a/rs)*wind" field for next iteration 
!         of the algorithm to find the medium/departure point.

!**   Interface.
!     ----------
!        *CALL* *LARCINA(......)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KST       - first element of arrays where computations are performed.
!          KPROF     - depth of work.
!          YDSL      - SL_STRUCT definition
!          KHVI      - 1/0: Cubic Hermite vertical interpolations are needed/not needed.
!          KSTABUF   - for a latitude IGL, KSTABUF(IGL) is the
!                      address of the element corresponding to
!                      (ILON=1,IGL) in the NPROMA arrays.
!          LDFINDVSEP - compute KVSEPC, KVSEPL if .TRUE.
!          LDSLHD    - key activating SLHD weights precomputation
!          LDSLHDQUAD - key activating quadratic weights precomputation
!          LDINTV    - if .TRUE., interpolate "(a/rs)*wind" components.
!          KTIP      - 1: interpolation grid for wind used in the traj research.
!                      2: interpolation grid for origin point (RHS of eqns).
!                      3: interpolation grid for origin point, U and V only
!                         (used in adjoint of semi-Lagrangian scheme). 
!          KROT      - KROT=1: computation of the elements po and pq
!                      of the wind displacement matrix.
!                      KROT=0: no computation.
!          LDPLANE   - switch: .T. = plane geometry; .F. = spherical geometry.
!          PLSDEPI   - (Number of points by latitude) / (2 * PI) .
!          KIBL      - index into YRGSGEOM/YRCSGEOM instances in YDGEOMETRY
!          PSCO      - information about geographic position of interpol. point.
!          PLEV      - vertical coordinate of the interpolation point.
!          PKAPPA    - kappa function ("coefficient of SLHD") based on the
!                      rescaled horizontal deformation of the flow evaluated
!                      at instant "t" for the final point F
!          PKAPPAT   - kappa functuion for T
!          PKAPPAM   - horizontal exchange coefficient for momentum in 3D turb.
!          PKAPPAH   - horizontal exchange coefficient for heat in 3D turb.
!          PSTDDISU  - zonal correction coef. for COMAD
!          PSTDDISV  - meridional correction coef. for COMAD
!          PSTDDISW  - vertical correction coef. for COMAD
!          PURL0     - U-component of "(a/rs)*wind".
!          PVRL0     - V-component of "(a/rs)*wind".
!          PWRL0     - "etadot".

!        INPUT/OUTPUT:
!          KVSEPC    - vertical separation (used in S/L adjoint, cubic interp.)
!          KVSEPL    - vertical separation (used in S/L adjoint, linear interp.)

!        OUTPUT:
!          PCCO      - information about comput. space position of interpol. point.
!          PUF0      - Interpolated U-"(a/rs)*wind".
!          PVF0      - Interpolated V-"(a/rs)*wind".
!          PWF0      - Interpolated "etadot".
!          PWFSM     - Smoothly interpolated "etadot".
!          KL0       - index of the four western points
!                      of the 16 points interpolation grid.
!          KLH0      - second value of index of the four western points
!                      of the 16 points interpolation grid if needed.
!          KLEV      - lower level of the vertical interpolation
!                      grid needed for vertical interpolations.
!          PLSCAW    - linear weights (distances) for interpolations.
!          PRSCAW    - non-linear weights for interpolations.
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
!      K. YESSAD, after the subroutine LAGINT0 written by Maurice IMBARD
!      Alain CRAPLET and Michel ROCHAS  METEO FRANCE/EERM/CRMD
!      Original : JUNE 1991.

!     Modifications.
!     --------------
!      F. Vana 08-Jan-2007 new argument KDEP for LAM adjoint
!      F. Vana 28-Aug-2007 cleaning of PVINTDS for (E)LASCAW
!      30-Jun-2008 J. Masek   Dataflow for new SLHD interpolators.
!      K. Yessad Nov 2008: rationalisation of dummy argument interfaces
!      K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!      K. Yessad (Aug 2009): use RIPI, RSLD
!      K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!      F. Vana 21-Feb-2011: horizontal turbulence
!      G. Mozdzynski (Jan 2011): OOPS cleaning, use of derived type SL_STRUCT
!      G. Mozdzynski (Feb 2011): OOPS cleaning, use of derived types TGSGEOM, TCSGEOM and TCSGLEG
!      G. Mozdzynski (May 2012): further cleaning
!      F. Vana  13-Feb-2014: kappaT for heat variables
!      T. Wilhelmsson (Sept 2013) Geometry and setup refactoring.
!      S. Malardel (Nov 2013): COMAD weights for SL interpolations
!      B. Bochenek (Apr 2015): Phasing: update
!      K. Yessad (March 2017): simplify level numbering in interpolator.
!     ------------------------------------------------------------------

USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE
USE GEOMETRY_MOD       , ONLY : GEOMETRY
USE PARKIND1           , ONLY : JPIM, JPRB

USE YOMCST             , ONLY : RPI
USE YOMDYNA            , ONLY : LVSPLIP, L3DTURB, LSLHD_OLD, LCOMAD, LCOMADH, LCOMADV
USE EINT_MOD           , ONLY : SL_STRUCT
USE STACK_MOD
#include "stack.h"

!     ------------------------------------------------------------------

IMPLICIT NONE

TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY
TYPE(MODEL_DYNAMICS_TYPE),INTENT(IN):: YDML_DYN
INTEGER(KIND=JPIM),INTENT(IN)    :: KHVI
INTEGER(KIND=JPIM),INTENT(IN)    :: KST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF
TYPE(SL_STRUCT)   ,INTENT(INOUT) :: YDSL
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTABUF(YDSL%NDGSAH:YDSL%NDGENH)
LOGICAL           ,INTENT(IN)    :: LDFINDVSEP
LOGICAL           ,INTENT(IN)    :: LDSLHD
LOGICAL           ,INTENT(IN)    :: LDSLHDQUAD
LOGICAL           ,INTENT(IN)    :: LDINTV
INTEGER(KIND=JPIM),INTENT(IN)    :: KTIP
INTEGER(KIND=JPIM),INTENT(IN)    :: KROT
LOGICAL           ,INTENT(IN)    :: LDPLANE
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSDEPI(YDSL%NDGSAH:YDSL%NDGENH)
INTEGER(KIND=JPIM),INTENT(IN)    :: KIBL
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTSCO%NDIM)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPA(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAT(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAM(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKAPPAH(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PURL0(YDSL%NASLB1,YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVRL0(YDSL%NASLB1,YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWRL0(YDSL%NASLB1,YDGEOMETRY%YRDIMV%NFLSA:YDGEOMETRY%YRDIMV%NFLEN)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISU(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTDDISW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
INTEGER(KIND=JPIM),INTENT(INOUT) :: KVSEPC
INTEGER(KIND=JPIM),INTENT(INOUT) :: KVSEPL
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCCO(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTCCO%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PUF0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVF0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWF0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWFSM(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
INTEGER(KIND=JPIM),INTENT(INOUT) :: KL0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3)
INTEGER(KIND=JPIM),INTENT(INOUT) :: KLH0(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,0:3)
INTEGER(KIND=JPIM),INTENT(INOUT) :: KLEV(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTLSCAW%NDIM)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRSCAW(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG,YDML_DYN%YYTRSCAW%NDIM)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KDEP(YDGEOMETRY%YRDIM%NPROMA,YDGEOMETRY%YRDIMV%NFLEVG)
TYPE(STACK) :: YDSTACK, YLSTACK

!     ------------------------------------------------------------------


temp (INTEGER(KIND=JPIM), ISEP, (YDGEOMETRY%YRDIM%NPROMA))
INTEGER(KIND=JPIM) :: IHOR, IMINSEP, IVSEP, IWIS, JLEV, JROF
INTEGER(KIND=JPIM) :: ISPLTHOI, IDIMK
LOGICAL :: LLDONE, LL3DTURB
REAL(KIND=JPRB) :: ZDSTRET,ZDEPI,ZPIS2

!     ------------------------------------------------------------------

#include "abor1.intfb.h"
#include "elarche.intfb.h"
#include "elascaw.intfb.h"
#include "laitli.intfb.h"

!     ------------------------------------------------------------------




!     ------------------------------------------------------------------

!*       1.    PRELIMINARY INITIALISATIONS.
!              ----------------------------



YLSTACK=YDSTACK

alloc (ISEP)
ZDSTRET=2.0_JPRB*YDGEOMETRY%YRGEM%RSTRET
ZDEPI=2.0_JPRB*RPI
ZPIS2=0.5_JPRB*RPI

! * Input variable IWIS for LASCAW.
IF (KTIP == 1) THEN
  ! * trajectory research.
  IWIS=101
  IHOR=1
ELSEIF (KTIP == 2) THEN
  ! * origin point interpolations.
  IWIS=103+KHVI
  IF (LVSPLIP) IWIS=105
  IHOR=1
ELSEIF (KTIP == 3) THEN
  IWIS=103
  IHOR=0    ! Used by adjoint - I wish I could remember why ! CT
ENDIF

ISPLTHOI=YDML_DYN%YRDYN%NSPLTHOI
LL3DTURB=L3DTURB
IDIMK=YDML_DYN%YRDYN%NSLDIMK

!     ------------------------------------------------------------------

!*       2.    COMPUTATION OF LAT LON OF THE INTERPOLATION POINT.
!              IF KROT=1 COMPUTATION OF THE WIND DISPLACEMENT MATRIX
!              FROM THE INTERPOLATION POINT TO THE FINAL POINT.
!              ( T FOR LATITUDE THETA, L FOR LONGITUDE LAMBDA).
!              PCCO(.,.,YYTCCO%M_RQX) = ( 1 / (1+cos(PHI)) )
!                  *( cos(TG)*cos(T) + (1+sin(TG)*sin(T))*cos(L-LG) )
!              PCCO(.,.,YYTCCO%M_RQY) = (-1 / (1+cos(PHI)) )
!                  *( sin(TG)+sin(T) ) * sin(L-LG)
!     ------------------------------------------------------------------

IF (LDPLANE) THEN

  IF (KROT == 1) THEN
    CALL ELARCHE(YDML_DYN,YDGEOMETRY%YRDIM%NPROMA,KST,KPROF,YDGEOMETRY%YRDIMV%NFLEVG,YDSL,YDGEOMETRY%YREGSL,YDGEOMETRY%YREGEO,YDGEOMETRY%YRGSGEOM(KIBL),PSCO,PCCO, YLSTACK)
  ENDIF

  CALL ELASCAW(&
   ! --- INPUT ----------------------------------------------------------------
   & YDGEOMETRY%YRVSPLIP,YDML_DYN%YRDYN%LSLHDHEAT,YDSL,YDGEOMETRY%YRDIM%NPROMA,IDIMK,KST,KPROF,YDGEOMETRY%YRDIMV%NFLEVG,&
   & YDGEOMETRY%YRDIMV%NFLSA,KSTABUF,IWIS,IHOR,KHVI,&
   & LDSLHD,LDSLHDQUAD,LSLHD_OLD,LL3DTURB,&
   & LCOMAD,LCOMADH,LCOMADV,&
   & ISPLTHOI,PSCO(:,:,YDML_DYN%YYTSCO%M_COSCO),PSCO(:,:,YDML_DYN%YYTSCO%M_SINCO),PLEV,&
   & YDGEOMETRY%YRVETA%VETAF,YDGEOMETRY%YRVSLETA%NVAUTF,&
   & YDGEOMETRY%YRVSLETA%VCUICO,YDGEOMETRY%YRVSLETA%VSLD,YDGEOMETRY%YRVSLETA%VSLDW,YDGEOMETRY%YRVSLETA%NRLEVX,YDGEOMETRY%YRVSLETA%VRLEVX,PKAPPA,PKAPPAT,PKAPPAM,PKAPPAH,&
   & PSTDDISU,PSTDDISV,PSTDDISW,&
   ! --- OUTPUT ---------------------------------------------------------------
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLAT),PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLAMAD),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLA(1)),PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLASLD(1)),PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLASLT),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLAMAD(1)),&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLO),PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLOMAD),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLO(1)),PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLOSLD(1)),PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLOSLT),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WCLOMAD(1)),&
   & KL0,KLH0,KLEV,&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDVER),PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDVERMAD),PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WVINTW),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WVINTWSLD),PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WVINTWSLT),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WVINTWMAD),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WVINTWS),&
   & PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WVDERW),PRSCAW(:,:,YDML_DYN%YYTRSCAW%M_WHVW),KDEP, YLSTACK)

ELSE

  CALL ABOR1 ('UNEXPECTED CASE')

ENDIF

IF (LDFINDVSEP) THEN

  IVSEP=KVSEPC-1
  LLDONE=.FALSE.
  DO WHILE (.NOT.LLDONE)
    IVSEP=IVSEP+1
    !$acc loop vector
    DO JROF=KST,KPROF
      ISEP(JROF)=KLEV(JROF,IVSEP+1)-KLEV(JROF,1)
    ENDDO
    !$acc loop vector
    DO JROF = KST, KPROF
    DO JLEV=2,YDGEOMETRY%YRDIMV%NFLEVG-IVSEP
      
        ISEP(JROF)=MIN(ISEP(JROF),(KLEV(JROF,JLEV+IVSEP)-KLEV(JROF,JLEV)))
      
    ENDDO
    ENDDO

    IMINSEP=YDGEOMETRY%YRDIMV%NFLEVG
    !$acc loop vector
    DO JROF=KST,KPROF
      IMINSEP=MIN(IMINSEP,ISEP(JROF))
    ENDDO
    LLDONE = IMINSEP >= 4 .OR. IVSEP == YDGEOMETRY%YRDIMV%NFLEVG-1
  ENDDO
  IF (IMINSEP < 4) THEN
    CALL ABOR1('LARCINA: UNABLE TO COMPUTE NVSEPC')
  ENDIF
  KVSEPC=MAX(KVSEPC,IVSEP)

  IVSEP=KVSEPL-1
  LLDONE=.FALSE.
  DO WHILE (.NOT.LLDONE)
    IVSEP=IVSEP+1
    !$acc loop vector
    DO JROF=KST,KPROF
      ISEP(JROF)=KLEV(JROF,IVSEP+1)-KLEV(JROF,1)
    ENDDO
    !$acc loop vector
    DO JROF = KST, KPROF
    DO JLEV=2,YDGEOMETRY%YRDIMV%NFLEVG-IVSEP
      
        ISEP(JROF)=MIN(ISEP(JROF),(KLEV(JROF,JLEV+IVSEP)-KLEV(JROF,JLEV)))
      
    ENDDO
    ENDDO

    IMINSEP=YDGEOMETRY%YRDIMV%NFLEVG
    !$acc loop vector
    DO JROF=KST,KPROF
      IMINSEP=MIN(IMINSEP,ISEP(JROF))
    ENDDO
    LLDONE = IMINSEP >= 2 .OR. IVSEP == YDGEOMETRY%YRDIMV%NFLEVG-1
  ENDDO
  IF (IMINSEP < 2) THEN
    CALL ABOR1('LARCINA: UNABLE TO COMPUTE NVSEPL')
  ENDIF
  KVSEPL=MAX(KVSEPL,IVSEP)

ENDIF

!     ------------------------------------------------------------------

!*       3.    INTERPOLATIONS OF ((a/rs)*U;(a/rs)*V;etadot)
!              FOR TRAJECTORY RESEARCH.
!              --------------------------------------------

!*      Trilinear interpolations.

IF (LDINTV) THEN
  CALL LAITLI(YDSL%NASLB1,YDGEOMETRY%YRDIM%NPROMA,KST,KPROF,YDGEOMETRY%YRDIMV%NFLEVG,YDGEOMETRY%YRDIMV%NFLSA,YDGEOMETRY%YRDIMV%NFLEN,&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLAT),&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLO+1:YDML_DYN%YYTLSCAW%M_WDLO+YDGEOMETRY%YRDIMV%NFLEVG),&
   & KL0(:,:,1:2),&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDVER),PURL0,PUF0, YLSTACK)
  CALL LAITLI(YDSL%NASLB1,YDGEOMETRY%YRDIM%NPROMA,KST,KPROF,YDGEOMETRY%YRDIMV%NFLEVG,YDGEOMETRY%YRDIMV%NFLSA,YDGEOMETRY%YRDIMV%NFLEN,&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLAT),&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLO+1:YDML_DYN%YYTLSCAW%M_WDLO+YDGEOMETRY%YRDIMV%NFLEVG),&
   & KL0(:,:,1:2),&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDVER),PVRL0,PVF0, YLSTACK)
  CALL LAITLI(YDSL%NASLB1,YDGEOMETRY%YRDIM%NPROMA,KST,KPROF,YDGEOMETRY%YRDIMV%NFLEVG,YDGEOMETRY%YRDIMV%NFLSA,YDGEOMETRY%YRDIMV%NFLEN,&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLAT),&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDLO+1:YDML_DYN%YYTLSCAW%M_WDLO+YDGEOMETRY%YRDIMV%NFLEVG),&
   & KL0(:,:,1:2),&
   & PLSCAW(:,:,YDML_DYN%YYTLSCAW%M_WDVER),PWRL0,PWF0, YLSTACK)
  IF(YDML_DYN%YRDYN%LSVTSM) THEN
    CALL ABOR1 ('UNEXPECTED CASE')
  ENDIF
ENDIF

!     ------------------------------------------------------------------



END SUBROUTINE LARCINA
