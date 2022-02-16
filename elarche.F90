!$acc routine (ELARCHE) vector
SUBROUTINE ELARCHE(YDML_DYN,KPROMA,KST,KPROF,KFLEV,YDSL,YDEGSL,YDEGEO,YDGSGEOM,PSCO,PCCO, YDSTACK)


!**** *ELARCHE  -  semi-Lagrangian scheme:
!                  research of (lat,lon) coordinates of the origin point

!     Purpose.
!     --------
!        Computes the (lat,lon) geographical coordinates of the
!        origin point and then it computes the displacement
!        matrix type "Rochas" (elements p, q, see the general doc
!        on S-L scheme). Optionally it calculates the vectorial
!        product 2 omega x R (Coriolis contribution) at the
!        origin point exactly.

!**   Interface.
!     ----------

!     *CALL* *ELARCHE(...)

!     Explicit arguments:
!     -------------------

!     INPUT:
!       KPROMA      - horizontal dimension
!       KST         - start of work
!       KPROF       - depth of work
!       KFLEV       - vertical dimension
!       YDSL        - SL_STRUCT definition.
!       YDGSGEOM    - grid point geometry
!       PSCO        - information about position of interpol. point in the
!                     fractional system (not in metres!).

!     INPUT/OUTPUT:
!       PCCO        - "RLON" and "RLAT":
!                     * input : coordinates of the "true" origin point
!                     in the fractional system (even out of the C+I zone),
!                     used for the LADVF option
!                     * output: components of the 2 omega x R

!     OUTPUT:
!       PCCO        - "RQX" and "RQY": elements of the wind displacement matrix (p,q)

!             W A R N I N G:
!             ==============

!       The output wind rotation matrix PCCO(.,.,'RQX' and 'RQY')
!       contains in fact product
!       of two matrices: A * B, A being the "true" (p,q) rotation
!       matrix as described in the documentation, B being the
!       rotation matrix which provides (ug, vg) wind vector (g denotes
!       here for geographic components) starting from the "physical"
!       (that means non reduced!) map wind (u,v) at the origin point.
!       Then "true" (p,q) matrix takes care for displacement of the
!       GEOGRAPHIC wind components between the origin and the final
!       points of the trajectory. The rest of transformation at the
!       final point is done by the rotation matrix back to the "map"
!       north and east later in LAPINE... In total 3 rotation matrices
!       are involved, similarly to stretched and tilted ARPEGE.

!       ELARCHE treats ONLY PLANE(!) geometry, i.e. STEREO/LAMBERT
!       and MERCATOR projections. The LAT/LON spherical LAM
!       configuration is not yet ready.

!     Implicit arguments:
!     -------------------

!     Method.
!     -------
!       See documentation

!     Externals.
!     ----------

!     Author.
!     -------
!       Radmila Bubnova
!       Original : 31 JANUARY 1996.

!     Modifications.
!     --------------
!    J-D Gril : 2009-Mar-23 adding Mercator Rotated Tilted Case
!                          (From P. Benard formulae)
!    F. Vana   28-Apr-2009 small fix in Lambert/Stereo case
!    K. Yessad (Aug 2009): always use root (QX,QY) for (p,q) variables names
!    K. Yessad (Jan 2011): introduce INTDYN_MOD structures.
!    G.Mozdzynski (Feb 2011): OOPS cleaning, use of derived type TGSGEOM
!    B. Bochenek (Apr 2015): Phasing: move some variables.
!    K. Yessad (Feb 2018): remove deep-layer formulations.
!     -----------------------------------------------------------------

USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE
USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMCST   , ONLY : ROMEGA   ,RA
USE YEMGEO   , ONLY : TEGEO
USE YEMGSL   , ONLY : TEGSL
USE YOMJFH   , ONLY : N_VMASS
USE YOMGSGEOM, ONLY : TGSGEOM
USE EINT_MOD , ONLY : SL_STRUCT
USE STACK_MOD
#include "stack.h"

!     -----------------------------------------------------------------

IMPLICIT NONE

TYPE(MODEL_DYNAMICS_TYPE),INTENT(IN):: YDML_DYN
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST    
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
TYPE(SL_STRUCT),   INTENT(IN)    :: YDSL
TYPE(TEGSL)       ,INTENT(IN)    :: YDEGSL
TYPE(TEGEO)       ,INTENT(IN)    :: YDEGEO
TYPE(TGSGEOM)     ,INTENT(IN)    :: YDGSGEOM
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSCO(KPROMA,KFLEV,YDML_DYN%YYTSCO%NDIM)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCCO(KPROMA,KFLEV,YDML_DYN%YYTCCO%NDIM)
TYPE(STACK) :: YDSTACK, YLSTACK

! ----------------------------------------------------------------------

LOGICAL :: LLCOMPUTE_OMVAK_O

INTEGER(KIND=JPIM) :: JLEV, JROF


temp (REAL(KIND=JPRB), ZRR_TMP, (KPROMA))
temp (REAL(KIND=JPRB), ZYY_TMP, (KPROMA))
temp (REAL(KIND=JPRB), ZXX_TMP, (KPROMA))

temp (REAL(KIND=JPRB), ZLON_TMP, (KPROMA))
temp (REAL(KIND=JPRB), ZCOSLAT_TMP, (KPROMA))
temp (REAL(KIND=JPRB), ZSINLAT_TMP, (KPROMA))


temp (REAL(KIND=JPRB), ZSINLON_TMP, (KPROMA))
REAL(KIND=JPRB) :: ZAA, ZBIN, ZCC, ZCCC, &
 & ZCODLAM, ZCOPHI, ZCOSCO, &
 & ZCOSLAT, ZCOSLATC, ZCOSLATP, ZCOSLATS, &
 & ZCOSLON,ZCOSLONC,ZCOSLON0R,ZCOSLONP,ZCOSLONS,&
 & ZDGUN, ZDLUN, ZLON, ZLONS, ZNORDX, ZNORDY, ZPP, &
 & ZQQ, ZREGPK, ZRR2, ZSIDLAM, &
 & ZSINLAT, ZSINLATC, ZSINLATP, ZSINLATS, &
 & ZSINLON, ZSINLONC,ZSINLON0R, ZSINLONP,ZSINLONS, &
 & ZSINSI, ZSS, ZSSS, ZXP, ZXX, ZYP, ZYY

REAL(KIND=JPRB) :: ZEPS 

! ----------------------------------------------------------------------

#include "abor1.intfb.h"

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------

!        0. Initialization.
!        ------------------

! Protection against use of MASS library vector routines


YLSTACK=YDSTACK

alloc (ZRR_TMP)
alloc (ZYY_TMP)
alloc (ZXX_TMP)
alloc (ZLON_TMP)
alloc (ZCOSLAT_TMP)
alloc (ZSINLAT_TMP)
alloc (ZSINLON_TMP)
IF(N_VMASS > 0) THEN
  CALL ABOR1(' ELARCHE: ALADIN DOES NOT SUPPORT YET THE MASS LIBRARY ROUTINES')
ENDIF

ZEPS    = EPSILON(1.0_JPRB)*100.0_JPRB
ZDLUN   = REAL(YDSL%NDLUNG,JPRB)
ZDGUN   = REAL(YDSL%NDGUNG,JPRB)

LLCOMPUTE_OMVAK_O=YDML_DYN%YRDYN%LADVF

! ----------------------------------------------------------------------

!        1. Calculations.
!        ----------------

IF (YDEGEO%LMAP) THEN

  IF(YDEGSL%REGPK > 0) THEN

    !    1. Lambert/Stereo case.
    !    -----------------------

    ZREGPK  = 1.0_JPRB/YDEGSL%REGPK
    !$acc loop vector
    DO JROF = KST, KPROF
    DO JLEV = 1, KFLEV

      

        IF(LLCOMPUTE_OMVAK_O) THEN
          ZXX_TMP(JROF) = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)-ZDLUN-YDEGSL%RIPORE)*YDEGEO%EDELX
          ZYY_TMP(JROF) = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)-ZDGUN-YDEGSL%RJPORE)*YDEGEO%EDELY
        ELSE
          ZXX_TMP(JROF) = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)-ZDLUN-YDEGSL%RIPORE)*YDEGEO%EDELX
          ZYY_TMP(JROF) = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)-ZDGUN-YDEGSL%RJPORE)*YDEGEO%EDELY
        ENDIF

        ZRR2= ZXX_TMP(JROF)**2 + ZYY_TMP(JROF)**2
        ZRR_TMP(JROF) = MAX(SQRT(ZRR2),ZEPS)
        ZAA = (ZRR2*YDEGSL%RNOBILE)**ZREGPK

        ! * calculation of geographical latitude: sinus and cosinus
        ZSINLAT_TMP(JROF) = (1.0_JPRB-ZAA)/(1.0_JPRB+ZAA)*YDEGSL%RHSUD
        ZCOSLAT_TMP(JROF) = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLAT_TMP(JROF)*ZSINLAT_TMP(JROF)))

        ! * calculation of geographical longitude
        ZLON_TMP(JROF)    = YDEGSL%RLON0R + ATAN2(ZXX_TMP(JROF),-ZYY_TMP(JROF)*YDEGSL%RHSUD)*ZREGPK

        ZSINLON_TMP(JROF) = SIN(ZLON_TMP(JROF))
      
! For an unknown reason (Intel compiler bug ?) this computation has to be split here
! in order to prevent non-reproducible results when the entire loop is vectorized
      
        ZCOSLON = COS(ZLON_TMP(JROF))

        ! * calculation of compas
        ZNORDX  = - YDEGSL%RHSUD*ZXX_TMP(JROF)/ZRR_TMP(JROF)
        ZNORDY  = - YDEGSL%RHSUD*ZYY_TMP(JROF)/ZRR_TMP(JROF)

        ! * calculation of the matrix (p,q) * the compas matrix
        ZCOSCO  = YDGSGEOM%GSQM2(JROF) * ZCOSLAT_TMP(JROF)
        ZSINSI  = YDGSGEOM%GEMU (JROF) * ZSINLAT_TMP(JROF)
        ZCODLAM = YDGSGEOM%GECLO(JROF) * ZCOSLON   +YDGSGEOM%GESLO(JROF) * ZSINLON_TMP(JROF)
        ZSIDLAM = YDGSGEOM%GESLO(JROF) * ZCOSLON   -YDGSGEOM%GECLO(JROF) * ZSINLON_TMP(JROF)
        ZCOPHI  = ZSINSI + ZCOSCO * ZCODLAM
        ZPP     = (ZCOSCO+(1.0_JPRB+ZSINSI)   *ZCODLAM)/(1.0_JPRB+ZCOPHI)
        ZQQ     = ((YDGSGEOM%GEMU(JROF)+ZSINLAT_TMP(JROF))*ZSIDLAM)/(1.0_JPRB+ZCOPHI)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQX) = ZPP * ZNORDY + ZQQ * ZNORDX
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQY) =-ZPP * ZNORDX + ZQQ * ZNORDY

        ! * calculation of the 2 omega x R vector
        IF (LLCOMPUTE_OMVAK_O) THEN
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)=  2.0_JPRB*ROMEGA*RA*ZCOSLAT_TMP(JROF)*ZNORDY
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)= -2.0_JPRB*ROMEGA*RA*ZCOSLAT_TMP(JROF)*ZNORDX
        ENDIF

      

    ENDDO
    ENDDO


  ELSEIF ((YDEGSL%REGPK == 0).AND.YDEGEO%LMRT) THEN

    !    2.1 Mercator Rotated Tilted case.
    !    ---------------------------------

    ZSINLONC = SIN(YDEGSL%RELONC)
    ZCOSLONC = COS(YDEGSL%RELONC)
    ZSINLATC = SIN(YDEGSL%RELATC)
    ZCOSLATC = COS(YDEGSL%RELATC)
    ZSINLON0R = SIN(YDEGSL%RLON0R)
    ZCOSLON0R = COS(YDEGSL%RLON0R)

    !$acc loop vector
    DO JROF = KST, KPROF
    DO JLEV = 1, KFLEV
      

        IF(LLCOMPUTE_OMVAK_O) THEN
          ZXP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)-ZDLUN)*YDEGEO%EDELX
          ZYP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)-ZDGUN)*YDEGEO%EDELY
        ELSE
          ZXP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)-ZDLUN)*YDEGEO%EDELX
          ZYP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)-ZDGUN)*YDEGEO%EDELY
        ENDIF

        ZXX = ZXP - YDEGSL%RIPORE*YDEGEO%EDELX
        ZYY = ZYP - YDEGSL%RJPORE*YDEGEO%EDELY
        ZAA = EXP(-2.0_JPRB* YDEGSL%RMERCAT*ZYY)

        ! * calculation of RT latitude theta" (sin and cos)
        ZSINLATS = (1.0_JPRB-ZAA)/(1.0_JPRB+ZAA)
        ZCOSLATS = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLATS*ZSINLATS))

        ! * calculation of RT longitude lambda" (and sin and cos)
        ZLONS   =  ZXX * YDEGSL%RMERCAT  
        ZSINLONS = SIN(ZLONS)
        ZCOSLONS = COS(ZLONS)

        ! * calculation of R latitude theta' : sin and cos
        ZSINLATP = ZCOSLON0R*ZSINLATS-ZSINLON0R*ZCOSLATS*ZSINLONS
        ZCOSLATP = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLATP*ZSINLATP))

        ! * calculation of R longitude lambda' : sin and cos
        ZBIN=0.5_JPRB+SIGN(0.5_JPRB,ZCOSLATP-ZEPS)
        ZSS=(ZSINLON0R*ZSINLATS&
         & +ZCOSLON0R*ZCOSLATS*ZSINLONS)/MAX(ZCOSLATP,ZEPS)
        ZCC=ZCOSLATS*ZCOSLONS/MAX(ZCOSLATP,ZEPS)
        ZSINLONP = ZBIN*(ZSS)
        ZCOSLONP = ZBIN*(ZCC)+(1.0_JPRB - ZBIN)

        ! * calculation of Geogr latitude theta : sin and cos   
        ZSINLAT = ZCOSLATC*ZSINLATP + ZSINLATC*ZCOSLATP*ZCOSLONP
        ZCOSLAT = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLAT*ZSINLAT))

        ! * calculation of Geogr longitude lambda : sin and cos
        ZBIN=0.5_JPRB+SIGN(0.5_JPRB,ZCOSLAT-ZEPS)
        ZSS=ZCOSLATP*ZSINLONP/MAX(ZCOSLAT,ZEPS)
        ZCC=(-ZSINLATC*ZSINLATP+ZCOSLATC*ZCOSLATP*ZCOSLONP)/MAX(ZCOSLAT,ZEPS)
        ZSSS = ZBIN*(ZSS)
        ZCCC = ZBIN*(ZCC)+(1.0_JPRB - ZBIN)
        ZSINLON = ZSINLONC*ZCCC + ZCOSLONC*ZSSS
        ZCOSLON = ZCOSLONC*ZCCC - ZSINLONC*ZSSS

        ! * calculation of compass at origin point
        ZBIN=0.5_JPRB+SIGN(0.5_JPRB,ZCOSLATS-ZEPS)
        ZCC = (ZCOSLON0R*(ZCOSLATC*ZCOSLAT+ZSINLATC*ZSINLAT*ZCCC)&
         & - ZSINLON0R *ZSINLAT*ZSSS)/MAX(ZCOSLATS,ZEPS)
        ZSS = -(ZCOSLON0R*ZSINLATC*ZSSS+ZSINLON0R *ZCCC)/MAX(ZCOSLATS,ZEPS)
        ZNORDX = ZBIN*(ZSS)
        ZNORDY = ZBIN*(ZCC)+(1.0_JPRB - ZBIN)

        ! * calculation of the matrix (p,q) * the compas matrix
        ZCOSCO  = YDGSGEOM%GSQM2(JROF) * ZCOSLAT
        ZSINSI  = YDGSGEOM%GEMU (JROF) * ZSINLAT
        ZCODLAM = YDGSGEOM%GECLO(JROF) * ZCOSLON + YDGSGEOM%GESLO(JROF) * ZSINLON
        ZSIDLAM = YDGSGEOM%GESLO(JROF) * ZCOSLON - YDGSGEOM%GECLO(JROF) * ZSINLON
        ZCOPHI  = ZSINSI + ZCOSCO * ZCODLAM
        ZPP     = (ZCOSCO+(1.0_JPRB+ZSINSI) * ZCODLAM)/(1.0_JPRB+ZCOPHI)
        ZQQ     = ((YDGSGEOM%GEMU(JROF)+ZSINLAT)*ZSIDLAM)/(1.0_JPRB+ZCOPHI)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQX) = ZPP * ZNORDY + ZQQ * ZNORDX
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQY) =-ZPP * ZNORDX + ZQQ * ZNORDY

        ! * calculation of the 2 omega x R vector
        IF (LLCOMPUTE_OMVAK_O) THEN
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)=  2.0_JPRB*ROMEGA*RA*ZCOSLAT*ZNORDY
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)= -2.0_JPRB*ROMEGA*RA*ZCOSLAT*ZNORDX
        ENDIF

      

    ENDDO
    ENDDO


  ELSEIF ((YDEGSL%REGPK == 0).AND..NOT.YDEGEO%LMRT) THEN

    !    2.2 Mercator case.
    !    ------------------

    !$acc loop vector
    DO JROF = KST, KPROF
    DO JLEV = 1, KFLEV
      

        IF(LLCOMPUTE_OMVAK_O) THEN
          ZXP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)-ZDLUN)*YDEGEO%EDELX
          ZYP = (PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)-ZDGUN)*YDEGEO%EDELY
        ELSE
          ZXP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_COSCO)-ZDLUN)*YDEGEO%EDELX
          ZYP = (PSCO(JROF,JLEV,YDML_DYN%YYTSCO%M_SINCO)-ZDGUN)*YDEGEO%EDELY
        ENDIF

        ZXX = ZXP - YDEGSL%RIPORE*YDEGEO%EDELX
        ZYY = ZYP - YDEGSL%RJPORE*YDEGEO%EDELY
        ZAA = EXP(-2.0_JPRB* YDEGSL%RMERCAT*ZYY)

        ! * calculation of geographical latitude: sinus and cosinus
        ZSINLAT = (1.0_JPRB-ZAA)/(1.0_JPRB+ZAA)
        ZCOSLAT = SQRT(MAX(0.0_JPRB,1.0_JPRB-ZSINLAT*ZSINLAT))

        ! * calculation of geographical longitude
        ZLON    = YDEGSL%RLON0R + ZXX * YDEGSL%RMERCAT
        ZSINLON = SIN(ZLON)
        ZCOSLON = COS(ZLON)

        ! * calculation of the matrix (p,q) * the compas matrix
        ZCOSCO  = YDGSGEOM%GSQM2(JROF) * ZCOSLAT
        ZSINSI  = YDGSGEOM%GEMU (JROF) * ZSINLAT
        ZCODLAM = YDGSGEOM%GECLO(JROF) * ZCOSLON + YDGSGEOM%GESLO(JROF) * ZSINLON
        ZSIDLAM = YDGSGEOM%GESLO(JROF) * ZCOSLON - YDGSGEOM%GECLO(JROF) * ZSINLON
        ZCOPHI  = ZSINSI + ZCOSCO * ZCODLAM
        ZPP     = (ZCOSCO+(1.0_JPRB+ZSINSI) * ZCODLAM)/(1.0_JPRB+ZCOPHI)
        ZQQ     = ((YDGSGEOM%GEMU(JROF)+ZSINLAT)*ZSIDLAM)/(1.0_JPRB+ZCOPHI)
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQX) = ZPP
        PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RQY) = ZQQ

        ! * calculation of the 2 omega x R vector
        IF (LLCOMPUTE_OMVAK_O) THEN
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLON)=  2.0_JPRB*ROMEGA*RA*ZCOSLAT
          PCCO(JROF,JLEV,YDML_DYN%YYTCCO%M_RLAT)=  0.0_JPRB
        ENDIF

      

    ENDDO
    ENDDO


  ENDIF

ELSE

  !      3. Academic case.
  !      -----------------

  !$acc loop vector
  DO JROF = KST, KPROF
    PCCO(JROF,1:KFLEV,YDML_DYN%YYTCCO%M_RQX) = 1.0_JPRB
  ENDDO

  !$acc loop vector
  DO JROF = KST, KPROF
    PCCO(JROF,1:KFLEV,YDML_DYN%YYTCCO%M_RQY) = 0.0_JPRB
  ENDDO


ENDIF

! ----------------------------------------------------------------------


END SUBROUTINE ELARCHE

