#ifdef NECSX
!option! -O extendreorder
#endif
SUBROUTINE LAITLI(KPROMA,KPROMB,KST,KPROF,KFLEV,&
 & KFLDN,KFLDX,&
 & PDLAT,PDLO,KL0,PDVER,&
 & PXSL,PXF)  


!**** *LAITLI  -  semi-LAgrangian scheme:
!                 Trilinear interpolations for one variable.

!     Purpose.
!     --------
!       Performs trilinear interpolations for one variable.

!**   Interface.
!     ----------
!        *CALL* *LAITLI(KPROMA,KPROMB,KST,KPROF,KFLEV
!                      ,KFLDN,KFLDX
!                      ,PDLAT,PDLO,KL0,PDVER
!                      ,PXSL,PXF)

!        Explicit arguments :
!        --------------------

!        INPUT:
!          KPROMA  - horizontal dimension for grid-point quantities.
!          KPROMB  - horizontal dimension for interpolation point
!                    quantities.
!          KST     - first element of arrays where
!                    computations are performed.
!          KPROF   - depth of work.
!          KFLEV   - vertical dimension.
!          KFLDN   - number of the first field.
!          KFLDX   - number of the last field.
!          PDLAT   - weight (distance) for horizontal linear interpolation
!                    on a same latitude.
!          PDLO    - weights (distances) for horizontal linear interpolation
!                    on a same longitude.
!          KL0     - indices of the four western points
!                    of the 16 points interpolation grid.
!          PDVER   - weights (distances) for vertical linear interpolation
!                    on a same vertical.
!          PXSL    - semi-lagrangian variable.

!        OUTPUT:
!          PXF     - interpolated variable.

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!        No external.
!        Called by LARCIN.

!     Reference.
!     ----------

!     Author.
!     -------
!        K. YESSAD, after the subroutine LAGINL3
!        written by Maurice IMBARD, Alain CRAPLET and Michel ROCHAS
!        METEO-FRANCE, CNRM/GMAP.

!     Modifications.
!     --------------
!        Original : FEBRUARY 1992.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!        F. Vana       26-Aug-2008 optimization for NEC
!        F. Courteille 16-Sep-2009 optimization for NEC SX-9
!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIA

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMB 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST    
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLAT(KPROMB,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMB,KFLEV,2) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMB,KFLEV,2) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDVER(KPROMB,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KPROMA*(KFLDX-KFLDN+1)) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXF(KPROMB,KFLEV) 
!     ------------------------------------------------------------------

INTEGER(KIND=JPIA) :: IV1L1, IV1L2, IV2L1, IV2L2
INTEGER(KIND=JPIM) :: JLEV, JROF

REAL(KIND=JPRB) :: ZDLAT, ZDLO1, ZDLO2, ZDVER, ZINF, ZINFLO1,&
 & ZINFLO2, ZSUP, ZSUPLO1, ZSUPLO2  

!     ------------------------------------------------------------------


!     ------------------------------------------------------------------

!*       1.    INTERPOLATIONS.
!              ---------------

IV1L1=1+KPROMA
IV1L2=2+KPROMA
IV2L1=IV1L1+KPROMA
IV2L2=IV1L2+KPROMA

#ifdef NECSX
!CDIR OUTERUNROLL=4
#endif
!DIR$ PREFERVECTOR
DO JLEV=1,KFLEV
#ifdef NECSX
!CDIR GTHREORDER
#endif

!DIR$ NEXTSCALAR
  DO JROF=KST,KPROF

!     Computation of coordinates and distances.

    ZDLAT =PDLAT(JROF,JLEV)

    ZDLO1 =PDLO(JROF,JLEV,1)
    ZDLO2 =PDLO(JROF,JLEV,2)

    ZDVER =PDVER(JROF,JLEV)

!     Interpolation.

    ZSUPLO1=PXSL(KL0(JROF,JLEV,1)+IV1L1) + ZDLO1*&
     & ( PXSL(KL0(JROF,JLEV,1)+IV1L2)-PXSL(KL0(JROF,JLEV,1)+IV1L1) )  
    ZSUPLO2=PXSL(KL0(JROF,JLEV,2)+IV1L1) + ZDLO2*&
     & ( PXSL(KL0(JROF,JLEV,2)+IV1L2)-PXSL(KL0(JROF,JLEV,2)+IV1L1) )  
    ZSUP   =  ZSUPLO1 + ZDLAT*(ZSUPLO2-ZSUPLO1)

    ZINFLO1=PXSL(KL0(JROF,JLEV,1)+IV2L1) + ZDLO1*&
     & ( PXSL(KL0(JROF,JLEV,1)+IV2L2)-PXSL(KL0(JROF,JLEV,1)+IV2L1) )  
    ZINFLO2=PXSL(KL0(JROF,JLEV,2)+IV2L1) + ZDLO2*&
     & ( PXSL(KL0(JROF,JLEV,2)+IV2L2)-PXSL(KL0(JROF,JLEV,2)+IV2L1) )  
    ZINF   =  ZINFLO1 + ZDLAT*(ZINFLO2-ZINFLO1)

    PXF(JROF,JLEV)= ZSUP + ZDVER*(ZINF-ZSUP)

  ENDDO
ENDDO

!     ------------------------------------------------------------------

END SUBROUTINE LAITLI

