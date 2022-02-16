INTERFACE
!$acc routine (ELARCHE) vector
SUBROUTINE ELARCHE(YDML_DYN,KPROMA,KST,KPROF,KFLEV,YDSL,YDEGSL,YDEGEO,YDGSGEOM,PSCO,PCCO, YDSTACK)

USE MODEL_DYNAMICS_MOD , ONLY : MODEL_DYNAMICS_TYPE
USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMCST   , ONLY : ROMEGA   ,RA
USE YEMGEO   , ONLY : TEGEO
USE YEMGSL   , ONLY : TEGSL
USE YOMJFH   , ONLY : N_VMASS
USE YOMGSGEOM, ONLY : TGSGEOM
USE EINT_MOD , ONLY : SL_STRUCT
USE STACK_MOD

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

END SUBROUTINE ELARCHE

END INTERFACE