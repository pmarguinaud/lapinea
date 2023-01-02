INTERFACE
#ifdef NECSX

#endif
!$acc routine(LAITLI) seq
SUBROUTINE LAITLI(KPROMA,KPROMB,KST,KPROF,KFLEV,&
 & KFLDN,KFLDX,&
 & PDLAT,PDLO,KL0,PDVER,&
 & PXSL,PXF,KSTPT,KSTSZ,PSTACK)  

USE PARKIND1  ,ONLY : JPIM     ,JPRB     ,JPIA

INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROMB 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLDX 
INTEGER(KIND=JPIM),INTENT(IN)    :: KST    
INTEGER(KIND=JPIM),INTENT(IN)    :: KPROF 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLAT(KPROMB,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDLO(KPROMB,KFLEV,1:2) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KL0(KPROMB,KFLEV,1:2) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDVER(KPROMB,KFLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXSL(KPROMA*(KFLDX-KFLDN+1)) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXF(KPROMB,KFLEV) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL   (KIND=JPRB),INTENT(INOUT) :: PSTACK (KSTSZ)

#ifdef NECSX

#endif

#ifdef NECSX

#endif

END SUBROUTINE LAITLI

END INTERFACE
