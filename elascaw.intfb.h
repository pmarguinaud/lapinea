INTERFACE

!$acc routine (ELASCAW) vector
SUBROUTINE ELASCAW(&
 
 & YDVSPLIP,LDSLHDHEAT,YDSL,KPROMB,KDIMK,KST,KPROF,KFLEV,&
 & KFLDN,KSTABUF,KWIS,KHOR,KHVI,&
 & LDSLHD,LDSLHDQUAD,LDSLHD_OLD,LD3DTURB,&
 & LDCOMAD,LDCOMADH,LDCOMADV,KSPLTHOI,&
 & PLON,PLAT,PLEV,&
 & PVETA,KVAUT,&
 & PVCUICO,PVSLD,PVSLDW,KRLEVX,PVRLEVX,&
 & PKAPPA,PKAPPAT,PKAPPAM,PKAPPAH,&
 & PSTDDISU,PSTDDISV,PSTDDISW,&
 
 & PDLAT,PDLAMAD,PCLA,PCLASLD,PCLASLT,PCLAMAD,&
 & PDLO ,PDLOMAD,PCLO,PCLOSLD,PCLOSLT,PCLOMAD,&
 & KL0,KLH0,KLEV,&
 & PDVER,PDVERMAD,PVINTW,PVINTWSLD,PVINTWSLT,PVINTWMAD,PVINTWS,&
 & PVDERW,PHVW,KDEP, YDSTACK)

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMDYNA  , ONLY : LSLHD    ,LSLHDQUAD
USE YOMMP0   , ONLY : LOPT_SCALAR,NPROC

USE EINT_MOD , ONLY : SL_STRUCT,JPDUP
USE YOMVSPLIP , ONLY : TVSPLIP
USE STACK_MOD

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
TYPE(STACK) :: YDSTACK, YLSTACK

END SUBROUTINE ELASCAW

END INTERFACE