MODULE COPY_TPTRSLB2_MOD

USE PTRSLB2, ONLY : TPTRSLB2

INTERFACE COPY
MODULE PROCEDURE COPY_TPTRSLB2
END INTERFACE

CONTAINS

SUBROUTINE COPY_TPTRSLB2 (YD)

IMPLICIT NONE
TYPE (TPTRSLB2), INTENT (IN) :: YD

!$acc update device (YD%NFLDSLB2)

!$acc update device (YD%MSLBUF2)

!$acc update device (YD%MSLB2DBBC1)

!$acc update device (YD%MSLB2DPHI1)

!$acc update device (YD%MSLB2USI)

!$acc update device (YD%MSLB2VSI)

!$acc update device (YD%MSLB2TSI)

!$acc update device (YD%MSLB2PDSI)

!$acc update device (YD%MSLB2VDSI)

!$acc update device (YD%MSLB2SPSI)

!$acc update device (YD%MSLB2VVEL)

!$acc update device (YD%MSLB2URL)

!$acc update device (YD%MSLB2VRL)

!$acc update device (YD%MSLB2WRL)

!$acc update device (YD%MSLB2URL5)

!$acc update device (YD%MSLB2VRL5)

!$acc update device (YD%MSLB2WRL5)

!$acc update device (YD%MSLB2USI5)

!$acc update device (YD%MSLB2VSI5)

!$acc update device (YD%MSLB2U15)

!$acc update device (YD%MSLB2V15)

!$acc update device (YD%MSLB2T15)

!$acc update device (YD%MSLB2Q15)

!$acc update device (YD%MSLB2KAPPA)

!$acc update device (YD%MSLB2KAPPAT)

!$acc update device (YD%MSLB2KAPPAM)

!$acc update device (YD%MSLB2KAPPAH)

!$acc update device (YD%MSLB2KAPPA5)

!$acc update device (YD%MSLB2KAPPAT5)

!$acc update device (YD%MSLB2GWF)

!$acc update device (YD%MSLB2GDW)

!$acc update device (YD%MSLB2GWS)

!$acc update device (YD%MSLB2STDDISU)

!$acc update device (YD%MSLB2STDDISV)

!$acc update device (YD%MSLB2STDDISW)

END SUBROUTINE

END MODULE
