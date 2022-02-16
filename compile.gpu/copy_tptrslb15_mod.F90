MODULE COPY_TPTRSLB15_MOD

USE PTRSLB15, ONLY : TPTRSLB15

INTERFACE COPY
MODULE PROCEDURE COPY_TPTRSLB15
END INTERFACE

CONTAINS

SUBROUTINE COPY_TPTRSLB15 (YD)

IMPLICIT NONE
TYPE (TPTRSLB15), INTENT (IN) :: YD

!$acc update device (YD%NFLDSLB15)

!$acc enter data create (YD%RPARSL15)
!$acc update device (YD%RPARSL15)
!$acc enter data attach (YD%RPARSL15)

!$acc update device (YD%MSLBUF15)

!$acc update device (YD%MSLB1UR05)

!$acc update device (YD%MSLB1VR05)

!$acc update device (YD%MSLB1WR05)

!$acc update device (YD%MSLB1UR95)

!$acc update device (YD%MSLB1VR95)

!$acc update device (YD%MSLB1U05)

!$acc update device (YD%MSLB1V05)

!$acc update device (YD%MSLB1T05)

!$acc update device (YD%MSLB1C05)

!$acc update device (YD%MSLB1SP05)

!$acc update device (YD%MSLB1U95)

!$acc update device (YD%MSLB1V95)

!$acc update device (YD%MSLB1T95)

!$acc update device (YD%MSLB1GFL95)

!$acc update device (YD%MSLB1C95)

!$acc update device (YD%MSLB1SP95)

END SUBROUTINE

END MODULE
