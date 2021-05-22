MODULE COPY_TPTRSLB1_MOD

USE PTRSLB1, ONLY : TPTRSLB1

INTERFACE COPY
MODULE PROCEDURE COPY_TPTRSLB1
END INTERFACE

CONTAINS

SUBROUTINE COPY_TPTRSLB1 (YD)

IMPLICIT NONE
TYPE (TPTRSLB1), INTENT (IN) :: YD

!$acc update device (YD%NFLDSLB1)

!$acc enter data create (YD%RPARSL1)
!$acc update device (YD%RPARSL1)
!$acc enter data attach (YD%RPARSL1)

!$acc update device (YD%MSLBUF1)

!$acc update device (YD%MSLB1U9)

!$acc update device (YD%MSLB1V9)

!$acc update device (YD%MSLB1T9)

!$acc update device (YD%MSLB1GFL9)

!$acc update device (YD%MSLB1GFLSP9)

!$acc update device (YD%MSLB1PD9)

!$acc update device (YD%MSLB1VD9)

!$acc update device (YD%MSLB1NHX9)

!$acc update device (YD%MSLB1UR0)

!$acc update device (YD%MSLB1VR0)

!$acc update device (YD%MSLB1WR0)

!$acc update device (YD%MSLB1WRA)

!$acc update device (YD%MSLB1UR9)

!$acc update device (YD%MSLB1VR9)

!$acc update device (YD%MSLB1DBBC9)

!$acc update device (YD%MSLB1DPHI9)

!$acc update device (YD%MSLB1GWS9)

!$acc update device (YD%MSLB1U0)

!$acc update device (YD%MSLB1V0)

!$acc update device (YD%MSLB1T0)

!$acc update device (YD%MSLB1PD0)

!$acc update device (YD%MSLB1VD0)

!$acc update device (YD%MSLB1C9)

!$acc update device (YD%MSLB1SP9)

!$acc update device (YD%MSLB1SP0)

!$acc update device (YD%MSLB1C0)

!$acc update device (YD%MSLB1UP9)

!$acc update device (YD%MSLB1VP9)

!$acc update device (YD%MSLB1TP9)

!$acc update device (YD%MSLB1GFLP9)

!$acc update device (YD%MSLB1U9_SI)

!$acc update device (YD%MSLB1V9_SI)

!$acc update device (YD%MSLB1T9_SI)

!$acc update device (YD%MSLB1PD9_SI)

!$acc update device (YD%MSLB1VD9_SI)

!$acc update device (YD%MSLB1C9_SI)

!$acc update device (YD%MSLB1UF9)

!$acc update device (YD%MSLB1VF9)

!$acc update device (YD%MSLB1TF9)

!$acc update device (YD%MSLB1GFLF9)

!$acc update device (YD%MSLB1VDF9)

!$acc update device (YD%MSLB1GFLSPF9)

!$acc update device (YD%MSLB1U9_NL)

!$acc update device (YD%MSLB1V9_NL)

!$acc update device (YD%MSLB1T9_NL)

!$acc update device (YD%MSLB1PD9_NL)

!$acc update device (YD%MSLB1VD9_NL)

!$acc update device (YD%MSLB1C9_NL)

END SUBROUTINE

END MODULE
