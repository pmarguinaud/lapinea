MODULE COPY_TPTRGPPC_MOD

USE PTRGPPC, ONLY : TPTRGPPC

INTERFACE COPY
MODULE PROCEDURE COPY_TPTRGPPC
END INTERFACE

CONTAINS

SUBROUTINE COPY_TPTRGPPC (YD)

IMPLICIT NONE
TYPE (TPTRGPPC), INTENT (IN) :: YD

!$acc update device (YD%NFGPPC)

!$acc update device (YD%MGPPC)

!$acc update device (YD%MGPPCF_U)

!$acc update device (YD%MGPPCF_V)

!$acc update device (YD%MGPPCF_T)

!$acc update device (YD%MGPPCF_SPD)

!$acc update device (YD%MGPPCF_SVD)

!$acc update device (YD%MGPPCF_SP)

!$acc update device (YD%MGPPCF_CP)

!$acc update device (YD%MGPPCF_NHX)

!$acc update device (YD%MGPPCF_UP)

!$acc update device (YD%MGPPCF_VP)

!$acc update device (YD%MGPPCF_TP)

!$acc update device (YD%MGPPCF_GFLP)

!$acc update device (YD%MGPPCF_BBC)

!$acc update device (YD%MGPPC5)

!$acc update device (YD%MGPPCF_U5)

!$acc update device (YD%MGPPCF_V5)

!$acc update device (YD%MGPPCF_T5)

!$acc update device (YD%MGPPCF_SPD5)

!$acc update device (YD%MGPPCF_SVD5)

!$acc update device (YD%MGPPCF_SP5)

END SUBROUTINE

END MODULE
