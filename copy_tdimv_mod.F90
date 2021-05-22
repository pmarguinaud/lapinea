MODULE COPY_TDIMV_MOD

USE YOMDIMV, ONLY : TDIMV

INTERFACE COPY
MODULE PROCEDURE COPY_TDIMV
END INTERFACE

CONTAINS

SUBROUTINE COPY_TDIMV (YD)

IMPLICIT NONE
TYPE (TDIMV), INTENT (IN) :: YD

!$acc update device (YD%NFLEVG)

!$acc update device (YD%NFLEVL)

!$acc update device (YD%NFLEVLMX)

!$acc update device (YD%NFLSUR)

!$acc update device (YD%NFLSUL)

!$acc update device (YD%NFLSA)

!$acc update device (YD%NFLEN)

!$acc update device (YD%NIOLEVG)

END SUBROUTINE

END MODULE
