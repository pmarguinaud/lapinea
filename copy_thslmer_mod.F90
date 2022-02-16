MODULE COPY_THSLMER_MOD

USE YOMHSLMER, ONLY : THSLMER

INTERFACE COPY
MODULE PROCEDURE COPY_THSLMER
END INTERFACE

CONTAINS

SUBROUTINE COPY_THSLMER (YD)

IMPLICIT NONE
TYPE (THSLMER), INTENT (IN) :: YD

!$acc enter data create (YD%RIPI)
!$acc update device (YD%RIPI)
!$acc enter data attach (YD%RIPI)

!$acc enter data create (YD%RSLD)
!$acc update device (YD%RSLD)
!$acc enter data attach (YD%RSLD)

!$acc enter data create (YD%RSLDW)
!$acc update device (YD%RSLDW)
!$acc enter data attach (YD%RSLDW)

!$acc enter data create (YD%R3DTW)
!$acc update device (YD%R3DTW)
!$acc enter data attach (YD%R3DTW)

END SUBROUTINE

END MODULE
