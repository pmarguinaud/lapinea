MODULE COPY_TDIMF_MOD

USE YOMDIMF, ONLY : TDIMF

INTERFACE COPY
MODULE PROCEDURE COPY_TDIMF
END INTERFACE

CONTAINS

SUBROUTINE COPY_TDIMF (YD)

IMPLICIT NONE
TYPE (TDIMF), INTENT (IN) :: YD

!$acc update device (YD%NFTHER)

!$acc update device (YD%NF3D)

!$acc update device (YD%NFD2D)

!$acc update device (YD%NFC2D)

!$acc update device (YD%NS3D)

!$acc update device (YD%NS2D)

!$acc update device (YD%NS1D)

!$acc enter data create (YD%NGRBSP3)
!$acc update device (YD%NGRBSP3)
!$acc enter data attach (YD%NGRBSP3)

!$acc enter data create (YD%NGRBSP2)
!$acc update device (YD%NGRBSP2)
!$acc enter data attach (YD%NGRBSP2)

!$acc update device (YD%LVOR)

!$acc update device (YD%LADER)

!$acc update device (YD%LUVDER)

!$acc update device (YD%LSPT)

END SUBROUTINE

END MODULE
