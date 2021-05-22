MODULE COPY_TVSPLIP_MOD

USE YOMVSPLIP, ONLY : TVSPLIP

INTERFACE COPY
MODULE PROCEDURE COPY_TVSPLIP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TVSPLIP (YD)

IMPLICIT NONE
TYPE (TVSPLIP), INTENT (IN) :: YD

!$acc enter data create (YD%RVSPTRI)
!$acc update device (YD%RVSPTRI)
!$acc enter data attach (YD%RVSPTRI)

!$acc enter data create (YD%RVSPC)
!$acc update device (YD%RVSPC)
!$acc enter data attach (YD%RVSPC)

!$acc enter data create (YD%RFVV)
!$acc update device (YD%RFVV)
!$acc enter data attach (YD%RFVV)

END SUBROUTINE

END MODULE
