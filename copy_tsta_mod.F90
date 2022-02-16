MODULE COPY_TSTA_MOD

USE YOMSTA, ONLY : TSTA

INTERFACE COPY
MODULE PROCEDURE COPY_TSTA
END INTERFACE

CONTAINS

SUBROUTINE COPY_TSTA (YD)

IMPLICIT NONE
TYPE (TSTA), INTENT (IN) :: YD

!$acc enter data create (YD%STPREH)
!$acc update device (YD%STPREH)
!$acc enter data attach (YD%STPREH)

!$acc enter data create (YD%STPRE)
!$acc update device (YD%STPRE)
!$acc enter data attach (YD%STPRE)

!$acc enter data create (YD%STPHI)
!$acc update device (YD%STPHI)
!$acc enter data attach (YD%STPHI)

!$acc enter data create (YD%STTEM)
!$acc update device (YD%STTEM)
!$acc enter data attach (YD%STTEM)

!$acc enter data create (YD%STDEN)
!$acc update device (YD%STDEN)
!$acc enter data attach (YD%STDEN)

!$acc enter data create (YD%STZ)
!$acc update device (YD%STZ)
!$acc enter data attach (YD%STZ)

END SUBROUTINE

END MODULE
