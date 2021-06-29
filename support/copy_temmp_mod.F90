MODULE COPY_TEMMP_MOD

USE YEMMP, ONLY : TEMMP

INTERFACE COPY
MODULE PROCEDURE COPY_TEMMP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TEMMP (YD)

IMPLICIT NONE
TYPE (TEMMP), INTENT (IN) :: YD

!$acc enter data create (YD%NEPROCN)
!$acc update device (YD%NEPROCN)
!$acc enter data attach (YD%NEPROCN)

!$acc update device (YD%NUEMP)

!$acc enter data create (YD%MYENS)
!$acc update device (YD%MYENS)
!$acc enter data attach (YD%MYENS)

!$acc enter data create (YD%NUEMPP)
!$acc update device (YD%NUEMPP)
!$acc enter data attach (YD%NUEMPP)

!$acc enter data create (YD%NEALLNS)
!$acc update device (YD%NEALLNS)
!$acc enter data attach (YD%NEALLNS)

!$acc enter data create (YD%NEPTRNS)
!$acc update device (YD%NEPTRNS)
!$acc enter data attach (YD%NEPTRNS)

END SUBROUTINE

END MODULE
