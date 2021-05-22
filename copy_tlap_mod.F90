MODULE COPY_TLAP_MOD

USE YOMLAP, ONLY : TLAP

INTERFACE COPY
MODULE PROCEDURE COPY_TLAP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TLAP (YD)

IMPLICIT NONE
TYPE (TLAP), INTENT (IN) :: YD

!$acc enter data create (YD%NASN0)
!$acc update device (YD%NASN0)
!$acc enter data attach (YD%NASN0)

!$acc enter data create (YD%NASM0)
!$acc update device (YD%NASM0)
!$acc enter data attach (YD%NASM0)

!$acc enter data create (YD%NASM0G)
!$acc update device (YD%NASM0G)
!$acc enter data attach (YD%NASM0G)

!$acc enter data create (YD%NVALUE)
!$acc update device (YD%NVALUE)
!$acc enter data attach (YD%NVALUE)

!$acc enter data create (YD%MYMS)
!$acc update device (YD%MYMS)
!$acc enter data attach (YD%MYMS)

!$acc enter data create (YD%NSPZERO)
!$acc update device (YD%NSPZERO)
!$acc enter data attach (YD%NSPZERO)

!$acc enter data create (YD%NSE0L)
!$acc update device (YD%NSE0L)
!$acc enter data attach (YD%NSE0L)

!$acc enter data create (YD%RLAPDI)
!$acc update device (YD%RLAPDI)
!$acc enter data attach (YD%RLAPDI)

!$acc enter data create (YD%RLAPIN)
!$acc update device (YD%RLAPIN)
!$acc enter data attach (YD%RLAPIN)

END SUBROUTINE

END MODULE
