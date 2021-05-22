MODULE COPY_TOROG_MOD

USE YOMOROG, ONLY : TOROG

INTERFACE COPY
MODULE PROCEDURE COPY_TOROG
END INTERFACE

CONTAINS

SUBROUTINE COPY_TOROG (YD)

IMPLICIT NONE
TYPE (TOROG), INTENT (IN) :: YD

!$acc enter data create (YD%OROG)
!$acc update device (YD%OROG)
!$acc enter data attach (YD%OROG)

!$acc enter data create (YD%OROGL)
!$acc update device (YD%OROGL)
!$acc enter data attach (YD%OROGL)

!$acc enter data create (YD%OROGM)
!$acc update device (YD%OROGM)
!$acc enter data attach (YD%OROGM)

!$acc enter data create (YD%OROGLL)
!$acc update device (YD%OROGLL)
!$acc enter data attach (YD%OROGLL)

!$acc enter data create (YD%OROGMM)
!$acc update device (YD%OROGMM)
!$acc enter data attach (YD%OROGMM)

!$acc enter data create (YD%OROGLM)
!$acc update device (YD%OROGLM)
!$acc enter data attach (YD%OROGLM)

END SUBROUTINE

END MODULE
