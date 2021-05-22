MODULE COPY_TVAB_MOD

USE YOMVERT, ONLY : TVAB

INTERFACE COPY
MODULE PROCEDURE COPY_TVAB
END INTERFACE

CONTAINS

SUBROUTINE COPY_TVAB (YD)

IMPLICIT NONE
TYPE (TVAB), INTENT (IN) :: YD

!$acc enter data create (YD%VALH)
!$acc update device (YD%VALH)
!$acc enter data attach (YD%VALH)

!$acc enter data create (YD%VBH)
!$acc update device (YD%VBH)
!$acc enter data attach (YD%VBH)

!$acc enter data create (YD%VAH)
!$acc update device (YD%VAH)
!$acc enter data attach (YD%VAH)

!$acc enter data create (YD%VC)
!$acc update device (YD%VC)
!$acc enter data attach (YD%VC)

!$acc enter data create (YD%VAF)
!$acc update device (YD%VAF)
!$acc enter data attach (YD%VAF)

!$acc enter data create (YD%VBF)
!$acc update device (YD%VBF)
!$acc enter data attach (YD%VBF)

!$acc enter data create (YD%VDELA)
!$acc update device (YD%VDELA)
!$acc enter data attach (YD%VDELA)

!$acc enter data create (YD%VDELB)
!$acc update device (YD%VDELB)
!$acc enter data attach (YD%VDELB)

END SUBROUTINE

END MODULE
