MODULE COPY_TEDYN_MOD

USE YEMDYN, ONLY : TEDYN

INTERFACE COPY
MODULE PROCEDURE COPY_TEDYN
END INTERFACE

CONTAINS

SUBROUTINE COPY_TEDYN (YD)

IMPLICIT NONE
TYPE (TEDYN), INTENT (IN) :: YD

!$acc enter data create (YD%RDIVORE)
!$acc update device (YD%RDIVORE)
!$acc enter data attach (YD%RDIVORE)

!$acc enter data create (YD%RDIDIVE)
!$acc update device (YD%RDIDIVE)
!$acc enter data attach (YD%RDIDIVE)

!$acc enter data create (YD%RDITE)
!$acc update device (YD%RDITE)
!$acc enter data attach (YD%RDITE)

!$acc enter data create (YD%RDIGFLE)
!$acc update device (YD%RDIGFLE)
!$acc enter data attach (YD%RDIGFLE)

!$acc enter data create (YD%RDIPDE)
!$acc update device (YD%RDIPDE)
!$acc enter data attach (YD%RDIPDE)

!$acc enter data create (YD%RDIVDE)
!$acc update device (YD%RDIVDE)
!$acc enter data attach (YD%RDIVDE)

!$acc enter data create (YD%RDISPE)
!$acc update device (YD%RDISPE)
!$acc enter data attach (YD%RDISPE)

!$acc enter data create (YD%RDSVORE)
!$acc update device (YD%RDSVORE)
!$acc enter data attach (YD%RDSVORE)

!$acc enter data create (YD%RDSDIVE)
!$acc update device (YD%RDSDIVE)
!$acc enter data attach (YD%RDSDIVE)

!$acc enter data create (YD%RDSVDE)
!$acc update device (YD%RDSVDE)
!$acc enter data attach (YD%RDSVDE)

!$acc enter data create (YD%REFILV)
!$acc update device (YD%REFILV)
!$acc enter data attach (YD%REFILV)

!$acc enter data create (YD%REFILD)
!$acc update device (YD%REFILD)
!$acc enter data attach (YD%REFILD)

!$acc update device (YD%LESIDG)

!$acc update device (YD%RTHRESIDG)

!$acc update device (YD%XMALD)

!$acc update device (YD%TCDIS)

END SUBROUTINE

END MODULE
