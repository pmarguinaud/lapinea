MODULE COPY_TEDIM_MOD

USE YEMDIM, ONLY : TEDIM

INTERFACE COPY
MODULE PROCEDURE COPY_TEDIM
END INTERFACE

CONTAINS

SUBROUTINE COPY_TEDIM (YD)

IMPLICIT NONE
TYPE (TEDIM), INTENT (IN) :: YD

!$acc update device (YD%NSECPLG)

!$acc update device (YD%NBZONG)

!$acc update device (YD%NBZONL)

!$acc update device (YD%NNOEXTZG)

!$acc update device (YD%NNOEXTZL)

!$acc enter data create (YD%NISMAX)
!$acc update device (YD%NISMAX)
!$acc enter data attach (YD%NISMAX)

!$acc enter data create (YD%NISNAX)
!$acc update device (YD%NISNAX)
!$acc enter data attach (YD%NISNAX)

!$acc update device (YD%LBIPINCI)

!$acc update device (YD%NBIPINCIX)

!$acc update device (YD%NBIPINCIY)

!$acc update device (YD%NEDOM)

END SUBROUTINE

END MODULE
