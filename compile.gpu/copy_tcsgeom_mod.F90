MODULE COPY_TCSGEOM_MOD

USE YOMCSGEOM, ONLY : TCSGEOM

INTERFACE COPY
MODULE PROCEDURE COPY_TCSGEOM
END INTERFACE

CONTAINS

SUBROUTINE COPY_TCSGEOM (YD)

IMPLICIT NONE
TYPE (TCSGEOM), INTENT (IN) :: YD

!$acc enter data create (YD%RCOLON)
!$acc update device (YD%RCOLON)
!$acc enter data attach (YD%RCOLON)

!$acc enter data create (YD%RSILON)
!$acc update device (YD%RSILON)
!$acc enter data attach (YD%RSILON)

!$acc enter data create (YD%RINDX)
!$acc update device (YD%RINDX)
!$acc enter data attach (YD%RINDX)

!$acc enter data create (YD%RINDY)
!$acc update device (YD%RINDY)
!$acc enter data attach (YD%RINDY)

!$acc enter data create (YD%RATATH)
!$acc update device (YD%RATATH)
!$acc enter data attach (YD%RATATH)

!$acc enter data create (YD%RATATX)
!$acc update device (YD%RATATX)
!$acc enter data attach (YD%RATATX)

END SUBROUTINE

END MODULE
