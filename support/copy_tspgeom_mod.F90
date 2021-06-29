MODULE COPY_TSPGEOM_MOD

USE TYPE_SPGEOM, ONLY : TSPGEOM

INTERFACE COPY
MODULE PROCEDURE COPY_TSPGEOM
END INTERFACE

CONTAINS

SUBROUTINE COPY_TSPGEOM (YD)

IMPLICIT NONE
TYPE (TSPGEOM), INTENT (IN) :: YD

!$acc enter data create (YD%GMR)
!$acc update device (YD%GMR)
!$acc enter data attach (YD%GMR)

!$acc enter data create (YD%SCGMAP)
!$acc update device (YD%SCGMAP)
!$acc enter data attach (YD%SCGMAP)

!$acc update device (YD%ESCGMAP)

END SUBROUTINE

END MODULE
