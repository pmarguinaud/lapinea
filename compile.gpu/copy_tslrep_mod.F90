MODULE COPY_TSLREP_MOD

USE YOMSLREP, ONLY : TSLREP

INTERFACE COPY
MODULE PROCEDURE COPY_TSLREP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TSLREP (YD)

IMPLICIT NONE
TYPE (TSLREP), INTENT (IN) :: YD

!$acc update device (YD%NGPTOTAD)

!$acc enter data create (YD%NADMAP)
!$acc update device (YD%NADMAP)
!$acc enter data attach (YD%NADMAP)

!$acc enter data create (YD%NADCORE)
!$acc update device (YD%NADCORE)
!$acc enter data attach (YD%NADCORE)

!$acc enter data create (YD%LADCORE)
!$acc update device (YD%LADCORE)
!$acc enter data attach (YD%LADCORE)

!$acc enter data create (YD%RSASIGN)
!$acc update device (YD%RSASIGN)
!$acc enter data attach (YD%RSASIGN)

END SUBROUTINE

END MODULE
