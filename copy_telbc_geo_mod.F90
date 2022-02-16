MODULE COPY_TELBC_GEO_MOD

USE YEMLBC_GEO, ONLY : TELBC_GEO

INTERFACE COPY
MODULE PROCEDURE COPY_TELBC_GEO
END INTERFACE

CONTAINS

SUBROUTINE COPY_TELBC_GEO (YD)

IMPLICIT NONE
TYPE (TELBC_GEO), INTENT (IN) :: YD

!$acc update device (YD%NEDLST)

!$acc enter data create (YD%NIND_LIST)
!$acc update device (YD%NIND_LIST)
!$acc enter data attach (YD%NIND_LIST)

!$acc enter data create (YD%NIND_LEN)
!$acc update device (YD%NIND_LEN)
!$acc enter data attach (YD%NIND_LEN)

END SUBROUTINE

END MODULE
