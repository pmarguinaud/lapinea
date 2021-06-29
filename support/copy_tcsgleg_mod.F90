MODULE COPY_TCSGLEG_MOD

USE YOMLEG, ONLY : TCSGLEG

INTERFACE COPY
MODULE PROCEDURE COPY_TCSGLEG
END INTERFACE

CONTAINS

SUBROUTINE COPY_TCSGLEG (YD)

IMPLICIT NONE
TYPE (TCSGLEG), INTENT (IN) :: YD

!$acc enter data create (YD%RW)
!$acc update device (YD%RW)
!$acc enter data attach (YD%RW)

!$acc enter data create (YD%RMU)
!$acc update device (YD%RMU)
!$acc enter data attach (YD%RMU)

!$acc enter data create (YD%R1MU2)
!$acc update device (YD%R1MU2)
!$acc enter data attach (YD%R1MU2)

!$acc enter data create (YD%R1MUI)
!$acc update device (YD%R1MUI)
!$acc enter data attach (YD%R1MUI)

!$acc enter data create (YD%R1MUA)
!$acc update device (YD%R1MUA)
!$acc enter data attach (YD%R1MUA)

!$acc enter data create (YD%RSQM2)
!$acc update device (YD%RSQM2)
!$acc enter data attach (YD%RSQM2)

!$acc enter data create (YD%R1QM2)
!$acc update device (YD%R1QM2)
!$acc enter data attach (YD%R1QM2)

!$acc enter data create (YD%RACTHE)
!$acc update device (YD%RACTHE)
!$acc enter data attach (YD%RACTHE)

!$acc enter data create (YD%RLATIG)
!$acc update device (YD%RLATIG)
!$acc enter data attach (YD%RLATIG)

!$acc enter data create (YD%RLATI)
!$acc update device (YD%RLATI)
!$acc enter data attach (YD%RLATI)

END SUBROUTINE

END MODULE
