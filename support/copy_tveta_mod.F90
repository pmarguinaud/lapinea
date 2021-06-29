MODULE COPY_TVETA_MOD

USE YOMVERT, ONLY : TVETA

INTERFACE COPY
MODULE PROCEDURE COPY_TVETA
END INTERFACE

CONTAINS

SUBROUTINE COPY_TVETA (YD)

IMPLICIT NONE
TYPE (TVETA), INTENT (IN) :: YD

!$acc enter data create (YD%VETAH)
!$acc update device (YD%VETAH)
!$acc enter data attach (YD%VETAH)

!$acc enter data create (YD%VFE_ETAH)
!$acc update device (YD%VFE_ETAH)
!$acc enter data attach (YD%VFE_ETAH)

!$acc enter data create (YD%VETAF)
!$acc update device (YD%VETAF)
!$acc enter data attach (YD%VETAF)

!$acc enter data create (YD%VFE_ETAF)
!$acc update device (YD%VFE_ETAF)
!$acc enter data attach (YD%VFE_ETAF)

!$acc enter data create (YD%VFE_RDETAH)
!$acc update device (YD%VFE_RDETAH)
!$acc enter data attach (YD%VFE_RDETAH)

END SUBROUTINE

END MODULE
