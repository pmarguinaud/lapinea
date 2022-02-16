MODULE COPY_TVSLETA_MOD

USE YOMVSLETA, ONLY : TVSLETA

INTERFACE COPY
MODULE PROCEDURE COPY_TVSLETA
END INTERFACE

CONTAINS

SUBROUTINE COPY_TVSLETA (YD)

IMPLICIT NONE
TYPE (TVSLETA), INTENT (IN) :: YD

!$acc enter data create (YD%VCUICO)
!$acc update device (YD%VCUICO)
!$acc enter data attach (YD%VCUICO)

!$acc enter data create (YD%VCUICOH)
!$acc update device (YD%VCUICOH)
!$acc enter data attach (YD%VCUICOH)

!$acc enter data create (YD%VSLD)
!$acc update device (YD%VSLD)
!$acc enter data attach (YD%VSLD)

!$acc enter data create (YD%VSLDH)
!$acc update device (YD%VSLDH)
!$acc enter data attach (YD%VSLDH)

!$acc enter data create (YD%VSLDW)
!$acc update device (YD%VSLDW)
!$acc enter data attach (YD%VSLDW)

!$acc enter data create (YD%VSLDWH)
!$acc update device (YD%VSLDWH)
!$acc enter data attach (YD%VSLDWH)

!$acc enter data create (YD%VRDETAR)
!$acc update device (YD%VRDETAR)
!$acc enter data attach (YD%VRDETAR)

!$acc update device (YD%NRLEVX)

!$acc update device (YD%VRLEVX)

!$acc enter data create (YD%NVAUTF)
!$acc update device (YD%NVAUTF)
!$acc enter data attach (YD%NVAUTF)

!$acc enter data create (YD%NVAUTH)
!$acc update device (YD%NVAUTH)
!$acc enter data attach (YD%NVAUTH)

END SUBROUTINE

END MODULE
