MODULE COPY_TGEM_MOD

USE YOMGEM, ONLY : TGEM

INTERFACE COPY
MODULE PROCEDURE COPY_TGEM
END INTERFACE

CONTAINS

SUBROUTINE COPY_TGEM (YD)

IMPLICIT NONE
TYPE (TGEM), INTENT (IN) :: YD

!$acc update device (YD%NGPTOT)

!$acc update device (YD%NGPTOT_CAP)

!$acc update device (YD%NGPTOTMX)

!$acc update device (YD%NGPTOTG)

!$acc enter data create (YD%NGPTOTL)
!$acc update device (YD%NGPTOTL)
!$acc enter data attach (YD%NGPTOTL)

!$acc update device (YD%RDELXN)

!$acc update device (YD%SLHDP)

!$acc update device (YD%RMUCEN)

!$acc update device (YD%RLOCEN)

!$acc update device (YD%RSTRET)

!$acc update device (YD%NSTTYP)

!$acc update device (YD%NHTYP)

!$acc update device (YD%RNLGINC)

!$acc update device (YD%R4JP)

!$acc update device (YD%RC2P1)

!$acc update device (YD%RC2M1)

!$acc update device (YD%RCOR0)

!$acc update device (YD%RCOR1)

!$acc enter data create (YD%NLOEN)
!$acc update device (YD%NLOEN)
!$acc enter data attach (YD%NLOEN)

!$acc enter data create (YD%NLOENG)
!$acc update device (YD%NLOENG)
!$acc enter data attach (YD%NLOENG)

!$acc enter data create (YD%NMEN)
!$acc update device (YD%NMEN)
!$acc enter data attach (YD%NMEN)

!$acc enter data create (YD%NMENG)
!$acc update device (YD%NMENG)
!$acc enter data attach (YD%NMENG)

!$acc enter data create (YD%NDGLU)
!$acc update device (YD%NDGLU)
!$acc enter data attach (YD%NDGLU)

!$acc enter data create (YD%NSTAGP)
!$acc update device (YD%NSTAGP)
!$acc enter data attach (YD%NSTAGP)

!$acc enter data create (YD%NTSTAGP)
!$acc update device (YD%NTSTAGP)
!$acc enter data attach (YD%NTSTAGP)

END SUBROUTINE

END MODULE
