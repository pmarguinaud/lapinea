MODULE COPY_TVFE_MOD

USE YOMVERT, ONLY : TVFE

INTERFACE COPY
MODULE PROCEDURE COPY_TVFE
END INTERFACE

CONTAINS

SUBROUTINE COPY_TVFE (YD)

IMPLICIT NONE
TYPE (TVFE), INTENT (IN) :: YD

!$acc enter data create (YD%VFE_KNOT)
!$acc update device (YD%VFE_KNOT)
!$acc enter data attach (YD%VFE_KNOT)

!$acc enter data create (YD%RINTE)
!$acc update device (YD%RINTE)
!$acc enter data attach (YD%RINTE)

!$acc enter data create (YD%RINTBF00)
!$acc update device (YD%RINTBF00)
!$acc enter data attach (YD%RINTBF00)

!$acc enter data create (YD%RINTBF11)
!$acc update device (YD%RINTBF11)
!$acc enter data attach (YD%RINTBF11)

!$acc enter data create (YD%RDERI)
!$acc update device (YD%RDERI)
!$acc enter data attach (YD%RDERI)

!$acc enter data create (YD%RDERB)
!$acc update device (YD%RDERB)
!$acc enter data attach (YD%RDERB)

!$acc enter data create (YD%RDERBF00)
!$acc update device (YD%RDERBF00)
!$acc enter data attach (YD%RDERBF00)

!$acc enter data create (YD%RDERBF01)
!$acc update device (YD%RDERBF01)
!$acc enter data attach (YD%RDERBF01)

!$acc enter data create (YD%RDERBF10)
!$acc update device (YD%RDERBF10)
!$acc enter data attach (YD%RDERBF10)

!$acc enter data create (YD%RDERBF11)
!$acc update device (YD%RDERBF11)
!$acc enter data attach (YD%RDERBF11)

!$acc enter data create (YD%RDERBH00)
!$acc update device (YD%RDERBH00)
!$acc enter data attach (YD%RDERBH00)

!$acc enter data create (YD%RDERBH01)
!$acc update device (YD%RDERBH01)
!$acc enter data attach (YD%RDERBH01)

!$acc enter data create (YD%RDDERI)
!$acc update device (YD%RDDERI)
!$acc enter data attach (YD%RDDERI)

!$acc enter data create (YD%RDDERBF01)
!$acc update device (YD%RDDERBF01)
!$acc enter data attach (YD%RDDERBF01)

!$acc enter data create (YD%RDDERBF11)
!$acc update device (YD%RDDERBF11)
!$acc enter data attach (YD%RDDERBF11)

!$acc enter data create (YD%RINTBF11_IMPL)
!$acc update device (YD%RINTBF11_IMPL)
!$acc enter data attach (YD%RINTBF11_IMPL)

!$acc enter data create (YD%RDERBF01_IMPL)
!$acc update device (YD%RDERBF01_IMPL)
!$acc enter data attach (YD%RDERBF01_IMPL)

!$acc enter data create (YD%RDERBF10_IMPL)
!$acc update device (YD%RDERBF10_IMPL)
!$acc enter data attach (YD%RDERBF10_IMPL)

!$acc enter data create (YD%RDERBF11_IMPL)
!$acc update device (YD%RDERBF11_IMPL)
!$acc enter data attach (YD%RDERBF11_IMPL)

!$acc enter data create (YD%RDERBH01_IMPL)
!$acc update device (YD%RDERBH01_IMPL)
!$acc enter data attach (YD%RDERBH01_IMPL)

!$acc enter data create (YD%RDDERBF01_IMPL)
!$acc update device (YD%RDDERBF01_IMPL)
!$acc enter data attach (YD%RDDERBF01_IMPL)

!$acc enter data create (YD%RDDERBF10_IMPL)
!$acc update device (YD%RDDERBF10_IMPL)
!$acc enter data attach (YD%RDDERBF10_IMPL)

!$acc enter data create (YD%RDDERBF11_IMPL)
!$acc update device (YD%RDDERBF11_IMPL)
!$acc enter data attach (YD%RDDERBF11_IMPL)

!$acc enter data create (YD%RINTGW)
!$acc update device (YD%RINTGW)
!$acc enter data attach (YD%RINTGW)

!$acc enter data create (YD%RDERGW)
!$acc update device (YD%RDERGW)
!$acc enter data attach (YD%RDERGW)

END SUBROUTINE

END MODULE
