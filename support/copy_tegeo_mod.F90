MODULE COPY_TEGEO_MOD

USE YEMGEO, ONLY : TEGEO

INTERFACE COPY
MODULE PROCEDURE COPY_TEGEO
END INTERFACE

CONTAINS

SUBROUTINE COPY_TEGEO (YD)

IMPLICIT NONE
TYPE (TEGEO), INTENT (IN) :: YD

!$acc update device (YD%ERPK)

!$acc update device (YD%ELON1)

!$acc update device (YD%ELAT1)

!$acc update device (YD%ELON2)

!$acc update device (YD%ELAT2)

!$acc update device (YD%ELON0)

!$acc update device (YD%ELAT0)

!$acc update device (YD%ELONC)

!$acc update device (YD%ELATC)

!$acc update device (YD%EDELX)

!$acc update device (YD%EDELY)

!$acc update device (YD%ELX)

!$acc update device (YD%ELY)

!$acc update device (YD%EXWN)

!$acc update device (YD%EYWN)

!$acc update device (YD%RCORI_ACAD)

!$acc update device (YD%RLAT_ACAD)

!$acc update device (YD%RLON_ACAD)

!$acc update device (YD%LMAP)

!$acc update device (YD%LMRT)

!$acc update device (YD%LREDEL_IN_METRES)

END SUBROUTINE

END MODULE
