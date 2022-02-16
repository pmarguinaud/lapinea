MODULE COPY_TRSCAW_MOD

USE INTDYNSL_MOD, ONLY : TRSCAW

INTERFACE COPY
MODULE PROCEDURE COPY_TRSCAW
END INTERFACE

CONTAINS

SUBROUTINE COPY_TRSCAW (YD)

IMPLICIT NONE
TYPE (TRSCAW), INTENT (IN) :: YD

!$acc update device (YD%M_WCLO)

!$acc update device (YD%M_WCLA)

!$acc update device (YD%M_WVINTW)

!$acc update device (YD%M_WCLOSLD)

!$acc update device (YD%M_WCLASLD)

!$acc update device (YD%M_WCLOSLT)

!$acc update device (YD%M_WCLASLT)

!$acc update device (YD%M_WVINTWSLD)

!$acc update device (YD%M_WVINTWSLT)

!$acc update device (YD%M_WCLOMAD)

!$acc update device (YD%M_WCLAMAD)

!$acc update device (YD%M_WVINTWMAD)

!$acc update device (YD%M_WVINTWS)

!$acc update device (YD%M_WVDERW)

!$acc update device (YD%M_WHVW)

!$acc update device (YD%NDIM)

END SUBROUTINE

END MODULE
