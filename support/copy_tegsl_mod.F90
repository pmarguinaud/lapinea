MODULE COPY_TEGSL_MOD

USE YEMGSL, ONLY : TEGSL

INTERFACE COPY
MODULE PROCEDURE COPY_TEGSL
END INTERFACE

CONTAINS

SUBROUTINE COPY_TEGSL (YD)

IMPLICIT NONE
TYPE (TEGSL), INTENT (IN) :: YD

!$acc update device (YD%RIPORE)

!$acc update device (YD%RJPORE)

!$acc update device (YD%REGPK)

!$acc update device (YD%RNOBILE)

!$acc update device (YD%RHSUD)

!$acc update device (YD%RMERCAT)

!$acc update device (YD%RLON0R)

!$acc update device (YD%RELONC)

!$acc update device (YD%RELATC)

END SUBROUTINE

END MODULE
