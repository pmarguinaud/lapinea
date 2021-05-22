MODULE COPY_TDIM_MOD

USE YOMDIM, ONLY : TDIM

INTERFACE COPY
MODULE PROCEDURE COPY_TDIM
END INTERFACE

CONTAINS

SUBROUTINE COPY_TDIM (YD)

IMPLICIT NONE
TYPE (TDIM), INTENT (IN) :: YD

!$acc update device (YD%NDGLG)

!$acc update device (YD%NDGLL)

!$acc update device (YD%NDGNH)

!$acc update device (YD%NDGSUR)

!$acc update device (YD%NDGSAG)

!$acc update device (YD%NDGSAL)

!$acc update device (YD%NDGSAH)

!$acc update device (YD%NDGSAFPH)

!$acc update device (YD%NDGENG)

!$acc update device (YD%NDGENL)

!$acc update device (YD%NDGENH)

!$acc update device (YD%NDGENFPH)

!$acc update device (YD%NDGUNG)

!$acc update device (YD%NDGUXG)

!$acc update device (YD%NDGUNL)

!$acc update device (YD%NDGUXL)

!$acc update device (YD%NDLON)

!$acc update device (YD%NDSUR1)

!$acc update device (YD%NSTENCILWIDE)

!$acc update device (YD%NDLSUR)

!$acc update device (YD%NDLSM)

!$acc update device (YD%NDLUNG)

!$acc update device (YD%NDLUXG)

!$acc enter data create (YD%NDLUNL)
!$acc update device (YD%NDLUNL)
!$acc enter data attach (YD%NDLUNL)

!$acc enter data create (YD%NDLUXL)
!$acc update device (YD%NDLUXL)
!$acc enter data attach (YD%NDLUXL)

!$acc update device (YD%NPROMA)

!$acc update device (YD%NPROMA9)

!$acc update device (YD%NPROMM)

!$acc update device (YD%NPROMM9)

!$acc update device (YD%NPROMNH)

!$acc update device (YD%NPROMNH9)

!$acc update device (YD%NGPBLKS)

!$acc update device (YD%LOPTPROMA)

!$acc update device (YD%NRESOL)

!$acc update device (YD%NSMAX)

!$acc update device (YD%NMSMAX)

!$acc update device (YD%NVARMAX)

!$acc update device (YD%NSEFRE)

!$acc update device (YD%NSPECG)

!$acc update device (YD%NSPEC2G)

!$acc update device (YD%NSPEC)

!$acc update device (YD%NSPEC2)

!$acc update device (YD%NSPEC2MX)

!$acc update device (YD%NCMAX)

!$acc update device (YD%NUMP)

!$acc update device (YD%NUMCP)

END SUBROUTINE

END MODULE
