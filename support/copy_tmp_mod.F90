MODULE COPY_TMP_MOD

USE YOMMP, ONLY : TMP

INTERFACE COPY
MODULE PROCEDURE COPY_TMP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TMP (YD)

IMPLICIT NONE
TYPE (TMP), INTENT (IN) :: YD

!$acc enter data create (YD%NUMPP)
!$acc update device (YD%NUMPP)
!$acc enter data attach (YD%NUMPP)

!$acc enter data create (YD%NPROCM)
!$acc update device (YD%NPROCM)
!$acc enter data attach (YD%NPROCM)

!$acc enter data create (YD%NPTRMS)
!$acc update device (YD%NPTRMS)
!$acc enter data attach (YD%NPTRMS)

!$acc enter data create (YD%NALLMS)
!$acc update device (YD%NALLMS)
!$acc enter data attach (YD%NALLMS)

!$acc enter data create (YD%NPTRLS)
!$acc update device (YD%NPTRLS)
!$acc enter data attach (YD%NPTRLS)

!$acc enter data create (YD%NPTRSV)
!$acc update device (YD%NPTRSV)
!$acc enter data attach (YD%NPTRSV)

!$acc enter data create (YD%NPTRSVF)
!$acc update device (YD%NPTRSVF)
!$acc enter data attach (YD%NPTRSVF)

!$acc enter data create (YD%NPTRMF)
!$acc update device (YD%NPTRMF)
!$acc enter data attach (YD%NPTRMF)

!$acc enter data create (YD%NSPSTAF)
!$acc update device (YD%NSPSTAF)
!$acc enter data attach (YD%NSPSTAF)

!$acc enter data create (YD%NUMLL)
!$acc update device (YD%NUMLL)
!$acc enter data attach (YD%NUMLL)

!$acc enter data create (YD%NPTRLL)
!$acc update device (YD%NPTRLL)
!$acc enter data attach (YD%NPTRLL)

!$acc enter data create (YD%MYLEVS)
!$acc update device (YD%MYLEVS)
!$acc enter data attach (YD%MYLEVS)

!$acc enter data create (YD%NPSURF)
!$acc update device (YD%NPSURF)
!$acc enter data attach (YD%NPSURF)

!$acc enter data create (YD%NSTA)
!$acc update device (YD%NSTA)
!$acc enter data attach (YD%NSTA)

!$acc enter data create (YD%NONL)
!$acc update device (YD%NONL)
!$acc enter data attach (YD%NONL)

!$acc enter data create (YD%NPTRFRSTLAT)
!$acc update device (YD%NPTRFRSTLAT)
!$acc enter data attach (YD%NPTRFRSTLAT)

!$acc enter data create (YD%NPTRLSTLAT)
!$acc update device (YD%NPTRLSTLAT)
!$acc enter data attach (YD%NPTRLSTLAT)

!$acc enter data create (YD%NPTRLAT)
!$acc update device (YD%NPTRLAT)
!$acc enter data attach (YD%NPTRLAT)

!$acc enter data create (YD%NFRSTLAT)
!$acc update device (YD%NFRSTLAT)
!$acc enter data attach (YD%NFRSTLAT)

!$acc enter data create (YD%NLSTLAT)
!$acc update device (YD%NLSTLAT)
!$acc enter data attach (YD%NLSTLAT)

!$acc enter data create (YD%NBSETLEV)
!$acc update device (YD%NBSETLEV)
!$acc enter data attach (YD%NBSETLEV)

!$acc enter data create (YD%NGLOBALINDEX)
!$acc update device (YD%NGLOBALINDEX)
!$acc enter data attach (YD%NGLOBALINDEX)

!$acc enter data create (YD%NGLOBALAT)
!$acc update device (YD%NGLOBALAT)
!$acc enter data attach (YD%NGLOBALAT)

!$acc enter data create (YD%NGLOBALPROC)
!$acc update device (YD%NGLOBALPROC)
!$acc enter data attach (YD%NGLOBALPROC)

!$acc enter data create (YD%NLOCALINDEX)
!$acc update device (YD%NLOCALINDEX)
!$acc enter data attach (YD%NLOCALINDEX)

!$acc enter data create (YD%NLATGPP)
!$acc update device (YD%NLATGPP)
!$acc enter data attach (YD%NLATGPP)

!$acc enter data create (YD%NLONGPP)
!$acc update device (YD%NLONGPP)
!$acc enter data attach (YD%NLONGPP)

!$acc enter data create (YD%LSPLITLAT)
!$acc update device (YD%LSPLITLAT)
!$acc enter data attach (YD%LSPLITLAT)

!$acc enter data create (YD%MYLATS)
!$acc update device (YD%MYLATS)
!$acc enter data attach (YD%MYLATS)

!$acc update device (YD%NPSP)

!$acc update device (YD%NSPEC2V)

!$acc update device (YD%NSPEC2VF)

!$acc update device (YD%NSPEC2VDDH)

!$acc update device (YD%NSPEC2V_NH)

!$acc update device (YD%NSPEC2V_NHX)

!$acc update device (YD%NBSETSP)

!$acc update device (YD%NFRSTLOFF)

!$acc update device (YD%MYFRSTACTLAT)

!$acc update device (YD%MYLSTACTLAT)

!$acc update device (YD%NPTRFLOFF)

!$acc enter data create (YD%NPOSSP)
!$acc update device (YD%NPOSSP)
!$acc enter data attach (YD%NPOSSP)

!$acc enter data create (YD%NDIM0G)
!$acc update device (YD%NDIM0G)
!$acc enter data attach (YD%NDIM0G)

END SUBROUTINE

END MODULE
