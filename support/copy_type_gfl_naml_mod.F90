MODULE COPY_TYPE_GFL_NAML_MOD

USE YOM_YGFL, ONLY : TYPE_GFL_NAML

INTERFACE COPY
MODULE PROCEDURE COPY_TYPE_GFL_NAML
END INTERFACE

CONTAINS

SUBROUTINE COPY_TYPE_GFL_NAML (YD)

IMPLICIT NONE
TYPE (TYPE_GFL_NAML), INTENT (IN) :: YD

!$acc update device (YD%CNAME)

!$acc update device (YD%IGRBCODE)

!$acc update device (YD%NREQIN)

!$acc update device (YD%REFVALI)

!$acc update device (YD%LREQOUT)

!$acc update device (YD%LGPINGP)

!$acc update device (YD%LGP)

!$acc update device (YD%LSP)

!$acc update device (YD%LCDERS)

!$acc update device (YD%LT9)

!$acc update device (YD%LT1)

!$acc update device (YD%LT5)

!$acc update device (YD%LPHY)

!$acc update device (YD%LPT)

!$acc update device (YD%LTRAJIO)

!$acc update device (YD%LDIAG)

!$acc update device (YD%LPC)

!$acc update device (YD%LADV)

!$acc update device (YD%LADV5)

!$acc update device (YD%LINTLIN)

!$acc update device (YD%LTDIABLIN)

!$acc update device (YD%LHORTURB)

!$acc update device (YD%LQM)

!$acc update device (YD%LQMH)

!$acc update device (YD%LQM3D)

!$acc update device (YD%LQML3D)

!$acc update device (YD%LSLHD)

!$acc update device (YD%LCOMAD)

!$acc update device (YD%LHV)

!$acc update device (YD%LVSPLIP)

!$acc update device (YD%NCOUPLING)

!$acc update device (YD%REFVALC)

!$acc update device (YD%NCOUPLO4)

!$acc update device (YD%LASSIM)

!$acc update device (YD%IGRIBDV)

!$acc update device (YD%IGRIBTC)

!$acc update device (YD%IGRIBSFC)

!$acc update device (YD%IGRIBDIAG)

!$acc update device (YD%LDIFF)

!$acc update device (YD%LCONV)

!$acc update device (YD%LNEGFIX)

!$acc update device (YD%LMASSFIX)

!$acc update device (YD%BETAMFBC)

!$acc update device (YD%RMOLMASS)

!$acc update device (YD%REFOLD)

!$acc update device (YD%HENRYA)

!$acc update device (YD%HENRYB)

END SUBROUTINE

END MODULE
