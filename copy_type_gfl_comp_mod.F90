MODULE COPY_TYPE_GFL_COMP_MOD

USE YOM_YGFL, ONLY : TYPE_GFL_COMP

INTERFACE COPY
MODULE PROCEDURE COPY_TYPE_GFL_COMP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TYPE_GFL_COMP (YD)

IMPLICIT NONE
TYPE (TYPE_GFL_COMP), INTENT (IN) :: YD

!$acc update device (YD%CNAME)

!$acc update device (YD%IGRBCODE)

!$acc update device (YD%LADV)

!$acc update device (YD%LADV5)

!$acc update device (YD%LTDIABLIN)

!$acc update device (YD%LHORTURB)

!$acc update device (YD%NREQIN)

!$acc update device (YD%LREQOUT)

!$acc update device (YD%LGPINGP)

!$acc update device (YD%LGP)

!$acc update device (YD%LSP)

!$acc update device (YD%LCDERS)

!$acc update device (YD%LACTIVE)

!$acc update device (YD%LTHERMACT)

!$acc update device (YD%R)

!$acc update device (YD%RCP)

!$acc update device (YD%LT9)

!$acc update device (YD%LT1)

!$acc update device (YD%LT5)

!$acc update device (YD%LPHY)

!$acc update device (YD%LPT)

!$acc update device (YD%LTRAJIO)

!$acc update device (YD%LDIAG)

!$acc update device (YD%LPC)

!$acc update device (YD%REFVALI)

!$acc update device (YD%LADJUST0)

!$acc update device (YD%LADJUST1)

!$acc update device (YD%NCOUPLING)

!$acc update device (YD%REFVALC)

!$acc update device (YD%LBIPER)

!$acc update device (YD%CSLINT)

!$acc update device (YD%MP)

!$acc update device (YD%MPL)

!$acc update device (YD%MPM)

!$acc update device (YD%MP9)

!$acc update device (YD%MP9_PH)

!$acc update device (YD%MP1)

!$acc update device (YD%MP5)

!$acc update device (YD%MP5L)

!$acc update device (YD%MP5M)

!$acc update device (YD%MPSLP)

!$acc update device (YD%MPSP)

!$acc update device (YD%MP_SPL)

!$acc update device (YD%MP_SL1)

!$acc update device (YD%MP_SLX)

!$acc update device (YD%MPPT)

!$acc update device (YD%MPPC)

!$acc update device (YD%LWATER)

!$acc update device (YD%LPRECIP)

!$acc update device (YD%RLZER)

!$acc update device (YD%NCOUPLO4)

!$acc update device (YD%LASSIM)

!$acc update device (YD%IGRIBDV)

!$acc update device (YD%IGRIBTC)

!$acc update device (YD%IGRIBSFC)

!$acc update device (YD%IGRIBDIAG)

!$acc update device (YD%LDIFF)

!$acc update device (YD%LCONV)

!$acc update device (YD%RMOLMASS)

!$acc update device (YD%REFOLD)

!$acc update device (YD%HENRYA)

!$acc update device (YD%HENRYB)

!$acc update device (YD%LNEGFIX)

!$acc update device (YD%LCOMAD)

!$acc update device (YD%LMASSFIX)

!$acc update device (YD%BETAMFBC)

!$acc update device (YD%IFID)

END SUBROUTINE

END MODULE
