MODULE COPY_TYPE_GFLD_MOD

USE YOM_YGFL, ONLY : TYPE_GFLD

INTERFACE COPY
MODULE PROCEDURE COPY_TYPE_GFLD
END INTERFACE

CONTAINS

SUBROUTINE COPY_TYPE_GFLD (YD)
USE COPY_TYPE_GFL_COMP_MOD
USE COPY_TYPE_GFL_NAML_MOD
IMPLICIT NONE
TYPE (TYPE_GFLD), INTENT (IN) :: YD
INTEGER :: J1, J2

!$acc update device (YD%NUMFLDS)

!$acc update device (YD%NDERS)

!$acc update device (YD%NUMSPFLDS)

!$acc update device (YD%NUMGPFLDS)

!$acc update device (YD%NUMFLDS9)

!$acc update device (YD%NUMFLDS1)

!$acc update device (YD%NUMSPFLDS1)

!$acc update device (YD%NUMFLDS5)

!$acc update device (YD%NUMFLDSPHY)

!$acc update device (YD%NUMFLDS_SPL)

!$acc update device (YD%NUMFLDS_SL1)

!$acc update device (YD%NUMFLDSPC)

!$acc update device (YD%NDIM)

!$acc update device (YD%NUMFLDSPT)

!$acc update device (YD%NDIM0)

!$acc update device (YD%NDIM9)

!$acc update device (YD%NDIM1)

!$acc update device (YD%NDIM5)

!$acc update device (YD%NDIMSLP)

!$acc update device (YD%NDIM_SPL)

!$acc update device (YD%NDIMPT)

!$acc update device (YD%NDIMPC)

!$acc update device (YD%NGFL_EXT)

!$acc update device (YD%NGFL_FORC)

!$acc update device (YD%NGFL_EZDIAG)

!$acc update device (YD%NGHG)

!$acc update device (YD%NAERO)

!$acc update device (YD%NACTAERO)

!$acc update device (YD%NDDHAERO)

!$acc update device (YD%NERA40)

!$acc update device (YD%NNOGW)

!$acc update device (YD%NAEROUT)

!$acc update device (YD%NUVP)

!$acc update device (YD%NSLDIA)

!$acc update device (YD%NSLDIAGP)

!$acc update device (YD%NGFL_PHYS)

!$acc update device (YD%NCRM)

!$acc update device (YD%LAERCHEM)

!$acc update device (YD%LTRCMFIX)

!$acc update device (YD%LTRCMFIX_PS)

!$acc update device (YD%LAERAOT)

!$acc update device (YD%LAERLISI)

!$acc update device (YD%LAEROUT)

!$acc update device (YD%LUVPOUT)

!$acc update device (YD%LSPPTGFL)

!$acc update device (YD%NGEMS)

!$acc update device (YD%NCHEM)

!$acc update device (YD%NCHEM_ASSIM)

!$acc update device (YD%NCHEM_FLX)

!$acc update device (YD%NCHEM_DV)

!$acc update device (YD%NCHEM_TC)

!$acc update device (YD%NCHEM_SCV)

!$acc update device (YD%NLIMA)

!$acc update device (YD%NAERO_WVL_DIAG)

!$acc update device (YD%NAERO_WVL_DIAG_TYPES)

!$acc update device (YD%NNEGAFIX)

!$acc update device (YD%NOPTNEGFIX)

!$acc update device (YD%LQM3DCONS)

!$acc update device (YD%LADVNEGFIX)

!$acc update device (YD%LTRCMFBC)

!$acc update device (YD%LTRCMFPR)

!$acc update device (YD%LTRCMFMG)

!$acc update device (YD%LTRCMFP)

!$acc update device (YD%LTRCMFA_DIF)

!$acc update device (YD%LTRCMFA_LAP)

!$acc update device (YD%LTRCMFA_VER)

!$acc update device (YD%LEXTRADF)

!$acc update device (YD%NFLDSFIX)

!$acc update device (YD%NOPTMFBC)

!$acc update device (YD%NOPTMFPR)

!$acc update device (YD%NOPTVFE)

!$acc update device (YD%NMFDIAGLEV)

!$acc update device (YD%NMFIXFLDS)

!$acc update device (YD%NNEGFLDS)

!$acc update device (YD%ZMFIXEPS)

DO J1 = LBOUND (YD%YCOMP, 1), UBOUND (YD%YCOMP, 1)
  CALL COPY (YD%YCOMP (J1))
ENDDO

!$acc enter data create (YD%YLASTCOMP)
CALL COPY (YD%YLASTCOMP)
!$acc enter data attach (YD%YLASTCOMP)

!$acc enter data create (YD%YQ)
CALL COPY (YD%YQ)
!$acc enter data attach (YD%YQ)

!$acc enter data create (YD%YI)
CALL COPY (YD%YI)
!$acc enter data attach (YD%YI)

!$acc enter data create (YD%YL)
CALL COPY (YD%YL)
!$acc enter data attach (YD%YL)

!$acc enter data create (YD%YLCONV)
CALL COPY (YD%YLCONV)
!$acc enter data attach (YD%YLCONV)

!$acc enter data create (YD%YICONV)
CALL COPY (YD%YICONV)
!$acc enter data attach (YD%YICONV)

!$acc enter data create (YD%YRCONV)
CALL COPY (YD%YRCONV)
!$acc enter data attach (YD%YRCONV)

!$acc enter data create (YD%YSCONV)
CALL COPY (YD%YSCONV)
!$acc enter data attach (YD%YSCONV)

!$acc enter data create (YD%YIRAD)
CALL COPY (YD%YIRAD)
!$acc enter data attach (YD%YIRAD)

!$acc enter data create (YD%YLRAD)
CALL COPY (YD%YLRAD)
!$acc enter data attach (YD%YLRAD)

!$acc enter data create (YD%YS)
CALL COPY (YD%YS)
!$acc enter data attach (YD%YS)

!$acc enter data create (YD%YR)
CALL COPY (YD%YR)
!$acc enter data attach (YD%YR)

!$acc enter data create (YD%YG)
CALL COPY (YD%YG)
!$acc enter data attach (YD%YG)

!$acc enter data create (YD%YH)
CALL COPY (YD%YH)
!$acc enter data attach (YD%YH)

!$acc enter data create (YD%YTKE)
CALL COPY (YD%YTKE)
!$acc enter data attach (YD%YTKE)

!$acc enter data create (YD%YTTE)
CALL COPY (YD%YTTE)
!$acc enter data attach (YD%YTTE)

!$acc enter data create (YD%YEFB1)
CALL COPY (YD%YEFB1)
!$acc enter data attach (YD%YEFB1)

!$acc enter data create (YD%YEFB2)
CALL COPY (YD%YEFB2)
!$acc enter data attach (YD%YEFB2)

!$acc enter data create (YD%YEFB3)
CALL COPY (YD%YEFB3)
!$acc enter data attach (YD%YEFB3)

!$acc enter data create (YD%YA)
CALL COPY (YD%YA)
!$acc enter data attach (YD%YA)

!$acc enter data create (YD%YO3)
CALL COPY (YD%YO3)
!$acc enter data attach (YD%YO3)

!$acc enter data create (YD%YSRC)
CALL COPY (YD%YSRC)
!$acc enter data attach (YD%YSRC)

!$acc enter data create (YD%YMXL)
CALL COPY (YD%YMXL)
!$acc enter data attach (YD%YMXL)

!$acc enter data create (YD%YSHTUR)
CALL COPY (YD%YSHTUR)
!$acc enter data attach (YD%YSHTUR)

!$acc enter data create (YD%YFQTUR)
CALL COPY (YD%YFQTUR)
!$acc enter data attach (YD%YFQTUR)

!$acc enter data create (YD%YFSTUR)
CALL COPY (YD%YFSTUR)
!$acc enter data attach (YD%YFSTUR)

!$acc enter data create (YD%YCPF)
CALL COPY (YD%YCPF)
!$acc enter data attach (YD%YCPF)

!$acc enter data create (YD%YSPF)
CALL COPY (YD%YSPF)
!$acc enter data attach (YD%YSPF)

!$acc enter data create (YD%YCVGQ)
CALL COPY (YD%YCVGQ)
!$acc enter data attach (YD%YCVGQ)

!$acc enter data create (YD%YQVA)
CALL COPY (YD%YQVA)
!$acc enter data attach (YD%YQVA)

!$acc enter data create (YD%YGHG)
DO J1 = LBOUND (YD%YGHG, 1), UBOUND (YD%YGHG, 1)
  CALL COPY (YD%YGHG (J1))
ENDDO
!$acc enter data attach (YD%YGHG)

!$acc enter data create (YD%YCHEM)
DO J1 = LBOUND (YD%YCHEM, 1), UBOUND (YD%YCHEM, 1)
  CALL COPY (YD%YCHEM (J1))
ENDDO
!$acc enter data attach (YD%YCHEM)

!$acc enter data create (YD%YAERO)
DO J1 = LBOUND (YD%YAERO, 1), UBOUND (YD%YAERO, 1)
  CALL COPY (YD%YAERO (J1))
ENDDO
!$acc enter data attach (YD%YAERO)

!$acc enter data create (YD%YLRCH4)
CALL COPY (YD%YLRCH4)
!$acc enter data attach (YD%YLRCH4)

!$acc enter data create (YD%YFORC)
DO J1 = LBOUND (YD%YFORC, 1), UBOUND (YD%YFORC, 1)
  CALL COPY (YD%YFORC (J1))
ENDDO
!$acc enter data attach (YD%YFORC)

!$acc enter data create (YD%YEZDIAG)
DO J1 = LBOUND (YD%YEZDIAG, 1), UBOUND (YD%YEZDIAG, 1)
  CALL COPY (YD%YEZDIAG (J1))
ENDDO
!$acc enter data attach (YD%YEZDIAG)

!$acc enter data create (YD%YERA40)
DO J1 = LBOUND (YD%YERA40, 1), UBOUND (YD%YERA40, 1)
  CALL COPY (YD%YERA40 (J1))
ENDDO
!$acc enter data attach (YD%YERA40)

!$acc enter data create (YD%YNOGW)
DO J1 = LBOUND (YD%YNOGW, 1), UBOUND (YD%YNOGW, 1)
  CALL COPY (YD%YNOGW (J1))
ENDDO
!$acc enter data attach (YD%YNOGW)

!$acc enter data create (YD%YSLDIA)
DO J1 = LBOUND (YD%YSLDIA, 1), UBOUND (YD%YSLDIA, 1)
  CALL COPY (YD%YSLDIA (J1))
ENDDO
!$acc enter data attach (YD%YSLDIA)

!$acc enter data create (YD%YAERAOT)
DO J1 = LBOUND (YD%YAERAOT, 1), UBOUND (YD%YAERAOT, 1)
  CALL COPY (YD%YAERAOT (J1))
ENDDO
!$acc enter data attach (YD%YAERAOT)

!$acc enter data create (YD%YAERLISI)
DO J1 = LBOUND (YD%YAERLISI, 1), UBOUND (YD%YAERLISI, 1)
  CALL COPY (YD%YAERLISI (J1))
ENDDO
!$acc enter data attach (YD%YAERLISI)

!$acc enter data create (YD%YAEROUT)
DO J1 = LBOUND (YD%YAEROUT, 1), UBOUND (YD%YAEROUT, 1)
  CALL COPY (YD%YAEROUT (J1))
ENDDO
!$acc enter data attach (YD%YAEROUT)

!$acc enter data create (YD%YUVP)
DO J1 = LBOUND (YD%YUVP, 1), UBOUND (YD%YUVP, 1)
  CALL COPY (YD%YUVP (J1))
ENDDO
!$acc enter data attach (YD%YUVP)

!$acc enter data create (YD%YPHYS)
DO J1 = LBOUND (YD%YPHYS, 1), UBOUND (YD%YPHYS, 1)
  CALL COPY (YD%YPHYS (J1))
ENDDO
!$acc enter data attach (YD%YPHYS)

!$acc enter data create (YD%YRSPEC)
CALL COPY (YD%YRSPEC)
!$acc enter data attach (YD%YRSPEC)

!$acc enter data create (YD%YSDSAT)
CALL COPY (YD%YSDSAT)
!$acc enter data attach (YD%YSDSAT)

!$acc enter data create (YD%YCVV)
CALL COPY (YD%YCVV)
!$acc enter data attach (YD%YCVV)

!$acc enter data create (YD%YRKTH)
CALL COPY (YD%YRKTH)
!$acc enter data attach (YD%YRKTH)

!$acc enter data create (YD%YRKTQV)
CALL COPY (YD%YRKTQV)
!$acc enter data attach (YD%YRKTQV)

!$acc enter data create (YD%YRKTQC)
CALL COPY (YD%YRKTQC)
!$acc enter data attach (YD%YRKTQC)

!$acc enter data create (YD%YUOM)
CALL COPY (YD%YUOM)
!$acc enter data attach (YD%YUOM)

!$acc enter data create (YD%YUAL)
CALL COPY (YD%YUAL)
!$acc enter data attach (YD%YUAL)

!$acc enter data create (YD%YDOM)
CALL COPY (YD%YDOM)
!$acc enter data attach (YD%YDOM)

!$acc enter data create (YD%YDAL)
CALL COPY (YD%YDAL)
!$acc enter data attach (YD%YDAL)

!$acc enter data create (YD%YUEN)
CALL COPY (YD%YUEN)
!$acc enter data attach (YD%YUEN)

!$acc enter data create (YD%YUNEBH)
CALL COPY (YD%YUNEBH)
!$acc enter data attach (YD%YUNEBH)

!$acc enter data create (YD%YCRM)
DO J1 = LBOUND (YD%YCRM, 1), UBOUND (YD%YCRM, 1)
  CALL COPY (YD%YCRM (J1))
ENDDO
!$acc enter data attach (YD%YCRM)

!$acc enter data create (YD%YLIMA)
DO J1 = LBOUND (YD%YLIMA, 1), UBOUND (YD%YLIMA, 1)
  CALL COPY (YD%YLIMA (J1))
ENDDO
!$acc enter data attach (YD%YLIMA)

!$acc enter data create (YD%YEXT)
DO J1 = LBOUND (YD%YEXT, 1), UBOUND (YD%YEXT, 1)
  CALL COPY (YD%YEXT (J1))
ENDDO
!$acc enter data attach (YD%YEXT)

CALL COPY (YD%YQ_NL)

CALL COPY (YD%YI_NL)

CALL COPY (YD%YL_NL)

CALL COPY (YD%YLCONV_NL)

CALL COPY (YD%YICONV_NL)

CALL COPY (YD%YRCONV_NL)

CALL COPY (YD%YSCONV_NL)

CALL COPY (YD%YIRAD_NL)

CALL COPY (YD%YLRAD_NL)

CALL COPY (YD%YS_NL)

CALL COPY (YD%YR_NL)

CALL COPY (YD%YG_NL)

CALL COPY (YD%YH_NL)

CALL COPY (YD%YTKE_NL)

CALL COPY (YD%YTTE_NL)

CALL COPY (YD%YEFB1_NL)

CALL COPY (YD%YEFB2_NL)

CALL COPY (YD%YEFB3_NL)

CALL COPY (YD%YA_NL)

CALL COPY (YD%YO3_NL)

CALL COPY (YD%YSRC_NL)

CALL COPY (YD%YMXL_NL)

CALL COPY (YD%YSHTUR_NL)

CALL COPY (YD%YFQTUR_NL)

CALL COPY (YD%YFSTUR_NL)

CALL COPY (YD%YCPF_NL)

CALL COPY (YD%YSPF_NL)

CALL COPY (YD%YCVGQ_NL)

CALL COPY (YD%YQVA_NL)

DO J1 = LBOUND (YD%YGHG_NL, 1), UBOUND (YD%YGHG_NL, 1)
  CALL COPY (YD%YGHG_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YCHEM_NL, 1), UBOUND (YD%YCHEM_NL, 1)
  CALL COPY (YD%YCHEM_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YAERO_NL, 1), UBOUND (YD%YAERO_NL, 1)
  CALL COPY (YD%YAERO_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YERA40_NL, 1), UBOUND (YD%YERA40_NL, 1)
  CALL COPY (YD%YERA40_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YNOGW_NL, 1), UBOUND (YD%YNOGW_NL, 1)
  CALL COPY (YD%YNOGW_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YSLDIA_NL, 1), UBOUND (YD%YSLDIA_NL, 1)
  CALL COPY (YD%YSLDIA_NL (J1))
ENDDO

CALL COPY (YD%YLRCH4_NL)

DO J1 = LBOUND (YD%YAERAOT_NL, 1), UBOUND (YD%YAERAOT_NL, 1)
  CALL COPY (YD%YAERAOT_NL (J1))
ENDDO

DO J2 = LBOUND (YD%YAERLISI_NL, 2), UBOUND (YD%YAERLISI_NL, 2)
  DO J1 = LBOUND (YD%YAERLISI_NL, 1), UBOUND (YD%YAERLISI_NL, 1)
    CALL COPY (YD%YAERLISI_NL (J1, J2))
  ENDDO
ENDDO

DO J1 = LBOUND (YD%YAEROUT_NL, 1), UBOUND (YD%YAEROUT_NL, 1)
  CALL COPY (YD%YAEROUT_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YUVP_NL, 1), UBOUND (YD%YUVP_NL, 1)
  CALL COPY (YD%YUVP_NL (J1))
ENDDO

CALL COPY (YD%YRKTH_NL)

CALL COPY (YD%YRKTQV_NL)

CALL COPY (YD%YRKTQC_NL)

DO J1 = LBOUND (YD%YPHYS_NL, 1), UBOUND (YD%YPHYS_NL, 1)
  CALL COPY (YD%YPHYS_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YCRM_NL, 1), UBOUND (YD%YCRM_NL, 1)
  CALL COPY (YD%YCRM_NL (J1))
ENDDO

CALL COPY (YD%YRSPEC_NL)

CALL COPY (YD%YSDSAT_NL)

CALL COPY (YD%YCVV_NL)

DO J1 = LBOUND (YD%YFORC_NL, 1), UBOUND (YD%YFORC_NL, 1)
  CALL COPY (YD%YFORC_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YEZDIAG_NL, 1), UBOUND (YD%YEZDIAG_NL, 1)
  CALL COPY (YD%YEZDIAG_NL (J1))
ENDDO

DO J1 = LBOUND (YD%YEXT_NL, 1), UBOUND (YD%YEXT_NL, 1)
  CALL COPY (YD%YEXT_NL (J1))
ENDDO

CALL COPY (YD%YUOM_NL)

CALL COPY (YD%YUAL_NL)

CALL COPY (YD%YDOM_NL)

CALL COPY (YD%YDAL_NL)

CALL COPY (YD%YUEN_NL)

CALL COPY (YD%YUNEBH_NL)

DO J1 = LBOUND (YD%YLIMA_NL, 1), UBOUND (YD%YLIMA_NL, 1)
  CALL COPY (YD%YLIMA_NL (J1))
ENDDO

END SUBROUTINE

END MODULE
