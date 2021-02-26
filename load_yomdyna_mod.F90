MODULE LOAD_YOMDYNA_MOD

USE YOMDYNA

CONTAINS

SUBROUTINE LOAD_YOMDYNA (KLUN)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN

READ (KLUN) NPDVAR
!$acc update device (NPDVAR)
READ (KLUN) NVDVAR
!$acc update device (NVDVAR)
READ (KLUN) ND4SYS
!$acc update device (ND4SYS)
READ (KLUN) LNHX
!$acc update device (LNHX)
READ (KLUN) LNHXDER
!$acc update device (LNHXDER)
READ (KLUN) LGWADV
!$acc update device (LGWADV)
READ (KLUN) NGWADVSI
!$acc update device (NGWADVSI)
READ (KLUN) LRDBBC
!$acc update device (LRDBBC)
READ (KLUN) LSI_NHEE
!$acc update device (LSI_NHEE)
READ (KLUN) LNHQE_C2
!$acc update device (LNHQE_C2)
READ (KLUN) LNHQE_SIHYD
!$acc update device (LNHQE_SIHYD)
READ (KLUN) LNHEE_SVDLAPL_FIRST
!$acc update device (LNHEE_SVDLAPL_FIRST)
READ (KLUN) LSLHD
!$acc update device (LSLHD)
READ (KLUN) LSLHD_W
!$acc update device (LSLHD_W)
READ (KLUN) LSLHD_T
!$acc update device (LSLHD_T)
READ (KLUN) LSLHD_SPD
!$acc update device (LSLHD_SPD)
READ (KLUN) LSLHD_SVD
!$acc update device (LSLHD_SVD)
READ (KLUN) LSLHD_GFL
!$acc update device (LSLHD_GFL)
READ (KLUN) LSLHD_OLD
!$acc update device (LSLHD_OLD)
READ (KLUN) LSLHD_STATIC
!$acc update device (LSLHD_STATIC)
READ (KLUN) LSLHDQUAD
!$acc update device (LSLHDQUAD)
READ (KLUN) LSLHDVER
!$acc update device (LSLHDVER)
READ (KLUN) SLHDKMIN
!$acc update device (SLHDKMIN)
READ (KLUN) SLHDKMAX
!$acc update device (SLHDKMAX)
READ (KLUN) SLHDKREF
!$acc update device (SLHDKREF)
READ (KLUN) SLHDEPSH
!$acc update device (SLHDEPSH)
READ (KLUN) SLHDEPSV
!$acc update device (SLHDEPSV)
READ (KLUN) LGRADSP
!$acc update device (LGRADSP)
READ (KLUN) LGRADGP
!$acc update device (LGRADGP)
READ (KLUN) L3DTURB
!$acc update device (L3DTURB)
READ (KLUN) LSLDIA
!$acc update device (LSLDIA)
READ (KLUN) LRPRSLTRJ
!$acc update device (LRPRSLTRJ)
READ (KLUN) LRALTVDISP
!$acc update device (LRALTVDISP)
READ (KLUN) LVSPLIP
!$acc update device (LVSPLIP)
READ (KLUN) LCOMAD
!$acc update device (LCOMAD)
READ (KLUN) LCOMADH
!$acc update device (LCOMADH)
READ (KLUN) LCOMADV
!$acc update device (LCOMADV)
READ (KLUN) LCOMAD_W
!$acc update device (LCOMAD_W)
READ (KLUN) LCOMAD_T
!$acc update device (LCOMAD_T)
READ (KLUN) LCOMAD_SPD
!$acc update device (LCOMAD_SPD)
READ (KLUN) LCOMAD_SVD
!$acc update device (LCOMAD_SVD)
READ (KLUN) LCOMAD_SP
!$acc update device (LCOMAD_SP)
READ (KLUN) LCOMAD_GFL
!$acc update device (LCOMAD_GFL)
READ (KLUN) LNESCT
!$acc update device (LNESCT)
READ (KLUN) LNESCV
!$acc update device (LNESCV)
READ (KLUN) LNESC
!$acc update device (LNESC)
READ (KLUN) LSETTLST
!$acc update device (LSETTLST)
READ (KLUN) LSETTLSV
!$acc update device (LSETTLSV)
READ (KLUN) LSETTLS
!$acc update device (LSETTLS)
READ (KLUN) LELTRA
!$acc update device (LELTRA)
READ (KLUN) LSLINLC2
!$acc update device (LSLINLC2)
READ (KLUN) LSLINL
!$acc update device (LSLINL)
READ (KLUN) LAPRXPK
!$acc update device (LAPRXPK)
READ (KLUN) NDLNPR
!$acc update device (NDLNPR)
READ (KLUN) RHYDR0
!$acc update device (RHYDR0)
READ (KLUN) LRUBC
!$acc update device (LRUBC)
READ (KLUN) LPC_FULL
!$acc update device (LPC_FULL)
READ (KLUN) LPC_CHEAP
!$acc update device (LPC_CHEAP)
READ (KLUN) LPC_CHEAP2
!$acc update device (LPC_CHEAP2)
READ (KLUN) LMIXETTLS
!$acc update device (LMIXETTLS)
READ (KLUN) LMIXETTLS_PRINT
!$acc update device (LMIXETTLS_PRINT)
READ (KLUN) RMIXNL_TRH
!$acc update device (RMIXNL_TRH)
READ (KLUN) LDRY_ECMWF
!$acc update device (LDRY_ECMWF)
READ (KLUN) L_RDRY_VD
!$acc update device (L_RDRY_VD)
END SUBROUTINE


END MODULE
