MODULE LOAD_YOMCVER_MOD

USE YOMCVER

CONTAINS

SUBROUTINE LOAD_YOMCVER (KLUN)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN

READ (KLUN) NVSCH
!$acc update device (NVSCH)
READ (KLUN) NVFE_TYPE
!$acc update device (NVFE_TYPE)
READ (KLUN) NVFE_ORDER
!$acc update device (NVFE_ORDER)
READ (KLUN) NVFE_INTBC
!$acc update device (NVFE_INTBC)
READ (KLUN) NVFE_DERBC
!$acc update device (NVFE_DERBC)
READ (KLUN) NVFE_INTERNALS
!$acc update device (NVFE_INTERNALS)
READ (KLUN) NVFE_BC
!$acc update device (NVFE_BC)
READ (KLUN) LVERTFE
!$acc update device (LVERTFE)
READ (KLUN) LVFE_LAPL
!$acc update device (LVFE_LAPL)
READ (KLUN) LVFE_LAPL_BC
!$acc update device (LVFE_LAPL_BC)
READ (KLUN) LVFE_LAPL_TBC
!$acc update device (LVFE_LAPL_TBC)
READ (KLUN) LVFE_LAPL_BBC
!$acc update device (LVFE_LAPL_BBC)
READ (KLUN) LVFE_LAPL2PI
!$acc update device (LVFE_LAPL2PI)
READ (KLUN) RLAPL2PI
!$acc update device (RLAPL2PI)
READ (KLUN) LVFE_X_TERM
!$acc update device (LVFE_X_TERM)
READ (KLUN) LVFE_Z_TERM
!$acc update device (LVFE_Z_TERM)
READ (KLUN) LVFE_GW
!$acc update device (LVFE_GW)
READ (KLUN) LVFE_DELNHPRE
!$acc update device (LVFE_DELNHPRE)
READ (KLUN) LVFE_GWMPA
!$acc update device (LVFE_GWMPA)
READ (KLUN) LVFE_CENTRI
!$acc update device (LVFE_CENTRI)
READ (KLUN) RVFE_CENTRI
!$acc update device (RVFE_CENTRI)
READ (KLUN) LVFE_APPROX
!$acc update device (LVFE_APPROX)
READ (KLUN) LVFE_ECMWF
!$acc update device (LVFE_ECMWF)
READ (KLUN) LVFE_LAPL_HALF
!$acc update device (LVFE_LAPL_HALF)
READ (KLUN) LVFE_FIX_ORDER
!$acc update device (LVFE_FIX_ORDER)
READ (KLUN) LVFE_GW_HALF
!$acc update device (LVFE_GW_HALF)
READ (KLUN) LVFE_MAXIMAS
!$acc update device (LVFE_MAXIMAS)
READ (KLUN) LVFE_VERBOSE
!$acc update device (LVFE_VERBOSE)
READ (KLUN) RMINDETA
!$acc update device (RMINDETA)
END SUBROUTINE


END MODULE
