MODULE LOAD_YOMCST_MOD

USE YOMCST

CONTAINS

SUBROUTINE LOAD_YOMCST (KLUN)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN

READ (KLUN) RPI
!$acc update device (RPI)
READ (KLUN) RCLUM
!$acc update device (RCLUM)
READ (KLUN) RHPLA
!$acc update device (RHPLA)
READ (KLUN) RKBOL
!$acc update device (RKBOL)
READ (KLUN) RNAVO
!$acc update device (RNAVO)
READ (KLUN) RDAY
!$acc update device (RDAY)
READ (KLUN) RDAYI
!$acc update device (RDAYI)
READ (KLUN) RHOUR
!$acc update device (RHOUR)
READ (KLUN) REA
!$acc update device (REA)
READ (KLUN) REPSM
!$acc update device (REPSM)
READ (KLUN) RSIYEA
!$acc update device (RSIYEA)
READ (KLUN) RSIDAY
!$acc update device (RSIDAY)
READ (KLUN) ROMEGA
!$acc update device (ROMEGA)
READ (KLUN) RA
!$acc update device (RA)
READ (KLUN) RG
!$acc update device (RG)
READ (KLUN) R1SA
!$acc update device (R1SA)
READ (KLUN) RSIGMA
!$acc update device (RSIGMA)
READ (KLUN) RI0
!$acc update device (RI0)
READ (KLUN) R
!$acc update device (R)
READ (KLUN) RMD
!$acc update device (RMD)
READ (KLUN) RMV
!$acc update device (RMV)
READ (KLUN) RMO3
!$acc update device (RMO3)
READ (KLUN) RD
!$acc update device (RD)
READ (KLUN) RV
!$acc update device (RV)
READ (KLUN) RCPD
!$acc update device (RCPD)
READ (KLUN) RCPV
!$acc update device (RCPV)
READ (KLUN) RCVD
!$acc update device (RCVD)
READ (KLUN) RCVV
!$acc update device (RCVV)
READ (KLUN) RKAPPA
!$acc update device (RKAPPA)
READ (KLUN) RETV
!$acc update device (RETV)
READ (KLUN) RMCO2
!$acc update device (RMCO2)
READ (KLUN) RMCH4
!$acc update device (RMCH4)
READ (KLUN) RMN2O
!$acc update device (RMN2O)
READ (KLUN) RMCO
!$acc update device (RMCO)
READ (KLUN) RMHCHO
!$acc update device (RMHCHO)
READ (KLUN) RMNO2
!$acc update device (RMNO2)
READ (KLUN) RMSO2
!$acc update device (RMSO2)
READ (KLUN) RMSO4
!$acc update device (RMSO4)
READ (KLUN) RCW
!$acc update device (RCW)
READ (KLUN) RCS
!$acc update device (RCS)
READ (KLUN) RATM
!$acc update device (RATM)
READ (KLUN) RTT
!$acc update device (RTT)
READ (KLUN) RLVTT
!$acc update device (RLVTT)
READ (KLUN) RLSTT
!$acc update device (RLSTT)
READ (KLUN) RLVZER
!$acc update device (RLVZER)
READ (KLUN) RLSZER
!$acc update device (RLSZER)
READ (KLUN) RLMLT
!$acc update device (RLMLT)
READ (KLUN) RDT
!$acc update device (RDT)
READ (KLUN) RESTT
!$acc update device (RESTT)
READ (KLUN) RGAMW
!$acc update device (RGAMW)
READ (KLUN) RBETW
!$acc update device (RBETW)
READ (KLUN) RALPW
!$acc update device (RALPW)
READ (KLUN) RGAMS
!$acc update device (RGAMS)
READ (KLUN) RBETS
!$acc update device (RBETS)
READ (KLUN) RALPS
!$acc update device (RALPS)
READ (KLUN) RALPD
!$acc update device (RALPD)
READ (KLUN) RBETD
!$acc update device (RBETD)
READ (KLUN) RGAMD
!$acc update device (RGAMD)
READ (KLUN) RSNAN
!$acc update device (RSNAN)
END SUBROUTINE


END MODULE
