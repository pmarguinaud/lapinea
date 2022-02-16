MODULE LOAD_YOMCT0_MOD

USE YOMCT0

CONTAINS

SUBROUTINE LOAD_YOMCT0 (KLUN)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN

READ (KLUN) NCONF
!$acc update device (NCONF)
READ (KLUN) NFPOS
!$acc update device (NFPOS)
READ (KLUN) LR3D
!$acc update device (LR3D)
READ (KLUN) LR2D
!$acc update device (LR2D)
READ (KLUN) LRSHW
!$acc update device (LRSHW)
READ (KLUN) LRVEQ
!$acc update device (LRVEQ)
READ (KLUN) NOPT_MEMORY
!$acc update device (NOPT_MEMORY)
READ (KLUN) LALLOPR
!$acc update device (LALLOPR)
READ (KLUN) LFDBOP
!$acc update device (LFDBOP)
READ (KLUN) LECFPOS
!$acc update device (LECFPOS)
READ (KLUN) LGRBOP
!$acc update device (LGRBOP)
READ (KLUN) LFBDAP
!$acc update device (LFBDAP)
READ (KLUN) LARPEGEF
!$acc update device (LARPEGEF)
READ (KLUN) LARPEGEF_TRAJHR
!$acc update device (LARPEGEF_TRAJHR)
READ (KLUN) LARPEGEF_TRAJBG
!$acc update device (LARPEGEF_TRAJBG)
READ (KLUN) LARPEGEF_RDGP_INIT
!$acc update device (LARPEGEF_RDGP_INIT)
READ (KLUN) LARPEGEF_RDGP_TRAJHR
!$acc update device (LARPEGEF_RDGP_TRAJHR)
READ (KLUN) LARPEGEF_RDGP_TRAJBG
!$acc update device (LARPEGEF_RDGP_TRAJBG)
READ (KLUN) CNDISPP
!$acc update device (CNDISPP)
READ (KLUN) CFPNCF
!$acc update device (CFPNCF)
READ (KLUN) CFDIRLST
!$acc update device (CFDIRLST)
READ (KLUN) CNPPATH
!$acc update device (CNPPATH)
READ (KLUN) CSCRIPT_PPSERVER
!$acc update device (CSCRIPT_PPSERVER)
READ (KLUN) CSCRIPT_LAMRTC
!$acc update device (CSCRIPT_LAMRTC)
READ (KLUN) LNF
!$acc update device (LNF)
READ (KLUN) LSMSSIG
!$acc update device (LSMSSIG)
READ (KLUN) CEVENT
!$acc update device (CEVENT)
READ (KLUN) CMETER
!$acc update device (CMETER)
READ (KLUN) LOPDIS
!$acc update device (LOPDIS)
READ (KLUN) NCYCLE
!$acc update device (NCYCLE)
READ (KLUN) CNMEXP
!$acc update device (CNMEXP)
READ (KLUN) CFCLASS
!$acc update device (CFCLASS)
READ (KLUN) CTYPE
!$acc update device (CTYPE)
READ (KLUN) LBACKG
!$acc update device (LBACKG)
READ (KLUN) LSPBSBAL
!$acc update device (LSPBSBAL)
READ (KLUN) LMINIM
!$acc update device (LMINIM)
READ (KLUN) LIOLEVG
!$acc update device (LIOLEVG)
READ (KLUN) NUNDEFLD
!$acc update device (NUNDEFLD)
READ (KLUN) LREFOUT
!$acc update device (LREFOUT)
READ (KLUN) LREFGEN
!$acc update device (LREFGEN)
READ (KLUN) LSLAG
!$acc update device (LSLAG)
READ (KLUN) LTWOTL
!$acc update device (LTWOTL)
READ (KLUN) LREGETA
!$acc update device (LREGETA)
READ (KLUN) LVFE_REGETA
!$acc update device (LVFE_REGETA)
READ (KLUN) NQUAD
!$acc update device (NQUAD)
READ (KLUN) LNHEE
!$acc update device (LNHEE)
READ (KLUN) LNHQE
!$acc update device (LNHQE)
READ (KLUN) LNHDYN
!$acc update device (LNHDYN)
READ (KLUN) N2DINI
!$acc update device (N2DINI)
READ (KLUN) N3DINI
!$acc update device (N3DINI)
READ (KLUN) LSPRT
!$acc update device (LSPRT)
READ (KLUN) NSPPR
!$acc update device (NSPPR)
READ (KLUN) NFRPOS
!$acc update device (NFRPOS)
READ (KLUN) NFRCORM
!$acc update device (NFRCORM)
READ (KLUN) NFRCO
!$acc update device (NFRCO)
READ (KLUN) NFRHIS
!$acc update device (NFRHIS)
READ (KLUN) NFRSFXHIS
!$acc update device (NFRSFXHIS)
READ (KLUN) NFRMASSCON
!$acc update device (NFRMASSCON)
READ (KLUN) NFRGDI
!$acc update device (NFRGDI)
READ (KLUN) NFRSDI
!$acc update device (NFRSDI)
READ (KLUN) NFRDHFG
!$acc update device (NFRDHFG)
READ (KLUN) NFRDHFZ
!$acc update device (NFRDHFZ)
READ (KLUN) NFRDHFD
!$acc update device (NFRDHFD)
READ (KLUN) NFRDHP
!$acc update device (NFRDHP)
READ (KLUN) N6BINS
!$acc update device (N6BINS)
READ (KLUN) NPOSTS
!$acc update device (NPOSTS)
READ (KLUN) NPOSTSMIN
!$acc update device (NPOSTSMIN)
READ (KLUN) NHISTS
!$acc update device (NHISTS)
READ (KLUN) NHISTSMIN
!$acc update device (NHISTSMIN)
READ (KLUN) NSFXHISTS
!$acc update device (NSFXHISTS)
READ (KLUN) NSFXHISTSMIN
!$acc update device (NSFXHISTSMIN)
READ (KLUN) NMASSCONS
!$acc update device (NMASSCONS)
READ (KLUN) NGDITS
!$acc update device (NGDITS)
READ (KLUN) NSDITS
!$acc update device (NSDITS)
READ (KLUN) NDHFGTS
!$acc update device (NDHFGTS)
READ (KLUN) NDHFZTS
!$acc update device (NDHFZTS)
READ (KLUN) NDHFDTS
!$acc update device (NDHFDTS)
READ (KLUN) NDHPTS
!$acc update device (NDHPTS)
READ (KLUN) L4DVAR
!$acc update device (L4DVAR)
READ (KLUN) LCANARI
!$acc update device (LCANARI)
READ (KLUN) NTASKS_CANARI
!$acc update device (NTASKS_CANARI)
READ (KLUN) LGUESS
!$acc update device (LGUESS)
READ (KLUN) LOBS
!$acc update device (LOBS)
READ (KLUN) LSIMOB
!$acc update device (LSIMOB)
READ (KLUN) LOBSC1
!$acc update device (LOBSC1)
READ (KLUN) LSCREEN
!$acc update device (LSCREEN)
READ (KLUN) LSCREEN_OPENMP
!$acc update device (LSCREEN_OPENMP)
READ (KLUN) L_SPLIT_SCREEN
!$acc update device (L_SPLIT_SCREEN)
READ (KLUN) L_SCREEN_CALL
!$acc update device (L_SCREEN_CALL)
READ (KLUN) LMONITORING
!$acc update device (LMONITORING)
READ (KLUN) LOBSREF
!$acc update device (LOBSREF)
READ (KLUN) LIFSMIN
!$acc update device (LIFSMIN)
READ (KLUN) LIFSTRAJ
!$acc update device (LIFSTRAJ)
READ (KLUN) NCNTVAR
!$acc update device (NCNTVAR)
READ (KLUN) LOLDPP
!$acc update device (LOLDPP)
READ (KLUN) NSTEPINI
!$acc update device (NSTEPINI)
READ (KLUN) NINTERPTRAJ
!$acc update device (NINTERPTRAJ)
READ (KLUN) NINTERPINCR
!$acc update device (NINTERPINCR)
READ (KLUN) LINFLAT
!$acc update device (LINFLAT)
READ (KLUN) LINFLP9
!$acc update device (LINFLP9)
READ (KLUN) LINFLF1
!$acc update device (LINFLF1)
READ (KLUN) L_OOPS
!$acc update device (L_OOPS)
READ (KLUN) LECMWF
!$acc update device (LECMWF)
READ (KLUN) LELAM
!$acc update device (LELAM)
READ (KLUN) LRPLANE
!$acc update device (LRPLANE)
READ (KLUN) LAROME
!$acc update device (LAROME)
READ (KLUN) LCALLSFX
!$acc update device (LCALLSFX)
READ (KLUN) LSFXLSM
!$acc update device (LSFXLSM)
READ (KLUN) LSFCFLX
!$acc update device (LSFCFLX)
READ (KLUN) REXTSHF
!$acc update device (REXTSHF)
READ (KLUN) REXTLHF
!$acc update device (REXTLHF)
READ (KLUN) LROUGH
!$acc update device (LROUGH)
READ (KLUN) REXTZ0M
!$acc update device (REXTZ0M)
READ (KLUN) REXTZ0H
!$acc update device (REXTZ0H)
READ (KLUN) LCOUPLO4
!$acc update device (LCOUPLO4)
READ (KLUN) LCOUPLO4_ENV
!$acc update device (LCOUPLO4_ENV)
READ (KLUN) LSFORC
!$acc update device (LSFORC)
READ (KLUN) LSFORCS
!$acc update device (LSFORCS)
READ (KLUN) LWRSPECA_GP
!$acc update device (LWRSPECA_GP)
READ (KLUN) LSUSPECA_GP
!$acc update device (LSUSPECA_GP)
READ (KLUN) LWRSPECA_GP_UV
!$acc update device (LWRSPECA_GP_UV)
READ (KLUN) LSUSPECA_GP_UV
!$acc update device (LSUSPECA_GP_UV)
READ (KLUN) LGRIB_API
!$acc update device (LGRIB_API)
END SUBROUTINE


END MODULE
