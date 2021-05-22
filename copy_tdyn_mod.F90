MODULE COPY_TDYN_MOD

USE YOMDYN, ONLY : TDYN

INTERFACE COPY
MODULE PROCEDURE COPY_TDYN
END INTERFACE

CONTAINS

SUBROUTINE COPY_TDYN (YD)

IMPLICIT NONE
TYPE (TDYN), INTENT (IN) :: YD

!$acc update device (YD%REPS1)

!$acc update device (YD%REPS2)

!$acc update device (YD%REPSM1)

!$acc update device (YD%REPSM2)

!$acc update device (YD%REPSP1)

!$acc update device (YD%HDIRVOR)

!$acc update device (YD%HDIRDIV)

!$acc update device (YD%HDIRT)

!$acc update device (YD%HDIRQ)

!$acc update device (YD%HDIRO3)

!$acc update device (YD%HDIRPD)

!$acc update device (YD%HDIRVD)

!$acc update device (YD%HDIRSP)

!$acc update device (YD%HRDIRVOR)

!$acc update device (YD%HRDIRDIV)

!$acc update device (YD%HRDIRT)

!$acc update device (YD%HRDIRQ)

!$acc update device (YD%HRDIRO3)

!$acc update device (YD%HRDIRPD)

!$acc update device (YD%HRDIRVD)

!$acc update device (YD%HRDIRSP)

!$acc update device (YD%RRDXTAU)

!$acc update device (YD%RDAMPVOR)

!$acc update device (YD%RDAMPDIV)

!$acc update device (YD%RDAMPT)

!$acc update device (YD%RDAMPQ)

!$acc update device (YD%RDAMPO3)

!$acc update device (YD%RDAMPPD)

!$acc update device (YD%RDAMPVD)

!$acc update device (YD%RDAMPSP)

!$acc update device (YD%LNEWHD)

!$acc update device (YD%REXPDH)

!$acc update device (YD%FRANDH)

!$acc update device (YD%SLEVDH)

!$acc update device (YD%SLEVDH1)

!$acc update device (YD%SLEVDH2)

!$acc update device (YD%SLEVDH3)

!$acc update device (YD%NSREFDH)

!$acc update device (YD%RATIO_HDI_TOP)

!$acc update device (YD%NPROFILEHD)

!$acc update device (YD%RPROFHDBT)

!$acc update device (YD%RPROFHDTP)

!$acc update device (YD%RPROFHDMX)

!$acc update device (YD%RPROFHDEX)

!$acc update device (YD%LRDISPE_EC)

!$acc enter data create (YD%RDIVOR)
!$acc update device (YD%RDIVOR)
!$acc enter data attach (YD%RDIVOR)

!$acc enter data create (YD%RDIDIV)
!$acc update device (YD%RDIDIV)
!$acc enter data attach (YD%RDIDIV)

!$acc enter data create (YD%RDITG)
!$acc update device (YD%RDITG)
!$acc enter data attach (YD%RDITG)

!$acc enter data create (YD%RDIGFL)
!$acc update device (YD%RDIGFL)
!$acc enter data attach (YD%RDIGFL)

!$acc enter data create (YD%RDIPD)
!$acc update device (YD%RDIPD)
!$acc enter data attach (YD%RDIPD)

!$acc enter data create (YD%RDIVD)
!$acc update device (YD%RDIVD)
!$acc enter data attach (YD%RDIVD)

!$acc enter data create (YD%RDISP)
!$acc update device (YD%RDISP)
!$acc enter data attach (YD%RDISP)

!$acc enter data create (YD%RDHI)
!$acc update device (YD%RDHI)
!$acc enter data attach (YD%RDHI)

!$acc update device (YD%LSTRHD)

!$acc update device (YD%HDTIME_STRHD)

!$acc update device (YD%LTOP_VOR)

!$acc update device (YD%NTOP_VOR_TRUNC)

!$acc update device (YD%NTOP_VOR_BOT)

!$acc enter data create (YD%SLHDA)
!$acc update device (YD%SLHDA)
!$acc enter data attach (YD%SLHDA)

!$acc update device (YD%SLHDA0)

!$acc update device (YD%SLHDA0T)

!$acc update device (YD%SLHDB)

!$acc update device (YD%SLHDBT)

!$acc enter data create (YD%SLHDD0)
!$acc update device (YD%SLHDD0)
!$acc enter data attach (YD%SLHDD0)

!$acc update device (YD%SLHDD00)

!$acc update device (YD%SLHDD00T)

!$acc update device (YD%SLHDDIV)

!$acc update device (YD%SLHDRATDDIV)

!$acc update device (YD%SLHDHOR)

!$acc update device (YD%LSLHDHEAT)

!$acc update device (YD%HDSRVOR)

!$acc update device (YD%HDSRDIV)

!$acc update device (YD%HDSRVD)

!$acc update device (YD%HRDSRVOR)

!$acc update device (YD%HRDSRDIV)

!$acc update device (YD%HRDSRVD)

!$acc update device (YD%RDAMPVORS)

!$acc update device (YD%RDAMPDIVS)

!$acc update device (YD%RDAMPVDS)

!$acc update device (YD%RDAMPHDS)

!$acc update device (YD%REXPDHS)

!$acc update device (YD%SLEVDHS)

!$acc update device (YD%SLEVDHS1)

!$acc update device (YD%SLEVDHS2)

!$acc update device (YD%SDRED)

!$acc enter data create (YD%RDSVOR)
!$acc update device (YD%RDSVOR)
!$acc enter data attach (YD%RDSVOR)

!$acc enter data create (YD%RDSDIV)
!$acc update device (YD%RDSDIV)
!$acc enter data attach (YD%RDSDIV)

!$acc enter data create (YD%RDSVD)
!$acc update device (YD%RDSVD)
!$acc enter data attach (YD%RDSVD)

!$acc enter data create (YD%RDHS)
!$acc update device (YD%RDHS)
!$acc enter data attach (YD%RDHS)

!$acc update device (YD%LRFRIC)

!$acc update device (YD%LRFRICISOTR)

!$acc enter data create (YD%RCORDIT)
!$acc update device (YD%RCORDIT)
!$acc enter data attach (YD%RCORDIT)

!$acc enter data create (YD%RCORDIH)
!$acc update device (YD%RCORDIH)
!$acc enter data attach (YD%RCORDIH)

!$acc enter data create (YD%RCORDIF)
!$acc update device (YD%RCORDIF)
!$acc enter data attach (YD%RCORDIF)

!$acc update device (YD%VMAX1)

!$acc update device (YD%VMAX2)

!$acc update device (YD%RMAX_D3)

!$acc update device (YD%LBOUND_D3)

!$acc enter data create (YD%RKRF)
!$acc update device (YD%RKRF)
!$acc enter data attach (YD%RKRF)

!$acc update device (YD%NMAXLEVRF)

!$acc update device (YD%RRFZ1)

!$acc update device (YD%RRFPLM)

!$acc update device (YD%RRFTAU)

!$acc update device (YD%RTEMRB)

!$acc update device (YD%NRUBC)

!$acc update device (YD%LSIDG)

!$acc update device (YD%BETADT)

!$acc update device (YD%RBT)

!$acc update device (YD%RBTS2)

!$acc update device (YD%REFGEO)

!$acc update device (YD%SIPR)

!$acc update device (YD%SITR)

!$acc update device (YD%SITRA)

!$acc update device (YD%SITRUB)

!$acc update device (YD%SIPRUB)

!$acc update device (YD%SITIME)

!$acc update device (YD%SIRPRG)

!$acc update device (YD%SIRPRN)

!$acc update device (YD%NSITER)

!$acc update device (YD%NCURRENT_ITER)

!$acc update device (YD%LRHDI_LASTITERPC)

!$acc update device (YD%NITERHELM)

!$acc update device (YD%NOPT_SITRA)

!$acc enter data create (YD%SIALPH)
!$acc update device (YD%SIALPH)
!$acc enter data attach (YD%SIALPH)

!$acc enter data create (YD%SILNPR)
!$acc update device (YD%SILNPR)
!$acc enter data attach (YD%SILNPR)

!$acc enter data create (YD%SIDELP)
!$acc update device (YD%SIDELP)
!$acc enter data attach (YD%SIDELP)

!$acc enter data create (YD%SIRDEL)
!$acc update device (YD%SIRDEL)
!$acc enter data attach (YD%SIRDEL)

!$acc enter data create (YD%SITLAH)
!$acc update device (YD%SITLAH)
!$acc enter data attach (YD%SITLAH)

!$acc enter data create (YD%SITLAF)
!$acc update device (YD%SITLAF)
!$acc enter data attach (YD%SITLAF)

!$acc enter data create (YD%SIDPHI)
!$acc update device (YD%SIDPHI)
!$acc enter data attach (YD%SIDPHI)

!$acc enter data create (YD%SIB)
!$acc update device (YD%SIB)
!$acc enter data attach (YD%SIB)

!$acc enter data create (YD%SIMO)
!$acc update device (YD%SIMO)
!$acc enter data attach (YD%SIMO)

!$acc enter data create (YD%SIMI)
!$acc update device (YD%SIMI)
!$acc enter data attach (YD%SIMI)

!$acc enter data create (YD%SIVP)
!$acc update device (YD%SIVP)
!$acc enter data attach (YD%SIVP)

!$acc enter data create (YD%SIHEG)
!$acc update device (YD%SIHEG)
!$acc enter data attach (YD%SIHEG)

!$acc enter data create (YD%SIHEG2)
!$acc update device (YD%SIHEG2)
!$acc enter data attach (YD%SIHEG2)

!$acc enter data create (YD%SIHEGB)
!$acc update device (YD%SIHEGB)
!$acc enter data attach (YD%SIHEGB)

!$acc enter data create (YD%SIHEGB2)
!$acc update device (YD%SIHEGB2)
!$acc enter data attach (YD%SIHEGB2)

!$acc enter data create (YD%SIFAC)
!$acc update device (YD%SIFAC)
!$acc enter data attach (YD%SIFAC)

!$acc enter data create (YD%SIFACI)
!$acc update device (YD%SIFACI)
!$acc enter data attach (YD%SIFACI)

!$acc enter data create (YD%SI_ILAPKSSI)
!$acc update device (YD%SI_ILAPKSSI)
!$acc enter data attach (YD%SI_ILAPKSSI)

!$acc enter data create (YD%SITRAM)
!$acc update device (YD%SITRAM)
!$acc enter data attach (YD%SITRAM)

!$acc update device (YD%VNORM)

!$acc update device (YD%NVLAG)

!$acc update device (YD%NWLAG)

!$acc update device (YD%NTLAG)

!$acc update device (YD%NSPDLAG)

!$acc update device (YD%NSVDLAG)

!$acc update device (YD%NSPLTHOI)

!$acc update device (YD%LSPLTHOIGFL)

!$acc update device (YD%NSLDIMK)

!$acc update device (YD%NITMP)

!$acc update device (YD%VETAON)

!$acc update device (YD%VETAOX)

!$acc update device (YD%LSETTLSVF)

!$acc update device (YD%LSETFSTAT)

!$acc update device (YD%RW2TLFF)

!$acc update device (YD%VESL)

!$acc update device (YD%XIDT)

!$acc update device (YD%LQMW)

!$acc update device (YD%LQMHW)

!$acc update device (YD%LQMT)

!$acc update device (YD%LQMHT)

!$acc update device (YD%LQMP)

!$acc update device (YD%LQMHP)

!$acc update device (YD%LQMPD)

!$acc update device (YD%LQMHPD)

!$acc update device (YD%LQMVD)

!$acc update device (YD%LQMHVD)

!$acc update device (YD%LADVF)

!$acc update device (YD%LIMPF)

!$acc update device (YD%L2TLFF)

!$acc update device (YD%RCMSLP0)

!$acc update device (YD%NCOMP_CVGQ)

!$acc update device (YD%LSVTSM)

!$acc update device (YD%RPRES_SVTSM)

!$acc update device (YD%RPRES_SETTLSVF)

!$acc update device (YD%NFLEVSF)

!$acc update device (YD%RSCALE)

!$acc update device (YD%RSCALEOFF)

!$acc update device (YD%LHDIFFM)

!$acc update device (YD%LSPECVIS)

!$acc update device (YD%NDIFFACT)

!$acc update device (YD%LGPSTRESS)

!$acc update device (YD%RCLSTRESS)

!$acc update device (YD%RCLPOLE)

!$acc update device (YD%NVSEPC)

!$acc update device (YD%NVSEPL)

!$acc update device (YD%LFINDVSEP)

!$acc update device (YD%LMASCOR)

!$acc update device (YD%LMASDRY)

!$acc update device (YD%LGPMASCOR)

!$acc update device (YD%NGPMASCOR)

!$acc update device (YD%GPMASSI)

!$acc update device (YD%GMASSI)

!$acc update device (YD%GMASS0)

!$acc update device (YD%GMASSINC)

!$acc enter data create (YD%SIBI)
!$acc update device (YD%SIBI)
!$acc enter data attach (YD%SIBI)

END SUBROUTINE

END MODULE
