MODULE LOAD_YOMMP0_MOD

USE YOMMP0

CONTAINS

SUBROUTINE LOAD_YOMMP0 (KLUN)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
INTEGER :: IL1(1), IU1(1), IL2(2), IU2(2)
LOGICAL :: LNGPSET2PE, LNPRCIDS, LN_REGIONS
READ (KLUN) LNPRCIDS
IF (LNPRCIDS) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (NPRCIDS (IL1(1):IU1(1)))
  READ (KLUN) NPRCIDS
!$acc update device (NPRCIDS)
ENDIF
READ (KLUN) LNGPSET2PE
IF (LNGPSET2PE) THEN
  READ (KLUN) IL2
  READ (KLUN) IU2
  ALLOCATE (NGPSET2PE (IL2(1):IU2(1), IL2(2):IU2(2)))
  READ (KLUN) NGPSET2PE
!$acc update device (NGPSET2PE)
ENDIF
READ (KLUN) N_REGIONS_NS
!$acc update device (N_REGIONS_NS)
READ (KLUN) N_REGIONS_EW
!$acc update device (N_REGIONS_EW)
READ (KLUN) LN_REGIONS
IF (LN_REGIONS) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (N_REGIONS (IL1(1):IU1(1)))
  READ (KLUN) N_REGIONS
!$acc update device (N_REGIONS)
ENDIF
READ (KLUN) LSPLIT
!$acc update device (LSPLIT)
READ (KLUN) LEQ_REGIONS
!$acc update device (LEQ_REGIONS)
READ (KLUN) LSPLITOUT
!$acc update device (LSPLITOUT)
READ (KLUN) LSYNC_SLCOM
!$acc update device (LSYNC_SLCOM)
READ (KLUN) LSYNC_TRANS
!$acc update device (LSYNC_TRANS)
READ (KLUN) LSLDEBUG
!$acc update device (LSLDEBUG)
READ (KLUN) LSLONDEM
!$acc update device (LSLONDEM)
READ (KLUN) MP_TYPE
!$acc update device (MP_TYPE)
READ (KLUN) MBX_SIZE
!$acc update device (MBX_SIZE)
READ (KLUN) MYPROC
!$acc update device (MYPROC)
READ (KLUN) MYSETA
!$acc update device (MYSETA)
READ (KLUN) MYSETB
!$acc update device (MYSETB)
READ (KLUN) MYSETW
!$acc update device (MYSETW)
READ (KLUN) MYSETV
!$acc update device (MYSETV)
READ (KLUN) MYSETM
!$acc update device (MYSETM)
READ (KLUN) MYSETN
!$acc update device (MYSETN)
READ (KLUN) MY_REGION_NS
!$acc update device (MY_REGION_NS)
READ (KLUN) MY_REGION_EW
!$acc update device (MY_REGION_EW)
READ (KLUN) NFLDIN
!$acc update device (NFLDIN)
READ (KLUN) NSLPAD
!$acc update device (NSLPAD)
READ (KLUN) NOUTTYPE
!$acc update device (NOUTTYPE)
READ (KLUN) NWRTOUT
!$acc update device (NWRTOUT)
READ (KLUN) NDISTIO
!$acc update device (NDISTIO)
READ (KLUN) LUSEWRGRIDALL
!$acc update device (LUSEWRGRIDALL)
READ (KLUN) NCOMBFLEN
!$acc update device (NCOMBFLEN)
READ (KLUN) LMPOFF
!$acc update device (LMPOFF)
READ (KLUN) NSPECRESMIN
!$acc update device (NSPECRESMIN)
READ (KLUN) LOUTPUT
!$acc update device (LOUTPUT)
READ (KLUN) NOUTPUT
!$acc update device (NOUTPUT)
READ (KLUN) NPRINTLEV
!$acc update device (NPRINTLEV)
READ (KLUN) LMPDIAG
!$acc update device (LMPDIAG)
READ (KLUN) LOPT_SCALAR
!$acc update device (LOPT_SCALAR)
READ (KLUN) LOPT_RS6K
!$acc update device (LOPT_RS6K)
READ (KLUN) LSCMEC
!$acc update device (LSCMEC)
READ (KLUN) NTRANS_SYNC_LEVEL
!$acc update device (NTRANS_SYNC_LEVEL)
READ (KLUN) NSLCOMM_SYNC_LEVEL
!$acc update device (NSLCOMM_SYNC_LEVEL)
READ (KLUN) NPROC
!$acc update device (NPROC)
READ (KLUN) NPRGPNS
!$acc update device (NPRGPNS)
READ (KLUN) NPRGPEW
!$acc update device (NPRGPEW)
READ (KLUN) NPRTRNS
!$acc update device (NPRTRNS)
READ (KLUN) NPRTRN
!$acc update device (NPRTRN)
READ (KLUN) NPRTRW
!$acc update device (NPRTRW)
READ (KLUN) NPRTRV
!$acc update device (NPRTRV)
READ (KLUN) NSTRIN
!$acc update device (NSTRIN)
READ (KLUN) NSTROUT
!$acc update device (NSTROUT)
READ (KLUN) M_BARRINC_DIWRGRID
!$acc update device (M_BARRINC_DIWRGRID)
READ (KLUN) L_GATHERV_WRGP
!$acc update device (L_GATHERV_WRGP)
END SUBROUTINE


END MODULE
