MODULE COPY_SL_STRUCT_MOD

USE EINT_MOD, ONLY : SL_STRUCT

INTERFACE COPY
MODULE PROCEDURE COPY_SL_STRUCT
END INTERFACE

CONTAINS

SUBROUTINE COPY_SL_STRUCT (YD)

IMPLICIT NONE
TYPE (SL_STRUCT), INTENT (IN) :: YD

!$acc update device (YD%CVER)

!$acc update device (YD%NSLGROUP)

!$acc enter data create (YD%NSLSTA)
!$acc update device (YD%NSLSTA)
!$acc enter data attach (YD%NSLSTA)

!$acc enter data create (YD%NSLONL)
!$acc update device (YD%NSLONL)
!$acc enter data attach (YD%NSLONL)

!$acc enter data create (YD%NSLOFF)
!$acc update device (YD%NSLOFF)
!$acc enter data attach (YD%NSLOFF)

!$acc enter data create (YD%NSLPTSWEST)
!$acc update device (YD%NSLPTSWEST)
!$acc enter data attach (YD%NSLPTSWEST)

!$acc enter data create (YD%NSLPTSEAST)
!$acc update device (YD%NSLPTSEAST)
!$acc enter data attach (YD%NSLPTSEAST)

!$acc enter data create (YD%NSLEXT)
!$acc update device (YD%NSLEXT)
!$acc enter data attach (YD%NSLEXT)

!$acc enter data create (YD%LCOMPLAT)
!$acc update device (YD%LCOMPLAT)
!$acc enter data attach (YD%LCOMPLAT)

!$acc enter data create (YD%NLATGLO)
!$acc update device (YD%NLATGLO)
!$acc enter data attach (YD%NLATGLO)

!$acc enter data create (YD%DIST1GP)
!$acc update device (YD%DIST1GP)
!$acc enter data attach (YD%DIST1GP)

!$acc enter data create (YD%NSLSENDPOS)
!$acc update device (YD%NSLSENDPOS)
!$acc enter data attach (YD%NSLSENDPOS)

!$acc enter data create (YD%NSLRECVPOS)
!$acc update device (YD%NSLRECVPOS)
!$acc enter data attach (YD%NSLRECVPOS)

!$acc enter data create (YD%NSLSENDPTR)
!$acc update device (YD%NSLSENDPTR)
!$acc enter data attach (YD%NSLSENDPTR)

!$acc enter data create (YD%NSLRECVPTR)
!$acc update device (YD%NSLRECVPTR)
!$acc enter data attach (YD%NSLRECVPTR)

!$acc enter data create (YD%NSLCOMM)
!$acc update device (YD%NSLCOMM)
!$acc enter data attach (YD%NSLCOMM)

!$acc enter data create (YD%LSLCOMM)
!$acc update device (YD%LSLCOMM)
!$acc enter data attach (YD%LSLCOMM)

!$acc update device (YD%NASLB1)

!$acc update device (YD%NASLB1_TRUE)

!$acc update device (YD%NSLPAD)

!$acc update device (YD%LSLT_ARRAYS_INIT)

!$acc update device (YD%LSLONDEM)

!$acc update device (YD%LSLONDEM_ACTIVE)

!$acc update device (YD%NUNUSEDHALO)

!$acc update device (YD%DISTUNUSEDHALO)

!$acc enter data create (YD%MASK_SL1)
!$acc update device (YD%MASK_SL1)
!$acc enter data attach (YD%MASK_SL1)

!$acc enter data create (YD%MASK_SL2)
!$acc update device (YD%MASK_SL2)
!$acc enter data attach (YD%MASK_SL2)

!$acc enter data create (YD%MASK_SL2T)
!$acc update device (YD%MASK_SL2T)
!$acc enter data attach (YD%MASK_SL2T)

!$acc enter data create (YD%MASK_SLD)
!$acc update device (YD%MASK_SLD)
!$acc enter data attach (YD%MASK_SLD)

!$acc update device (YD%NSLPROCS)

!$acc update device (YD%NSLRPT)

!$acc update device (YD%NSLSPT)

!$acc update device (YD%NSLWIDEN)

!$acc update device (YD%NSLWIDES)

!$acc update device (YD%NSLWIDEE)

!$acc update device (YD%NSLWIDEW)

!$acc update device (YD%NSLWIDE)

!$acc enter data create (YD%NSLMAP)
!$acc update device (YD%NSLMAP)
!$acc enter data attach (YD%NSLMAP)

!$acc enter data create (YD%NSLCORE)
!$acc update device (YD%NSLCORE)
!$acc enter data attach (YD%NSLCORE)

!$acc enter data create (YD%LSLCORE)
!$acc update device (YD%LSLCORE)
!$acc enter data attach (YD%LSLCORE)

!$acc enter data create (YD%MASK_SLTOT)
!$acc update device (YD%MASK_SLTOT)
!$acc enter data attach (YD%MASK_SLTOT)

!$acc update device (YD%NDGLG)

!$acc update device (YD%NDLON)

!$acc update device (YD%NDGSAG)

!$acc update device (YD%NDGENG)

!$acc update device (YD%NDGSAL)

!$acc update device (YD%NDGENL)

!$acc update device (YD%NDGSAH)

!$acc update device (YD%NDGENH)

!$acc update device (YD%NGPTOT)

!$acc update device (YD%NDGUXL)

!$acc update device (YD%NDLUNG)

!$acc update device (YD%NDLUXG)

!$acc update device (YD%NDGUNG)

!$acc update device (YD%NDGUXG)

!$acc update device (YD%NDSUR1)

!$acc update device (YD%NDLSUR)

!$acc update device (YD%NDGSUR)

!$acc update device (YD%NPTRFLOFF)

!$acc update device (YD%NFRSTLOFF)

!$acc update device (YD%MYFRSTACTLAT)

!$acc update device (YD%MYLSTACTLAT)

!$acc enter data create (YD%NLOENG)
!$acc update device (YD%NLOENG)
!$acc enter data attach (YD%NLOENG)

END SUBROUTINE

END MODULE
