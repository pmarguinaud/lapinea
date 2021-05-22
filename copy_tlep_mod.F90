MODULE COPY_TLEP_MOD

USE YEMLAP, ONLY : TLEP

INTERFACE COPY
MODULE PROCEDURE COPY_TLEP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TLEP (YD)

IMPLICIT NONE
TYPE (TLEP), INTENT (IN) :: YD

!$acc enter data create (YD%NCPL2M)
!$acc update device (YD%NCPL2M)
!$acc enter data attach (YD%NCPL2M)

!$acc enter data create (YD%NCPL4M)
!$acc update device (YD%NCPL4M)
!$acc enter data attach (YD%NCPL4M)

!$acc enter data create (YD%NCPLM)
!$acc update device (YD%NCPLM)
!$acc enter data attach (YD%NCPLM)

!$acc enter data create (YD%NCPL2N)
!$acc update device (YD%NCPL2N)
!$acc enter data attach (YD%NCPL2N)

!$acc enter data create (YD%NCPL4N)
!$acc update device (YD%NCPL4N)
!$acc enter data attach (YD%NCPL4N)

!$acc enter data create (YD%NCPLN)
!$acc update device (YD%NCPLN)
!$acc enter data attach (YD%NCPLN)

!$acc enter data create (YD%RLEPDIN)
!$acc update device (YD%RLEPDIN)
!$acc enter data attach (YD%RLEPDIN)

!$acc enter data create (YD%RLEPINN)
!$acc update device (YD%RLEPINN)
!$acc enter data attach (YD%RLEPINN)

!$acc enter data create (YD%RLEPDIM)
!$acc update device (YD%RLEPDIM)
!$acc enter data attach (YD%RLEPDIM)

!$acc enter data create (YD%RLEPINM)
!$acc update device (YD%RLEPINM)
!$acc enter data attach (YD%RLEPINM)

!$acc enter data create (YD%NESM0)
!$acc update device (YD%NESM0)
!$acc enter data attach (YD%NESM0)

!$acc enter data create (YD%NESPZERO)
!$acc update device (YD%NESPZERO)
!$acc enter data attach (YD%NESPZERO)

!$acc enter data create (YD%NESM0G)
!$acc update device (YD%NESM0G)
!$acc enter data attach (YD%NESM0G)

!$acc enter data create (YD%NPME)
!$acc update device (YD%NPME)
!$acc enter data attach (YD%NPME)

!$acc enter data create (YD%NPNE)
!$acc update device (YD%NPNE)
!$acc enter data attach (YD%NPNE)

!$acc enter data create (YD%MVALUE)
!$acc update device (YD%MVALUE)
!$acc enter data attach (YD%MVALUE)

END SUBROUTINE

END MODULE
