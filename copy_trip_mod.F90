MODULE COPY_TRIP_MOD

USE YOMRIP, ONLY : TRIP

INTERFACE COPY
MODULE PROCEDURE COPY_TRIP
END INTERFACE

CONTAINS

SUBROUTINE COPY_TRIP (YD)

IMPLICIT NONE
TYPE (TRIP), INTENT (IN) :: YD

!$acc update device (YD%NSTART)

!$acc update device (YD%NSTOP)

!$acc update device (YD%CSTOP)

PRINT *, " CPU : CSTOP = ", YD%CSTOP, YD%CSTOP == 'h1', YD%CSTOP == 'h2'

!$acc serial present (YD)
PRINT *, " GPU : CSTOP = ", YD%CSTOP, YD%CSTOP == 'h1', YD%CSTOP == 'h2'
!$acc end serial

!$acc update device (YD%NSTADD)

!$acc update device (YD%NSTASS)

!$acc update device (YD%NFOST)

!$acc update device (YD%RSTATI)

!$acc update device (YD%RTIMTR)

!$acc update device (YD%RHGMT)

!$acc update device (YD%REQTIM)

!$acc update device (YD%RSOVR)

!$acc update device (YD%RDEASO)

!$acc update device (YD%RDECLI)

!$acc update device (YD%RWSOVR)

!$acc update device (YD%RIP0)

!$acc update device (YD%RCODEC)

!$acc update device (YD%RSIDEC)

!$acc update device (YD%RCOVSR)

!$acc update device (YD%RSIVSR)

!$acc update device (YD%RCODECN)

!$acc update device (YD%RSIDECN)

!$acc update device (YD%RCOVSRN)

!$acc update device (YD%RSIVSRN)

!$acc update device (YD%RCODECF)

!$acc update device (YD%RSIDECF)

!$acc update device (YD%RCOVSRF)

!$acc update device (YD%RSIVSRF)

!$acc update device (YD%TSTEP)

!$acc update device (YD%TDT)

!$acc update device (YD%RDTSA)

!$acc update device (YD%RDTSA2)

!$acc update device (YD%RDTS62)

!$acc update device (YD%RDTS22)

!$acc update device (YD%RTDT)

!$acc update device (YD%RDECLU)

!$acc update device (YD%RTMOLT)

!$acc update device (YD%RIP0LU)

!$acc update device (YD%RCODECLU)

!$acc update device (YD%RSIDECLU)

!$acc update device (YD%RCOVSRLU)

!$acc update device (YD%RSIVSRLU)

!$acc serial present (YD)
PRINT *, " GPU : CSTOP = ", YD%CSTOP, YD%CSTOP == 'h1', YD%CSTOP == 'h2'
!$acc end serial

END SUBROUTINE

END MODULE
