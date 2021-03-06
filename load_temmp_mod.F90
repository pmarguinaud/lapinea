MODULE LOAD_TEMMP_MOD

USE YEMMP, ONLY : TEMMP

INTERFACE LOAD
MODULE PROCEDURE LOAD_TEMMP
END INTERFACE

CONTAINS

SUBROUTINE LOAD_TEMMP (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TEMMP), INTENT (OUT) :: YD
INTEGER :: IL1(1), IU1(1)
LOGICAL :: LMYENS, LNEALLNS, LNEPROCN, LNEPTRNS, LNUEMPP
READ (KLUN) LNEPROCN
IF (LNEPROCN) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%NEPROCN (IL1(1):IU1(1)))
  READ (KLUN) YD%NEPROCN
ELSE
  NULLIFY (YD%NEPROCN)
ENDIF
READ (KLUN) YD%NUEMP
READ (KLUN) LMYENS
IF (LMYENS) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%MYENS (IL1(1):IU1(1)))
  READ (KLUN) YD%MYENS
ELSE
  NULLIFY (YD%MYENS)
ENDIF
READ (KLUN) LNUEMPP
IF (LNUEMPP) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%NUEMPP (IL1(1):IU1(1)))
  READ (KLUN) YD%NUEMPP
ELSE
  NULLIFY (YD%NUEMPP)
ENDIF
READ (KLUN) LNEALLNS
IF (LNEALLNS) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%NEALLNS (IL1(1):IU1(1)))
  READ (KLUN) YD%NEALLNS
ELSE
  NULLIFY (YD%NEALLNS)
ENDIF
READ (KLUN) LNEPTRNS
IF (LNEPTRNS) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%NEPTRNS (IL1(1):IU1(1)))
  READ (KLUN) YD%NEPTRNS
ELSE
  NULLIFY (YD%NEPTRNS)
ENDIF
END SUBROUTINE


END MODULE
