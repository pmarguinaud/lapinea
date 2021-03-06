MODULE LOAD_TPTRSLB15_MOD

USE PTRSLB15, ONLY : TPTRSLB15

INTERFACE LOAD
MODULE PROCEDURE LOAD_TPTRSLB15
END INTERFACE

CONTAINS

SUBROUTINE LOAD_TPTRSLB15 (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TPTRSLB15), INTENT (OUT) :: YD
INTEGER :: IL1(1), IU1(1)
LOGICAL :: LRPARSL15
READ (KLUN) YD%NFLDSLB15
READ (KLUN) LRPARSL15
IF (LRPARSL15) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%RPARSL15 (IL1(1):IU1(1)))
  READ (KLUN) YD%RPARSL15
ENDIF
READ (KLUN) YD%MSLBUF15
READ (KLUN) YD%MSLB1UR05
READ (KLUN) YD%MSLB1VR05
READ (KLUN) YD%MSLB1WR05
READ (KLUN) YD%MSLB1UR95
READ (KLUN) YD%MSLB1VR95
READ (KLUN) YD%MSLB1U05
READ (KLUN) YD%MSLB1V05
READ (KLUN) YD%MSLB1T05
READ (KLUN) YD%MSLB1C05
READ (KLUN) YD%MSLB1SP05
READ (KLUN) YD%MSLB1U95
READ (KLUN) YD%MSLB1V95
READ (KLUN) YD%MSLB1T95
READ (KLUN) YD%MSLB1GFL95
READ (KLUN) YD%MSLB1C95
READ (KLUN) YD%MSLB1SP95
END SUBROUTINE


END MODULE
