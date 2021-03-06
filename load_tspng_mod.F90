MODULE LOAD_TSPNG_MOD

USE SPNG_MOD, ONLY : TSPNG

INTERFACE LOAD
MODULE PROCEDURE LOAD_TSPNG
END INTERFACE

CONTAINS

SUBROUTINE LOAD_TSPNG (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TSPNG), INTENT (OUT) :: YD
INTEGER :: IL2(2), IU2(2)
LOGICAL :: LRSPONGF
READ (KLUN) YD%LNSPONGE
READ (KLUN) LRSPONGF
IF (LRSPONGF) THEN
  READ (KLUN) IL2
  READ (KLUN) IU2
  ALLOCATE (YD%RSPONGF (IL2(1):IU2(1), IL2(2):IU2(2)))
  READ (KLUN) YD%RSPONGF
ENDIF
END SUBROUTINE


END MODULE
