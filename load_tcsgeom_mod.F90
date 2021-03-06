MODULE LOAD_TCSGEOM_MOD

USE YOMCSGEOM, ONLY : TCSGEOM

INTERFACE LOAD
MODULE PROCEDURE LOAD_TCSGEOM
END INTERFACE

CONTAINS

SUBROUTINE LOAD_TCSGEOM (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TCSGEOM), INTENT (OUT) :: YD
INTEGER :: IL1(1), IU1(1)
LOGICAL :: LRATATH, LRATATX, LRCOLON, LRINDX, LRINDY, LRSILON
READ (KLUN) LRCOLON
IF (LRCOLON) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%RCOLON (IL1(1):IU1(1)))
  READ (KLUN) YD%RCOLON
ELSE
  NULLIFY (YD%RCOLON)
ENDIF
READ (KLUN) LRSILON
IF (LRSILON) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%RSILON (IL1(1):IU1(1)))
  READ (KLUN) YD%RSILON
ELSE
  NULLIFY (YD%RSILON)
ENDIF
READ (KLUN) LRINDX
IF (LRINDX) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%RINDX (IL1(1):IU1(1)))
  READ (KLUN) YD%RINDX
ELSE
  NULLIFY (YD%RINDX)
ENDIF
READ (KLUN) LRINDY
IF (LRINDY) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%RINDY (IL1(1):IU1(1)))
  READ (KLUN) YD%RINDY
ELSE
  NULLIFY (YD%RINDY)
ENDIF
READ (KLUN) LRATATH
IF (LRATATH) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%RATATH (IL1(1):IU1(1)))
  READ (KLUN) YD%RATATH
ELSE
  NULLIFY (YD%RATATH)
ENDIF
READ (KLUN) LRATATX
IF (LRATATX) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%RATATX (IL1(1):IU1(1)))
  READ (KLUN) YD%RATATX
ELSE
  NULLIFY (YD%RATATX)
ENDIF
END SUBROUTINE


END MODULE
