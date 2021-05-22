SUBROUTINE _PP_GEOMETRY_(YDGEOMETRY)

USE TYPE_GEOMETRY

IMPLICIT NONE

TYPE(GEOMETRY)   ,INTENT(INOUT) :: YDGEOMETRY

INTEGER :: I, J

PRINT *, " YDGEOMETRY%YRGEM%NGPTOT "
PRINT *, YDGEOMETRY%YRGEM%NGPTOT

PRINT *, " YDGEOMETRY%YRGEM%NGPTOTL "
DO J = 1, SIZE (YDGEOMETRY%YRGEM%NGPTOTL, 2)
DO I = 1, SIZE (YDGEOMETRY%YRGEM%NGPTOTL, 1)
  PRINT *, YDGEOMETRY%YRGEM%NGPTOTL (I, J)
ENDDO
ENDDO

PRINT *, " YDGEOMETRY%YRVAB%VALH "
DO I = 1, SIZE (YDGEOMETRY%YRVAB%VALH)
  PRINT *, YDGEOMETRY%YRVAB%VALH (I)
ENDDO

PRINT *, " YDGEOMETRY%YRCSGEOM%RCOLON "
DO I = 1, SIZE (YDGEOMETRY%YRCSGEOM)
  PRINT *, I
  DO J = 1, SIZE (YDGEOMETRY%YRCSGEOM (I)%RCOLON)
    PRINT *, YDGEOMETRY%YRCSGEOM (I)%RCOLON (J)
  ENDDO
ENDDO

END
