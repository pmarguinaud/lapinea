PROGRAM MAIN

USE LOAD_GEOMETRY_MOD
USE COPY_GEOMETRY_MOD

IMPLICIT NONE

TYPE (GEOMETRY) :: YDGEOMETRY

#define _PP_GEOMETRY_ PR_GEOMETRY_CPU
#include "pp_geometry.intf.h"
#undef _PP_GEOMETRY_

#define _PP_GEOMETRY_ PR_GEOMETRY_GPU
!$acc routine(PR_GEOMETRY_GPU) seq
#include "pp_geometry.intf.h"
#undef _PP_GEOMETRY_

OPEN (77, FILE="data.8/YDGEOMETRY.IN.001", FORM='UNFORMATTED')
CALL LOAD (77, YDGEOMETRY) 
CLOSE (77)


CALL PR_GEOMETRY_CPU (YDGEOMETRY)

PRINT *, '-----------------------------------------'

CALL FLUSH 

!$acc enter data create (YDGEOMETRY)
CALL COPY (YDGEOMETRY)

!$acc kernels present (YDGEOMETRY)
CALL PR_GEOMETRY_GPU (YDGEOMETRY)
!$acc end kernels

END

#define _PP_GEOMETRY_ PR_GEOMETRY_CPU
#include "pp_geometry.body.h"
#undef _PP_GEOMETRY_

#define _PP_GEOMETRY_ PR_GEOMETRY_GPU
!$acc routine(PR_GEOMETRY_GPU) seq
#include "pp_geometry.body.h"
#undef _PP_GEOMETRY_
