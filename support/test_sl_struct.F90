PROGRAM MAIN

USE EINT_MOD
USE LOAD_SL_STRUCT_MOD
USE COPY_SL_STRUCT_MOD

IMPLICIT NONE

TYPE (SL_STRUCT) :: YDSL

#define _PP_SL_STRUCT_ PR_SL_STRUCT_CPU
#include "pp_sl_struct.intf.h"
#undef _PP_SL_STRUCT_

#define _PP_SL_STRUCT_ PR_SL_STRUCT_GPU
!$acc routine(PR_SL_STRUCT_GPU) seq
#include "pp_sl_struct.intf.h"
#undef _PP_SL_STRUCT_

OPEN (77, FILE="data.8/YDSL.IN.001", FORM='UNFORMATTED')
CALL LOAD (77, YDSL) 
CLOSE (77)


CALL PR_SL_STRUCT_CPU (YDSL)

PRINT *, '-----------------------------------------'

CALL FLUSH 

!$acc enter data create (YDSL)
CALL COPY (YDSL)

!$acc kernels present (YDSL)
CALL PR_SL_STRUCT_GPU (YDSL)
!$acc end kernels

END

#define _PP_SL_STRUCT_ PR_SL_STRUCT_CPU
#include "pp_sl_struct.body.h"
#undef _PP_SL_STRUCT_

#define _PP_SL_STRUCT_ PR_SL_STRUCT_GPU
!$acc routine(PR_SL_STRUCT_GPU) seq
#include "pp_sl_struct.body.h"
#undef _PP_SL_STRUCT_
