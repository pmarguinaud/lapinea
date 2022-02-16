PROGRAM MAIN

USE LOAD_MODEL_DYNAMICS_TYPE_MOD
USE COPY_MODEL_DYNAMICS_TYPE_MOD

IMPLICIT NONE

TYPE (MODEL_DYNAMICS_TYPE) :: YDML_DYN

#define _PP_MODEL_DYNAMICS_TYPE_ PR_MODEL_DYNAMICS_TYPE_CPU
#include "pp_model_dynamics_type.intf.h"
#undef _PP_MODEL_DYNAMICS_TYPE_

#define _PP_MODEL_DYNAMICS_TYPE_ PR_MODEL_DYNAMICS_TYPE_GPU
!$acc routine(PR_MODEL_DYNAMICS_TYPE_GPU) seq
#include "pp_model_dynamics_type.intf.h"
#undef _PP_MODEL_DYNAMICS_TYPE_

OPEN (77, FILE="data.8/YDML_DYN.IN.001", FORM='UNFORMATTED')
CALL LOAD (77, YDML_DYN) 
CLOSE (77)


CALL PR_MODEL_DYNAMICS_TYPE_CPU (YDML_DYN)

PRINT *, '-----------------------------------------'

CALL FLUSH 

!$acc enter data create (YDML_DYN)
CALL COPY (YDML_DYN)

!$acc kernels present (YDML_DYN)
CALL PR_MODEL_DYNAMICS_TYPE_GPU (YDML_DYN)
!$acc end kernels

END

#define _PP_MODEL_DYNAMICS_TYPE_ PR_MODEL_DYNAMICS_TYPE_CPU
#include "pp_model_dynamics_type.body.h"
#undef _PP_MODEL_DYNAMICS_TYPE_

#define _PP_MODEL_DYNAMICS_TYPE_ PR_MODEL_DYNAMICS_TYPE_GPU
!$acc routine(PR_MODEL_DYNAMICS_TYPE_GPU) seq
#include "pp_model_dynamics_type.body.h"
#undef _PP_MODEL_DYNAMICS_TYPE_
