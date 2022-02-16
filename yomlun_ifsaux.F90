MODULE YOMLUN_IFSAUX


#include "create.h"

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Logical units used by code

!     NULOUT :   output unit
!     NULERR :   unit number for comparison with reference run

!     NULDRHACK : output unit for drHack pseudo xml file (see
!     dr_hook_util.F90)
INTEGER(KIND=JPIM) :: NULOUT = 6
create (NULOUT)
INTEGER(KIND=JPIM) :: NULERR = 0
create (NULERR)
INTEGER(KIND=JPIM) :: NULDRHACK = 999

create (NULDRHACK)
!     ------------------------------------------------------------------
END MODULE YOMLUN_IFSAUX
