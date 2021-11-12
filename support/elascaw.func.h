! functions

REAL(KIND=JPRB) :: FHLO1, FHLO2, FHLO3, FHLO4, PD

! auxiliary functions for Hermite cubic interpolation

FHLO1(PD)= (1.0_JPRB-PD)*(1.0_JPRB-PD)*(1.0_JPRB+2.0_JPRB*PD)
FHLO2(PD)= PD*PD*(3._JPRB-2.0_JPRB*PD)
FHLO3(PD)= PD*(1.0_JPRB-PD)*(1.0_JPRB-PD)
FHLO4(PD)=-PD*PD*(1.0_JPRB-PD)

