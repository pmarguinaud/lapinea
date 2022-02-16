!     ------------------------------------------------------------------

REAL(KIND=JPRB),PARAMETER :: PP6_R=1.0_JPRB/6.0_JPRB
REAL(KIND=JPRB) :: PD
REAL(KIND=JPRB) :: FLAG1, FLAG2, FLAG3
REAL(KIND=JPRB) :: FQUAD1, FQUAD2, FQUAD3

! weights for cubic Lagrange interpolation (regular nodes)

FLAG1(PD)= 0.5_JPRB*(PD+1.0_JPRB)   *(PD-1.0_JPRB)*(PD-2.0_JPRB)
FLAG2(PD)=-0.5_JPRB*(PD+1.0_JPRB)*PD              *(PD-2.0_JPRB)
FLAG3(PD)= PP6_R   *(PD+1.0_JPRB)*PD*(PD-1.0_JPRB)

! weights for quadratic SLHD interpolation (regular nodes)
FQUAD1(PD)=(1.0_JPRB-PD)*(1.0_JPRB+0.25_JPRB*PD)
FQUAD2(PD)=PD*(1.25_JPRB-0.25_JPRB*PD)
FQUAD3(PD)=0.25_JPRB*PD*(PD-1.0_JPRB)

!     ------------------------------------------------------------------

