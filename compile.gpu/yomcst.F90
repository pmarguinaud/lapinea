MODULE YOMCST


#include "create.h"

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Common of physical constants
!     You will find the meanings in the annex 1 of the documentation

! A1.0 Fundamental constants
! * RPI          : number Pi
! * RCLUM        : light velocity
! * RHPLA        : Planck constant
! * RKBOL        : Bolzmann constant
! * RNAVO        : Avogadro number
REAL(KIND=JPRB) :: RPI
create (RPI)
REAL(KIND=JPRB) :: RCLUM
create (RCLUM)
REAL(KIND=JPRB) :: RHPLA
create (RHPLA)
REAL(KIND=JPRB) :: RKBOL
create (RKBOL)
REAL(KIND=JPRB) :: RNAVO

create (RNAVO)
! A1.1 Astronomical constants
! * RDAY         : duration of the solar day
! * RDAYI        : invariant time unit of 86400s
! * RHOUR        : duration of the solar hour 
! * REA          : astronomical unit (mean distance Earth-sun)
! * REPSM        : polar axis tilting angle
! * RSIYEA       : duration of the sideral year
! * RSIDAY       : duration of the sideral day
! * ROMEGA       : angular velocity of the Earth rotation
REAL(KIND=JPRB) :: RDAY
create (RDAY)
REAL(KIND=JPRB) :: RDAYI
create (RDAYI)
REAL(KIND=JPRB) :: RHOUR
create (RHOUR)
REAL(KIND=JPRB) :: REA
create (REA)
REAL(KIND=JPRB) :: REPSM
create (REPSM)
REAL(KIND=JPRB) :: RSIYEA
create (RSIYEA)
REAL(KIND=JPRB) :: RSIDAY
create (RSIDAY)
REAL(KIND=JPRB) :: ROMEGA

create (ROMEGA)
! A1.2 Geoide
! * RA           : Earth radius
! * RG           : gravity constant
! * R1SA         : 1/RA
REAL(KIND=JPRB) :: RA
create (RA)
REAL(KIND=JPRB) :: RG
create (RG)
REAL(KIND=JPRB) :: R1SA

create (R1SA)
! A1.3 Radiation
! * RSIGMA       : Stefan-Bolzman constant
! * RI0          : solar constant
REAL(KIND=JPRB) :: RSIGMA
create (RSIGMA)
REAL(KIND=JPRB) :: RI0

create (RI0)
! A1.4 Thermodynamic gas phase
! * R            : perfect gas constant
! * RMD          : dry air molar mass
! * RMV          : vapour water molar mass
! * RMO3         : ozone molar mass
! * RD           : R_dry (dry air constant)
! * RV           : R_vap (vapour water constant)
! * RCPD         : Cp_dry (dry air calorific capacity at constant pressure)
! * RCPV         : Cp_vap (vapour calorific capacity at constant pressure)
! * RCVD         : Cv_dry (dry air calorific capacity at constant volume)
! * RCVV         : Cv_vap (vapour calorific capacity at constant volume)
! * RKAPPA       : Kappa = R_dry/Cp_dry
! * RETV         : R_vap/R_dry - 1
! * RMCO2        : CO2 (carbon dioxyde) molar mass
! * RMCH4        : CH4 (methane) molar mass
! * RMN2O        : N2O molar mass
! * RMCO         : CO (carbon monoxyde) molar mass
! * RMHCHO       : HCHO molar mass
! * RMNO2        : NO2 (nitrogen dioxyde) molar mass
! * RMSO2        : SO2 (sulfur dioxyde) molar mass
! * RMSO4        : SO4 (sulphate) molar mass
REAL(KIND=JPRB) :: R
create (R)
REAL(KIND=JPRB) :: RMD
create (RMD)
REAL(KIND=JPRB) :: RMV
create (RMV)
REAL(KIND=JPRB) :: RMO3
create (RMO3)
REAL(KIND=JPRB) :: RD
create (RD)
REAL(KIND=JPRB) :: RV
create (RV)
REAL(KIND=JPRB) :: RCPD
create (RCPD)
REAL(KIND=JPRB) :: RCPV
create (RCPV)
REAL(KIND=JPRB) :: RCVD
create (RCVD)
REAL(KIND=JPRB) :: RCVV
create (RCVV)
REAL(KIND=JPRB) :: RKAPPA
create (RKAPPA)
REAL(KIND=JPRB) :: RETV
create (RETV)
REAL(KIND=JPRB) :: RMCO2
create (RMCO2)
REAL(KIND=JPRB) :: RMCH4
create (RMCH4)
REAL(KIND=JPRB) :: RMN2O
create (RMN2O)
REAL(KIND=JPRB) :: RMCO
create (RMCO)
REAL(KIND=JPRB) :: RMHCHO
create (RMHCHO)
REAL(KIND=JPRB) :: RMNO2
create (RMNO2)
REAL(KIND=JPRB) :: RMSO2
create (RMSO2)
REAL(KIND=JPRB) :: RMSO4

create (RMSO4)
! A1.5,6 Thermodynamic liquid,solid phases
! * RCW          : Cw (calorific capacity of liquid water)
! * RCS          : Cs (calorific capacity of solid water)
REAL(KIND=JPRB) :: RCW
create (RCW)
REAL(KIND=JPRB) :: RCS

create (RCS)
! A1.7 Thermodynamic transition of phase
! * RATM         : pre_n = "normal" pressure
! * RTT          : Tt = temperature of water fusion at "pre_n"
! * RLVTT        : RLvTt = vaporisation latent heat at T=Tt
! * RLSTT        : RLsTt = sublimation latent heat at T=Tt
! * RLVZER       : RLv0 = vaporisation latent heat at T=0K
! * RLSZER       : RLs0 = sublimation latent heat at T=0K
! * RLMLT        : RLMlt = melting latent heat at T=Tt
! * RDT          : Tt - Tx(ew-ei)
REAL(KIND=JPRB) :: RATM
create (RATM)
REAL(KIND=JPRB) :: RTT
create (RTT)
REAL(KIND=JPRB) :: RLVTT
create (RLVTT)
REAL(KIND=JPRB) :: RLSTT
create (RLSTT)
REAL(KIND=JPRB) :: RLVZER
create (RLVZER)
REAL(KIND=JPRB) :: RLSZER
create (RLSZER)
REAL(KIND=JPRB) :: RLMLT
create (RLMLT)
REAL(KIND=JPRB) :: RDT

create (RDT)
! A1.8 Curve of saturation
! * RESTT        : es(Tt) = saturation vapour tension at T=Tt
! * RGAMW        : Rgamw = (Cw-Cp_vap)/R_vap
! * RBETW        : Rbetw = RLvTt/R_vap + Rgamw*Tt
! * RALPW        : Ralpw = log(es(Tt)) + Rbetw/Tt + Rgamw*log(Tt)
! * RGAMS        : Rgams = (Cs-Cp_vap)/R_vap
! * RBETS        : Rbets = RLsTt/R_vap + Rgams*Tt
! * RALPS        : Ralps = log(es(Tt)) + Rbets/Tt + Rgams*log(Tt)
! * RALPD        : Ralpd = Ralps - Ralpw
! * RBETD        : Rbetd = Rbets - Rbetw
! * RGAMD        : Rgamd = Rgams - Rgamw
REAL(KIND=JPRB) :: RESTT
create (RESTT)
REAL(KIND=JPRB) :: RGAMW
create (RGAMW)
REAL(KIND=JPRB) :: RBETW
create (RBETW)
REAL(KIND=JPRB) :: RALPW
create (RALPW)
REAL(KIND=JPRB) :: RGAMS
create (RGAMS)
REAL(KIND=JPRB) :: RBETS
create (RBETS)
REAL(KIND=JPRB) :: RALPS
create (RALPS)
REAL(KIND=JPRB) :: RALPD
create (RALPD)
REAL(KIND=JPRB) :: RBETD
create (RBETD)
REAL(KIND=JPRB) :: RGAMD

create (RGAMD)
! NaN value
CHARACTER(LEN=8), PARAMETER :: CSNAN = &
  & CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(244)//CHAR(127)
create (CSNAN)
REAL(KIND=JPRB) :: RSNAN

create (RSNAN)
!    ------------------------------------------------------------------
END MODULE YOMCST
