MODULE PAR_GFL
USE PARKIND1 , ONLY : JPIM
USE CRMDIMS   ,ONLY : CRMBUFSIZE
! Parametars used by GFL (and elsewhere)

! JPGFL : Max number of GFL fields
! JPNAMED_GFL : Number of currently pre-defined components of GFL
! JPGHG : Number of greenhouse gas fields
! JPCHEM : Number of chemical species
! JPAERO : Number of active aerosol fields
! JPAEROUT: Number of output aerosol fields
! JPUVP : Number of output from UV processor
! JPERA40 : Number of ERA40 diagnostic fields
! JPCH4S  : Number of added fields related to methane
! JPNOGW  : Number of diagnostic fields for NORO GWD SCHEME
! JPSLDIA : Number of SL dynamics diagnostic fields
! JPCHEM_ASSIM : Maximum number of assimilated of chemical species
! JPCRM : Number of CRM columns (prognostic variables space)
!-------------------------------------------------------------------------

INTEGER(KIND=JPIM), PARAMETER :: JPCRM=CRMBUFSIZE
INTEGER(KIND=JPIM), PARAMETER :: JPGFL=2163+JPCRM
INTEGER(KIND=JPIM), PARAMETER :: JPNAMED_GFL=27
INTEGER(KIND=JPIM), PARAMETER :: JPGHG=3
INTEGER(KIND=JPIM), PARAMETER :: JPCHEM=200
INTEGER(KIND=JPIM), PARAMETER :: JPCHEM_ASSIM=5
INTEGER(KIND=JPIM), PARAMETER :: JPAERO=19
INTEGER(KIND=JPIM), PARAMETER :: JPFORC=1100
INTEGER(KIND=JPIM), PARAMETER :: JPERA40=14
INTEGER(KIND=JPIM), PARAMETER :: JPSLDIA=7
INTEGER(KIND=JPIM), PARAMETER :: JPEZDIAG=50
INTEGER(KIND=JPIM), PARAMETER :: JPCH4S=1
INTEGER(KIND=JPIM), PARAMETER :: JPNOGW=2
INTEGER(KIND=JPIM), PARAMETER :: JPAEROUT=17
INTEGER(KIND=JPIM), PARAMETER :: JPUVP=2
INTEGER(KIND=JPIM), PARAMETER :: JPPHYS=8
INTEGER(KIND=JPIM), PARAMETER :: JPLIMA=50   
INTEGER(KIND=JPIM), PARAMETER :: GRIB_CODE_GFL_PHYS=81  ! AJGDB hopefully harmless

END MODULE PAR_GFL
