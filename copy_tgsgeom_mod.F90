MODULE COPY_TGSGEOM_MOD

USE YOMGSGEOM, ONLY : TGSGEOM

INTERFACE COPY
MODULE PROCEDURE COPY_TGSGEOM
END INTERFACE

CONTAINS

SUBROUTINE COPY_TGSGEOM (YD)

IMPLICIT NONE
TYPE (TGSGEOM), INTENT (IN) :: YD

!$acc enter data create (YD%RCORI)
!$acc update device (YD%RCORI)
!$acc enter data attach (YD%RCORI)

!$acc enter data create (YD%RCORIC)
!$acc update device (YD%RCORIC)
!$acc enter data attach (YD%RCORIC)

!$acc enter data create (YD%GEMU)
!$acc update device (YD%GEMU)
!$acc enter data attach (YD%GEMU)

!$acc enter data create (YD%GSQM2)
!$acc update device (YD%GSQM2)
!$acc enter data attach (YD%GSQM2)

!$acc enter data create (YD%GELAM)
!$acc update device (YD%GELAM)
!$acc enter data attach (YD%GELAM)

!$acc enter data create (YD%GELAT)
!$acc update device (YD%GELAT)
!$acc enter data attach (YD%GELAT)

!$acc enter data create (YD%GECLO)
!$acc update device (YD%GECLO)
!$acc enter data attach (YD%GECLO)

!$acc enter data create (YD%GESLO)
!$acc update device (YD%GESLO)
!$acc enter data attach (YD%GESLO)

!$acc enter data create (YD%GM)
!$acc update device (YD%GM)
!$acc enter data attach (YD%GM)

!$acc enter data create (YD%GMAPPA)
!$acc update device (YD%GMAPPA)
!$acc enter data attach (YD%GMAPPA)

!$acc enter data create (YD%GOMVRL)
!$acc update device (YD%GOMVRL)
!$acc enter data attach (YD%GOMVRL)

!$acc enter data create (YD%GOMVRM)
!$acc update device (YD%GOMVRM)
!$acc enter data attach (YD%GOMVRM)

!$acc enter data create (YD%GNORDL)
!$acc update device (YD%GNORDL)
!$acc enter data attach (YD%GNORDL)

!$acc enter data create (YD%GNORDM)
!$acc update device (YD%GNORDM)
!$acc enter data attach (YD%GNORDM)

!$acc enter data create (YD%GNORDLCL)
!$acc update device (YD%GNORDLCL)
!$acc enter data attach (YD%GNORDLCL)

!$acc enter data create (YD%GNORDMCL)
!$acc update device (YD%GNORDMCL)
!$acc enter data attach (YD%GNORDMCL)

!$acc enter data create (YD%GNORDMCM)
!$acc update device (YD%GNORDMCM)
!$acc enter data attach (YD%GNORDMCM)

!$acc enter data create (YD%GAW)
!$acc update device (YD%GAW)
!$acc enter data attach (YD%GAW)

!$acc enter data create (YD%NGPLAT)
!$acc update device (YD%NGPLAT)
!$acc enter data attach (YD%NGPLAT)

!$acc enter data create (YD%NUNIQUEGP)
!$acc update device (YD%NUNIQUEGP)
!$acc enter data attach (YD%NUNIQUEGP)

END SUBROUTINE

END MODULE
