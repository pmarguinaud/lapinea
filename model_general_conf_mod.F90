
MODULE MODEL_GENERAL_CONF_MOD
  USE TYPE_GEOMETRY, ONLY : GEOMETRY
  USE YOMDIMF      , ONLY : TDIMF
  USE YOM_YGFL     , ONLY : TYPE_GFLD
  USE YOMRIP       , ONLY : TRIP
  IMPLICIT NONE

  TYPE MODEL_GENERAL_CONF_TYPE

    TYPE(GEOMETRY), POINTER :: GEOM => NULL()

    TYPE(TDIMF)             :: YRDIMF                  !! number of fields
    TYPE(TYPE_GFLD)         :: YGFL                    !! gfl descriptors
    TYPE(TRIP)              :: YRRIP     !! TEMPORARY TREATMENT OF TIME, SHOULD CHANGE AT CY45
 
  END TYPE MODEL_GENERAL_CONF_TYPE

  !---------------------------------------------------------------------

END MODULE MODEL_GENERAL_CONF_MOD

