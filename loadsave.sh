#!/bin/bash

set -x

dir_in=.
dir_out=.

function loadsave ()
{
  f="${!#}"  
  set -- "${@:1:$#-1}"
  ./loadsave.pl $* $f
}

if [ 1 -eq 1 ] 
then

for f in \
  type_geometry.F90 yomvert.F90 yomvsleta.F90 yomsta.F90 yomvsplip.F90 type_spgeom.F90 yomorog.F90 \
  yomgsgeom.F90 yomcsgeom.F90 yomhslmer.F90 yomvsleta.F90 yomvsplip.F90 yomgem.F90 yommp.F90 \
  yomdimv.F90 yomdim.F90 yomleg.F90 yomlap.F90 yomvert.F90 yemdim.F90 yemgeo.F90 yemmp.F90 \
  yemlap.F90 yemgsl.F90 yemlbc_geo.F90 eint_mod.F90 yomdimf.F90 yomrip.F90
do
  loadsave --dir=$dir_out --types $dir_in/$f 
done


loadsave --dir=$dir_out --types --skip MODEL_GENERAL_CONF_TYPE%GEOM $dir_in/model_general_conf_mod.F90
loadsave --dir=$dir_out --types --skip TYPE_GFL_COMP%PREVIOUS --skip TYPE_GFLD%YAERO_WVL_DIAG_NL $dir_in/yom_ygfl.F90


for f in \
  model_dynamics_mod.F90 yomdyn.F90 yemdyn.F90 spng_mod.F90 ptrgppc.F90 \
  intdynsl_mod.F90 yomslrep.F90 ptrslb1.F90 ptrslb2.F90 yomtnh.F90 ptrslb15.F90
do
  loadsave --dir=$dir_out --types $dir_in/$f
done

fi

