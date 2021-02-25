include Makefile.inc

abor1.intfb.ok: abor1.intfb.h 
	touch abor1.intfb.ok

crmdims.mod: crmdims.o

eint_mod.mod: eint_mod.o

elarche.intfb.ok: elarche.intfb.h model_dynamics_mod.mod parkind1.mod yomcst.mod yemgeo.mod yemgsl.mod yomjfh.mod yomgsgeom.mod eint_mod.mod
	touch elarche.intfb.ok

elarmes.intfb.ok: elarmes.intfb.h model_dynamics_mod.mod geometry_mod.mod parkind1.mod yomcst.mod yomct0.mod yomdyna.mod yomlun.mod yomrip.mod eint_mod.mod
	touch elarmes.intfb.ok

elascaw.intfb.ok: elascaw.intfb.h parkind1.mod yomdyna.mod yommp0.mod eint_mod.mod yomvsplip.mod
	touch elascaw.intfb.ok

geometry_mod.mod: geometry_mod.o

intdynsl_mod.mod: intdynsl_mod.o

laitli.intfb.ok: laitli.intfb.h parkind1.mod
	touch laitli.intfb.ok

lapinea.intfb.ok: lapinea.intfb.h model_dynamics_mod.mod model_general_conf_mod.mod geometry_mod.mod parkind1.mod yomct0.mod yomdyna.mod yomcver.mod yomcst.mod eint_mod.mod
	touch lapinea.intfb.ok

larcina.intfb.ok: larcina.intfb.h model_dynamics_mod.mod geometry_mod.mod parkind1.mod yomcst.mod yomdyna.mod eint_mod.mod
	touch larcina.intfb.ok

larcinha.intfb.ok: larcinha.intfb.h model_dynamics_mod.mod geometry_mod.mod parkind1.mod yomcst.mod yomct0.mod yomdyna.mod yomlun.mod eint_mod.mod
	touch larcinha.intfb.ok

lascaw_clo.intfb.ok: lascaw_clo.intfb.h parkind1.mod yomdyna.mod yomdyn.mod
	touch lascaw_clo.intfb.ok

lascaw_vintw.intfb.ok: lascaw_vintw.intfb.h parkind1.mod yomdyna.mod yomdyn.mod eint_mod.mod
	touch lascaw_vintw.intfb.ok

load_geometry_mod.mod: load_geometry_mod.o

load_mod.mod: load_mod.o

load_model_dynamics_type_mod.mod: load_model_dynamics_type_mod.o

load_model_general_conf_type_mod.mod: load_model_general_conf_type_mod.o

load_sl_struct_mod.mod: load_sl_struct_mod.o

load_tcco_mod.mod: load_tcco_mod.o

load_tcsgeom_mod.mod: load_tcsgeom_mod.o

load_tcsgleg_mod.mod: load_tcsgleg_mod.o

load_tdim_mod.mod: load_tdim_mod.o

load_tdimf_mod.mod: load_tdimf_mod.o

load_tdimv_mod.mod: load_tdimv_mod.o

load_tdyn_mod.mod: load_tdyn_mod.o

load_tedim_mod.mod: load_tedim_mod.o

load_tedyn_mod.mod: load_tedyn_mod.o

load_tegeo_mod.mod: load_tegeo_mod.o

load_tegsl_mod.mod: load_tegsl_mod.o

load_telbc_geo_mod.mod: load_telbc_geo_mod.o

load_temmp_mod.mod: load_temmp_mod.o

load_tgem_mod.mod: load_tgem_mod.o

load_tgsgeom_mod.mod: load_tgsgeom_mod.o

load_thslmer_mod.mod: load_thslmer_mod.o

load_tlap_mod.mod: load_tlap_mod.o

load_tlep_mod.mod: load_tlep_mod.o

load_tlscaw_mod.mod: load_tlscaw_mod.o

load_tmp_mod.mod: load_tmp_mod.o

load_torog_mod.mod: load_torog_mod.o

load_tptrgppc_mod.mod: load_tptrgppc_mod.o

load_tptrslb15_mod.mod: load_tptrslb15_mod.o

load_tptrslb1_mod.mod: load_tptrslb1_mod.o

load_tptrslb2_mod.mod: load_tptrslb2_mod.o

load_trip_mod.mod: load_trip_mod.o

load_trscaw_mod.mod: load_trscaw_mod.o

load_tsco_mod.mod: load_tsco_mod.o

load_tslrep_mod.mod: load_tslrep_mod.o

load_tspgeom_mod.mod: load_tspgeom_mod.o

load_tspng_mod.mod: load_tspng_mod.o

load_tsta_mod.mod: load_tsta_mod.o

load_ttnh_mod.mod: load_ttnh_mod.o

load_tvab_mod.mod: load_tvab_mod.o

load_tvertical_geom_mod.mod: load_tvertical_geom_mod.o

load_tveta_mod.mod: load_tveta_mod.o

load_tvfe_mod.mod: load_tvfe_mod.o

load_tvsleta_mod.mod: load_tvsleta_mod.o

load_tvsplip_mod.mod: load_tvsplip_mod.o

load_type_gfl_comp_mod.mod: load_type_gfl_comp_mod.o

load_type_gfl_naml_mod.mod: load_type_gfl_naml_mod.o

load_type_gfld_mod.mod: load_type_gfld_mod.o

load_yomcst_mod.mod: load_yomcst_mod.o

load_yomct0_mod.mod: load_yomct0_mod.o

load_yomcver_mod.mod: load_yomcver_mod.o

load_yomdyna_mod.mod: load_yomdyna_mod.o

load_yomjfh_mod.mod: load_yomjfh_mod.o

load_yomlun_mod.mod: load_yomlun_mod.o

load_yommp0_mod.mod: load_yommp0_mod.o

model_dynamics_mod.mod: model_dynamics_mod.o

model_general_conf_mod.mod: model_general_conf_mod.o

par_gfl.mod: par_gfl.o

parkind1.mod: parkind1.o

ptrgppc.mod: ptrgppc.o

ptrslb1.mod: ptrslb1.o

ptrslb15.mod: ptrslb15.o

ptrslb2.mod: ptrslb2.o

spng_mod.mod: spng_mod.o

type_geometry.mod: type_geometry.o

type_spgeom.mod: type_spgeom.o

xrd_getoptions.mod: xrd_getoptions.o

xrd_unix_env.mod: xrd_unix_env.o

yemdim.mod: yemdim.o

yemdyn.mod: yemdyn.o

yemgeo.mod: yemgeo.o

yemgsl.mod: yemgsl.o

yemlap.mod: yemlap.o

yemlbc_geo.mod: yemlbc_geo.o

yemmp.mod: yemmp.o

yoe_aerodiag.mod: yoe_aerodiag.o

yom_ygfl.mod: yom_ygfl.o

yomcsgeom.mod: yomcsgeom.o

yomcst.mod: yomcst.o

yomct0.mod: yomct0.o

yomcver.mod: yomcver.o

yomdim.mod: yomdim.o

yomdimf.mod: yomdimf.o

yomdimv.mod: yomdimv.o

yomdyn.mod: yomdyn.o

yomdyna.mod: yomdyna.o

yomgem.mod: yomgem.o

yomgsgeom.mod: yomgsgeom.o

yomhslmer.mod: yomhslmer.o

yomjfh.mod: yomjfh.o

yomlap.mod: yomlap.o

yomleg.mod: yomleg.o

yomlun.mod: yomlun.o

yomlun_ifsaux.mod: yomlun_ifsaux.o

yommp.mod: yommp.o

yommp0.mod: yommp0.o

yomorog.mod: yomorog.o

yomrip.mod: yomrip.o

yomslrep.mod: yomslrep.o

yomsta.mod: yomsta.o

yomtnh.mod: yomtnh.o

yomvert.mod: yomvert.o

yomvsleta.mod: yomvsleta.o

yomvsplip.mod: yomvsplip.o

abor1.o: abor1.F90 
	$(FC) -c abor1.F90

crmdims.o: crmdims.F90 parkind1.mod
	$(FC) -c crmdims.F90

eint_mod.o: eint_mod.F90 parkind1.mod
	$(FC) -c eint_mod.F90

elarche.o: elarche.F90 model_dynamics_mod.mod parkind1.mod yomcst.mod yemgeo.mod yemgsl.mod yomjfh.mod yomgsgeom.mod eint_mod.mod abor1.intfb.ok
	$(FC) -c elarche.F90

elarmes.o: elarmes.F90 model_dynamics_mod.mod geometry_mod.mod parkind1.mod yomcst.mod yomct0.mod yomdyna.mod yomlun.mod yomrip.mod eint_mod.mod abor1.intfb.ok larcina.intfb.ok
	$(FC) -c elarmes.F90

elascaw.o: elascaw.F90 parkind1.mod yomdyna.mod yommp0.mod eint_mod.mod yomvsplip.mod lascaw_clo.intfb.ok lascaw_vintw.intfb.ok
	$(FC) -c elascaw.F90

geometry_mod.o: geometry_mod.F90 parkind1.mod type_geometry.mod
	$(FC) -c geometry_mod.F90

intdynsl_mod.o: intdynsl_mod.F90 parkind1.mod yomdyna.mod yomdyn.mod
	$(FC) -c intdynsl_mod.F90

laitli.o: laitli.F90 parkind1.mod
	$(FC) -c laitli.F90

lapinea.o: lapinea.F90 model_dynamics_mod.mod model_general_conf_mod.mod geometry_mod.mod parkind1.mod yomct0.mod yomdyna.mod yomcver.mod yomcst.mod eint_mod.mod elarmes.intfb.ok larcina.intfb.ok larcinha.intfb.ok abor1.intfb.ok
	$(FC) -c lapinea.F90

larcina.o: larcina.F90 model_dynamics_mod.mod geometry_mod.mod parkind1.mod yomcst.mod yomdyna.mod eint_mod.mod abor1.intfb.ok elarche.intfb.ok elascaw.intfb.ok laitli.intfb.ok
	$(FC) -c larcina.F90

larcinha.o: larcinha.F90 model_dynamics_mod.mod geometry_mod.mod parkind1.mod yomcst.mod yomct0.mod yomdyna.mod yomlun.mod eint_mod.mod abor1.intfb.ok elascaw.intfb.ok
	$(FC) -c larcinha.F90

lascaw_clo.o: lascaw_clo.F90 parkind1.mod yomdyna.mod yomdyn.mod
	$(FC) -c lascaw_clo.F90

lascaw_vintw.o: lascaw_vintw.F90 parkind1.mod yomdyna.mod yomdyn.mod eint_mod.mod
	$(FC) -c lascaw_vintw.F90

load_geometry_mod.o: load_geometry_mod.F90 type_geometry.mod load_tcsgeom_mod.mod load_tcsgleg_mod.mod load_tdim_mod.mod load_tdimv_mod.mod load_tedim_mod.mod load_tegeo_mod.mod load_tegsl_mod.mod load_telbc_geo_mod.mod load_temmp_mod.mod load_tgem_mod.mod load_tgsgeom_mod.mod load_thslmer_mod.mod load_tlap_mod.mod load_tlep_mod.mod load_tmp_mod.mod load_torog_mod.mod load_tspgeom_mod.mod load_tsta_mod.mod load_tvab_mod.mod load_tvertical_geom_mod.mod load_tveta_mod.mod load_tvfe_mod.mod load_tvsleta_mod.mod load_tvsplip_mod.mod
	$(FC) -c load_geometry_mod.F90

load_mod.o: load_mod.F90 parkind1.mod
	$(FC) -c load_mod.F90

load_model_dynamics_type_mod.o: load_model_dynamics_type_mod.F90 model_dynamics_mod.mod load_sl_struct_mod.mod load_tcco_mod.mod load_tdyn_mod.mod load_tedyn_mod.mod load_tlscaw_mod.mod load_tptrgppc_mod.mod load_tptrslb1_mod.mod load_tptrslb15_mod.mod load_tptrslb2_mod.mod load_trscaw_mod.mod load_tsco_mod.mod load_tslrep_mod.mod load_tspng_mod.mod load_ttnh_mod.mod
	$(FC) -c load_model_dynamics_type_mod.F90

load_model_general_conf_type_mod.o: load_model_general_conf_type_mod.F90 model_general_conf_mod.mod load_tdimf_mod.mod load_trip_mod.mod load_type_gfld_mod.mod
	$(FC) -c load_model_general_conf_type_mod.F90

load_sl_struct_mod.o: load_sl_struct_mod.F90 eint_mod.mod
	$(FC) -c load_sl_struct_mod.F90

load_tcco_mod.o: load_tcco_mod.F90 intdynsl_mod.mod
	$(FC) -c load_tcco_mod.F90

load_tcsgeom_mod.o: load_tcsgeom_mod.F90 yomcsgeom.mod
	$(FC) -c load_tcsgeom_mod.F90

load_tcsgleg_mod.o: load_tcsgleg_mod.F90 yomleg.mod
	$(FC) -c load_tcsgleg_mod.F90

load_tdim_mod.o: load_tdim_mod.F90 yomdim.mod
	$(FC) -c load_tdim_mod.F90

load_tdimf_mod.o: load_tdimf_mod.F90 yomdimf.mod
	$(FC) -c load_tdimf_mod.F90

load_tdimv_mod.o: load_tdimv_mod.F90 yomdimv.mod
	$(FC) -c load_tdimv_mod.F90

load_tdyn_mod.o: load_tdyn_mod.F90 yomdyn.mod
	$(FC) -c load_tdyn_mod.F90

load_tedim_mod.o: load_tedim_mod.F90 yemdim.mod
	$(FC) -c load_tedim_mod.F90

load_tedyn_mod.o: load_tedyn_mod.F90 yemdyn.mod
	$(FC) -c load_tedyn_mod.F90

load_tegeo_mod.o: load_tegeo_mod.F90 yemgeo.mod
	$(FC) -c load_tegeo_mod.F90

load_tegsl_mod.o: load_tegsl_mod.F90 yemgsl.mod
	$(FC) -c load_tegsl_mod.F90

load_telbc_geo_mod.o: load_telbc_geo_mod.F90 yemlbc_geo.mod
	$(FC) -c load_telbc_geo_mod.F90

load_temmp_mod.o: load_temmp_mod.F90 yemmp.mod
	$(FC) -c load_temmp_mod.F90

load_tgem_mod.o: load_tgem_mod.F90 yomgem.mod
	$(FC) -c load_tgem_mod.F90

load_tgsgeom_mod.o: load_tgsgeom_mod.F90 yomgsgeom.mod
	$(FC) -c load_tgsgeom_mod.F90

load_thslmer_mod.o: load_thslmer_mod.F90 yomhslmer.mod
	$(FC) -c load_thslmer_mod.F90

load_tlap_mod.o: load_tlap_mod.F90 yomlap.mod
	$(FC) -c load_tlap_mod.F90

load_tlep_mod.o: load_tlep_mod.F90 yemlap.mod
	$(FC) -c load_tlep_mod.F90

load_tlscaw_mod.o: load_tlscaw_mod.F90 intdynsl_mod.mod
	$(FC) -c load_tlscaw_mod.F90

load_tmp_mod.o: load_tmp_mod.F90 yommp.mod
	$(FC) -c load_tmp_mod.F90

load_torog_mod.o: load_torog_mod.F90 yomorog.mod
	$(FC) -c load_torog_mod.F90

load_tptrgppc_mod.o: load_tptrgppc_mod.F90 ptrgppc.mod
	$(FC) -c load_tptrgppc_mod.F90

load_tptrslb15_mod.o: load_tptrslb15_mod.F90 ptrslb15.mod
	$(FC) -c load_tptrslb15_mod.F90

load_tptrslb1_mod.o: load_tptrslb1_mod.F90 ptrslb1.mod
	$(FC) -c load_tptrslb1_mod.F90

load_tptrslb2_mod.o: load_tptrslb2_mod.F90 ptrslb2.mod
	$(FC) -c load_tptrslb2_mod.F90

load_trip_mod.o: load_trip_mod.F90 yomrip.mod
	$(FC) -c load_trip_mod.F90

load_trscaw_mod.o: load_trscaw_mod.F90 intdynsl_mod.mod
	$(FC) -c load_trscaw_mod.F90

load_tsco_mod.o: load_tsco_mod.F90 intdynsl_mod.mod
	$(FC) -c load_tsco_mod.F90

load_tslrep_mod.o: load_tslrep_mod.F90 yomslrep.mod
	$(FC) -c load_tslrep_mod.F90

load_tspgeom_mod.o: load_tspgeom_mod.F90 type_spgeom.mod
	$(FC) -c load_tspgeom_mod.F90

load_tspng_mod.o: load_tspng_mod.F90 spng_mod.mod
	$(FC) -c load_tspng_mod.F90

load_tsta_mod.o: load_tsta_mod.F90 yomsta.mod
	$(FC) -c load_tsta_mod.F90

load_ttnh_mod.o: load_ttnh_mod.F90 yomtnh.mod
	$(FC) -c load_ttnh_mod.F90

load_tvab_mod.o: load_tvab_mod.F90 yomvert.mod
	$(FC) -c load_tvab_mod.F90

load_tvertical_geom_mod.o: load_tvertical_geom_mod.F90 yomvert.mod load_tvab_mod.mod load_tveta_mod.mod load_tvfe_mod.mod
	$(FC) -c load_tvertical_geom_mod.F90

load_tveta_mod.o: load_tveta_mod.F90 yomvert.mod
	$(FC) -c load_tveta_mod.F90

load_tvfe_mod.o: load_tvfe_mod.F90 yomvert.mod
	$(FC) -c load_tvfe_mod.F90

load_tvsleta_mod.o: load_tvsleta_mod.F90 yomvsleta.mod
	$(FC) -c load_tvsleta_mod.F90

load_tvsplip_mod.o: load_tvsplip_mod.F90 yomvsplip.mod
	$(FC) -c load_tvsplip_mod.F90

load_type_gfl_comp_mod.o: load_type_gfl_comp_mod.F90 yom_ygfl.mod
	$(FC) -c load_type_gfl_comp_mod.F90

load_type_gfl_naml_mod.o: load_type_gfl_naml_mod.F90 yom_ygfl.mod
	$(FC) -c load_type_gfl_naml_mod.F90

load_type_gfld_mod.o: load_type_gfld_mod.F90 yom_ygfl.mod load_type_gfl_comp_mod.mod load_type_gfl_naml_mod.mod
	$(FC) -c load_type_gfld_mod.F90

load_yomcst_mod.o: load_yomcst_mod.F90 yomcst.mod
	$(FC) -c load_yomcst_mod.F90

load_yomct0_mod.o: load_yomct0_mod.F90 yomct0.mod
	$(FC) -c load_yomct0_mod.F90

load_yomcver_mod.o: load_yomcver_mod.F90 yomcver.mod
	$(FC) -c load_yomcver_mod.F90

load_yomdyna_mod.o: load_yomdyna_mod.F90 yomdyna.mod
	$(FC) -c load_yomdyna_mod.F90

load_yomjfh_mod.o: load_yomjfh_mod.F90 yomjfh.mod
	$(FC) -c load_yomjfh_mod.F90

load_yomlun_mod.o: load_yomlun_mod.F90 yomlun.mod
	$(FC) -c load_yomlun_mod.F90

load_yommp0_mod.o: load_yommp0_mod.F90 yommp0.mod
	$(FC) -c load_yommp0_mod.F90

model_dynamics_mod.o: model_dynamics_mod.F90 yomdyn.mod yemdyn.mod spng_mod.mod ptrgppc.mod intdynsl_mod.mod yomslrep.mod ptrslb1.mod ptrslb2.mod ptrslb15.mod yomtnh.mod eint_mod.mod
	$(FC) -c model_dynamics_mod.F90

model_general_conf_mod.o: model_general_conf_mod.F90 type_geometry.mod yomdimf.mod yom_ygfl.mod yomrip.mod
	$(FC) -c model_general_conf_mod.F90

par_gfl.o: par_gfl.F90 parkind1.mod crmdims.mod
	$(FC) -c par_gfl.F90

parkind1.o: parkind1.F90 
	$(FC) -c parkind1.F90

ptrgppc.o: ptrgppc.F90 parkind1.mod
	$(FC) -c ptrgppc.F90

ptrslb1.o: ptrslb1.F90 parkind1.mod
	$(FC) -c ptrslb1.F90

ptrslb15.o: ptrslb15.F90 parkind1.mod
	$(FC) -c ptrslb15.F90

ptrslb2.o: ptrslb2.F90 parkind1.mod
	$(FC) -c ptrslb2.F90

spng_mod.o: spng_mod.F90 parkind1.mod yomcst.mod yomct0.mod yomlun.mod
	$(FC) -c spng_mod.F90

type_geometry.o: type_geometry.F90 yomvert.mod yomsta.mod yomlap.mod yomleg.mod yomdim.mod yomdimv.mod yommp.mod yomgem.mod yomvsplip.mod yomvsleta.mod yomhslmer.mod yomcsgeom.mod yomgsgeom.mod yomorog.mod type_spgeom.mod yemdim.mod yemgeo.mod yemmp.mod yemlap.mod yemgsl.mod yemlbc_geo.mod
	$(FC) -c type_geometry.F90

type_spgeom.o: type_spgeom.F90 parkind1.mod
	$(FC) -c type_spgeom.F90

wrap_lapinea.o: wrap_lapinea.F90 load_geometry_mod.mod load_sl_struct_mod.mod load_model_general_conf_type_mod.mod load_model_dynamics_type_mod.mod load_mod.mod load_yomct0_mod.mod load_yomdyna_mod.mod load_yomcver_mod.mod load_yomcst_mod.mod load_yommp0_mod.mod load_yomlun_mod.mod load_yomjfh_mod.mod xrd_getoptions.mod parkind1.mod lapinea.intfb.ok
	$(FC) -c wrap_lapinea.F90

xrd_getoptions.o: xrd_getoptions.F90 parkind1.mod xrd_unix_env.mod
	$(FC) -c xrd_getoptions.F90

xrd_unix_env.o: xrd_unix_env.F90 parkind1.mod
	$(FC) -c xrd_unix_env.F90

yemdim.o: yemdim.F90 parkind1.mod
	$(FC) -c yemdim.F90

yemdyn.o: yemdyn.F90 parkind1.mod
	$(FC) -c yemdyn.F90

yemgeo.o: yemgeo.F90 parkind1.mod
	$(FC) -c yemgeo.F90

yemgsl.o: yemgsl.F90 parkind1.mod
	$(FC) -c yemgsl.F90

yemlap.o: yemlap.F90 parkind1.mod
	$(FC) -c yemlap.F90

yemlbc_geo.o: yemlbc_geo.F90 parkind1.mod
	$(FC) -c yemlbc_geo.F90

yemmp.o: yemmp.F90 parkind1.mod
	$(FC) -c yemmp.F90

yoe_aerodiag.o: yoe_aerodiag.F90 parkind1.mod
	$(FC) -c yoe_aerodiag.F90

yom_ygfl.o: yom_ygfl.F90 parkind1.mod yoe_aerodiag.mod par_gfl.mod
	$(FC) -c yom_ygfl.F90

yomcsgeom.o: yomcsgeom.F90 parkind1.mod
	$(FC) -c yomcsgeom.F90

yomcst.o: yomcst.F90 parkind1.mod
	$(FC) -c yomcst.F90

yomct0.o: yomct0.F90 parkind1.mod
	$(FC) -c yomct0.F90

yomcver.o: yomcver.F90 parkind1.mod yomlun.mod yomct0.mod
	$(FC) -c yomcver.F90

yomdim.o: yomdim.F90 parkind1.mod
	$(FC) -c yomdim.F90

yomdimf.o: yomdimf.F90 parkind1.mod
	$(FC) -c yomdimf.F90

yomdimv.o: yomdimv.F90 parkind1.mod
	$(FC) -c yomdimv.F90

yomdyn.o: yomdyn.F90 parkind1.mod
	$(FC) -c yomdyn.F90

yomdyna.o: yomdyna.F90 parkind1.mod
	$(FC) -c yomdyna.F90

yomgem.o: yomgem.F90 parkind1.mod
	$(FC) -c yomgem.F90

yomgsgeom.o: yomgsgeom.F90 parkind1.mod
	$(FC) -c yomgsgeom.F90

yomhslmer.o: yomhslmer.F90 parkind1.mod
	$(FC) -c yomhslmer.F90

yomjfh.o: yomjfh.F90 parkind1.mod
	$(FC) -c yomjfh.F90

yomlap.o: yomlap.F90 parkind1.mod
	$(FC) -c yomlap.F90

yomleg.o: yomleg.F90 parkind1.mod
	$(FC) -c yomleg.F90

yomlun.o: yomlun.F90 parkind1.mod yomlun_ifsaux.mod
	$(FC) -c yomlun.F90

yomlun_ifsaux.o: yomlun_ifsaux.F90 parkind1.mod
	$(FC) -c yomlun_ifsaux.F90

yommp.o: yommp.F90 parkind1.mod
	$(FC) -c yommp.F90

yommp0.o: yommp0.F90 parkind1.mod
	$(FC) -c yommp0.F90

yomorog.o: yomorog.F90 parkind1.mod
	$(FC) -c yomorog.F90

yomrip.o: yomrip.F90 parkind1.mod
	$(FC) -c yomrip.F90

yomslrep.o: yomslrep.F90 parkind1.mod
	$(FC) -c yomslrep.F90

yomsta.o: yomsta.F90 parkind1.mod
	$(FC) -c yomsta.F90

yomtnh.o: yomtnh.F90 parkind1.mod
	$(FC) -c yomtnh.F90

yomvert.o: yomvert.F90 parkind1.mod yomct0.mod yomcver.mod yomdyna.mod
	$(FC) -c yomvert.F90

yomvsleta.o: yomvsleta.F90 parkind1.mod
	$(FC) -c yomvsleta.F90

yomvsplip.o: yomvsplip.F90 parkind1.mod
	$(FC) -c yomvsplip.F90

wrap_lapinea.x: wrap_lapinea.o abor1.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o
	$(FC) -o wrap_lapinea.x wrap_lapinea.o abor1.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o


clean:
	\rm -f *.o *.xml *.a *.x *.mod
