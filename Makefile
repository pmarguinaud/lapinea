include Makefile.inc

abor1.intfb.ok: abor1.intfb.h 
	touch abor1.intfb.ok

elarche.intfb.ok: elarche.intfb.h model_dynamics_mod.o parkind1.o yomcst.o yemgeo.o yemgsl.o yomjfh.o yomgsgeom.o eint_mod.o
	touch elarche.intfb.ok

elarmes.intfb.ok: elarmes.intfb.h model_dynamics_mod.o geometry_mod.o parkind1.o yomcst.o yomct0.o yomdyna.o yomlun.o yomrip.o eint_mod.o
	touch elarmes.intfb.ok

elascaw.intfb.ok: elascaw.intfb.h parkind1.o yomdyna.o yommp0.o eint_mod.o yomvsplip.o
	touch elascaw.intfb.ok

laitli.intfb.ok: laitli.intfb.h parkind1.o
	touch laitli.intfb.ok

lapinea.intfb.ok: lapinea.intfb.h model_dynamics_mod.o model_general_conf_mod.o geometry_mod.o parkind1.o yomct0.o yomdyna.o yomcver.o yomcst.o eint_mod.o
	touch lapinea.intfb.ok

larcina.intfb.ok: larcina.intfb.h model_dynamics_mod.o geometry_mod.o parkind1.o yomcst.o yomdyna.o eint_mod.o
	touch larcina.intfb.ok

larcinha.intfb.ok: larcinha.intfb.h model_dynamics_mod.o geometry_mod.o parkind1.o yomcst.o yomct0.o yomdyna.o yomlun.o eint_mod.o
	touch larcinha.intfb.ok

lascaw_clo.intfb.ok: lascaw_clo.intfb.h parkind1.o yomdyna.o yomdyn.o
	touch lascaw_clo.intfb.ok

lascaw_vintw.intfb.ok: lascaw_vintw.intfb.h parkind1.o yomdyna.o yomdyn.o eint_mod.o
	touch lascaw_vintw.intfb.ok

create.ok: create.h
	touch create.ok
pp_geometry.body.ok: pp_geometry.body.h
	touch pp_geometry.body.ok
pp_geometry.intf.ok: pp_geometry.intf.h
	touch pp_geometry.intf.ok
pp_model_dynamics_type.body.ok: pp_model_dynamics_type.body.h
	touch pp_model_dynamics_type.body.ok
pp_model_dynamics_type.intf.ok: pp_model_dynamics_type.intf.h
	touch pp_model_dynamics_type.intf.ok
pp_sl_struct.body.ok: pp_sl_struct.body.h
	touch pp_sl_struct.body.ok
pp_sl_struct.intf.ok: pp_sl_struct.intf.h
	touch pp_sl_struct.intf.ok
temp.ok: temp.h
	touch temp.ok
abor1.o: abor1.F90 
	$(FC) -c abor1.F90

copy_geometry_mod.o: copy_geometry_mod.F90 type_geometry.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimv_mod.o copy_tedim_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tspgeom_mod.o copy_tsta_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o
	$(FC) -c copy_geometry_mod.F90

copy_model_dynamics_type_mod.o: copy_model_dynamics_type_mod.F90 model_dynamics_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tdyn_mod.o copy_tedyn_mod.o copy_tlscaw_mod.o copy_tptrgppc_mod.o copy_tptrslb1_mod.o copy_tptrslb15_mod.o copy_tptrslb2_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspng_mod.o copy_ttnh_mod.o
	$(FC) -c copy_model_dynamics_type_mod.F90

copy_model_general_conf_type_mod.o: copy_model_general_conf_type_mod.F90 model_general_conf_mod.o copy_tdimf_mod.o copy_trip_mod.o copy_type_gfld_mod.o
	$(FC) -c copy_model_general_conf_type_mod.F90

copy_sl_struct_mod.o: copy_sl_struct_mod.F90 eint_mod.o
	$(FC) -c copy_sl_struct_mod.F90

copy_tcco_mod.o: copy_tcco_mod.F90 intdynsl_mod.o
	$(FC) -c copy_tcco_mod.F90

copy_tcsgeom_mod.o: copy_tcsgeom_mod.F90 yomcsgeom.o
	$(FC) -c copy_tcsgeom_mod.F90

copy_tcsgleg_mod.o: copy_tcsgleg_mod.F90 yomleg.o
	$(FC) -c copy_tcsgleg_mod.F90

copy_tdim_mod.o: copy_tdim_mod.F90 yomdim.o
	$(FC) -c copy_tdim_mod.F90

copy_tdimf_mod.o: copy_tdimf_mod.F90 yomdimf.o
	$(FC) -c copy_tdimf_mod.F90

copy_tdimv_mod.o: copy_tdimv_mod.F90 yomdimv.o
	$(FC) -c copy_tdimv_mod.F90

copy_tdyn_mod.o: copy_tdyn_mod.F90 yomdyn.o
	$(FC) -c copy_tdyn_mod.F90

copy_tedim_mod.o: copy_tedim_mod.F90 yemdim.o
	$(FC) -c copy_tedim_mod.F90

copy_tedyn_mod.o: copy_tedyn_mod.F90 yemdyn.o
	$(FC) -c copy_tedyn_mod.F90

copy_tegeo_mod.o: copy_tegeo_mod.F90 yemgeo.o
	$(FC) -c copy_tegeo_mod.F90

copy_tegsl_mod.o: copy_tegsl_mod.F90 yemgsl.o
	$(FC) -c copy_tegsl_mod.F90

copy_telbc_geo_mod.o: copy_telbc_geo_mod.F90 yemlbc_geo.o
	$(FC) -c copy_telbc_geo_mod.F90

copy_temmp_mod.o: copy_temmp_mod.F90 yemmp.o
	$(FC) -c copy_temmp_mod.F90

copy_tgem_mod.o: copy_tgem_mod.F90 yomgem.o
	$(FC) -c copy_tgem_mod.F90

copy_tgsgeom_mod.o: copy_tgsgeom_mod.F90 yomgsgeom.o
	$(FC) -c copy_tgsgeom_mod.F90

copy_thslmer_mod.o: copy_thslmer_mod.F90 yomhslmer.o
	$(FC) -c copy_thslmer_mod.F90

copy_tlap_mod.o: copy_tlap_mod.F90 yomlap.o
	$(FC) -c copy_tlap_mod.F90

copy_tlep_mod.o: copy_tlep_mod.F90 yemlap.o
	$(FC) -c copy_tlep_mod.F90

copy_tlscaw_mod.o: copy_tlscaw_mod.F90 intdynsl_mod.o
	$(FC) -c copy_tlscaw_mod.F90

copy_tmp_mod.o: copy_tmp_mod.F90 yommp.o
	$(FC) -c copy_tmp_mod.F90

copy_torog_mod.o: copy_torog_mod.F90 yomorog.o
	$(FC) -c copy_torog_mod.F90

copy_tptrgppc_mod.o: copy_tptrgppc_mod.F90 ptrgppc.o
	$(FC) -c copy_tptrgppc_mod.F90

copy_tptrslb15_mod.o: copy_tptrslb15_mod.F90 ptrslb15.o
	$(FC) -c copy_tptrslb15_mod.F90

copy_tptrslb1_mod.o: copy_tptrslb1_mod.F90 ptrslb1.o
	$(FC) -c copy_tptrslb1_mod.F90

copy_tptrslb2_mod.o: copy_tptrslb2_mod.F90 ptrslb2.o
	$(FC) -c copy_tptrslb2_mod.F90

copy_trip_mod.o: copy_trip_mod.F90 yomrip.o
	$(FC) -c copy_trip_mod.F90

copy_trscaw_mod.o: copy_trscaw_mod.F90 intdynsl_mod.o
	$(FC) -c copy_trscaw_mod.F90

copy_tsco_mod.o: copy_tsco_mod.F90 intdynsl_mod.o
	$(FC) -c copy_tsco_mod.F90

copy_tslrep_mod.o: copy_tslrep_mod.F90 yomslrep.o
	$(FC) -c copy_tslrep_mod.F90

copy_tspgeom_mod.o: copy_tspgeom_mod.F90 type_spgeom.o
	$(FC) -c copy_tspgeom_mod.F90

copy_tspng_mod.o: copy_tspng_mod.F90 spng_mod.o
	$(FC) -c copy_tspng_mod.F90

copy_tsta_mod.o: copy_tsta_mod.F90 yomsta.o
	$(FC) -c copy_tsta_mod.F90

copy_ttnh_mod.o: copy_ttnh_mod.F90 yomtnh.o
	$(FC) -c copy_ttnh_mod.F90

copy_tvab_mod.o: copy_tvab_mod.F90 yomvert.o
	$(FC) -c copy_tvab_mod.F90

copy_tvertical_geom_mod.o: copy_tvertical_geom_mod.F90 yomvert.o copy_tvab_mod.o copy_tveta_mod.o copy_tvfe_mod.o
	$(FC) -c copy_tvertical_geom_mod.F90

copy_tveta_mod.o: copy_tveta_mod.F90 yomvert.o
	$(FC) -c copy_tveta_mod.F90

copy_tvfe_mod.o: copy_tvfe_mod.F90 yomvert.o
	$(FC) -c copy_tvfe_mod.F90

copy_tvsleta_mod.o: copy_tvsleta_mod.F90 yomvsleta.o
	$(FC) -c copy_tvsleta_mod.F90

copy_tvsplip_mod.o: copy_tvsplip_mod.F90 yomvsplip.o
	$(FC) -c copy_tvsplip_mod.F90

copy_type_aero_wvl_diag_mod.o: copy_type_aero_wvl_diag_mod.F90 yoe_aerodiag.o
	$(FC) -c copy_type_aero_wvl_diag_mod.F90

copy_type_gfl_comp_mod.o: copy_type_gfl_comp_mod.F90 yom_ygfl.o
	$(FC) -c copy_type_gfl_comp_mod.F90

copy_type_gfl_naml_mod.o: copy_type_gfl_naml_mod.F90 yom_ygfl.o
	$(FC) -c copy_type_gfl_naml_mod.F90

copy_type_gfld_mod.o: copy_type_gfld_mod.F90 yom_ygfl.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o
	$(FC) -c copy_type_gfld_mod.F90

crmdims.o: crmdims.F90 parkind1.o
	$(FC) -c crmdims.F90

eint_mod.o: eint_mod.F90 parkind1.o
	$(FC) -c eint_mod.F90

elarche.o: elarche.F90 model_dynamics_mod.o parkind1.o yomcst.o yemgeo.o yemgsl.o yomjfh.o yomgsgeom.o eint_mod.o temp.ok abor1.intfb.ok
	$(FC) -c elarche.F90

elarmes.o: elarmes.F90 model_dynamics_mod.o geometry_mod.o parkind1.o yomcst.o yomct0.o yomdyna.o yomlun.o yomrip.o eint_mod.o temp.ok abor1.intfb.ok larcina.intfb.ok
	$(FC) -c elarmes.F90

elascaw.o: elascaw.F90 parkind1.o yomdyna.o yommp0.o eint_mod.o yomvsplip.o temp.ok lascaw_clo.intfb.ok lascaw_vintw.intfb.ok abor1.intfb.ok
	$(FC) -c elascaw.F90

geometry_mod.o: geometry_mod.F90 parkind1.o type_geometry.o
	$(FC) -c geometry_mod.F90

intdynsl_mod.o: intdynsl_mod.F90 parkind1.o yomdyna.o yomdyn.o
	$(FC) -c intdynsl_mod.F90

laitli.o: laitli.F90 parkind1.o temp.ok
	$(FC) -c laitli.F90

lapinea.o: lapinea.F90 model_dynamics_mod.o model_general_conf_mod.o geometry_mod.o parkind1.o yomct0.o yomdyna.o yomcver.o yomcst.o eint_mod.o temp.ok elarmes.intfb.ok larcina.intfb.ok larcinha.intfb.ok abor1.intfb.ok
	$(FC) -c lapinea.F90

larcina.o: larcina.F90 model_dynamics_mod.o geometry_mod.o parkind1.o yomcst.o yomdyna.o eint_mod.o temp.ok abor1.intfb.ok elarche.intfb.ok elascaw.intfb.ok laitli.intfb.ok
	$(FC) -c larcina.F90

larcinha.o: larcinha.F90 model_dynamics_mod.o geometry_mod.o parkind1.o yomcst.o yomct0.o yomdyna.o yomlun.o eint_mod.o temp.ok abor1.intfb.ok elascaw.intfb.ok
	$(FC) -c larcinha.F90

lascaw_clo.o: lascaw_clo.F90 parkind1.o yomdyna.o yomdyn.o temp.ok
	$(FC) -c lascaw_clo.F90

lascaw_vintw.o: lascaw_vintw.F90 parkind1.o yomdyna.o yomdyn.o eint_mod.o temp.ok
	$(FC) -c lascaw_vintw.F90

load_geometry_mod.o: load_geometry_mod.F90 type_geometry.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimv_mod.o load_tedim_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tmp_mod.o load_torog_mod.o load_tspgeom_mod.o load_tsta_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o
	$(FC) -c load_geometry_mod.F90

load_mod.o: load_mod.F90 parkind1.o
	$(FC) -c load_mod.F90

load_model_dynamics_type_mod.o: load_model_dynamics_type_mod.F90 model_dynamics_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tdyn_mod.o load_tedyn_mod.o load_tlscaw_mod.o load_tptrgppc_mod.o load_tptrslb1_mod.o load_tptrslb15_mod.o load_tptrslb2_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspng_mod.o load_ttnh_mod.o
	$(FC) -c load_model_dynamics_type_mod.F90

load_model_general_conf_type_mod.o: load_model_general_conf_type_mod.F90 model_general_conf_mod.o load_tdimf_mod.o load_trip_mod.o load_type_gfld_mod.o
	$(FC) -c load_model_general_conf_type_mod.F90

load_sl_struct_mod.o: load_sl_struct_mod.F90 eint_mod.o
	$(FC) -c load_sl_struct_mod.F90

load_tcco_mod.o: load_tcco_mod.F90 intdynsl_mod.o
	$(FC) -c load_tcco_mod.F90

load_tcsgeom_mod.o: load_tcsgeom_mod.F90 yomcsgeom.o
	$(FC) -c load_tcsgeom_mod.F90

load_tcsgleg_mod.o: load_tcsgleg_mod.F90 yomleg.o
	$(FC) -c load_tcsgleg_mod.F90

load_tdim_mod.o: load_tdim_mod.F90 yomdim.o
	$(FC) -c load_tdim_mod.F90

load_tdimf_mod.o: load_tdimf_mod.F90 yomdimf.o
	$(FC) -c load_tdimf_mod.F90

load_tdimv_mod.o: load_tdimv_mod.F90 yomdimv.o
	$(FC) -c load_tdimv_mod.F90

load_tdyn_mod.o: load_tdyn_mod.F90 yomdyn.o
	$(FC) -c load_tdyn_mod.F90

load_tedim_mod.o: load_tedim_mod.F90 yemdim.o
	$(FC) -c load_tedim_mod.F90

load_tedyn_mod.o: load_tedyn_mod.F90 yemdyn.o
	$(FC) -c load_tedyn_mod.F90

load_tegeo_mod.o: load_tegeo_mod.F90 yemgeo.o
	$(FC) -c load_tegeo_mod.F90

load_tegsl_mod.o: load_tegsl_mod.F90 yemgsl.o
	$(FC) -c load_tegsl_mod.F90

load_telbc_geo_mod.o: load_telbc_geo_mod.F90 yemlbc_geo.o
	$(FC) -c load_telbc_geo_mod.F90

load_temmp_mod.o: load_temmp_mod.F90 yemmp.o
	$(FC) -c load_temmp_mod.F90

load_tgem_mod.o: load_tgem_mod.F90 yomgem.o
	$(FC) -c load_tgem_mod.F90

load_tgsgeom_mod.o: load_tgsgeom_mod.F90 yomgsgeom.o
	$(FC) -c load_tgsgeom_mod.F90

load_thslmer_mod.o: load_thslmer_mod.F90 yomhslmer.o
	$(FC) -c load_thslmer_mod.F90

load_tlap_mod.o: load_tlap_mod.F90 yomlap.o
	$(FC) -c load_tlap_mod.F90

load_tlep_mod.o: load_tlep_mod.F90 yemlap.o
	$(FC) -c load_tlep_mod.F90

load_tlscaw_mod.o: load_tlscaw_mod.F90 intdynsl_mod.o
	$(FC) -c load_tlscaw_mod.F90

load_tmp_mod.o: load_tmp_mod.F90 yommp.o
	$(FC) -c load_tmp_mod.F90

load_torog_mod.o: load_torog_mod.F90 yomorog.o
	$(FC) -c load_torog_mod.F90

load_tptrgppc_mod.o: load_tptrgppc_mod.F90 ptrgppc.o
	$(FC) -c load_tptrgppc_mod.F90

load_tptrslb15_mod.o: load_tptrslb15_mod.F90 ptrslb15.o
	$(FC) -c load_tptrslb15_mod.F90

load_tptrslb1_mod.o: load_tptrslb1_mod.F90 ptrslb1.o
	$(FC) -c load_tptrslb1_mod.F90

load_tptrslb2_mod.o: load_tptrslb2_mod.F90 ptrslb2.o
	$(FC) -c load_tptrslb2_mod.F90

load_trip_mod.o: load_trip_mod.F90 yomrip.o
	$(FC) -c load_trip_mod.F90

load_trscaw_mod.o: load_trscaw_mod.F90 intdynsl_mod.o
	$(FC) -c load_trscaw_mod.F90

load_tsco_mod.o: load_tsco_mod.F90 intdynsl_mod.o
	$(FC) -c load_tsco_mod.F90

load_tslrep_mod.o: load_tslrep_mod.F90 yomslrep.o
	$(FC) -c load_tslrep_mod.F90

load_tspgeom_mod.o: load_tspgeom_mod.F90 type_spgeom.o
	$(FC) -c load_tspgeom_mod.F90

load_tspng_mod.o: load_tspng_mod.F90 spng_mod.o
	$(FC) -c load_tspng_mod.F90

load_tsta_mod.o: load_tsta_mod.F90 yomsta.o
	$(FC) -c load_tsta_mod.F90

load_ttnh_mod.o: load_ttnh_mod.F90 yomtnh.o
	$(FC) -c load_ttnh_mod.F90

load_tvab_mod.o: load_tvab_mod.F90 yomvert.o
	$(FC) -c load_tvab_mod.F90

load_tvertical_geom_mod.o: load_tvertical_geom_mod.F90 yomvert.o load_tvab_mod.o load_tveta_mod.o load_tvfe_mod.o
	$(FC) -c load_tvertical_geom_mod.F90

load_tveta_mod.o: load_tveta_mod.F90 yomvert.o
	$(FC) -c load_tveta_mod.F90

load_tvfe_mod.o: load_tvfe_mod.F90 yomvert.o
	$(FC) -c load_tvfe_mod.F90

load_tvsleta_mod.o: load_tvsleta_mod.F90 yomvsleta.o
	$(FC) -c load_tvsleta_mod.F90

load_tvsplip_mod.o: load_tvsplip_mod.F90 yomvsplip.o
	$(FC) -c load_tvsplip_mod.F90

load_type_gfl_comp_mod.o: load_type_gfl_comp_mod.F90 yom_ygfl.o
	$(FC) -c load_type_gfl_comp_mod.F90

load_type_gfl_naml_mod.o: load_type_gfl_naml_mod.F90 yom_ygfl.o
	$(FC) -c load_type_gfl_naml_mod.F90

load_type_gfld_mod.o: load_type_gfld_mod.F90 yom_ygfl.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o
	$(FC) -c load_type_gfld_mod.F90

load_yomcst_mod.o: load_yomcst_mod.F90 yomcst.o
	$(FC) -c load_yomcst_mod.F90

load_yomct0_mod.o: load_yomct0_mod.F90 yomct0.o
	$(FC) -c load_yomct0_mod.F90

load_yomcver_mod.o: load_yomcver_mod.F90 yomcver.o
	$(FC) -c load_yomcver_mod.F90

load_yomdyna_mod.o: load_yomdyna_mod.F90 yomdyna.o
	$(FC) -c load_yomdyna_mod.F90

load_yomjfh_mod.o: load_yomjfh_mod.F90 yomjfh.o
	$(FC) -c load_yomjfh_mod.F90

load_yomlun_mod.o: load_yomlun_mod.F90 yomlun.o
	$(FC) -c load_yomlun_mod.F90

load_yommp0_mod.o: load_yommp0_mod.F90 yommp0.o
	$(FC) -c load_yommp0_mod.F90

model_dynamics_mod.o: model_dynamics_mod.F90 yomdyn.o yemdyn.o spng_mod.o ptrgppc.o intdynsl_mod.o yomslrep.o ptrslb1.o ptrslb2.o ptrslb15.o yomtnh.o eint_mod.o
	$(FC) -c model_dynamics_mod.F90

model_general_conf_mod.o: model_general_conf_mod.F90 type_geometry.o yomdimf.o yom_ygfl.o yomrip.o
	$(FC) -c model_general_conf_mod.F90

par_gfl.o: par_gfl.F90 parkind1.o crmdims.o
	$(FC) -c par_gfl.F90

parkind1.o: parkind1.F90 
	$(FC) -c parkind1.F90

ptrgppc.o: ptrgppc.F90 parkind1.o
	$(FC) -c ptrgppc.F90

ptrslb1.o: ptrslb1.F90 parkind1.o
	$(FC) -c ptrslb1.F90

ptrslb15.o: ptrslb15.F90 parkind1.o
	$(FC) -c ptrslb15.F90

ptrslb2.o: ptrslb2.F90 parkind1.o
	$(FC) -c ptrslb2.F90

spng_mod.o: spng_mod.F90 parkind1.o yomcst.o yomct0.o yomlun.o
	$(FC) -c spng_mod.F90

test_geometry.o: test_geometry.F90 load_geometry_mod.o copy_geometry_mod.o pp_geometry.intf.ok pp_geometry.intf.ok pp_geometry.body.ok pp_geometry.body.ok
	$(FC) -c test_geometry.F90

test_model_dynamics_type.o: test_model_dynamics_type.F90 load_model_dynamics_type_mod.o copy_model_dynamics_type_mod.o pp_model_dynamics_type.intf.ok pp_model_dynamics_type.intf.ok pp_model_dynamics_type.body.ok pp_model_dynamics_type.body.ok
	$(FC) -c test_model_dynamics_type.F90

test_sl_struct.o: test_sl_struct.F90 eint_mod.o load_sl_struct_mod.o copy_sl_struct_mod.o pp_sl_struct.intf.ok pp_sl_struct.intf.ok pp_sl_struct.body.ok pp_sl_struct.body.ok
	$(FC) -c test_sl_struct.F90

type_geometry.o: type_geometry.F90 yomvert.o yomsta.o yomlap.o yomleg.o yomdim.o yomdimv.o yommp.o yomgem.o yomvsplip.o yomvsleta.o yomhslmer.o yomcsgeom.o yomgsgeom.o yomorog.o type_spgeom.o yemdim.o yemgeo.o yemmp.o yemlap.o yemgsl.o yemlbc_geo.o
	$(FC) -c type_geometry.F90

type_spgeom.o: type_spgeom.F90 parkind1.o
	$(FC) -c type_spgeom.F90

wrap_lapinea.o: wrap_lapinea.F90 load_geometry_mod.o load_sl_struct_mod.o load_model_general_conf_type_mod.o load_model_dynamics_type_mod.o load_mod.o load_yomct0_mod.o load_yomdyna_mod.o load_yomcver_mod.o load_yomcst_mod.o load_yommp0_mod.o load_yomlun_mod.o load_yomjfh_mod.o xrd_getoptions.o parkind1.o lapinea.intfb.ok
	$(FC) -c wrap_lapinea.F90

xrd_getoptions.o: xrd_getoptions.F90 parkind1.o xrd_unix_env.o
	$(FC) -c xrd_getoptions.F90

xrd_unix_env.o: xrd_unix_env.F90 parkind1.o
	$(FC) -c xrd_unix_env.F90

yemdim.o: yemdim.F90 parkind1.o
	$(FC) -c yemdim.F90

yemdyn.o: yemdyn.F90 parkind1.o
	$(FC) -c yemdyn.F90

yemgeo.o: yemgeo.F90 parkind1.o
	$(FC) -c yemgeo.F90

yemgsl.o: yemgsl.F90 parkind1.o
	$(FC) -c yemgsl.F90

yemlap.o: yemlap.F90 parkind1.o
	$(FC) -c yemlap.F90

yemlbc_geo.o: yemlbc_geo.F90 parkind1.o
	$(FC) -c yemlbc_geo.F90

yemmp.o: yemmp.F90 parkind1.o
	$(FC) -c yemmp.F90

yoe_aerodiag.o: yoe_aerodiag.F90 parkind1.o
	$(FC) -c yoe_aerodiag.F90

yom_ygfl.o: yom_ygfl.F90 parkind1.o yoe_aerodiag.o par_gfl.o
	$(FC) -c yom_ygfl.F90

yomcsgeom.o: yomcsgeom.F90 parkind1.o
	$(FC) -c yomcsgeom.F90

yomcst.o: yomcst.F90 parkind1.o create.ok
	$(FC) -c yomcst.F90

yomct0.o: yomct0.F90 parkind1.o create.ok
	$(FC) -c yomct0.F90

yomcver.o: yomcver.F90 parkind1.o yomlun.o yomct0.o create.ok
	$(FC) -c yomcver.F90

yomdim.o: yomdim.F90 parkind1.o
	$(FC) -c yomdim.F90

yomdimf.o: yomdimf.F90 parkind1.o
	$(FC) -c yomdimf.F90

yomdimv.o: yomdimv.F90 parkind1.o
	$(FC) -c yomdimv.F90

yomdyn.o: yomdyn.F90 parkind1.o
	$(FC) -c yomdyn.F90

yomdyna.o: yomdyna.F90 parkind1.o create.ok
	$(FC) -c yomdyna.F90

yomgem.o: yomgem.F90 parkind1.o
	$(FC) -c yomgem.F90

yomgsgeom.o: yomgsgeom.F90 parkind1.o
	$(FC) -c yomgsgeom.F90

yomhslmer.o: yomhslmer.F90 parkind1.o
	$(FC) -c yomhslmer.F90

yomjfh.o: yomjfh.F90 parkind1.o create.ok
	$(FC) -c yomjfh.F90

yomlap.o: yomlap.F90 parkind1.o
	$(FC) -c yomlap.F90

yomleg.o: yomleg.F90 parkind1.o
	$(FC) -c yomleg.F90

yomlun.o: yomlun.F90 parkind1.o yomlun_ifsaux.o create.ok
	$(FC) -c yomlun.F90

yomlun_ifsaux.o: yomlun_ifsaux.F90 parkind1.o create.ok
	$(FC) -c yomlun_ifsaux.F90

yommp.o: yommp.F90 parkind1.o
	$(FC) -c yommp.F90

yommp0.o: yommp0.F90 parkind1.o create.ok
	$(FC) -c yommp0.F90

yomorog.o: yomorog.F90 parkind1.o
	$(FC) -c yomorog.F90

yomrip.o: yomrip.F90 parkind1.o
	$(FC) -c yomrip.F90

yomslrep.o: yomslrep.F90 parkind1.o
	$(FC) -c yomslrep.F90

yomsta.o: yomsta.F90 parkind1.o
	$(FC) -c yomsta.F90

yomtnh.o: yomtnh.F90 parkind1.o
	$(FC) -c yomtnh.F90

yomvert.o: yomvert.F90 parkind1.o yomct0.o yomcver.o yomdyna.o
	$(FC) -c yomvert.F90

yomvsleta.o: yomvsleta.F90 parkind1.o
	$(FC) -c yomvsleta.F90

yomvsplip.o: yomvsplip.F90 parkind1.o
	$(FC) -c yomvsplip.F90

test_geometry.x: test_geometry.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o
	$(FC) -o test_geometry.x test_geometry.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o

test_model_dynamics_type.x: test_model_dynamics_type.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o
	$(FC) -o test_model_dynamics_type.x test_model_dynamics_type.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o

test_sl_struct.x: test_sl_struct.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o
	$(FC) -o test_sl_struct.x test_sl_struct.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o

wrap_lapinea.x: wrap_lapinea.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o
	$(FC) -o wrap_lapinea.x wrap_lapinea.o abor1.o copy_geometry_mod.o copy_model_dynamics_type_mod.o copy_model_general_conf_type_mod.o copy_sl_struct_mod.o copy_tcco_mod.o copy_tcsgeom_mod.o copy_tcsgleg_mod.o copy_tdim_mod.o copy_tdimf_mod.o copy_tdimv_mod.o copy_tdyn_mod.o copy_tedim_mod.o copy_tedyn_mod.o copy_tegeo_mod.o copy_tegsl_mod.o copy_telbc_geo_mod.o copy_temmp_mod.o copy_tgem_mod.o copy_tgsgeom_mod.o copy_thslmer_mod.o copy_tlap_mod.o copy_tlep_mod.o copy_tlscaw_mod.o copy_tmp_mod.o copy_torog_mod.o copy_tptrgppc_mod.o copy_tptrslb15_mod.o copy_tptrslb1_mod.o copy_tptrslb2_mod.o copy_trip_mod.o copy_trscaw_mod.o copy_tsco_mod.o copy_tslrep_mod.o copy_tspgeom_mod.o copy_tspng_mod.o copy_tsta_mod.o copy_ttnh_mod.o copy_tvab_mod.o copy_tvertical_geom_mod.o copy_tveta_mod.o copy_tvfe_mod.o copy_tvsleta_mod.o copy_tvsplip_mod.o copy_type_aero_wvl_diag_mod.o copy_type_gfl_comp_mod.o copy_type_gfl_naml_mod.o copy_type_gfld_mod.o crmdims.o eint_mod.o elarche.o elarmes.o elascaw.o geometry_mod.o intdynsl_mod.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o load_geometry_mod.o load_mod.o load_model_dynamics_type_mod.o load_model_general_conf_type_mod.o load_sl_struct_mod.o load_tcco_mod.o load_tcsgeom_mod.o load_tcsgleg_mod.o load_tdim_mod.o load_tdimf_mod.o load_tdimv_mod.o load_tdyn_mod.o load_tedim_mod.o load_tedyn_mod.o load_tegeo_mod.o load_tegsl_mod.o load_telbc_geo_mod.o load_temmp_mod.o load_tgem_mod.o load_tgsgeom_mod.o load_thslmer_mod.o load_tlap_mod.o load_tlep_mod.o load_tlscaw_mod.o load_tmp_mod.o load_torog_mod.o load_tptrgppc_mod.o load_tptrslb15_mod.o load_tptrslb1_mod.o load_tptrslb2_mod.o load_trip_mod.o load_trscaw_mod.o load_tsco_mod.o load_tslrep_mod.o load_tspgeom_mod.o load_tspng_mod.o load_tsta_mod.o load_ttnh_mod.o load_tvab_mod.o load_tvertical_geom_mod.o load_tveta_mod.o load_tvfe_mod.o load_tvsleta_mod.o load_tvsplip_mod.o load_type_gfl_comp_mod.o load_type_gfl_naml_mod.o load_type_gfld_mod.o load_yomcst_mod.o load_yomct0_mod.o load_yomcver_mod.o load_yomdyna_mod.o load_yomjfh_mod.o load_yomlun_mod.o load_yommp0_mod.o model_dynamics_mod.o model_general_conf_mod.o par_gfl.o parkind1.o ptrgppc.o ptrslb1.o ptrslb15.o ptrslb2.o spng_mod.o type_geometry.o type_spgeom.o xrd_getoptions.o xrd_unix_env.o yemdim.o yemdyn.o yemgeo.o yemgsl.o yemlap.o yemlbc_geo.o yemmp.o yoe_aerodiag.o yom_ygfl.o yomcsgeom.o yomcst.o yomct0.o yomcver.o yomdim.o yomdimf.o yomdimv.o yomdyn.o yomdyna.o yomgem.o yomgsgeom.o yomhslmer.o yomjfh.o yomlap.o yomleg.o yomlun.o yomlun_ifsaux.o yommp.o yommp0.o yomorog.o yomrip.o yomslrep.o yomsta.o yomtnh.o yomvert.o yomvsleta.o yomvsplip.o


subclean:
	\rm -f abor1.o elarche.o elarmes.o elascaw.o laitli.o lapinea.o larcina.o larcinha.o lascaw_clo.o lascaw_vintw.o test_geometry.o test_model_dynamics_type.o test_sl_struct.o wrap_lapinea.o

clean:
	\rm -f *.o *.xml *.a *.x *.mod *.optrpt

tidy:
	\rm -f *.xml *.optrpt
