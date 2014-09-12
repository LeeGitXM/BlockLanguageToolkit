-- Map G2 FinalDiagnosis calc methods into a python module.
insert into ProcedureMap values ('calc-cstr_ml_problem-output-gda','ils.fd.CstrMl.calculate');
insert into ProcedureMap values ('calc-cstr_c2_problem-output-gda','ils.fd.CstrC2.calculate');
insert into ProcedureMap values ('calc-cstr_ens_problem-output-gda','ils.fd.CstrEnb.calculate');
insert into ProcedureMap values ('calc-cstr_mlr_problem-output-gda','ils.fd.CstrMlr.calculate');
insert into ProcedureMap values ('calc-cstr_dml_problem-output-gda','ils.fd.CstrDml.calculate');
insert into ProcedureMap values ('calc-cstr_dc2_problem-output-gda','ils.fd.CstrDc2.calculate');
insert into ProcedureMap values ('calc-cstr_denb_problem-output-gda','ils.fd.CstrDenb.calculate');
insert into ProcedureMap values ('calc-cstr_satb_problem-output-gda','ils.fd.CstrStab.calculate');
insert into ProcedureMap values ('calc-cstr_oil_problem-output-gda','ils.fd.CstrOil.calculate');
insert into ProcedureMap values ('calc-cstr_polysplit-output-gda','ils.fd.CstrPolysplit.calculate');
insert into ProcedureMap values ('calc-cstr_ca_problem-output-gda','ils.fd.CstrCa.calculate');
insert into ProcedureMap values ('do_split_flying_switch-gda','ils.fd.SplitFlyingSwitch.calculate');
insert into ProcedureMap values ('do_c_flying_switch-gda','ils.fd.CFlyingSwitch.calculate');
insert into ProcedureMap values ('do_series_flying_switch-gda','ils.fd.SeriesFlyingSwitch.calculate');
insert into ProcedureMap values ('do_single_flying_switch-gda','ils.fd.SingleFlyingSwitch.calculate');
insert into ProcedureMap values ('do_split_rate_chg-gda','ils.fd.SplitRateChange.calculate');
insert into ProcedureMap values ('do_rate_chng-gda','ils.fd.RateChange.calculate');
insert into ProcedureMap values ('do_series_rate_chg-gda','ils.fd.SeriesRateChange.calculate');
insert into ProcedureMap values ('do_single_rate_chg-gda','ils.fd.SingleRateChange.calculate');
insert into ProcedureMap values ('constant','ils.fd.Constant.calculate');
insert into ProcedureMap values ('calc-no-output-gda','ils.fd.NoOutput.calculate');
insert into ProcedureMap values ('calc-baler-temp-problem-output-gda','ils.fd.BalerTemperature.calculate');
insert into ProcedureMap values ('calc-reslurry-problem-output-gda','ils.fd.Reslurry.calculate');
insert into ProcedureMap values ('calc-baler-vol-output-gda','ils.fd.BalerVolume.calculate');
insert into ProcedureMap values ('prod-mooney-gda','ils.fd.ProductMooney.calculate');
-- Map G2 utility methods into Python equivalents
insert into ProcedureMap values ('g2-tw-popup','ils.utility.Window.displayClient');
