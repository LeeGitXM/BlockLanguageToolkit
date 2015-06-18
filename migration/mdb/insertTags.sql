-- Map G2 GSI names to Ignition Tags
-- Columns are GSI name, tag path
-- NOTE: value is not a legitimate tag name
-- NOTE: Tag names must be unique, folder placement is immaterial.
--
-- The following tags represent Source/Sink connections in []DiagnosticToolkit/Connections
--
insert into TagMap values ('AB-BALER-VOL-LAB-SQC-PATH','[]Connections/ABBalerVolLab','STRING');
insert into TagMap values ('AB-BALER-VOL-FTNIR-SQC-PATH','[]Connections/ABBalerVolFtnir','STRING');
insert into TagMap values ('C-D-E-BALER-TEMP-SQC','[]Connections/CDEBalerTemp','STRING');
insert into TagMap values ('C-RX_GRD_PATH-GDA','[]Connections/CRxGrd','STRING');
insert into TagMap values ('C_FLYING_SWITCH_OUTPUT','[]Connections/CFlyingSwitchOutput','STRING');
insert into TagMap values ('C2_SQC_TO_C2_CSTR-GDA','[]Connections/C2SqcToC2Cstr','STRING');
insert into TagMap values ('CA-SQC-TO-CRX-GDA','[]Connections/CaSqcToCrx','STRING');
insert into TagMap values ('CAT_IS_IN','[]Connections/CatIsIn-a','STRING');
insert into TagMap values ('CAT_IS_IN_PATH-GDA','[]Connections/CatIsIn-b','STRING');
insert into TagMap values ('CAT_IS_OUT_PATH-GDA','[]Connections/CatIsOut','STRING');
insert into TagMap values ('CD-BALER-VOL-LAB-SQC-PATH','[]Connections/CDBalerVolLab','STRING');
insert into TagMap values ('CD-BALER-VOL-FTNIR-SQC-PATH','[]Connections/CDBalerVolFtnir','STRING');
insert into TagMap values ('E-BALER-VOL-LAB-SQC-PATH','[]Connections/EBalerVolLab','STRING');
insert into TagMap values ('E-BALER-VOL-FTNIR-SQC-PATH','[]Connections/EBalerVolFtnir','STRING');
insert into TagMap values ('FRNT_AVG_C2_NOT_HIGH_PATH-GDA','[]Connections/FrntAvgC2NotHigh','STRING');
insert into TagMap values ('FRNT_LNGTH_ABOVE_SP_PATH-GDA','[]Connections/FrntLngthAboveSP','STRING');
insert into TagMap values ('ML_HIGH-GDA','[]Connections/MlHigh','STRING');
insert into TagMap values ('ML_LOW_PATH-GDA','[]Connections/MlLow','STRING');
insert into TagMap values ('MOONEY_SQC_TO_CSTR_MOONEY-GDA','[]Connections/MooneySqcToCstrMooney','STRING');
insert into TagMap values ('PREMIX_LINE_FRESH-PATH-GDA','[]Connections/PremixLineFresh','STRING');
insert into TagMap values ('PREMIX_TEMP_AVAIL_PATH-GDA','[]Connections/PremixTempAvail','STRING');
insert into TagMap values ('PREMIX_TEMP_CHG_PERMITTED-PATH-GDA','[]Connections/PremixTempChgPermitted','STRING');
insert into TagMap values ('PREMIX_TEMP_LESS_THAN_MAX_PATH-GDA','[]Connections/PremixTempLessThanMax','STRING');
insert into TagMap values ('POLY_RATE_CHANGE_OUTPUT','[]Connections/PolyRateChangeOutput','STRING');
insert into TagMap values ('RATE_CHANGE_OUTPUT','[]Connections/RateChangeOutput','STRING');
insert into TagMap values ('RX-ML-OK-PATH-GDA','[]Connections/RxMlOk','STRING');
insert into TagMap values ('SDSTRM_C3C2_RATIO','[]Connections/SdstrmC3C2Ratio','STRING');
insert into TagMap values ('SERIES_FLYING_SWITCH_OUTPUT','[]Connections/SeriesFlyingSwitchOutput','STRING');
insert into TagMap values ('SERIES_RATE_CHANGE_OUTPUT','[]Connections/SeriesRateChangeOutput','STRING');
insert into TagMap values ('SINGLE_RATE_CHANGE_OUTPUT','[]Connections/SingleRateChangeOutput','STRING');
insert into TagMap values ('SINGLE_FLYING_SWITCH_OUTPUT','[]Connections/SingleFlyingSwitchOutput','STRING');
insert into TagMap values ('SPLIT_FLYING_SWITCH_OUTPUT','[]Connections/SplitFlyingSwitchOutput','STRING');
insert into TagMap values ('SPLIT_RATE_CHANGE_OUTPUT','[]Connections/SplitRateChangeOutput','STRING');
insert into TagMap values ('STAB_SQC_TO_CRX-GDA','[]Connections/StabSqcToCrx','STRING');
insert into TagMap values ('UNIFORM_CNTR_TPR_TIPS_PATH-GDA','[]Connections/UniformCntrTprTips','STRING');
insert into TagMap values ('FRNT_LNGTH_BELOW_SETPOINT_TO_SF-4-GDA','[]Connections/FrntLngthBelowSetpointSF4','STRING');
insert into TagMap values ('FRNT_LNGTH_SHORT_TO_MOONEY_LOGIC-GDA','[]Connections/FrntLngthShortToMooneyLogic','STRING');
--
-- Lab data path
--
insert into TagMap values ('MOONEY-LAB-DATA','[]LabData/RLA3/MOONEY-LAB-DATA/value','DOUBLE');
insert into TagMap values ('MLR-LAB-DATA','[]LabData/RLA3/MLR-LAB-DATA/value','DOUBLE');
insert into TagMap values ('C2-LAB-DATA','[]LabData/RLA3/C2-LAB-DATA/value','DOUBLE');
insert into TagMap values ('C9-LAB-DATA','[]LabData/RLA3/C9-LAB-DATA/value','DOUBLE');
insert into TagMap values ('DML-LAB-DATA','[]LabData/RLA3/DML-LAB-DATA/value','DOUBLE');
insert into TagMap values ('DC2-LAB-DATA','[]LabData/RLA3/DC2-LAB-DATA/value','DOUBLE');
insert into TagMap values ('DC9-LAB-DATA','[]LabData/RLA3/DC9-LAB-DATA/value','DOUBLE');
insert into TagMap values ('CA-LAB-DATA','[]LabData/RLA3/CA-LAB-DATA/value','DOUBLE');
insert into TagMap values ('STAB-LAB-DATA','[]LabData/RLA3/STAB-LAB-DATA/value','DOUBLE');
insert into TagMap values ('OIL-LAB-DATA','[]LabData/RLA3/OIL-LAB-DATA/value','DOUBLE');
insert into TagMap values ('PROD-ML-LAB-DATA','[]LabData/RLA3/PROD-ML-LAB-DATA/value','DOUBLE');
insert into TagMap values ('POLYSPLIT-DATA','[]LabData/RLA3/POLYSPLIT-DATA/value','DOUBLE');
insert into TagMap values ('POLYSPLIT-DATA','[]LabData/RLA3/POLYSPLIT-DATA-SQC/value','DOUBLE');
insert into TagMap values ('C2-LAB-DATA-FOR-R1-NLC','[]LabData/RLA3/C2-LAB-DATA-FOR-R1-NLC/value','DOUBLE');
--
insert into TagMap values ('A-BALER-TEMP-LAB-DATA','[]LabData/VFU/A-BALER-TEMP-LAB-DATA/value','DOUBLE');
insert into TagMap values ('B-BALER-TEMP-LAB-DATA','[]LabData/VFU/B-BALER-TEMP-LAB-DATA/value','DOUBLE');
insert into TagMap values ('C-BALER-TEMP-LAB-DATA','[]LabData/VFU/C-BALER-TEMP-LAB-DATA/value','DOUBLE');
insert into TagMap values ('D-BALER-TEMP-LAB-DATA','[]LabData/VFU/D-BALER-TEMP-LAB-DATA/value','DOUBLE');
insert into TagMap values ('E-BALER-TEMP-LAB-DATA','[]LabData/VFU/E-BALER-TEMP-LAB-DATA/value','DOUBLE');
--
insert into TagMap values ('AB-BALER-VOL-LAB-DATA','[]LabData/VFU/AB-BALER-VOL-LAB-DATA/value','DOUBLE');
insert into TagMap values ('CD-BALER-VOL-LAB-DATA','[]LabData/VFU/CD-BALER-VOL-LAB-DATA/value','DOUBLE');
insert into TagMap values ('E-BALER-VOL-LAB-DATA','[]LabData/VFU/E-BALER-VOL-LAB-DATA/value','DOUBLE');
--
insert into TagMap values ('AB-BALER-VOL-FTNIR-DATA','[]LabData/VFU/AB-BALER-VOL-FTNIR/val ue','DOUBLE');  
insert into TagMap values ('CD-BALER-VOL-FTNIR-DATA','[]LabData/VFU/CD-BALER-VOL-FTNIR/value','DOUBLE');
insert into TagMap values ('E-BALER-VOL-FTNIR-DATA','[]LabData/VFU/E-BALER-VOL-FTNIR/value','DOUBLE');
--
-- UDT paths for SQC problems
--
insert into TagMap values ('[the state  of MOONEY-LAB-DATA]','[]LabData/RLA3/MOONEY-LAB-DATA-SQC/state','STRING');
insert into TagMap values ('[the target of MOONEY-LAB-DATA]','[]LabData/RLA3/MOONEY-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of MLR-LAB-DATA]','[]LabData/RLA3/MLR-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of C2-LAB-DATA]','[]LabData/RLA3/C2-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of c9-lab-data]','[]LabData/RLA3/C9-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of DML-LAB-DATA]','[]LabData/RLA3/DML-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of DC2-LAB-DATA]','[]LabData/RLA3/DC2-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of Dc9-lab-data]','[]LabData/RLA3/DC9-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of CA-LAB-DATA]','[]LabData/RLA3/CA-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of STAB-LAB-DATA]','[]LabData/RLA3/STAB-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of OIL-LAB-DATA]','[]LabData/RLA3/OIL-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of PROD-ML-LAB-DATA]','[]LabData/RLA3/PROD-ML-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of POLYSPLIT-DATA]','[]LabData/RLA3/POLYSPLIT-DATA-SQC/target','DOUBLE');
--
insert into TagMap values ('[the target of A-BALER-TEMP-LAB-DATA]','[]LabData/VFU/A-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of B-BALER-TEMP-LAB-DATA]','[]LabData/VFU/B-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of C-BALER-TEMP-LAB-DATA]','[]LabData/VFU/C-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of D-BALER-TEMP-LAB-DATA]','[]LabData/VFU/D-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of E-BALER-TEMP-LAB-DATA]','[]LabData/VFU/E-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
--
insert into TagMap values ('[the target of AB-BALER-VOL-lab-DATA]','[]LabData/VFU/AB-BALER-VOL-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of CD-BALER-VOL-lab-DATA]','[]LabData/VFU/CD-BALER-VOL-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of E-BALER-VOL-lab-DATA]','[]LabData/VFU/E-BALER-VOL-LAB-DATA-SQC/target','DOUBLE');
--
insert into TagMap values ('[the target of AB-BALER-VOL-ftnir-DATA]','[]LabData/VFU/AB-BALER-VOL-FTNIR-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of CD-BALER-VOL-ftnir-DATA]','[]LabData/VFU/CD-BALER-VOL-FTNIR-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of E-BALER-VOL-ftnir-DATA]','[]Tags/VFU/E-BALER-VOL-FTNIR-DATA-SQC/target','DOUBLE');
--
insert into TagMap values ('[the standard-deviation of MOONEY-LAB-DATA]','[]LabData/RLA3/MOONEY-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of MLR-LAB-DATA]','[]LabData/RLA3/MLR-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of C2-LAB-DATA]','[]LabData/RLA3/C2-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of c9-lab-data]','[]LabData/RLA3/C9-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of DML-LAB-DATA]','[]LabData/RLA3/DML-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of DC2-LAB-DATA]','[]LabData/RLA3/DC2-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of Dc9-lab-data]','[]LabData/RLA3/DC9-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of CA-LAB-DATA]','[]LabData/RLA3/CA-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of STAB-LAB-DATA]','[]LabData/RLA3/STAB-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of OIL-LAB-DATA]','[]LabData/RLA3/OIL-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of PROD-ML-LAB-DATA]','[]LabData/RLA3/PROD-ML-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of POLYSPLIT-DATA]','[]LabData/RLA3/POLYSPLIT-DATA-SQC/standardDeviation','DOUBLE');
--
insert into TagMap values ('[the standard-deviation of A-BALER-TEMP-LAB-DATA]','[]LabData/VFU/A-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of B-BALER-TEMP-LAB-DATA]','[]LabData/VFU/B-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
 insert into TagMap values ('[the standard-deviation of C-BALER-TEMP-LAB-DATA]','[]LabData/VFU/C-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of D-BALER-TEMP-LAB-DATA]','[]LabData/VFU/D-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of E-BALER-TEMP-LAB-DATA]','[]LabData/VFU/E-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
--
insert into TagMap values ('[the standard-deviation of AB-BALER-VOL-lab-DATA]','[]LabData/VFU/AB-BALER-VOL-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of CD-BALER-VOL-lab-DATA]','[]LabData/VFU/CD-BALER-VOL-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of E-BALER-VOL-lab-DATA]','[]LabData/VFU/E-BALER-VOL-LAB-DATA-SQC/standardDeviation','DOUBLE');
--
insert into TagMap values ('[the standard-deviation of AB-BALER-VOL-ftnir-DATA]','[]LabData/VFU/AB-BALER-VOL-FTNIR-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of CD-BALER-VOL-ftnir-DATA]','[]LabData/VFU/CD-BALER-VOL-FTNIR-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of E-BALER-VOL-ftnir-DATA]','[]LabData/VFU/E-BALER-VOL-FTNIR-DATA-SQC/standardDeviation','DOUBLE');
--
--    C-Rx info tags - UDTs for input via OPC
--
insert into TagMap values ('C3_CONVERSION','[]DiagnosticToolkit/CRX/VRG521Z','DOUBLE');
insert into TagMap values ('CAT_EFFICIENCY','[]DiagnosticToolkit/CRX/VRG531Z-1','DOUBLE');
insert into TagMap values ('CAT_PREMIX_TEMP','[]DiagnosticToolkit/CRX/VCT205X','DOUBLE');
insert into TagMap values ('CRX-BLOCK-POLYMER-FLAG','[]DiagnosticToolkit/CRx/CRX_BLOCK_POLYMER_FLAG','DOUBLE');
insert into TagMap values ('FRNT_AVG_C2','[]DiagnosticToolkit/CRx/CRX_HB-9','DOUBLE');
insert into TagMap values ('MIXTEE_IN_USE_0_EAST_1_WEST','[]DiagnosticToolkit/CRx/VCT205X-2','DOUBLE');
insert into TagMap values ('FRNT_LNGTH','[]DiagnosticToolkit/CRx/CRX_HB-8','DOUBLE');
insert into TagMap values ('SDSTRM-C3C2-RATIO','[]DiagnosticToolkit/CRx/VRF503R-2','DOUBLE');
insert into TagMap values ('SD-STRM-C2_FLOW','[]DiagnosticToolkit/CRx/VRF202S','DOUBLE');
--
insert into TagMap values ('VRC032_SP','[]DiagnosticToolkit/CRx/VRC032_SP','DOUBLE');
insert into TagMap values ('VRC253_SP','[]DiagnosticToolkit/CRx/VRC253_SP','DOUBLE');
insert into TagMap values ('VRF214_SP','[]DiagnosticToolkit/CRx/VRF214_SP','DOUBLE');
insert into TagMap values ('VRF224_SP','[]DiagnosticToolkit/CRx/VRF224_SP','DOUBLE');
--
insert into TagMap values ('[the bad-value of C3_CONVERSION]','[]DiagnosticToolkit/CRX/VRG521Z/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of CAT_EFFICIENCY]','[]DiagnosticToolkit/CRX/VRG531Z-1//badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of CAT_PREMIX_TEMP]','[]DiagnosticToolkit/CRX/VCT205X/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of CRX-BLOCK-POLYMER-FLAG]','[]DiagnosticToolkit/CRx/CRX_BLOCK_POLYMER_FLAG/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of FRNT_AVG_C2]','[]DiagnosticToolkit/CRx/CRX_HB-9/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of MIXTEE_IN_USE_0_EAST_1_WEST]','[]DiagnosticToolkit/CRx/VCT205X-2/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of FRNT_LNGTH]','[]DiagnosticToolkit/CRx/CRX_HB-8/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of SDSTRM-C3C2-RATIO]','[]DiagnosticToolkit/CRx/VRF503R-2/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of SD-STRM-C2_FLOW]','[]DiagnosticToolkit/C STR/VRF202S/badValue','BOOLEAN');
--
-- new UDT for values derived from OPC input data (connected to a DataConditioner block)
-- 
insert into TagMap values ('CNTR_AVG_TPR_TIP_HT','[]UnitParameter/CRx/CNTR_AVG_TPR_TIP_HT/value','DOUBLE');
insert into TagMap values ('SS1_TAPER_TIP_HEIGHT','[]UnitParameter/CRx/SS1_TAPER_TIP_HEIGHT/value','DOUBLE');
insert into TagMap values ('SS2_TAPER_TIP_HEIGHT','[]UnitParameter/CRx/SS2_TAPER_TIP_HEIGHT/value','DOUBLE');
insert into TagMap values ('MOONEY_RESET_TIME_FOR_SF-3','[]UnitParameter/CRx/MOONEY_RESET_TIME_FOR_SF_3/value','DOUBLE');
insert into TagMap values ('FRNT_TPR_TIP_HT_DIFF','[]UnitParameter/CRx/FRNT_TPR_TIP_HT_DIFF/value','DOUBLE');
insert into TagMap values ('MAX_CNTR_TPR_TIP_DELTA_FM_AVG','[]UnitParameter/CRx/MAX_CNTR_TPR_TIP_DELTA_FM_AVG/value','DOUBLE');
insert into TagMap values ('FRNT_FEED_DIFF','[]UnitParameter/CRx/FRNT_FEED_DIFF/value','DOUBLE');
--
insert into TagMap values ('[the bad-value of CNTR_AVG_TPR_TIP_HT]','[]UnitParameter/CRx/CNTR_AVG_TPR_TIP_HT/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of SS1_TAPER_TIP_HEIGHT]','[]UnitParameter/CRx/SS1_TAPER_TIP_HEIGHT/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of SS2_TAPER_TIP_HEIGHT]','[]UnitParameter/CRx/SS2_TAPER_TIP_HEIGHT/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of MOONEY_RESET_TIME_FOR_SF-3]','[]UnitParameter/CRx/MOONEY_RESET_TIME_FOR_SF_3/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of FRNT_TPR_TIP_HT_DIFF]','[]UnitParameter/CRx/FRNT_TPR_TIP_HT_DIFF/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of MAX_CNTR_TPR_TIP_DELTA_FM_AVG]','[]UnitParameter/CRx/MAX_CNTR_TPR_TIP_DELTA_FM_AVG/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of FRNT_FEED_DIFF]','[]UnitParameter/CRx/FRNT_FEED_DIFF/badValue','BOOLEAN');
--
--    other info tags -  UDTs and memory
--_
insert into TagMap values ('C9-GRADE-FLAG','[]DiagnosticToolkit/CSTR/C9-Grade-Flag/value','DOUBLE');
insert into TagMap values ('OIL-GRADE-FLAG','[]DiagnosticToolkit/CSTR/Oil-Grade-Flag/value','DOUBLE');
insert into TagMap values ('RLA3-CURRENT-GRADE','[]DiagnosticToolkit/CSTR/RLA3-Current-Grade/value','DOUBLE');
insert into TagMap values ('RX_CONFIGURATION','[]DiagnosticToolkit/CSTR/RX_CONFIGURATION/value','DOUBLE');
insert into TagMap values ('ALKYL_FLOW','[]DiagnosticToolkit/CSTR/VCF262S/value','DOUBLE');
insert into TagMap values ('C2_FLOW','[]DiagnosticToolkit/CSTR/VRF002S/value','DOUBLE');

insert into TagMap values ('MLR-GRADE-FLAG','[]Recipe/Local/MLR-GRADE-FLAG','DOUBLE');
insert into TagMap values ('DML-SQC-FLAG','[]Recipe/Local/DML-SQC-FLAG','DOUBLE');
--
insert into TagMap values ('[the bad-value of C9-GRADE-FLAG]','[]DiagnosticToolkit/CSTR/C9-Grade-Flag/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of OIL-GRADE-FLAG]','[]DiagnosticToolkit/CSTR/OIL_GRADE_FLAG/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of RLA3-CURRENT-GRADE]','[]DiagnosticToolkit/CSTR/RLA3_CURRENT_GRADE/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of RX_CONFIGURATION]','[]DiagnosticToolkit/CSTR/RX_CONFIGURATION/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of RX_CONFIGURATION ]','[]DiagnosticToolkit/CSTR/RX_CONFIGURATION/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of ALKYL_FLOW]','[]DiagnosticToolkit/CSTR/VCF262S/badValue','BOOLEAN');
insert into TagMap values ('[the bad-value of C2_FLOW]','[]DiagnosticToolkit/CSTR/VRF002S/badValue','BOOLEAN');
--
-- crx_parameter instances built as memory tags since badValue is not required
--
insert into TagMap values ('C3CONV_LOW_LIMIT','[]UnitParameter/CRx/C3CONV_LOW_LIMIT','DOUBLE');
insert into TagMap values ('CATEFF_HIGH_LIMIT','[]UnitParameter/CRx/CATEFF_HIGH_LIMIT','DOUBLE');
insert into TagMap values ('CNTR_AVG_TPR_TIP_HT_MAX_DEADBAND','[]UnitParameter/CRx/CNTR_AVG_TPR_TIP_HT_MAX_DEADBAND','DOUBLE');
insert into TagMap values ('CNTR_AVG_TPR_TIP_HT_TARGET','[]UnitParameter/CRx/CNTR_AVG_TPR_TIP_HT_TARGET','DOUBLE');
insert into TagMap values ('FRNT_AVG_C2_HIGH_LIMIT','[]UnitParameter/CRx/FRNT_AVG_C2_HIGH_LIMIT','DOUBLE');
insert into TagMap values ('FRNT_LNGTH_HIGH_LIMIT','[]UnitParameter/CRx/FRNT_LNGTH_HIGH_LIMIT','DOUBLE');
insert into TagMap values ('FRNT_LNGTH_LOW_LIMIT','[]UnitParameter/CRx/FRNT_LNGTH_LOW_LIMIT','DOUBLE');
insert into TagMap values ('FRNT_LNGTH_TARGET','[]UnitParameter/CRx/FRNT_LNGTH_TARGET','DOUBLE');
insert into TagMap values ('FRNT_SDSTRM_MAX_DIFF','[]UnitParameter/CRx/FRNT_SDSTRM_MAX_DIFF','DOUBLE');
insert into TagMap values ('FRNT_TPR_TIP_HT_HIGH_LIMIT','[]UnitParameter/CRx/FRNT_TPR_TIP_HT_HIGH_LIMIT','DOUBLE');
insert into TagMap values ('FRNT_TPR_TIP_HT_MAX_DIFF','[]UnitParameter/CRx/FRNT_TPR_TIP_HT_MAX_DIFF','DOUBLE');
insert into TagMap values ('GAIN_CAT_C3','[]UnitParameter/CRx/GAIN_CAT_C3','DOUBLE');
insert into TagMap values ('GAIN_CAT_TEMP','[]UnitParameter/CRx/GAIN_CAT_TEMP','DOUBLE');
insert into TagMap values ('GAIN_LNGTH_TEMP','[]UnitParameter/CRx/GAIN_LENGTH_TEMP','DOUBLE');
insert into TagMap values ('PREMIX_TEMP_HIGH_LIMIT','[]UnitParameter/CRx/PREMIX_TEMP_HIGH_LIMIT','DOUBLE');
insert into TagMap values ('SDSTRM_C3-TO-C2_RATIO_HIGH_LIMIT','[]UnitParameter/CRx/SDSTRM_C3-TO-C2_RATIO_HIGH_LIMIT','DOUBLE');
insert into TagMap values ('SDSTRM_C3-TO-C2_RATIO_LOW_LIMIT','[]UnitParameter/CRx/SDSTRM_C3-TO-C2_RATIO_LOW_LIMIT','DOUBLE');
--
insert into TagMap values ('PREMIX_LINE_DATA_OK-GDA','[]UnitParameter/CRx/PREMIX_LINE_DATA_OK-GDA ','STRING');
insert into TagMap values ('PREMIX_LINE_FRESH-GDA-PARAMETER','[]UnitParameter/CRx/PREMIX_LINE_FRESH-GDA-PARAMETER','STRING');
--
insert into TagMap values ('PROD-CA-SQC-FLAG','[]Recipe/Local/PROD-CA-SQC-FLAG','DOUBLE');
insert into TagMap values ('POLYSPLIT-SQC-FLAG','[]Recipe/Local/POLYSPLIT-SQC-FLAG','DOUBLE');
-- 
-- simple memory tags 
--
insert into TagMap values ('CATIN-GDA-COUNTER','[]Site/CSTR/CATIN-GDA-COUNTER','INTEGER');
insert into TagMap values ('CATIN-GDA-PROD-ML-ACTIVE','[]Site/CSTR/CATIN-GDA-PROD-ML-ACTIVE','BOOLEAN');
insert into TagMap values ('FREEZER-SAMPLE-SWITCH','[]Site/VFU/FREEZER-SAMPLE-SWITCH','BOOLEAN');
insert into TagMap values ('PROD-ML-VFU-OK','[]Site/VFU/Prod_ML-VFU-OK','BOOLEAN');
insert into TagMap values ('SERIES_FLYING_SWITCH-GDA-INPUT','[]Site/POLY_FLYING_SWITCH_SYSTEM/SERIES_FLYING_SWITCH-GDA-INPUT','BOOLEAN');
insert into TagMap values ('SINGLE_FLYING_SWITCH-GDA-INPUT','[]Site/POLY_FLYING_SWITCH_SYSTEM/SINGLE_FLYING_SWITCH-GDA-INPUT','BOOLEAN');
insert into TagMap values ('SERIES_RATE_CHANGE-GDA-INPUT','[]Site/POLY_RATE_CHANGE_SYSTEM/SERIES_RATE_CHANGE-GDA-INPUT','BOOLEAN');
insert into TagMap values ('SINGLE_RATE_CHANGE-GDA-INPUT','[]Site/POLY_RATE_CHANGE_SYSTEM/SINGLE_RATE_CHANGE-GDA-INPUT','BOOLEAN');
insert into TagMap values ('SPLIT_FLYING_SWITCH-GDA-INPUT','[]Site/POLY_FLYING_SWITCH_SYSTEM/SPLIT_FLYING_SWITCH-GDA-INPU','BOOLEAN');
insert into TagMap values ('EM-GDA-LOGICAL-VARIABLE-XXX-2','[]Site/VFU/VFU_FREEZER_SWITCH_REMINDER','BOOLEAN');
insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-429','[]Site/CRX/ML-GRAVITY-TIME-STATE','BOOLEAN');
insert into TagMap values ('C_FLYING_SWITCH-GDA-INPUT','[]Site/POLY_RATE_CHANGE_SYSTEM/C_FLYING_SWITCH-GDA-INPUT','BOOLEAN');
insert into TagMap values ('POLY_RATE_CHANGE-GDA-INPUT','[]Site/POLY_FLYING_SWITCH_SYSTEM/POLY_RATE_CHANGE-GDA-INPUT','BOOLEAN');
insert into TagMap values ('SPLIT_RATE_CHANGE-GDA-INPUT','[]Site/POLY_RATE_CHANGE_SYSTEM/SPLIT_RATE_CHANGE-GDA-INPUT','BOOLEAN');
--
insert into TagMap values ('VFU-BALER-TEMP-CHK','[]Recipe/Local/VFU-BALER-TEMP-CHK','DOUBLE');
insert into TagMap values ('VFU-FTNIR-GRADE','[]Recipe/Local/VFU-FTNIR-GRADE','DOUBLE');
--
insert into TagMap values ('[the unix-sample-time of mooney-lab-data]','[]LabData/RLA3/MOONEY/sampleTime','DOUBLE');
insert into TagMap values ('C9-IN-CRUMB','[]LabData/RLA3/C9-IN-CRUMB/value','DOUBLE');
--
-- datum for these two references not available at this time
--
insert into TagMap values ('[the time-of-most-recent-recommendation-implementation of frnt_short_use_temp-gda]','[]Tags/FRNT_SHORT_USE_TEMP_GDA/implementationTime','INTEGER');
insert into TagMap values ('[the time-of-most-recent-grade-change of rla3-run-hours]','[]Tags/RLA3_RUN_HOURS/gradeChangeTime','INTEGER');
--
-- These tags do not exist in G2, but are necessary in the toolkit to replicate functionality of G2 parameters, variables and connection posts
--
insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-4651','[]Site/Parameters/SymbolicVariable4651','STRING');

--
-- The following have been found in calculation procedures and have yet to be mapped to "real" tag paths
-- Vistalon
insert into TagMap values ('ca_filtered_value','[]LabData/RLA3/CA-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('cast-disposition-entry','[]Site/CAST-DISPOSITION-ENTRY','DOUBLE');
insert into TagMap values ('cntr_avg_tpr_tip_ht','[]Site/Tags/CNTR-AVG-TPr_TIP/height','DOUBLE');
insert into TagMap values ('cntr_avg_tpr_tip_ht_target','[]Site/Tags/CNTR-AVG-TPr_TIP/targetHeight','DOUBLE');
insert into TagMap values ('cntr_depth_list-gda','[]Site/Tags/CNTR-DEPTH-LIST','STRING');     -- there is no list datatype
insert into TagMap values ('cntr_feed_list-gda','[]Site/Tags/CNTR-FEED-LIST','STRING');     -- there is no list datatype
insert into TagMap values ('cntr_tpr_ht_list-gda','[]Site/Tags/CNTR-TPR-HT-LIST','STRING');     -- there is no list datatype
insert into TagMap values ('d20-service-status','[]Site/SERVICE-STATUS/D20','DOUBLE');
insert into TagMap values ('d20a-service-status','[]Site/SERVICE-STATUS/D20a','DOUBLE');
insert into TagMap values ('d20b-service-status','[]Site/SERVICE-STATUS/D20b','DOUBLE');
insert into TagMap values ('dc2_filtered_value','[]LabData/RLA3/DC2-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('dml_filtered_value','[]LabData/RLA3/DML-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('denb_filtered_value','[]LabData/RLA3/DENB-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('enb_filtered_value','[]LabData/RLA3/ENB-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('ethylene_filtered_value','[]LabData/RLA3/ETHYLENE-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('frnt_avg_c2','[]Site/Tags/FRNT-AVG-C2','DOUBLE');
insert into TagMap values ('frnt_avg_c2_target','[]Site/Tags/FRNT-AVG-C2/target','DOUBLE');
insert into TagMap values ('gain_avg-c2_main-c3','[]Site/Tags/AVG-C2-MAIN-C3/gain','DOUBLE');
insert into TagMap values ('gain_frnt-c2_main-fd','[]Site/Tags/FRNT-C2-MAIN/gain','DOUBLE');
insert into TagMap values ('gain_frnt-c2_sdstrm-fd','[]Site/Tags/FRNT-C2-SDSTRM/gain','DOUBLE');
insert into TagMap values ('gain_cntr-tpr-tip_main-fd','[]Site/Tags/CNTR-TPR-TIM-MAIN/gain','DOUBLE');
insert into TagMap values ('gain_cntr-tpr-tip_ss-c3-to-c2','[]Site/Tags/CNTR-TPR-TIP-SS-C3-C2/gain','DOUBLE');
insert into TagMap values ('gain_ml_temp','[]Site/Tags/ML-TEMP/gain','DOUBLE');
insert into TagMap values ('hours-to-avg-prod-ml-gda','[]Site/PROD-ML-HOURS-TO-AVERAGE','DOUBLE');
insert into TagMap values ('hours-to-offset-rx-ml-gda','[]Site/RX-ML-HOURS-TO-OFFSET','DOUBLE');
insert into TagMap values ('main-c2','[]Site/Tags/MAIN-C2','DOUBLE');
insert into TagMap values ('main-c3','[]Site/Tags/MAIN-C3','DOUBLE');
insert into TagMap values ('mlr_filtered_value','[]LabData/RLA3/MLR-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('mooney-bias-gda','[]LabData/RLA3/MOONEY-LAB-DATA/bias','DOUBLE');
insert into TagMap values ('mooney_sqc-gda','[]LabData/RLA3/MOONEY-LAB-DATA/sqc','DOUBLE');
insert into TagMap values ('mooney_filtered_value','[]LabData/RLA3/MOONEY-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('oil_filtered_value','[]LabData/RLA3/OIL-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('ok_to_fetch_sc-4_data-gda','[]Site/RLA3/OK-TO-FETCH-SC4-DATA','BOOLEAN');
insert into TagMap values ('polysplit_filtered_value','[]LabData/RLA3/POLYSPLIT-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('prod-mooney-info','[]Site/RLA3/PROD-MOONEY-INFO','STRING');    -- A workspace
insert into TagMap values ('rla3-run-hours','[]Site/RLA3-RUN-HOURS','DOUBLE');
insert into TagMap values ('s88-rc-main-c2-rate-ramper','[]Site/S88-RC-MAIN/C2-RATE-RAMPER','DOUBLE');
insert into TagMap values ('s88-rc-r2-c2-rate-ramper','[]Site/S88-RC-MAIN/C2-R2-RAMPER','DOUBLE');
insert into TagMap values ('sdstrm-1','[]Site/Tags/SDSTRM-1','DOUBLE');
insert into TagMap values ('sdstrm-2','[]Site/Tags/SDSTRM-2','DOUBLE');
insert into TagMap values ('sdstrm-3','[]Site/Tags/SDSTRM-3','DOUBLE');
insert into TagMap values ('sdstrm-4','[]Site/Tags/SDSTRM-4','DOUBLE');
insert into TagMap values ('sdstrm-5','[]Site/Tags/SDSTRM-5','DOUBLE');
insert into TagMap values ('sdstrm-6','[]Site/Tags/SDSTRM-6','DOUBLE');
insert into TagMap values ('sdstrm-c2','[]Site/Tags/SDSTRM-C2','DOUBLE');
insert into TagMap values ('split_feed_grade','[]Site/CSTR/SPLIT_FEED_GRADE','BOOLEAN');
insert into TagMap values ('stab_filtered_value','[]LabData/RLA3/STAB-LAB-DATA/filteredValue','DOUBLE');
insert into TagMap values ('USE-NEW-SC2-CALC','[]Site/RLA3/USE-NEW-SC2-CALC','BOOLEAN');
insert into TagMap values ('[the depth-at-outlet of crx-zone-1]','[]Site/CSTR/CRX-ZONE-1/outletDepth','DOUBLE');
insert into TagMap values ('[the depth-at-outlet of crx-zone-2]','[]Site/CSTR/CRX-ZONE-2/outletDepth','DOUBLE');
insert into TagMap values ('[the height-at-inlet of crx-zone-2]','[]Site/CSTR/CRX-ZONE-2/inletHeight','DOUBLE');
insert into TagMap values ('[the height-at-inlet of crx-zone-3]','[]Site/CSTR/CRX-ZONE-3/inletHeight','DOUBLE');
insert into TagMap values ('[the sdstrm-monomer-flow of crx-zone-2]','[]Site/CSTR/CRX-ZONE-2/sdstrmMononerFlow','DOUBLE');
insert into TagMap values ('[the sdstrm-monomer-flow of crx-zone-3]','[]Site/CSTR/CRX-ZONE-3/sdstrmMononerFlow','DOUBLE');
insert into TagMap values ('[the c2-c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/C2C3','DOUBLE');
insert into TagMap values ('[the c2-c9-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/C2C9','DOUBLE');
insert into TagMap values ('[the c2-h2-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/C2H2','DOUBLE');
insert into TagMap values ('[the c2-r2c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/C2R2C3','DOUBLE');
insert into TagMap values ('[the c2-cat-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/C2Cat','DOUBLE');
insert into TagMap values ('[the dc2-r1c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/Dc2R1C3','DOUBLE');
insert into TagMap values ('[the dc2-r2c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/Dc2R2C3','DOUBLE');
insert into TagMap values ('[the dml-h2-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DmlH2','DOUBLE');
insert into TagMap values ('[the dml-nh3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DmlNh3','DOUBLE');
insert into TagMap values ('[the dml-r1c2-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DmlR1C2','DOUBLE');
insert into TagMap values ('[the dml-r1c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DmlR1C3','DOUBLE');
insert into TagMap values ('[the dml-r2c2-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DmlR2C2','DOUBLE');
insert into TagMap values ('[the dml-r1enb-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DmlR1Enb','DOUBLE');
insert into TagMap values ('[the dml-r2enb-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DmlR2Enb','DOUBLE');
insert into TagMap values ('[the denb-r1enb-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DenbR1Enb','DOUBLE');
insert into TagMap values ('[the denb-r2enb-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/DenbR2Enb','DOUBLE');
insert into TagMap values ('[the enb-cat-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/EnbCat','DOUBLE');
insert into TagMap values ('[the enb-c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/EnbC3','DOUBLE');
insert into TagMap values ('[the enb-c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/EnbC3','DOUBLE');
insert into TagMap values ('[the enb-enb-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/EnbEnb','DOUBLE');
insert into TagMap values ('[the enb-h2-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/EnbH2','DOUBLE');
insert into TagMap values ('[the enb-c3-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/EnbC3','DOUBLE');
insert into TagMap values ('[the ml-cat-gain of current-running-gains]','[]Site/CSTR/CURRENT-GAINS/MlCat','DOUBLE');
insert into TagMap values ('[the ml-h2-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlH2','DOUBLE');
insert into TagMap values ('[the ml-c3-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlC3','DOUBLE');
insert into TagMap values ('[the mlr-al-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlrAl','DOUBLE');
insert into TagMap values ('[the mlr-cat-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlrCat','DOUBLE');
insert into TagMap values ('[the mlr-c3-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlrC3','DOUBLE');
insert into TagMap values ('[the mlr-h2-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlrH2','DOUBLE');
insert into TagMap values ('[the mlr-nh3-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlrNh3','DOUBLE');
insert into TagMap values ('[the mlr-temp-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/MlrTemp','DOUBLE');
insert into TagMap values ('[the ps-r1c2-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/PsR1C2','DOUBLE');
insert into TagMap values ('[the ps-r2c2-gain of current-running-gains]', '[]Site/CSTR/CURRENT-GAINS/PsR2C2','DOUBLE');
insert into TagMap values ('[the lower_limit of MOONEY-LAB-DATA]','[]LabData/RLA3/MOONEY-LAB-DATA/lowerLimit','DOUBLE');
insert into TagMap values ('[the upper_limit of MOONEY-LAB-DATA]','[]LabData/RLA3/MOONEY-LAB-DATA/upperLimit','DOUBLE');
insert into TagMap values ('[the lower_limit of STAB-LAB-DATA]','[]LabData/RLA3/STAB-LAB-DATA/lowerLimit','DOUBLE');
insert into TagMap values ('VRC023_SP','[]Site/Tags/VRC023/sp','DOUBLE');
insert into TagMap values ('VRC062_SP','[]Site/Tags/VRC062/sp','DOUBLE');
insert into TagMap values ('VRC262_SP','[]Site/Tags/VRC262/sp','DOUBLE');
