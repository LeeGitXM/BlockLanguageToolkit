-- Map G2 GSI names to Ignition Tags
-- Columns are GSI name, tag path
-- NOTE: value is not a legitimate tag name
-- NOTE: Tag names must be unique, folder placement is immaterial.
--
-- The following tags represent Source/Sink connections
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
insert into TagMap values ('AB-BALER-VOL-FTNIR-DATA','[]LabData/VFU/AB-BALER-VOL-FTNIR/value','DOUBLE');  
insert into TagMap values ('CD-BALER-VOL-FTNIR-DATA','[]LabData/VFU/CD-BALER-VOL-FTNIR/value','DOUBLE');
insert into TagMap values ('E-BALER-VOL-FTNIR-DATA','[]LabData/VFU/E-BALER-VOL-FTNIR/value','DOUBLE');
--
-- UDT paths for SQC problems
--
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
insert into TagMap values ('C3_CONVERSION','[]DiagnosticToolkit/CRX/VRG521Z/value','DOUBLE');
insert into TagMap values ('CAT_EFFICIENCY','[]DiagnosticToolkit/CRX/VRG531Z-1/value','DOUBLE');
insert into TagMap values ('CAT_PREMIX_TEMP','[]DiagnosticToolkit/CRX/VCT205X/value','DOUBLE');
insert into TagMap values ('CRX-BLOCK-POLYMER-FLAG','[]DiagnosticToolkit/CRx/CRX_BLOCK_POLYMER_FLAG/value','DOUBLE');
insert into TagMap values ('FRNT_AVG_C2','[]DiagnosticToolkit/CRx/CRX_HB-9/value','DOUBLE');
insert into TagMap values ('MIXTEE_IN_USE_0_EAST_1_WEST','[]DiagnosticToolkit/CRx/VCT205X-2/value','DOUBLE');
insert into TagMap values ('FRNT_LNGTH','[]DiagnosticToolkit/CRx/CRX_HB-8/value','DOUBLE');
insert into TagMap values ('SDSTRM-C3C2-RATIO','[]DiagnosticToolkit/CRx/VRF503R-2/value','DOUBLE');
insert into TagMap values ('SD-STRM-C2_FLOW','[]DiagnosticToolkit/C STR/VRF202S/value','DOUBLE');
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
-- these tags do not exist in G2
--
-- insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-291','[]Site/Parameters/SymbolicVariable291','STRING');
-- insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-292','[]Site/Parameters/SymbolicVariable292','STRING');
-- insert into TagMap values ('EM-GDA-LOGICAL-VARIABLE-XXX-293','[]Site/Parameters/LogicalVariable293','STRING');
-- insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-490','[]Connections/ConnectionPost490','STRING');
-- insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-491','[]Connections/ConnectionPost491','STRING');
-- insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-492','[]Connections/ConnectionPost492','STRING');
-- insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-568','[]Connections/ConnectionPost568-a','STRING');
-- insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-569','[]Connections/ConnectionPost569','STRING');
-- insert into TagMap values ('GDL-INFERENCE-PATH-CONNECTION-POST-XXX-488','[]Connections/ConnectionPost488','STRING');
-- insert into TagMap values ('GDL-INFERENCE-PATH-CONNECTION-POST-XXX-489','[]Connections/ConnectionPost489','STRING');
-- insert into TagMap values ('GDL-INFERENCE-PATH-CONNECTION-POST-XXX-568','[]Connections/ConnectionPost568-b','STRING'); 

--
-- don't know what this does
--
insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-4651','[]Site/Parameters/SymbolicVariable4651','STRING');
--
