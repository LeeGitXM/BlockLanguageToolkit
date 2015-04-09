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
-- we should plan to give these "meaningful" names after examining their use in G2
--
insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-490','[]Connections/ConnectionPost490','STRING');
insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-491','[]Connections/ConnectionPost491','STRING');
insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-492','[]Connections/ConnectionPost492','STRING');
insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-568','[]Connections/ConnectionPost568-a','STRING');
insert into TagMap values ('GDL-DATA-PATH-CONNECTION-POST-XXX-569','[]Connections/ConnectionPost569','STRING');
insert into TagMap values ('GDL-INFERENCE-PATH-CONNECTION-POST-XXX-488','[]Connections/ConnectionPost488','STRING');
insert into TagMap values ('GDL-INFERENCE-PATH-CONNECTION-POST-XXX-489','[]Connections/ConnectionPost489','STRING');
insert into TagMap values ('GDL-INFERENCE-PATH-CONNECTION-POST-XXX-568','[]Connections/ConnectionPost568-b','STRING');
insert into TagMap values ('MY-CONNECTION-POST','[]Connections/MyConnectionPost','STRING');
--
-- Lab data path
-----   at this point I am imagining data from the lab tag using the rawValue attribute
-----   I recognize that this will change down the road reasoning will want to operate on validated data
--
insert into TagMap values ('MOONEY-LAB-DATA','[]LabData/ML-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('MLR-LAB-DATA','[]LabData/MLR-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('C2-LAB-DATA','[]LabData/C2-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('C9-LAB-DATA','[]LabData/C9-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('DML-LAB-DATA','[]LabData/DML-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('DC2-LAB-DATA','[]LabData/DC2-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('DC9-LAB-DATA','[]LabData/DC9-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('CA-LAB-DATA','[]LabData/CA-LAB-DATA','DOUBLE');
insert into TagMap values ('STAB-LAB-DATA','[]LabData/STAB-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('OIL-LAB-DATA','[]LabData/OIL-LAB-DATA/rawValue','DOUBLE');
insert into TagMap values ('PROD-ML-LAB-DATA','[]LabData/PROD-ML-LAB-DATA/highLimit','DOUBLE');
insert into TagMap values ('C2-LAB-DATA-FOR-R1-NLC','[]LabData/C2-R1-NLC','DOUBLE');
--
insert into TagMap values ('A-BALER-TEMP-LAB-DATA','[]LabData/A-BALER-TEMP/rawValue','DOUBLE');
insert into TagMap values ('B-BALER-TEMP-LAB-DATA','[]LabData/B-BALER-TEMP/rawValue','DOUBLE');
insert into TagMap values ('C-BALER-TEMP-LAB-DATA','[]LabData/C-BALER-TEMP/rawValue','DOUBLE');
insert into TagMap values ('D-BALER-TEMP-LAB-DATA','[]LabData/D-BALER-TEMP/rawValue','DOUBLE');
insert into TagMap values ('E-BALER-TEMP-LAB-DATA','[]LabData/E-BALER-TEMP/rawValue','DOUBLE');
insert into TagMap values ('AB-BALER-TEMP-LAB-DATA','[]LabData/AB-BALER-TEMP/rawValue','DOUBLE');
insert into TagMap values ('CD-BALER-TEMP-LAB-DATA','[]LabData/CD-BALER-TEMP/rawValue','DOUBLE');
--
insert into TagMap values ('AB-BALER-VOL-LAB-DATA','[]LabData/AB-BALER-VOL/rawValue','DOUBLE');
insert into TagMap values ('CD-BALER-VOL-LAB-DATA','[]LabData/CD-BALER-VOL/rawValue','DOUBLE');
insert into TagMap values ('E-BALER-VOL-LAB-DATA','[]LabData/E-BALER-VOL/rawValue','DOUBLE');
--
insert into TagMap values ('AB-BALER-VOL-FTNIR-DATA','[]LabData/AB-BALER-VOL-FTNIR/rawValue','DOUBLE');  
insert into TagMap values ('CD-BALER-VOL-FTNIR-DATA','[]LabData/CD-BALER-VOL-FTNIR/rawValue','DOUBLE');
insert into TagMap values ('E-BALER-VOL-FTNIR-DATA','[]LabData/E-BALER-VOL-FTNIR/rawValue','DOUBLE');
--
-- UDT paths for SQC problems
--
insert into TagMap values ('[the target of MOONEY-LAB-DATA]','[]LabData/ML-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of MLR-LAB-DATA]','[]LabData/MLR-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of C2-LAB-DATA]','[]LabData/C2-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of c9-lab-data]','[]LabData/C9-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of DML-LAB-DATA]','[]LabData/DML-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of DC2-LAB-DATA]','[]LabData/DC2-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of Dc9-lab-data]','[]LabData/DC9-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of CA-LAB-DATA]','[]LabData/CA-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of STAB-LAB-DATA]','[]LabData/STAB-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of OIL-LAB-DATA]','[]LabData/OIL-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of PROD-ML-LAB-DATA]','[]LabData/PROD-ML-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of POLYSPLIT-DATA]','[]LabData/POLYSPLIT-DATA-SQC/target','DOUBLE');
--
insert into TagMap values ('[the target of A-BALER-TEMP-LAB-DATA]','[]LabData/A-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of B-BALER-TEMP-LAB-DATA]','[]LabData/B-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of C-BALER-TEMP-LAB-DATA]','[]LabData/C-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of D-BALER-TEMP-LAB-DATA]','[]LabData/D-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of E-BALER-TEMP-LAB-DATA]','[]LabData/E-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of AB-BALER-TEMP-LAB-DATA]','[]LabData/AB-BALER-TEMP-LAB-DATA-SQC/target','DOUBLE');
----- no CD temp target reference?
--
insert into TagMap values ('[the target of AB-BALER-VOL-lab-DATA]','[]LabData/AB-BALER-VOL-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of CD-BALER-VOL-lab-DATA]','[]LabData/CD-BALER-VOL-LAB-DATA-SQC/target','DOUBLE');
insert into TagMap values ('[the target of E-BALER-VOL-lab-DATA]','[]LabData/E-BALER-VOL-LAB-DATA-SQC/target','DOUBLE');
--
insert into TagMap values ('[the target of AB-BALER-VOL-ftnir-DATA]','[]LabData/AB-BALER-VOL-FTNIR-SQC/target','DOUBLE');
insert into TagMap values ('[the target of CD-BALER-VOL-ftnir-DATA]','[]LabData/CD-BALER-VOL-FTNIR-SQC/target','DOUBLE');
insert into TagMap values ('[the target of E-BALER-VOL-ftnir-DATA]','[]Tags/E-BALER-VOL-FTNIR-SQC/target','DOUBLE');
--
insert into TagMap values ('[the standard-deviation of MOONEY-LAB-DATA]','[]LabData/ML-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of MLR-LAB-DATA]','[]LabData/MLR-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of C2-LAB-DATA]','[]LabData/C2-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of c9-lab-data]','[]LabData/C9-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of DML-LAB-DATA]','[]LabData/DML-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of DC2-LAB-DATA]','[]LabData/DC2-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of Dc9-lab-data]','[]LabData/DC9-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of CA-LAB-DATA]','[]LabData/CA-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of STAB-LAB-DATA]','[]LabData/STAB-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of OIL-LAB-DATA]','[]LabData/OIL-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of PROD-ML-LAB-DATA]','[]LabData/PROD-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of POLYSPLIT-DATA]','[]LabData/POLYSPLIT-DATA-SQC/standardDeviation','DOUBLE');
--
insert into TagMap values ('[the standard-deviation of A-BALER-TEMP-LAB-DATA]','[]LabData/A-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of B-BALER-TEMP-LAB-DATA]','[]LabData/B-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of C-BALER-TEMP-LAB-DATA]','[]LabData/C-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of D-BALER-TEMP-LAB-DATA]','[]LabData/D-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of E-BALER-TEMP-LAB-DATA]','[]LabData/E-BALER-TEMP-LAB-DATA-SQC/standardDeviation','DOUBLE');
----- no reference to ab and cd baler temp stddev?
--
insert into TagMap values ('[the standard-deviation of AB-BALER-VOL-lab-DATA]','[]LabData/AB-BALER-VOL-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of CD-BALER-VOL-lab-DATA]','[]LabData/CD-BALER-VOL-LAB-DATA-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of E-BALER-VOL-lab-DATA]','[]LabData/E-BALER-VOL-LAB-DATA-SQC/standardDeviation','DOUBLE');
--
insert into TagMap values ('[the standard-deviation of AB-BALER-VOL-ftnir-DATA]','[]LabData/AB-BALER-VOL-FTNIR-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of CD-BALER-VOL-ftnir-DATA]','[]LabData/CD-BALER-VOL-FTNIR-SQC/standardDeviation','DOUBLE');
insert into TagMap values ('[the standard-deviation of E-BALER-VOL-ftnir-DATA]','[]LabData/E-BALER-VOL-FTNIR-SQC/standardDeviation','DOUBLE');
--
-- assume that the UDT used here has both a value and a badValue attribute 
-----    for inputs from DCS, an OPC Tag Bad Flag may be used
-----    for inputs from the Recipe subsystm, perhaps a recipeWrite with attributes value and badValue (and others)
--
--    C-Rx info tags 
--
insert into TagMap values ('C3_CONVERSION','[]site/C-Rx/C3_CONVERSION/rawValue','DOUBLE');
insert into TagMap values ('CAT_EFFICIENCY','[]site/C-Rx/CAT_EFFICIENCY/rawValue','DOUBLE');
insert into TagMap values ('CAT_PREMIX_TEMP','[]site/C-Rx/CAT_PREMIX_TEMP/rawValue','DOUBLE');
insert into TagMap values ('CNTR_AVG_TPR_TIP_HT','[]site/C-Rx/CNTR_AVG_TPR_TIP_HT/rawValue','DOUBLE');
insert into TagMap values ('CRX-BLOCK-POLYMER-FLAG','[]site/C-Rx/CRX_BLOCK_POLYMER_FLAG/rawValue','DOUBLE');
insert into TagMap values ('FRNT_AVG_C2','[]site/C-Rx/FRNT_AVG_C2/rawValue','DOUBLE');
insert into TagMap values ('MIXTEE_IN_USE_0_EAST_1_WEST','[]site/C-Rx/MIXTEE_IN_USE_0_EAST_1_WEST/rawValue','DOUBLE');
insert into TagMap values ('FRNT_LNGTH','[]site/C-Rx/FRNT-LNGTH/rawValue','DOUBLE');
insert into TagMap values ('SS1_TAPER_TIP_HEIGHT','[]site/C-Rx/SS1_TAPER_TIP_HEIGHT/rawValue','DOUBLE');
insert into TagMap values ('SS2_TAPER_TIP_HEIGHT','[]site/C-Rx/SS2_TAPER_TIP_HEIGHT/rawValue','DOUBLE');
insert into TagMap values ('SDSTRM-C3C2-RATIO','[]site/C-Rx/SDSTRM_C3C2_RATIO/rawValue','DOUBLE');
insert into TagMap values ('MOONEY_RESET_TIME_FOR_SF-3','[]site/C-Rx/MOONEY_RESET_TIME_FOR_SF_3/rawValue','DOUBLE');
insert into TagMap values ('FRNT_TPR_TIP_HT_DIFF','[]site/C-Rx/FRNT_TPR_TIP_HT/diff','DOUBLE');
insert into TagMap values ('MAX_CNTR_TPR_TIP_DELTA_FM_AVG','[]site/C-Rx/MAX_CNTR_TPR_TIP_DELTA_FM_AVG/rawValue','DOUBLE');
insert into TagMap values ('FRNT_FEED_DIFF','[]site/C-Rx/FRNT_FEED_DIFF/rawValue','DOUBLE');
insert into TagMap values ('SD-STRM-C2_FLOW','[]site/C-Rx/SDSTRM_C2_FLOW/rawValue','DOUBLE');
--
insert into TagMap values ('[the bad-value of C3_CONVERSION]','[]site/C-Rx/C3_CONVERSION/badValue','STRING');
insert into TagMap values ('[the bad-value of CAT_EFFICIENCY]','[]site/C-Rx/CAT_EFFICIENCY/badValue','STRING');
insert into TagMap values ('[the bad-value of CAT_PREMIX_TEMP]','[]site/C-Rx/CAT_PREMIX_TEMP/badValue','STRING');
insert into TagMap values ('[the bad-value of CNTR_AVG_TPR_TIP_HT]','[]site/C-Rx/CNTR_AVG_TPR_TIP_HT/badValue','STRING');
insert into TagMap values ('[the bad-value of CRX-BLOCK-POLYMER-FLAG]','[]site/C-Rx/CRX_BLOCK_POLYMER_FLAG/badValue','STRING');
insert into TagMap values ('[the bad-value of FRNT_AVG_C2]','[]site/C-Rx/FRNT_AVG_C2/badValue','STRING');
insert into TagMap values ('[the bad-value of MIXTEE_IN_USE_0_EAST_1_WEST]','[]site/C-Rx/MIXTEE_IN_USE_0_EAST_1_WEST/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of FRNT_LNGTH]','[]site/C-Rx/FrontLength/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of SS1_TAPER_TIP_HEIGHT]','[]site/C-Rx/SS1_TAPER_TIP_HEIGHT/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of SS2_TAPER_TIP_HEIGHT]','[]site/C-Rx/SS2_TAPER_TIP_HEIGHT/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of SDSTRM-C3C2-RATIO]','[]site/C-Rx/SDSTRM_C3C2_RATIO/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of MOONEY_RESET_TIME_FOR_SF-3]','[]site/C-Rx/MOONEY_RESET_TIME_FOR_SF_3/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of FRNT_TPR_TIP_HT_DIFF]','[]site/C-Rx/FRNT_TPR_TIP_HT_DIFF/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of MAX_CNTR_TPR_TIP_DELTA_FM_AVG]','[]site/C-Rx/MAX_CNTR_TPR_TIP_DELTA_FM_AVG/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of FRNT_FEED_DIFF]','[]site/C-Rx/FRNT_FEED_DIFF/badValue','STRING');
insert into TagMap values ('[the bad-value of SD-STRM-C2_FLOW]','[]site/C-Rx/SDSTRM_C2_FLOW/badValue','DOUBLE');
--
--    other info tags
--
insert into TagMap values ('C9-GRADE-FLAG','[]Site/SINGLE-PERMISSIVES/C9_GRADE_FLAG/rawValue','DOUBLE');
insert into TagMap values ('OIL-GRADE-FLAG','[]Site/SINGLE-PERMISSIVES/OIL-GRADE-FLAG/rawValue','DOUBLE');
insert into TagMap values ('RLA3-CURRENT-GRADE','[]Site/SINGLE-PERMISSIVES/RLA3-CURRENT-GRADE/rawValue','DOUBLE');
insert into TagMap values ('RX_CONFIGURATION','[]Site/SINGLE-PERMISSIVES/RX_CONFIGURATION/rawValue','DOUBLE');
insert into TagMap values ('ALKYL_FLOW','[]LabData/ALKYL_FLOW/rawValue','DOUBLE');
insert into TagMap values ('C2_FLOW','[]LabData/C2_FLOW/rawValue','DOUBLE');
insert into TagMap values ('MLR-GRADE-FLAG','[]Site/SINGLE-PERMISSIVES/MLR-GRADE-FLAG/rawValue','DOUBLE');
insert into TagMap values ('DML-SQC-FLAG','[]Site/SINGLE-PERMISSIVES/DML_SQC_FLAG/rawValue','DOUBLE');
--
insert into TagMap values ('[the bad-value of C9-GRADE-FLAG]','[]Site/SINGLE-PERMISSIVES/C9_GRADE_FLAG/badValue','STRING');
insert into TagMap values ('[the bad-value of OIL-GRADE-FLAG]','[]Site/SINGLE-PERMISSIVES/OIL_GRADE_FLAG/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of RLA3-CURRENT-GRADE]','[]Site/SINGLE-PERMISSIVES/RLA3_CURRENT_GRADE/badValue','DOUBLE');
-- These differ by a space. FOund both versions in the export
insert into TagMap values ('[the bad-value of RX_CONFIGURATION]','[]Site/SINGLE-PERMISSIVES/RX_CONFIGURATION/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of RX_CONFIGURATION ]','[]Site/SINGLE-PERMISSIVES/RX_CONFIGURATION/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of ALKYL_FLOW]','[]LabData/ALKYL_FLOW/badValue','DOUBLE');
insert into TagMap values ('[the bad-value of C2_FLOW]','[]LabData/C2_FLOW/badValue','DOUBLE');
--
----- no reference to the bad-value of mlr-grade-flag or dml-sqc-flag ?
--
-- objects from G2 given names that described the datum, but not an attribute of an object 
-- perhaps a new UDT with highLimit, lowLimit, target, maxDeviation, deadband
--
insert into TagMap values ('PREMIX_TEMP_HIGH_LIMIT','[]site/C-Rx/PREMIX-TEMP/highLimit','DOUBLE');
insert into TagMap values ('CATEFF_HIGH_LIMIT','[]site/C-Rx/CAT-EFFICIENCY/highLimit','DOUBLE');
insert into TagMap values ('CAT_EFF_HIGH_LIMIT','[]site/C-Rx/CAT-EFFICIENCY/highLimit','DOUBLE');
insert into TagMap values ('FRNT_AVG_C2_HIGH_LIMIT','[]site/C-Rx/FRNT-AVG-C2/highLimit','DOUBLE');
insert into TagMap values ('FRNT_TPR_TIP_HT_HIGH_LIMIT','[]site/C-Rx/FRNT-TPR-TIP-HT/highLimit','DOUBLE');
insert into TagMap values ('FRNT_LNGTH_HIGH_LIMIT','[]site/C-Rx/FRNT-LNGTH/highLimit','DOUBLE');
insert into TagMap values ('SDSTRM_C3-TO-C2_RATIO_HIGH_LIMIT','[]site/C-Rx/SDSTRM-C3-TO-C2-RATIO/highLimit','DOUBLE');

insert into TagMap values ('PROD-CA-SQC-FLAG','[]Tags/PROD_CA_SQC_FLAG/highLimit','DOUBLE');
insert into TagMap values ('POLYSPLIT-SQC-FLAG','[]Tags/POLYSPLIT_SQC_FLAG/highLimit','DOUBLE');

--
insert into TagMap values ('SDSTRM_C3-TO-C2_RATIO_LOW_LIMIT','[]site/C-Rx/SDSTRM-C3-TO-C2-RATIO/lowLimit','DOUBLE');
insert into TagMap values ('C3CONV_LOW_LIMIT','[]site/C-Rx/C3-CONVERSION/lowLimit','DOUBLE');
insert into TagMap values ('FRNT_LNGTH_LOW_LIMIT','[]site/C-Rx/FRNT-LNGTH/lowLimit','DOUBLE');
--
insert into TagMap values ('FRNT_LNGTH_TARGET','[]site/C-Rx/FRNT-LNGTH/target','DOUBLE');
insert into TagMap values ('CNTR_AVG_TPR_TIP_HT_TARGET','[]site/C-Rx/CNTR-AVG-TPR-TIP-HT/target','DOUBLE');
--
insert into TagMap values ('FRNT_SDSTRM_MAX_DIFF','[]site/C-Rx/FRNT-SDSTRM/maxDeviation','DOUBLE');
insert into TagMap values ('FRNT_TPR_TIP_HT_MAX_DIFF','[]site/C-Rx/FRNT-TPR-TIP-HT/maxDeviation','DOUBLE');
------ check next tag for use in g2
insert into TagMap values ('CNTR_AVG_TPR_TIP_HT_MAX_DEADBAND','[]site/C-Rx/CNTR-AVG-TPR-TIP-HT/maxDeviation','DOUBLE');
--
insert into TagMap values ('PREMIX_LINE_DATA_OK-GDA','[]site/C-Rx/PREMIX-LINE-DATA-OK','BOOLEAN');
insert into TagMap values ('PREMIX_LINE_FRESH-GDA-PARAMETER','[]site/C-Rx/PREMIX-LINE-FRESH','BOOLEAN');
--
------ track down this object in g2
insert into TagMap values ('CRX_HB-3','[]site/C-Rx/CRX-HB-3','DOUBLE');
--
-- The following entries map the names of G2 parameter and variable objects into Ignition Tags
-- 
insert into TagMap values ('CATIN-GDA-COUNTER','[]Site/Parameters/CatinCounter','STRING');
insert into TagMap values ('CATIN-GDA-PROD-ML-ACTIVE','[]Site/Parameters/CatinProdMlActive','STRING');
insert into TagMap values ('FREEZER-SAMPLE-SWITCH','[]Site/Parameters/FreezerSampleSwitch','STRING');
insert into TagMap values ('SERIES_FLYING_SWITCH-GDA-INPUT','[]Site/Parameters/SeriesFlyingSwitchInput','STRING');
insert into TagMap values ('SINGLE_FLYING_SWITCH-GDA-INPUT','[]Site/Parameters/SingleFlyingSwitchInput','STRING');
insert into TagMap values ('SERIES_RATE_CHANGE-GDA-INPUT','[]Site/Parameters/SeriesRateChangeInput','STRING');
insert into TagMap values ('SINGLE_RATE_CHANGE-GDA-INPUT','[]Site/Parameters/SingleRateChangeInput','STRING');
insert into TagMap values ('SPLIT_FLYING_SWITCH-GDA-INPUT','[]Site/Parameters/SplitFlyingSwitchInput','STRING');
-- we should plan to give these "meaningful" names after examining their use in G2
insert into TagMap values ('EM-GDA-LOGICAL-VARIABLE-XXX-2','[]Site/Parameters/LogicalVariable2','STRING');
insert into TagMap values ('EM-GDA-LOGICAL-VARIABLE-XXX-293','[]Site/Parameters/LogicalVariable293','STRING');
insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-291','[]Site/Parameters/SymbolicVariable291','STRING');
insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-292','[]Site/Parameters/SymbolicVariable292','STRING');
insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-429','[]Site/Parameters/SymbolicVariable429','STRING');
insert into TagMap values ('EM-GDA-SYMBOLIC-VARIABLE-XXX-4651','[]Site/Parameters/SymbolicVariable4651','STRING');
--
----- questions with these references 
--
insert into TagMap values ('[the unix-sample-time of mooney-lab-data]','[]LabData/MOONEY/sampleTime','INTEGER');
insert into TagMap values ('[the time-of-most-recent-recommendation-implementation of frnt_short_use_temp-gda]','[]Tags/FRNT_SHORT_USE_TEMP_GDA/implementationTime','INTEGER');
insert into TagMap values ('[the time-of-most-recent-grade-change of rla3-run-hours]','[]Tags/RLA3_RUN_HOURS/gradeChangeTime','INTEGER');
--
----- redundent? multiple references found to these?
--
insert into TagMap values ('CD-BALER-VOL-ftnir-DATA','[]LabData/CD-BALER-VOL-FTNIR/rawValue','DOUBLE'); 
insert into TagMap values ('[the target of CD-BALER-VOL-lab-DATA]','[]LabData/CD-BALER-VOL-LAB-DATA/target','DOUBLE');
insert into TagMap values ('[the standard-deviation of CD-BALER-VOL-lab-DATA]','[]LabData/CD_BALER_VOL_LAB_DATA/standardDeviation','DOUBLE');
--
------ not certain about these last tags -- want to reconsider for later
--
insert into TagMap values ('POLYSPLIT-DATA','[]LabData/POLYSPLIT_DATA/highLimit','DOUBLE');
insert into TagMap values ('POLY_RATE_CHANGE-GDA-INPUT','[]Tags/POLY_RATE_CHANGE/gdaInput','DOUBLE');
insert into TagMap values ('PROD-ML-VFU-OK','[]Tags/PROD_ML_VFU_OK','DOUBLE');
insert into TagMap values ('VFU-BALER-TEMP-CHK','[]Tags/VFU_BALER_TEMP_CHK','DOUBLE');
insert into TagMap values ('VFU-FTNIR-GRADE','[]Tags/VFU_FTNIR_GRADE','DOUBLE');
insert into TagMap values ('C_FLYING_SWITCH-GDA-INPUT','[]Tags/C_FLYING_SWITCH','DOUBLE');
insert into TagMap values ('C3-PURITY-HI','[]Tags/C3_PURITY_HI','DOUBLE');
insert into TagMap values ('C6_RX_FEED-VNB','[]Tags/C6_RX_IN_FEED','DOUBLE');
insert into TagMap values ('C9-IN-CRUMB','[]Tags/C9_IN_CRUMB','DOUBLE');
insert into TagMap values ('C101-ETHYLENE','[]Tags/C101_ETHYLENE','DOUBLE');
insert into TagMap values ('SPLIT_RATE_CHANGE-GDA-INPUT','[]Tags/SPLIT_RATE_CHANGE','DOUBLE');
insert into TagMap values ('C9-SPEC-LIMIT-IN-FEED','[]LabData/C9_SPEC_LIMIT_IN_FEED','DOUBLE');


