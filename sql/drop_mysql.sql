DROP TABLE IF EXISTS DtFinalDiagnosisLog ;
DROP TABLE IF EXISTS DtDiagnosisEntry ;
DROP TABLE IF EXISTS alarm_events ;
DROP TABLE IF EXISTS alarm_event_data ;
DROP TABLE IF EXISTS DtApplicationManageQueue ;
DROP TABLE IF EXISTS BtReactor ;
DROP TABLE IF EXISTS CustomStatsTable ;
DROP TABLE IF EXISTS CustomRateTable ;
DROP TABLE IF EXISTS HBLabReport ;
DROP TABLE IF EXISTS LookupType ;
DROP TABLE IF EXISTS LtHDAInterface ;
DROP TABLE IF EXISTS LtOPCInterface ;
DROP TABLE IF EXISTS RtDownloadDetail ;
DROP TABLE IF EXISTS RtAllowedFlyingSwitch ;
DROP TABLE IF EXISTS RtAdhocCatalog ;
DROP TABLE IF EXISTS RoleTranslation ;
DROP TABLE IF EXISTS QueueMessageStatus ;
select 'QueueMaster';
DROP TABLE IF EXISTS QueueMaster ;
DROP TABLE IF EXISTS SfcChart ;
select 'RtWatchdog';
DROP TABLE IF EXISTS RtWatchdog ;
DROP TABLE IF EXISTS RtValueType ;
DROP TABLE IF EXISTS SfcRecipeDataType ;
DROP TABLE IF EXISTS SfcRecipeDataOutputType ;
DROP TABLE IF EXISTS SfcNames ;
DROP TABLE IF EXISTS SfcRecipeDataKeyMaster ;
DROP TABLE IF EXISTS TkAssociationType ;
DROP TABLE IF EXISTS TkAssociation ;
DROP TABLE IF EXISTS TkLogbook ;
DROP TABLE IF EXISTS SfcRunLog ;
DROP TABLE IF EXISTS TkUnitParameter ;
DROP TABLE IF EXISTS SfcValueType ;
DROP TABLE IF EXISTS SfcRecipeDataStash ;
DROP TABLE IF EXISTS SfcStepType ;
DROP TABLE IF EXISTS TkSite ;
DROP TABLE IF EXISTS TkMenuBar ;
DROP TABLE IF EXISTS TkMessageRequest ;
DROP TABLE IF EXISTS UIRGline ;
DROP TABLE IF EXISTS TkWriteLocation ;
DROP TABLE IF EXISTS UIRVistalon ;
DROP TABLE IF EXISTS UIRHB ;
DROP TABLE IF EXISTS Units ;
DROP TABLE IF EXISTS VersionLog ;
DROP TABLE IF EXISTS Version ;
DROP TABLE IF EXISTS UIRGlineInvolvedProperty ;
DROP TABLE IF EXISTS UnitAliases ;
DROP TABLE IF EXISTS TkMessageReply ;
DROP TABLE IF EXISTS TkLogbookDetail ;
DROP TABLE IF EXISTS TkUnitParameterBuffer ;
DROP TABLE IF EXISTS SfcStep ;
DROP TABLE IF EXISTS TkPost ;
DROP TABLE IF EXISTS SfcRecipeDataKeyDetail ;
DROP TABLE IF EXISTS SfcHierarchyHandler ;
DROP TABLE IF EXISTS QueueDetail ;
DROP TABLE IF EXISTS Lookup ;
DROP TABLE IF EXISTS DtTextRecommendation ;
DROP TABLE IF EXISTS BtBatchRun ;
DROP TABLE IF EXISTS BtStripperBatchLog ;
DROP TABLE IF EXISTS BtBatchLog ;
DROP TABLE IF EXISTS LtDisplayTable ;
DROP TABLE IF EXISTS RtRecipeFamily ;
DROP TABLE IF EXISTS SfcRecipeData ;
DROP TABLE IF EXISTS SfcControlPanel ;
DROP VIEW IF EXISTS SfcChartStepView ;
DROP TABLE IF EXISTS SfcHierarchy ;
DROP TABLE IF EXISTS SfcRecipeDataFolder ;
DROP VIEW IF EXISTS SfcHierarchyHandlerView ;
DROP VIEW IF EXISTS SfcRecipeDataKeyView ;
DROP TABLE IF EXISTS TkUnit ;
DROP TABLE IF EXISTS TkConsole ;
DROP VIEW IF EXISTS SfcStepView ;
DROP VIEW IF EXISTS TkPostView ;
DROP VIEW IF EXISTS UIRGlineView ;
DROP VIEW IF EXISTS UnitAliasesView ;
DROP VIEW IF EXISTS UnitsView ;
DROP VIEW IF EXISTS TkUnitView ;
DROP TABLE IF EXISTS SfcWindow ;
DROP VIEW IF EXISTS TkConsoleView ;
DROP TABLE IF EXISTS SfcRecipeDataTimer ;
DROP TABLE IF EXISTS SfcRecipeDataSQC ;
DROP TABLE IF EXISTS SfcRecipeDataRecipe ;
DROP TABLE IF EXISTS SfcRecipeDataOutputRamp ;
DROP VIEW IF EXISTS SfcRecipeDataView ;
DROP TABLE IF EXISTS SfcRecipeDataValue ;
DROP TABLE IF EXISTS SfcRecipeDataArray ;
DROP TABLE IF EXISTS SfcRecipeDataMatrix ;
DROP VIEW IF EXISTS SfcRecipeDataFolderView;
DROP TABLE IF EXISTS SfcRecipeDataArray ;
DROP VIEW IF EXISTS SfcHierarchyView ;
DROP TABLE IF EXISTS SfcControlPanelMessage ;
DROP TABLE IF EXISTS RtDownloadMaster ;
DROP TABLE IF EXISTS RtValueDefinition ;
DROP TABLE IF EXISTS RtSQCParameter ;
DROP TABLE IF EXISTS RtGain ;
DROP TABLE IF EXISTS RtEventParameter ;
DROP TABLE IF EXISTS RtGradeMaster ;
DROP TABLE IF EXISTS RtGradeDetail ;
DROP TABLE IF EXISTS LtValue ;
DROP TABLE IF EXISTS DtApplication ;
DROP VIEW IF EXISTS BtBatchView ;
DROP VIEW IF EXISTS BtStripperBatchView ;
DROP TABLE IF EXISTS LtDCSValue ;
DROP TABLE IF EXISTS LtDerivedValue ;
DROP TABLE IF EXISTS DtFamily ;
DROP TABLE IF EXISTS DtQuantOutput ;
DROP VIEW IF EXISTS DtApplicationView ;
DROP TABLE IF EXISTS LtDisplayTableDetails ;
DROP TABLE IF EXISTS LtHistory ;
DROP TABLE IF EXISTS LtLimit ;
DROP TABLE IF EXISTS LtPHDValue ;
DROP TABLE IF EXISTS LtLocalValue ;
DROP TABLE IF EXISTS LtValueViewed ;
DROP TABLE IF EXISTS SfcBusyNotification ;
DROP TABLE IF EXISTS RtEvent ;
DROP TABLE IF EXISTS LtSelector ;
DROP TABLE IF EXISTS RtSQCLimit ;
DROP VIEW IF EXISTS RtRecipeView ;
DROP TABLE IF EXISTS RtGainGrade ;
DROP TABLE IF EXISTS SfcDownloadGUI ;
DROP TABLE IF EXISTS SfcDialogMessage ;
DROP TABLE IF EXISTS SfcManualDataEntry ;
DROP TABLE IF EXISTS SfcInput ;
DROP VIEW IF EXISTS SfcRecipeDataArrayView ;
DROP TABLE IF EXISTS SfcRecipeDataInput ;
DROP TABLE IF EXISTS SfcRecipeDataArrayElement ;
DROP VIEW IF EXISTS SfcRecipeDataTimerView ;
DROP TABLE IF EXISTS SfcRecipeDataOutput ;
DROP VIEW IF EXISTS SfcRecipeDataMatrixView ;
DROP TABLE IF EXISTS SfcRecipeDataMatrixElement ;
DROP TABLE IF EXISTS SfcRecipeDataSimpleValue ;
DROP VIEW IF EXISTS SfcRecipeDataRecipeView ;
DROP TABLE IF EXISTS SfcSelectInput ;
DROP TABLE IF EXISTS SfcSaveData ;
DROP TABLE IF EXISTS SfcTimeDelayNotification ;
DROP VIEW IF EXISTS SfcRecipeDataSQCView ;
DROP TABLE IF EXISTS SfcReviewData ;
DROP TABLE IF EXISTS SfcReviewFlows ;
DROP TABLE IF EXISTS SfcReviewFlowsTable ;
DROP TABLE IF EXISTS SfcReviewDataTable ;
DROP VIEW IF EXISTS SfcRecipeDataSimpleValueView ;
DROP VIEW IF EXISTS SfcRecipeDataOutputView ;
DROP VIEW IF EXISTS SfcRecipeDataOutputRampView ;
DROP VIEW IF EXISTS SfcRecipeDataMatrixElementView ;
DROP VIEW IF EXISTS SfcRecipeDataInputView ;
DROP VIEW IF EXISTS SfcRecipeDataArrayElementView ;
DROP TABLE IF EXISTS SfcManualDataEntryTable ;
DROP TABLE IF EXISTS SfcDownloadGUITable ;
DROP VIEW IF EXISTS RtSQCLimitView ;
DROP TABLE IF EXISTS LtRelatedData ;
DROP VIEW IF EXISTS LtPHDValueView ;
DROP VIEW IF EXISTS RtGainView ;
DROP VIEW IF EXISTS LtValueView ;
DROP VIEW IF EXISTS LtLimitView ;
DROP VIEW IF EXISTS LtLocalValueView ;
DROP VIEW IF EXISTS LtSelectorView ;
DROP VIEW IF EXISTS LtLastValueView ;
DROP VIEW IF EXISTS LtDerivedValueView ;
DROP TABLE IF EXISTS DtFinalDiagnosis ;
DROP TABLE IF EXISTS DtQuantOutputRamp ;
DROP VIEW IF EXISTS DtQuantOutputDefinitionView ;
DROP TABLE IF EXISTS DtSQCDiagnosis ;
DROP VIEW IF EXISTS LtDCSValueView ;
DROP VIEW IF EXISTS DtSQCDiagnosisView ;
DROP VIEW IF EXISTS DtFinalDiagnosisView ;
DROP VIEW IF EXISTS DtFinalDiagnosisLogView ;
DROP TABLE IF EXISTS DtRecommendationDefinition ;
DROP TABLE IF EXISTS DtRecommendation ;
DROP VIEW IF EXISTS DtQuantOutputView ;