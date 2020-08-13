CREATE TABLE DtFinalDiagnosisLog(
	LogId int PRIMARY KEY NOT NULL,
	Timestamp datetime NOT NULL,
	FinalDiagnosisId int NOT NULL,
	State bit NULL,
	Active bit NULL
) ;
CREATE TABLE DtDiagnosisEntry(
	DiagnosisEntryId int PRIMARY KEY NOT NULL,
	FinalDiagnosisId int NOT NULL,
	Status varchar(50) NOT NULL,
	Timestamp datetime NOT NULL,
	Grade varchar(50) NOT NULL,
	TextRecommendation varchar(1000) NOT NULL,
	RecommendationStatus varchar(50) NOT NULL,
	Multiplier float NOT NULL,
	RecommendationErrorText varchar(1000) NULL
) ;
CREATE TABLE alarm_events(
	id int PRIMARY KEY NOT NULL,
	eventid varchar(255) NULL,
	source varchar(255) NULL,
	displaypath varchar(255) NULL,
	priority int NULL,
	eventtype int NULL,
	eventflags int NULL,
	eventtime datetime NULL
) ;
CREATE TABLE alarm_event_data(
	id int NULL,
	propname varchar(255) NULL,
	dtype int NULL,
	intvalue bigint NULL,
	floatvalue float NULL,
	strvalue varchar(255) NULL
) ;
CREATE TABLE DtApplicationManageQueue(
	ApplicationName varchar(250) NOT NULL,
	Provider varchar(50) NOT NULL,
	Timestamp datetime NOT NULL
) ;
CREATE TABLE BtReactor(
	ReactorId int PRIMARY KEY NOT NULL,
	ReactorName varchar(50) NOT NULL,
	TagName varchar(50) NOT NULL
) ;
CREATE TABLE CustomStatsTable(
	Id int PRIMARY KEY NOT NULL,
	Parameter varchar(50) NULL,
	Median float NULL,
	Mean float NULL,
	StandardDeviation float NULL
) ;
CREATE TABLE CustomRateTable(
	Id int PRIMARY KEY NOT NULL,
	Grade varchar(50) NULL,
	Rate int NULL,
	Cost int NULL
) ;
CREATE TABLE HBLabReport(
	Id bigint PRIMARY KEY NOT NULL,
	LabValue float NULL,
	USpec float NULL,
	UCtl float NULL,
	Target float NULL,
	LCtl float NULL,
	LSpec float NULL,
	ReceiveTime datetime NULL,
	ReceiveText text NULL,
	SampleTime datetime NULL,
	Status varchar(50) NULL,
	EventDescription varchar(50) NULL,
	BoxNoteText varchar(300) NULL,
	OCConfirm varchar(50) NULL,
	DeleteFlag bit NULL,
	ReasonForRejection varchar(300) NULL,
	UIRStatus varchar(50) NULL,
	ActionTaken varchar(300) NULL,
	DescriptionText varchar(300) NULL,
	CurrentGrade varchar(50) NULL,
	ProblemText varchar(300) NULL,
	UIRId int NULL,
	BackgroundColor varchar(20) NULL,
	ForegroundColor varchar(20) NULL
) ; 
CREATE TABLE LookupType(
	LookupTypeCode varchar(15) NOT NULL,
	LookupTypeName varchar(50) NOT NULL,
	LookupTypeDescription varchar(500) NULL
) ;
CREATE TABLE LtHDAInterface(
	InterfaceId int PRIMARY KEY NOT NULL,
	InterfaceName varchar(50) NOT NULL
) ;
CREATE TABLE LtOPCInterface(
	InterfaceId int PRIMARY KEY NOT NULL,
	InterfaceName varchar(50) NOT NULL
) ;
CREATE TABLE RtDownloadDetail(
	DetailId int PRIMARY KEY NOT NULL,
	MasterId int NOT NULL,
	Timestamp datetime NOT NULL,
	Tag varchar(1026) NOT NULL,
	OutputValue nvarchar(50) NULL,
	Success bit NOT NULL,
	StoreValue nvarchar(50) NULL,
	CompareValue nvarchar(50) NULL,
	RecommendedValue nvarchar(50) NULL,
	Reason varchar(2000) NULL,
	Error varchar(2000) NULL
) ;
CREATE TABLE RtAllowedFlyingSwitch(
	Id int PRIMARY KEY NOT NULL,
	CurrentGrade varchar(50) NOT NULL,
	NextGrade varchar(50) NOT NULL
) ;
CREATE TABLE RtAdhocCatalog(
	TableName varchar(500) NOT NULL
) ;
CREATE TABLE RoleTranslation(
	IgnitionRole varchar(50) NOT NULL,
	WindowsRole varchar(50) NOT NULL
) ;
CREATE TABLE QueueMessageStatus(
	StatusId int PRIMARY KEY NOT NULL,
	Severity real NOT NULL,
	MessageStatus nvarchar(15) NOT NULL,
	Color nvarchar(15) NOT NULL
) ;
CREATE TABLE QueueMaster(
	QueueId int PRIMARY KEY NOT NULL,
	QueueKey nvarchar(50) NOT NULL,
	Title nvarchar(100) NOT NULL,
	CheckpointTimestamp datetime NULL,
	AutoViewSeverityThreshold real NOT NULL,
	Position varchar(50) NOT NULL,
	AutoViewAdmin bit NOT NULL,
	AutoViewAE bit NOT NULL,
	AutoViewOperator bit NOT NULL
) ;
CREATE TABLE SfcChart(
	ChartId int PRIMARY KEY NOT NULL,
	ChartPath varchar(800) NULL,
	ChartResourceId int NULL,
	CreateTime datetime NULL,
	IsProduction bit NOT NULL
) ;
CREATE TABLE RtWatchdog(
	Observation int NOT NULL,
	Timestamp datetime NOT NULL
;
CREATE TABLE RtValueType(
	ValueTypeId int PRIMARY KEY NOT NULL,
	ValueType varchar(25) NOT NULL
) ;
CREATE TABLE SfcRecipeDataType(
	RecipeDataTypeId int PRIMARY KEY NOT NULL,
	RecipeDataType varchar(50) NOT NULL,
	JavaClassName varchar(50) NOT NULL
) ;
CREATE TABLE SfcRecipeDataOutputType(
	OutputTypeId int PRIMARY KEY NOT NULL,
	OutputType varchar(50) NOT NULL
) ;
CREATE TABLE SfcNames(
	SfcName varchar(500) PRIMARY KEY NOT NULL
) ;
CREATE TABLE SfcRecipeDataKeyMaster(
	KeyId int PRIMARY KEY NOT NULL,
	KeyName varchar(50) NOT NULL
) ;
CREATE TABLE TkAssociationType(
	AssociationTypeId int PRIMARY KEY NOT NULL,
	AssociationType varchar(100) NOT NULL
) ;
CREATE TABLE TkAssociation(
	AssociationId int PRIMARY KEY NOT NULL,
	Source varchar(250) NOT NULL,
	Sink varchar(250) NOT NULL,
	AssociationTypeId int NOT NULL
) ;
CREATE TABLE TkLogbook(
	LogbookId int PRIMARY KEY NOT NULL,
	LogbookName varchar(20) NOT NULL
) ;
CREATE TABLE SfcRunLog(
	RunId int PRIMARY KEY NOT NULL,
	ChartPath varchar(250) NOT NULL,
	StepName varchar(50) NOT NULL,
	StepType varchar(50) NOT NULL,
	StartTime datetime NOT NULL,
	EndTime datetime NULL,
	Status varchar(20) NULL,
	Notes varchar(2000) NULL
) ;
CREATE TABLE TkUnitParameter(
	UnitParameterId int PRIMARY KEY NOT NULL,
	UnitParameterTagName varchar(150) NOT NULL,
	LabValueName varchar(150) NULL
) ;
CREATE TABLE SfcValueType(
	ValueTypeId int PRIMARY KEY NOT NULL,
	ValueType varchar(50) NOT NULL
) ;
CREATE TABLE SfcRecipeDataStash(
	StashId int PRIMARY KEY NOT NULL,
	RxConfiguration varchar(50) NOT NULL,
	RecipeDataKey varchar(50) NOT NULL,
	RecipeDataAttribute varchar(50) NOT NULL,
	RecipeDataValue float NOT NULL
) ;
CREATE TABLE SfcStepType(
	StepTypeId int PRIMARY KEY NOT NULL,
	StepType varchar(50) NOT NULL,
	FactoryId varchar(50) NULL
) ;
CREATE TABLE TkSite(
	SiteName varchar(50) NOT NULL,
	GatewayStartupScript varchar(500) NOT NULL,
	ClientStartupScript varchar(500) NOT NULL
) ;
CREATE TABLE TkMenuBar(
	Id int PRIMARY KEY NOT NULL,
	Application varchar(50) NOT NULL,
	Menu varchar(50) NOT NULL,
	SubMenu varchar(50) NOT NULL,
	Enabled bit NOT NULL
) ;
CREATE TABLE TkMessageRequest(
	RequestId int PRIMARY KEY NOT NULL,
	RequestType varchar(50) NOT NULL,
	RequestTime datetime NOT NULL
) ;
CREATE TABLE UIRGline(
	UIRId int PRIMARY KEY NOT NULL,
	PostId int NULL,
	UIRTitle varchar(100) NULL,
	Originator varchar(100) NULL,
	ReportDate datetime NULL,
	ManualEntry bit NULL,
	IncidentStart datetime NULL,
	IncidentEnd datetime NULL,
	Reviewer varchar(100) NULL,
	Grade varchar(10) NULL,
	UnitsAffected varchar(2000) NULL,
	Summary varchar(2000) NULL,
	UIRNumber varchar(100) NULL,
	Area varchar(50) NULL,
	Category varchar(50) NULL,
	FollowUp varchar(50) NULL,
	TimeBasis varchar(10) NULL,
	TextReport nvarchar(1026) NULL,
	XMLReport nvarchar(1026) NULL
) ;
CREATE TABLE TkWriteLocation(
	WriteLocationId int PRIMARY KEY NOT NULL,
	Alias varchar(1026) NOT NULL,
	ServerName varchar(1026) NOT NULL,
	ScanClass varchar(1026) NOT NULL
) ;
CREATE TABLE UIRVistalon(
	UIRId int PRIMARY KEY NOT NULL,
	FormName varchar(100) NULL,
	PostId int NULL,
	Title varchar(100) NULL,
	Originator varchar(100) NULL,
	RecordDate datetime NULL,
	Operator varchar(100) NULL,
	ManualEntry bit NULL,
	Type varchar(100) NULL,
	IncidentStart datetime NULL,
	IncidentEnd datetime NULL,
	Reviewer varchar(100) NULL,
	Grade float NULL,
	Quality varchar(1000) NULL,
	Root varchar(1000) NULL,
	IncidentSummary varchar(2000) NULL,
	RootExplanation varchar(2000) NULL,
	GradeSummary varchar(2000) NULL,
	CorrectiveAction varchar(2000) NULL,
	BoxLine varchar(100) NULL,
	BoxStart varchar(100) NULL,
	BoxEnd varchar(100) NULL,
	CloseOut bit NULL,
	Post varchar(100) NULL,
	UIRNumber varchar(100) NULL
) ;
CREATE TABLE UIRHB(
	UIRId int PRIMARY KEY NOT NULL,
	FormName varchar(100) NULL,
	PostId int NULL,
	Title varchar(100) NULL,
	Originator varchar(100) NULL,
	RecordDate datetime NULL,
	Operator varchar(100) NULL,
	ManualEntry bit NULL,
	Type varchar(100) NULL,
	IncidentStart datetime NULL,
	IncidentEnd datetime NULL,
	Reviewer varchar(100) NULL,
	Grade float NULL,
	Quality varchar(1000) NULL,
	Root varchar(1000) NULL,
	IncidentSummary varchar(2000) NULL,
	RootExplanation varchar(2000) NULL,
	GradeSummary varchar(2000) NULL,
	CorrectiveAction varchar(2000) NULL,
	BoxFlagStart varchar(100) NULL,
	BoxFlagEnd varchar(100) NULL,
	FactStart datetime NULL,
	FactEnd datetime NULL,
	CloseOut bit NULL,
	Post varchar(100) NULL,
	UIRNumber varchar(100) NULL
) ;
CREATE TABLE Units(
	id int PRIMARY KEY NOT NULL,
	name varchar(64) NOT NULL,
	isBaseUnit bit NOT NULL,
	type varchar(64) NOT NULL,
	description varchar(2000) NULL,
	m float NULL,
	b float NULL
) ;
CREATE TABLE VersionLog(
	Version nchar(10) NOT NULL,
	ChangeDate datetime NOT NULL,
	ChangeDetail varchar(1000) NOT NULL
) ;
CREATE TABLE Version(
	VersionId int NOT NULL,
	Version nchar(10) NOT NULL,
	ReleaseDate datetime NOT NULL,
	InstallDate datetime NULL
) ;
CREATE TABLE UIRGlineInvolvedProperty(
	Id int PRIMARY KEY NOT NULL,
	UIRId int NOT NULL,
	PropertyName varchar(50) NOT NULL
) ;
CREATE TABLE UnitAliases(
	id int PRIMARY KEY NOT NULL,
	alias varchar(64) NOT NULL,
	name varchar(64) NOT NULL
) ;
CREATE TABLE TkMessageReply(
	ReplyId int PRIMARY KEY NOT NULL,
	RequestId int NOT NULL,
	Reply varchar(2000) NOT NULL,
	ReplyTime datetime NOT NULL,
	ClientId varchar(500) NOT NULL,
	IsolationMode bit NOT NULL
) ;
CREATE TABLE TkLogbookDetail(
	Id int PRIMARY KEY NOT NULL,
	LogbookId int NULL,
	Timestamp datetime NULL,
	Message varchar(2000) NULL
) ;
CREATE TABLE TkUnitParameterBuffer(
	UnitParameterId int NOT NULL,
	BufferIndex int NOT NULL,
	RawValue float NULL,
	SampleTime datetime NULL,
	ReceiptTime datetime NULL
) ;
CREATE TABLE SfcStep(
	StepId int PRIMARY KEY NOT NULL,
	StepUUID varchar(256) NOT NULL,
	StepName varchar(500) NOT NULL,
	StepTypeId int NOT NULL,
	ChartId int NOT NULL
) ;
CREATE TABLE TkPost(
	PostId int PRIMARY KEY NOT NULL,
	Post varchar(50) NOT NULL,
	MessageQueueId int NOT NULL,
	LogbookId int NOT NULL,
	DownloadActive bit NOT NULL
) ;
CREATE TABLE SfcRecipeDataKeyDetail(
	KeyId int NOT NULL,
	KeyValue varchar(20) NOT NULL,
	KeyIndex int NOT NULL
) ;
CREATE TABLE SfcHierarchyHandler(
	HierarchyId int PRIMARY KEY NOT NULL,
	ChartId int NOT NULL,
	Handler varchar(50) NOT NULL,
	HandlerChartId int NOT NULL
) ;
CREATE TABLE QueueDetail(
	Id int PRIMARY KEY NOT NULL,
	QueueId int NOT NULL,
	Timestamp datetime NOT NULL,
	StatusId int NOT NULL,
	Message nvarchar(2000) NOT NULL
) ;
CREATE TABLE Lookup(
	LookupId int PRIMARY KEY NOT NULL,
	LookupTypeCode varchar(15) NOT NULL,
	LookupName varchar(50) NOT NULL,
	LookupDescription varchar(500) NULL,
	Active bit NOT NULL
) ;
CREATE TABLE DtTextRecommendation(
	TextRecommendationId int PRIMARY KEY NOT NULL,
	DiagnosisEntryId int NOT NULL,
	TextRecommendation varchar(2500) NOT NULL
) ;
CREATE TABLE BtBatchRun(
	BatchRunId int PRIMARY KEY NOT NULL,
	ReactorId int NOT NULL,
	Grade float NOT NULL,
	BatchCount int NOT NULL,
	StartDate datetime NOT NULL,
	EndDate datetime NULL
) ;
CREATE TABLE BtStripperBatchLog(
	BatchId int PRIMARY KEY NOT NULL,
	BatchRunId int NULL,
	BatchNumber int NULL,
	BatchCount int NULL,
	Status varchar(50) NULL,
	CreationTime datetime NULL,
	LabResult float NULL,
	FillBegin datetime NULL,
	FillEnd datetime NULL,
	FillTime time(6) NULL,
	StripBegin datetime NULL,
	StripEnd datetime NULL,
	StripTime time(6) NULL,
	JD03Begin datetime NULL,
	JD03End datetime NULL,
	JD03Time time(6) NULL,
	TransferBegin datetime NULL,
	TransferEnd datetime NULL,
	TransferTime time(6) NULL,
	StandbyBegin datetime NULL,
	StandbyEnd datetime NULL,
	StandbyTime time(6) NULL,
	TotalStripperTime time(6) NULL,
	TotalChargeAmount float NULL
) ;
CREATE TABLE BtBatchLog(
	BatchId int PRIMARY KEY NOT NULL,
	BatchRunId int NULL,
	BatchNumber int NULL,
	BatchCount int NULL,
	Status varchar(50) NULL,
	CreationTime datetime NULL,
	LabResult float NULL,
	ChargeBegin datetime NULL,
	ChargeEnd datetime NULL,
	ChargeTime time(6) NULL,
	HeatUpBegin datetime NULL,
	HeatUpEnd datetime NULL,
	HeatUpTime time(6) NULL,
	SoakBegin datetime NULL,
	SoakEnd datetime NULL,
	SoakTime time(6) NULL,
	TransferBegin datetime NULL,
	TransferEnd datetime NULL,
	TransferTime time(6) NULL,
	StandbyBegin datetime NULL,
	StandbyEnd datetime NULL,
	StandbyTime time(6) NULL,
	TotalBatchTime time(6) NULL,
	TotalChargeAmount float NULL,
	AverageSoakTemp float NULL,
	SoakTimer float NULL
) ;
CREATE TABLE LtDisplayTable(
	DisplayTableId int PRIMARY KEY NOT NULL,
	DisplayTableTitle varchar(50) NOT NULL,
	DisplayPage int NOT NULL,
	DisplayOrder int NOT NULL,
	DisplayFlag bit NOT NULL,
	PostId int NOT NULL,
	OldTableName varchar(50) NULL
) ;
CREATE TABLE RtRecipeFamily(
	RecipeFamilyId int PRIMARY KEY NOT NULL,
	RecipeFamilyName varchar(50) NOT NULL,
	RecipeUnitPrefix varchar(50) NULL,
	RecipeNameAlias varchar(50) NULL,
	PostId int NULL,
	CurrentGrade varchar(50) NULL,
	CurrentVersion int NULL,
	Status varchar(50) NULL,
	ConfirmDownload bit NULL,
	Timestamp datetime NULL,
	Comment varchar(2000) NULL
) ;
CREATE TABLE SfcRecipeData(
	RecipeDataId int PRIMARY KEY NOT NULL,
	StepId int NOT NULL,
	RecipeDataKey varchar(50) NOT NULL,
	RecipeDataTypeId int NOT NULL,
	Label varchar(100) NULL,
	Description varchar(1000) NULL,
	Units varchar(50) NULL,
	RecipeDataFolderId int NULL
) ;
CREATE TABLE SfcControlPanel(
	ControlPanelId int PRIMARY KEY NOT NULL,
	ControlPanelName varchar(900) NOT NULL,
	PostId int NULL,
	ChartPath varchar(900) NOT NULL,
	ChartRunId varchar(900) NULL,
	Operation varchar(900) NULL,
	MsgQueue varchar(900) NULL,
	Originator varchar(900) NULL,
	Project varchar(900) NULL,
	IsolationMode bit NULL,
	EnableCancel bit NULL,
	EnablePause bit NULL,
	EnableReset bit NULL,
	EnableResume bit NULL,
	EnableStart bit NULL
) ;
CREATE VIEW SfcChartStepView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepName, SfcStep.StepUUID, SfcStepType.StepType
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcStepType ON SfcStep.StepTypeId = SfcStepType.StepTypeId
;
CREATE TABLE SfcHierarchy(
	HierarchyId int PRIMARY KEY NOT NULL,
	StepId int NOT NULL,
	ChartId int NOT NULL,
	ChildChartId int NOT NULL
) ;
CREATE TABLE SfcRecipeDataFolder(
	RecipeDataFolderId int PRIMARY KEY NOT NULL,
	RecipeDataKey varchar(50) NOT NULL,
	StepId int NOT NULL,
	RecipeDataType varchar(50) NOT NULL,
	ParentRecipeDataFolderId int NULL,
	Description varchar(1000) NULL,
	Label varchar(100) NULL
) ;
CREATE VIEW SfcHierarchyHandlerView
AS
SELECT     SfcHierarchyHandler.HierarchyId, SfcHierarchyHandler.ChartId, SfcChart.ChartPath, SfcHierarchyHandler.Handler, SfcHierarchyHandler.HandlerChartId, 
                      SfcChart_1.ChartPath AS HandlerChartPath, SfcChart.IsProduction
FROM         SfcHierarchyHandler INNER JOIN
                      SfcChart ON SfcHierarchyHandler.ChartId = SfcChart.ChartId INNER JOIN
                      SfcChart AS SfcChart_1 ON SfcHierarchyHandler.HandlerChartId = SfcChart_1.ChartId
;
CREATE VIEW SfcRecipeDataKeyView
AS
SELECT     SfcRecipeDataKeyMaster.KeyId, SfcRecipeDataKeyMaster.KeyName, SfcRecipeDataKeyDetail.KeyValue, SfcRecipeDataKeyDetail.KeyIndex
FROM         SfcRecipeDataKeyMaster INNER JOIN
                      SfcRecipeDataKeyDetail ON SfcRecipeDataKeyMaster.KeyId = SfcRecipeDataKeyDetail.KeyId
;
CREATE TABLE TkUnit(
	UnitId int PRIMARY KEY NOT NULL,
	UnitName varchar(50) NOT NULL,
	UnitPrefix varchar(50) NULL,
	UnitAlias varchar(50) NULL,
	PostId int NOT NULL,
	Grade varchar(50) NULL
) ;
CREATE TABLE TkConsole(
	ConsoleId int PRIMARY KEY NOT NULL,
	WindowName varchar(100) NOT NULL,
	ConsoleName varchar(100) NOT NULL,
	Priority int NOT NULL,
	PostId int NOT NULL
) ;
CREATE VIEW SfcStepView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepName, SfcStep.StepUUID, SfcStepType.StepType
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcStepType ON SfcStep.StepTypeId = SfcStepType.StepTypeId
;
CREATE VIEW TkPostView
AS
SELECT     TkPost.PostId, TkPost.Post, QueueMaster.QueueId, QueueMaster.QueueKey, TkLogbook.LogbookId, TkLogbook.logbookName, TkPost.DownloadActive
FROM         TkPost INNER JOIN
                      QueueMaster ON TkPost.MessageQueueId = QueueMaster.QueueId LEFT OUTER JOIN
                      TkLogbook ON TkPost.LogbookId = TkLogbook.LogbookId
;
CREATE VIEW UIRGlineView
AS
SELECT     UIRGline.UIRId, UIRGline.UIRNumber, UIRGline.UIRTitle, TkPost.Post, UIRGline.Originator, UIRGline.ReportDate, UIRGline.IncidentStart, UIRGline.IncidentEnd, 
                      UIRGline.Grade, UIRGline.Area, UIRGline.Category, UIRGline.TimeBasis, UIRGline.FollowUp, UIRGline.UnitsAffected, UIRGline.Summary, UIRGline.ManualEntry, 
                      UIRGline.Reviewer
FROM         UIRGline INNER JOIN
                      TkPost ON UIRGline.PostId = TkPost.PostId
;
CREATE VIEW UnitAliasesView
AS
SELECT     UnitAliases.id, UnitAliases.alias AS name, Units.isBaseUnit, Units.type, Units.description, Units.m, Units.b
FROM         UnitAliases INNER JOIN
                      Units ON UnitAliases.name = Units.name
;
CREATE VIEW UnitsView
AS
SELECT   * from Units
UNION
SELECT * from UnitAliasesView
CREATE VIEW TkUnitView
AS
SELECT     TkUnit.UnitId, TkUnit.UnitName, TkUnit.UnitPrefix, TkUnit.UnitAlias, TkPost.PostId, TkPost.Post
FROM         TkUnit LEFT OUTER JOIN
                      TkPost ON TkUnit.PostId = TkPost.PostId
;
CREATE TABLE SfcWindow(
	windowId int PRIMARY KEY NOT NULL,
	chartRunId varchar(900) NOT NULL,
	controlPanelId int NOT NULL,
	windowPath varchar(64) NOT NULL,
	buttonLabel varchar(64) NOT NULL,
	position varchar(100) NOT NULL,
	scale float NOT NULL,
	title varchar(100) NOT NULL
) ;
CREATE VIEW TkConsoleView
AS
SELECT     TkConsole.ConsoleId, TkConsole.ConsoleName, TkConsole.WindowName, TkConsole.Priority, TkPost.PostId, TkPost.Post
FROM         TkConsole INNER JOIN
                      TkPost ON TkConsole.PostId = TkPost.PostId
;
CREATE TABLE SfcRecipeDataTimer(
	RecipeDataId int NOT NULL,
	StartTime datetime NULL,
	StopTime datetime NULL,
	TimerState varchar(10) NULL,
	CumulativeMinutes float NULL
) ;
CREATE TABLE SfcRecipeDataSQC(
	RecipeDataId int NOT NULL,
	LowLimit float NULL,
	TargetValue float NULL,
	HighLimit float NULL
) ;
CREATE TABLE SfcRecipeDataRecipe(
	RecipeDataId int NOT NULL,
	PresentationOrder int NOT NULL,
	StoreTag varchar(1026) NULL,
	CompareTag varchar(1026) NULL,
	ModeAttribute varchar(1026) NULL,
	ModeValue varchar(1026) NULL,
	ChangeLevel varchar(1026) NULL,
	RecommendedValue varchar(1026) NULL,
	LowLimit varchar(1026) NULL,
	HighLimit varchar(1026) NULL
) ;
CREATE TABLE SfcRecipeDataOutputRamp(
	RecipeDataId int NOT NULL,
	RampTimeMinutes float NULL,
	UpdateFrequencySeconds float NULL
) ;
CREATE VIEW SfcRecipeDataView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, SfcRecipeData.RecipeDataFolderId, 
                      SfcRecipeDataFolder.RecipeDataKey AS FolderName
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcRecipeData INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId ON SfcStep.StepId = SfcRecipeData.StepId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE TABLE SfcRecipeDataValue(
	ValueId int PRIMARY KEY NOT NULL,
	RecipeDataId int NOT NULL,
	FloatValue float NULL,
	IntegerValue int NULL,
	StringValue varchar(1000) NULL,
	BooleanValue bit NULL
) ;
CREATE TABLE SfcRecipeDataMatrix(
	RecipeDataId int NOT NULL,
	ValueTypeId int NOT NULL,
	`Rows` int NOT NULL,
	`Columns` int NOT NULL,
	RowIndexKeyId int NULL,
	ColumnIndexKeyId int NULL
) ;
CREATE VIEW SfcRecipeDataFolderView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeDataFolder.RecipeDataFolderId, 
                      SfcRecipeDataFolder.RecipeDataKey, SfcRecipeDataFolder.ParentRecipeDataFolderId, SfcRecipeDataFolder.Description, SfcRecipeDataFolder.Label, 
                      SfcRecipeDataFolder.RecipeDataType, SfcRecipeDataFolder_1.RecipeDataKey AS ParentFolderName
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcRecipeDataFolder ON SfcStep.StepId = SfcRecipeDataFolder.StepId LEFT OUTER JOIN
                      SfcRecipeDataFolder AS SfcRecipeDataFolder_1 ON SfcRecipeDataFolder.ParentRecipeDataFolderId = SfcRecipeDataFolder_1.RecipeDataFolderId;
;
CREATE TABLE SfcRecipeDataArray(
	RecipeDataId int NOT NULL,
	ValueTypeId int NOT NULL,
	IndexKeyId int NULL
) ;
CREATE VIEW SfcHierarchyView
AS
SELECT     SfcHierarchy.HierarchyId, SfcHierarchy.StepId, SfcStepType.StepType, SfcHierarchy.ChartId, SfcHierarchy.ChildChartId, SfcChart.ChartPath, SfcChart.ChartResourceId, 
                      SfcChart_1.ChartPath AS ChildChartPath, SfcChart_1.ChartResourceId AS ChildResourceId, SfcStep.StepUUID, SfcStep.StepName, SfcStep.StepTypeId, SfcChart.IsProduction
FROM         SfcHierarchy INNER JOIN
                      SfcChart ON SfcHierarchy.ChartId = SfcChart.ChartId INNER JOIN
                      SfcChart AS SfcChart_1 ON SfcHierarchy.ChildChartId = SfcChart_1.ChartId INNER JOIN
                      SfcStep ON SfcHierarchy.StepId = SfcStep.StepId AND SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcStepType ON SfcStep.StepTypeId = SfcStepType.StepTypeId
;
CREATE TABLE SfcControlPanelMessage(
	id int PRIMARY KEY NOT NULL,
	controlPanelId int NOT NULL,
	message varchar(256) NOT NULL,
	priority varchar(20) NOT NULL,
	createTime datetime NOT NULL,
	ackRequired bit NOT NULL
) ;
CREATE TABLE RtDownloadMaster(
	MasterId int PRIMARY KEY NOT NULL,
	RecipeFamilyId int NOT NULL,
	Grade varchar(50) NOT NULL,
	Version int NOT NULL,
	Type varchar(50) NULL,
	DownloadStartTime datetime NOT NULL,
	DownloadEndTime datetime NULL,
	Status varchar(50) NULL,
	TotalDownloads int NULL,
	PassedDownloads int NULL,
	FailedDownloads int NULL
) ;
CREATE TABLE RtValueDefinition(
	RecipeFamilyId int NOT NULL,
	ValueId int PRIMARY KEY NOT NULL,
	PresentationOrder int NOT NULL,
	Description nvarchar(1026) NULL,
	StoreTag varchar(1026) NULL,
	CompareTag varchar(1026) NULL,
	ChangeLevel nvarchar(1026) NULL,
	ModeAttribute varchar(1026) NULL,
	ModeValue varchar(1026) NULL,
	WriteLocationId int NULL,
	ValueTypeId int NULL
) ;
CREATE TABLE RtSQCParameter(
	ParameterId int PRIMARY KEY NOT NULL,
	RecipeFamilyId int NOT NULL,
	Parameter varchar(1026) NOT NULL
) ; 
CREATE TABLE RtGain(
	ParameterId int PRIMARY KEY NOT NULL,
	RecipeFamilyId int NULL,
	Parameter nvarchar(1026) NOT NULL
) ;
CREATE TABLE RtEventParameter(
	ParameterId int PRIMARY KEY NOT NULL,
	RecipeFamilyId int NOT NULL,
	Parameter varchar(1026) NOT NULL
) ; 
CREATE TABLE RtGradeMaster(
	RecipeFamilyId int NOT NULL,
	Grade varchar(50) NOT NULL,
	Version int NOT NULL,
	Timestamp datetime NULL,
	Active bit NULL
) ;
CREATE TABLE RtGradeDetail(
	RecipeFamilyId int NOT NULL,
	Grade varchar(50) NOT NULL,
	Version int NOT NULL,
	ValueId int NOT NULL,
	RecommendedValue varchar(1026) NULL,
	LowLimit varchar(1026) NULL,
	HighLimit varchar(1026) NULL
) ; 
CREATE TABLE LtValue(
	ValueId int PRIMARY KEY NOT NULL,
	ValueName nvarchar(50) NOT NULL,
	UnitId int NOT NULL,
	DisplayDecimals int NOT NULL,
	Description nvarchar(500) NULL,
	ValidationProcedure varchar(250) NULL,
	LastHistoryId int NULL
) ;
CREATE TABLE DtApplication(
	ApplicationId int PRIMARY KEY NOT NULL,
	ApplicationName varchar(250) NOT NULL,
	UnitId int NULL,
	ApplicationUUID varchar(100) NULL,
	Description varchar(2000) NULL,
	IncludeInMainMenu bit NULL,
	MessageQueueId int NULL,
	GroupRampMethodId int NULL,
	DownloadAction varchar(50) NULL,
	NotificationStrategy varchar(50) NOT NULL,
	ClientId varchar(50) NULL,
	Managed bit NOT NULL
) ;
CREATE VIEW BtBatchView
AS
SELECT     BtReactor.ReactorId, BtReactor.ReactorName, BtReactor.TagName, BtBatchRun.BatchRunId, BtBatchRun.Grade, BtBatchRun.StartDate, BtBatchRun.EndDate, 
                      BtBatchLog.BatchId, BtBatchLog.BatchNumber, BtBatchLog.BatchCount, BtBatchLog.Status, BtBatchLog.CreationTime, BtBatchLog.LabResult, 
                      BtBatchLog.ChargeBegin, BtBatchLog.ChargeEnd, BtBatchLog.ChargeTime, BtBatchLog.HeatUpBegin, BtBatchLog.HeatUpEnd, BtBatchLog.HeatUpTime, 
                      BtBatchLog.SoakBegin, BtBatchLog.SoakEnd, BtBatchLog.SoakTime, BtBatchLog.TransferBegin, BtBatchLog.TransferEnd, BtBatchLog.TransferTime, 
                      BtBatchLog.StandbyBegin, BtBatchLog.StandbyEnd, BtBatchLog.StandbyTime, BtBatchLog.TotalBatchTime, BtBatchLog.TotalChargeAmount, 
                      BtBatchLog.AverageSoakTemp, BtBatchLog.SoakTimer
FROM         BtReactor INNER JOIN
                      BtBatchRun ON BtReactor.ReactorId = BtBatchRun.ReactorId INNER JOIN
                      BtBatchLog ON BtBatchRun.BatchRunId = BtBatchLog.BatchRunId
;
CREATE VIEW BtStripperBatchView
AS
SELECT     BtReactor.ReactorId, BtReactor.ReactorName, BtReactor.TagName, BtBatchRun.BatchRunId, BtBatchRun.Grade, BtBatchRun.StartDate, BtBatchRun.EndDate, 
                      BtStripperBatchLog.BatchId, BtStripperBatchLog.BatchNumber, BtStripperBatchLog.BatchCount, BtStripperBatchLog.Status, BtStripperBatchLog.CreationTime, 
                      BtStripperBatchLog.LabResult, BtStripperBatchLog.FillBegin, BtStripperBatchLog.FillEnd, BtStripperBatchLog.FillTime, BtStripperBatchLog.StripBegin, 
                      BtStripperBatchLog.StripEnd, BtStripperBatchLog.StripTime, BtStripperBatchLog.JD03Begin, BtStripperBatchLog.JD03End, BtStripperBatchLog.JD03Time, 
                      BtStripperBatchLog.TransferBegin, BtStripperBatchLog.TransferEnd, BtStripperBatchLog.TransferTime, BtStripperBatchLog.StandbyBegin, BtStripperBatchLog.StandbyEnd, 
                      BtStripperBatchLog.StandbyTime, BtStripperBatchLog.TotalStripperTime, BtStripperBatchLog.TotalChargeAmount
FROM         BtReactor INNER JOIN
                      BtBatchRun ON BtReactor.ReactorId = BtBatchRun.ReactorId INNER JOIN
                      BtStripperBatchLog ON BtBatchRun.BatchRunId = BtStripperBatchLog.BatchRunId
;
CREATE TABLE LtDCSValue(
	DCSValueId int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	ItemId varchar(100) NOT NULL,
	InterfaceId int NOT NULL,
	MinimumSampleIntervalSeconds int NOT NULL,
	AllowManualEntry bit NOT NULL
) ;
CREATE TABLE LtDerivedValue(
	DerivedValueId int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	TriggerValueId int NOT NULL,
	Callback varchar(250) NOT NULL,
	SampleTimeTolerance int NOT NULL,
	NewSampleWaitTime int NOT NULL,
	ResultItemId varchar(100) NULL,
	ResultInterfaceId int NULL
) ;
CREATE TABLE DtFamily(
	FamilyId int PRIMARY KEY NOT NULL,
	ApplicationId int NOT NULL,
	FamilyName varchar(250) NOT NULL,
	FamilyPriority float NOT NULL,
	FamilyUUID varchar(100) NULL,
	Description varchar(2000) NULL
) ;
CREATE TABLE DtQuantOutput(
	QuantOutputId int PRIMARY KEY NOT NULL,
	QuantOutputName varchar(750) NOT NULL,
	ApplicationId int NOT NULL,
	TagPath varchar(1000) NOT NULL,
	MostNegativeIncrement float NOT NULL,
	MostPositiveIncrement float NOT NULL,
	IgnoreMinimumIncrement bit NOT NULL,
	MinimumIncrement float NOT NULL,
	SetpointHighLimit float NOT NULL,
	SetpointLowLimit float NOT NULL,
	FeedbackMethodId int NOT NULL,
	IncrementalOutput bit NOT NULL,
	OutputLimitedStatus varchar(50) NULL,
	OutputLimited bit NULL,
	OutputPercent float NULL,
	FeedbackOutput float NULL,
	FeedbackOutputManual float NULL,
	FeedbackOutputConditioned float NULL,
	ManualOverride bit NULL,
	Active bit NULL,
	DownloadAction varchar(25) NULL,
	DownloadStatus varchar(100) NULL,
	CurrentSetpoint float NULL,
	FinalSetpoint float NULL,
	DisplayedRecommendation float NULL
) ;
CREATE VIEW DtApplicationView
AS
SELECT     DtApplication.ApplicationId, TkPost.Post, TkUnit.UnitName, DtApplication.ApplicationName, DtApplication.Description, DtApplication.IncludeInMainMenu, 
                      QueueMaster.QueueKey, DtApplication.DownloadAction, DtApplication.NotificationStrategy, DtApplication.Managed, Lookup.LookupName AS GroupRampMethodName, 
                      Lookup.LookupTypeCode AS GroupRampMethodCode
FROM         DtApplication INNER JOIN
                      TkUnit ON DtApplication.UnitId = TkUnit.UnitId INNER JOIN
                      QueueMaster ON DtApplication.MessageQueueId = QueueMaster.QueueId INNER JOIN
                      TkPost ON TkUnit.PostId = TkPost.PostId INNER JOIN
                      Lookup ON DtApplication.GroupRampMethodId = Lookup.LookupId
;
CREATE TABLE LtDisplayTableDetails(
	Id int PRIMARY KEY NOT NULL,
	DisplayTableId int NOT NULL,
	ValueId int NOT NULL,
	DisplayOrder int NOT NULL
) ;
CREATE TABLE LtHistory(
	HistoryId int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	RawValue float NOT NULL,
	SampleTime datetime NOT NULL,
	ReportTime datetime NOT NULL,
	Grade varchar(50) NULL
) ;
CREATE TABLE LtLimit(
	LimitId int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	LimitTypeId int NOT NULL,
	LimitSourceId int NOT NULL,
	RecipeParameterName varchar(100) NULL,
	UpperValidityLimit float NULL,
	LowerValidityLimit float NULL,
	UpperSQCLimit float NULL,
	LowerSQCLimit float NULL,
	UpperReleaseLimit float NULL,
	LowerReleaseLimit float NULL,
	Target float NULL,
	StandardDeviation float NULL,
	OPCUpperItemId varchar(50) NULL,
	OPCLowerItemId varchar(50) NULL,
	OPCInterfaceId int NULL
) ;
CREATE TABLE LtPHDValue(
	PHDValueId int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	ItemId varchar(50) NOT NULL,
	InterfaceId int NOT NULL,
	AllowManualEntry bit NOT NULL
) ;
CREATE TABLE LtLocalValue(
	LocalValueId int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	ItemId varchar(500) NULL,
	InterfaceId int NULL
) ;
CREATE TABLE LtValueViewed(
	id int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	Username varchar(25) NOT NULL,
	ViewTime datetime NOT NULL
) ;
CREATE TABLE SfcBusyNotification(
	windowId int NOT NULL,
	message varchar(900) NULL
) ;
CREATE TABLE RtEvent(
	ParameterId int NOT NULL,
	Grade varchar(50) NOT NULL,
	Value float NULL
) ;
CREATE TABLE LtSelector(
	SelectorId int PRIMARY KEY NOT NULL,
	ValueId int NOT NULL,
	hasSQCLimit bit NOT NULL,
	hasValidityLimit bit NOT NULL,
	hasReleaseLimit bit NOT NULL,
	sourceValueId int NULL
) ;
CREATE TABLE RtSQCLimit(
	ParameterId int NOT NULL,
	Grade varchar(50) NOT NULL,
	UpperLimit float NULL,
	LowerLimit float NULL
) ;
CREATE VIEW RtRecipeView
AS
SELECT     RtRecipeFamily.RecipeFamilyId, RtRecipeFamily.RecipeFamilyName, RtGradeMaster.Grade, RtGradeMaster.Version, RtGradeDetail.ValueId, 
                      RtValueDefinition.PresentationOrder, RtValueDefinition.Description, RtValueDefinition.StoreTag, RtValueDefinition.CompareTag, RtValueDefinition.ChangeLevel, 
                      RtValueDefinition.ModeAttribute, RtValueDefinition.ModeValue, RtGradeDetail.RecommendedValue, RtGradeDetail.LowLimit, RtGradeDetail.HighLimit, 
                      RtValueType.ValueType
FROM         RtRecipeFamily INNER JOIN
                      RtGradeMaster ON RtRecipeFamily.RecipeFamilyId = RtGradeMaster.RecipeFamilyId INNER JOIN
                      RtGradeDetail ON RtGradeMaster.RecipeFamilyId = RtGradeDetail.RecipeFamilyId AND RtGradeMaster.Grade = RtGradeDetail.Grade AND 
                      RtGradeMaster.Version = RtGradeDetail.Version INNER JOIN
                      RtValueDefinition ON RtGradeDetail.RecipeFamilyId = RtValueDefinition.RecipeFamilyId AND RtGradeDetail.ValueId = RtValueDefinition.ValueId INNER JOIN
                      RtValueType ON RtValueDefinition.ValueTypeId = RtValueType.ValueTypeId
;
CREATE TABLE RtGainGrade(
	ParameterId int NOT NULL,
	Grade varchar(50) NOT NULL,
	Gain float NULL
) ;
CREATE TABLE SfcDownloadGUI(
	WindowId int NOT NULL,
	State varchar(25) NOT NULL,
	TimerRecipeDataId int NOT NULL,
	LastUpdated datetime NULL,
	StartTime datetime NULL
) ;
CREATE TABLE SfcDialogMessage(
	windowId int NOT NULL,
	message varchar(900) NOT NULL,
	ackRequired bit NOT NULL,
	acknowledged bit NULL
) ;
CREATE TABLE SfcManualDataEntry(
	windowId int NOT NULL,
	requireAllInputs bit NOT NULL,
	complete bit NOT NULL,
	header varchar(1024) NULL
) ;
CREATE TABLE SfcInput(
	windowId int NOT NULL,
	prompt varchar(900) NOT NULL,
	lowLimit float NULL,
	highLimit float NULL,
	responseLocation varchar(25) NULL,
	targetStepId int NULL,
	keyAndAttribute varchar(255) NULL,
	defaultValue varchar(255) NULL,
	chartId varchar(200) NULL,
	stepId varchar(200) NULL
) ;
CREATE VIEW SfcRecipeDataArrayView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepName, SfcStep.StepUUID, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, SfcValueType.ValueType, SfcRecipeDataArray.ValueTypeId, 
                      SfcRecipeDataArray.IndexKeyId, SfcRecipeDataKeyMaster.KeyName, SfcRecipeData.RecipeDataFolderId AS FolderId, SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcRecipeData INNER JOIN
                      SfcRecipeDataArray ON SfcRecipeData.RecipeDataId = SfcRecipeDataArray.RecipeDataId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcValueType ON SfcRecipeDataArray.ValueTypeId = SfcValueType.ValueTypeId INNER JOIN
                      SfcStep ON SfcRecipeData.StepId = SfcStep.StepId INNER JOIN
                      SfcChart ON SfcStep.ChartId = SfcChart.ChartId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId LEFT OUTER JOIN
                      SfcRecipeDataKeyMaster ON SfcRecipeDataArray.IndexKeyId = SfcRecipeDataKeyMaster.KeyId
;
CREATE TABLE SfcRecipeDataInput(
	RecipeDataId int NOT NULL,
	ValueTypeId int NULL,
	Tag varchar(500) NULL,
	ErrorCode varchar(50) NULL,
	ErrorText varchar(5000) NULL,
	PVMonitorActive bit NULL,
	PVMonitorStatus varchar(25) NULL,
	PVValueId int NULL,
	TargetValueId int NULL
) ;
CREATE TABLE SfcRecipeDataArrayElement(
	RecipeDataId int NOT NULL,
	ArrayIndex int NOT NULL,
	ValueId int NULL
) ;
CREATE VIEW SfcRecipeDataTimerView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeDataType.JavaClassName, SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, 
                      SfcRecipeDataTimer.StartTime, SfcRecipeDataTimer.StopTime, SfcRecipeDataTimer.TimerState, SfcRecipeDataTimer.CumulativeMinutes, 
                      SfcRecipeData.RecipeDataFolderId AS FolderId, SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcRecipeData ON SfcStep.StepId = SfcRecipeData.StepId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcRecipeDataTimer ON SfcRecipeData.RecipeDataId = SfcRecipeDataTimer.RecipeDataId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE TABLE SfcRecipeDataOutput(
	RecipeDataId int NOT NULL,
	ValueTypeId int NOT NULL,
	OutputTypeId int NOT NULL,
	Tag varchar(500) NULL,
	Download bit NULL,
	DownloadStatus varchar(50) NULL,
	ErrorCode varchar(50) NULL,
	ErrorText varchar(1000) NULL,
	Timing float NULL,
	MaxTiming float NULL,
	ActualTiming float NULL,
	ActualDateTime datetime NULL,
	OutputValueId int NOT NULL,
	TargetValueId int NOT NULL,
	PVValueId int NOT NULL,
	PVMonitorActive bit NULL,
	PVMonitorStatus varchar(50) NULL,
	SetpointStatus varchar(50) NULL,
	WriteConfirm bit NULL,
	WriteConfirmed bit NULL
) ;
CREATE VIEW SfcRecipeDataMatrixView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, SfcRecipeDataType.RecipeDataType, SfcValueType.ValueType, SfcRecipeDataMatrix.Rows, 
                      SfcRecipeDataMatrix.Columns, SfcRecipeDataMatrix.RowIndexKeyId, SfcRecipeDataKeyMaster.KeyName AS RowIndexKeyName, SfcRecipeDataMatrix.ColumnIndexKeyId, 
                      SfcRecipeDataKeyMaster_1.KeyName AS ColumnIndexKeyName, SfcRecipeData.RecipeDataFolderId AS FolderId, SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcRecipeData ON SfcStep.StepId = SfcRecipeData.StepId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcRecipeDataMatrix ON SfcRecipeData.RecipeDataId = SfcRecipeDataMatrix.RecipeDataId INNER JOIN
                      SfcValueType ON SfcRecipeDataMatrix.ValueTypeId = SfcValueType.ValueTypeId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId LEFT OUTER JOIN
                      SfcRecipeDataKeyMaster ON SfcRecipeDataMatrix.RowIndexKeyId = SfcRecipeDataKeyMaster.KeyId LEFT OUTER JOIN
                      SfcRecipeDataKeyMaster AS SfcRecipeDataKeyMaster_1 ON SfcRecipeDataMatrix.ColumnIndexKeyId = SfcRecipeDataKeyMaster_1.KeyId
;
CREATE TABLE SfcRecipeDataMatrixElement(
	RecipeDataId int NOT NULL,
	RowIndex int NOT NULL,
	ColumnIndex int NOT NULL,
	ValueId int NOT NULL
) ;
CREATE TABLE SfcRecipeDataSimpleValue(
	RecipeDataId int NOT NULL,
	ValueTypeId int NULL,
	ValueId int NULL
) ;
CREATE VIEW SfcRecipeDataRecipeView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, SfcRecipeDataRecipe.PresentationOrder, 
                      SfcRecipeDataRecipe.StoreTag, SfcRecipeDataRecipe.CompareTag, SfcRecipeDataRecipe.ModeAttribute, SfcRecipeDataRecipe.ModeValue, 
                      SfcRecipeDataRecipe.ChangeLevel, SfcRecipeDataRecipe.RecommendedValue, SfcRecipeDataRecipe.LowLimit, SfcRecipeDataRecipe.HighLimit, 
                      SfcRecipeData.RecipeDataFolderId AS FolderId, SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcRecipeData ON SfcStep.StepId = SfcRecipeData.StepId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcRecipeDataRecipe ON SfcRecipeData.RecipeDataId = SfcRecipeDataRecipe.RecipeDataId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE TABLE SfcSelectInput(
	windowId int NOT NULL,
	prompt varchar(900) NOT NULL,
	choicesStepId int NOT NULL,
	choicesKey varchar(500) NOT NULL,
	responseLocation varchar(25) NULL,
	targetStepId int NULL,
	keyAndAttribute varchar(255) NULL,
	chartId varchar(200) NULL,
	stepId varchar(200) NULL
) ;
CREATE TABLE SfcSaveData(
	windowId int NOT NULL,
	textData varchar(1026) NULL,
	binaryData varbinary(1026) NULL,
	fileLocation varchar(25) NOT NULL,
	printText bit NOT NULL,
	viewText bit NOT NULL,
	showPrintDialog bit NOT NULL,
	filePath varchar(900) NULL
) ; 
CREATE TABLE SfcTimeDelayNotification(
	windowId int NOT NULL,
	message varchar(900) NOT NULL,
	endTime datetime NOT NULL
) ;
CREATE VIEW SfcRecipeDataSQCView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, SfcRecipeDataSQC.LowLimit, SfcRecipeDataSQC.TargetValue, SfcRecipeDataSQC.HighLimit, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeDataType.JavaClassName, SfcRecipeData.RecipeDataFolderId AS FolderId, 
                      SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcRecipeData ON SfcStep.StepId = SfcRecipeData.StepId INNER JOIN
                      SfcRecipeDataSQC ON SfcRecipeData.RecipeDataId = SfcRecipeDataSQC.RecipeDataId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE TABLE SfcReviewData(
	windowId int NOT NULL,
	showAdvice bit NOT NULL,
	targetStepUUID varchar(255) NULL,
	responseKey varchar(255) NULL,
	primaryTabLabel varchar(100) NULL,
	secondaryTabLabel varchar(100) NULL
) ;
CREATE TABLE SfcReviewFlows(
	windowId int NOT NULL,
	heading1 varchar(900) NOT NULL,
	heading2 varchar(900) NOT NULL,
	heading3 varchar(900) NOT NULL,
	targetStepUUID varchar(255) NOT NULL,
	responseKey varchar(255) NOT NULL,
	primaryTabLabel varchar(100) NULL,
	secondaryTabLabel varchar(100) NULL
) ;
CREATE TABLE SfcReviewFlowsTable(
	windowId int NOT NULL,
	rowNum int NOT NULL,
	configKey varchar(150) NOT NULL,
	advice varchar(900) NOT NULL,
	units varchar(900) NOT NULL,
	prompt varchar(900) NOT NULL,
	data1 varchar(150) NOT NULL,
	data2 varchar(150) NOT NULL,
	data3 varchar(150) NOT NULL,
	isPrimary bit NOT NULL
) ;
CREATE TABLE SfcReviewDataTable(
	windowId int NOT NULL,
	rowNum int NOT NULL,
	configKey varchar(150) NULL,
	prompt varchar(150) NULL,
	value varchar(150) NULL,
	units varchar(20) NULL,
	isPrimary bit NOT NULL,
	advice varchar(150) NULL
) ;
CREATE VIEW SfcRecipeDataSimpleValueView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeDataType.RecipeDataType, 
                      SfcRecipeDataType.JavaClassName, SfcRecipeData.RecipeDataKey, SfcRecipeData.Description, SfcRecipeData.Label, SfcRecipeData.Units, SfcValueType.ValueType, 
                      SfcRecipeDataSimpleValue.ValueId, SfcRecipeDataValue.FloatValue, SfcRecipeDataValue.IntegerValue, SfcRecipeDataValue.StringValue, SfcRecipeDataValue.BooleanValue, 
                      SfcRecipeDataSimpleValue.ValueTypeId, SfcRecipeData.RecipeDataFolderId AS FolderId, SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcRecipeData INNER JOIN
                      SfcRecipeDataSimpleValue ON SfcRecipeData.RecipeDataId = SfcRecipeDataSimpleValue.RecipeDataId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcValueType ON SfcRecipeDataSimpleValue.ValueTypeId = SfcValueType.ValueTypeId INNER JOIN
                      SfcStep ON SfcRecipeData.StepId = SfcStep.StepId INNER JOIN
                      SfcChart ON SfcStep.ChartId = SfcChart.ChartId INNER JOIN
                      SfcRecipeDataValue ON SfcRecipeDataSimpleValue.ValueId = SfcRecipeDataValue.ValueId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE VIEW SfcRecipeDataOutputView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepName, SfcStep.StepUUID, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeDataType.JavaClassName, SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeDataOutput.Tag, 
                      SfcRecipeData.Units, SfcValueType.ValueType, SfcRecipeDataOutputType.OutputType, SfcRecipeDataOutput.Download, SfcRecipeDataOutput.DownloadStatus, 
                      SfcRecipeDataOutput.ErrorCode, SfcRecipeDataOutput.ErrorText, SfcRecipeDataOutput.Timing, SfcRecipeDataOutput.MaxTiming, SfcRecipeDataOutput.ActualTiming, 
                      SfcRecipeDataOutput.ActualDateTime, SfcRecipeDataOutput.PVMonitorActive, SfcRecipeDataOutput.PVMonitorStatus, SfcRecipeDataOutput.WriteConfirm, 
                      SfcRecipeDataOutput.WriteConfirmed, SfcRecipeDataOutput.OutputValueId, SfcRecipeDataValue.FloatValue AS OutputFloatValue, 
                      SfcRecipeDataValue.IntegerValue AS OutputIntegerValue, SfcRecipeDataValue.StringValue AS OutputStringValue, SfcRecipeDataValue.BooleanValue AS OutputBooleanValue, 
                      SfcRecipeDataOutput.TargetValueId, SfcRecipeDataValue_1.FloatValue AS TargetFloatValue, SfcRecipeDataValue_1.IntegerValue AS TargetIntegerValue, 
                      SfcRecipeDataValue_1.StringValue AS TargetStringValue, SfcRecipeDataValue_1.BooleanValue AS TargetBooleanValue, SfcRecipeDataOutput.PVValueId, 
                      SfcRecipeDataValue_2.FloatValue AS PVFloatValue, SfcRecipeDataValue_2.IntegerValue AS PVIntegerValue, SfcRecipeDataValue_2.StringValue AS PVStringValue, 
                      SfcRecipeDataValue_2.BooleanValue AS PVBooleanValue, SfcRecipeDataOutput.SetpointStatus, SfcRecipeDataOutput.ValueTypeId, SfcRecipeDataOutput.OutputTypeId, 
                      SfcRecipeData.RecipeDataFolderId AS FolderId, SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcRecipeDataOutput INNER JOIN
                      SfcRecipeDataOutputType ON SfcRecipeDataOutput.OutputTypeId = SfcRecipeDataOutputType.OutputTypeId INNER JOIN
                      SfcRecipeData ON SfcRecipeDataOutput.RecipeDataId = SfcRecipeData.RecipeDataId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcValueType ON SfcRecipeDataOutput.ValueTypeId = SfcValueType.ValueTypeId INNER JOIN
                      SfcStep ON SfcRecipeData.StepId = SfcStep.StepId INNER JOIN
                      SfcChart ON SfcStep.ChartId = SfcChart.ChartId INNER JOIN
                      SfcRecipeDataValue ON SfcRecipeDataOutput.OutputValueId = SfcRecipeDataValue.ValueId INNER JOIN
                      SfcRecipeDataValue AS SfcRecipeDataValue_1 ON SfcRecipeDataOutput.TargetValueId = SfcRecipeDataValue_1.ValueId INNER JOIN
                      SfcRecipeDataValue AS SfcRecipeDataValue_2 ON SfcRecipeDataOutput.PVValueId = SfcRecipeDataValue_2.ValueId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE VIEW SfcRecipeDataOutputRampView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, SfcRecipeDataOutput.Tag, 
                      SfcRecipeDataOutputType.OutputType, SfcValueType.ValueType, SfcRecipeDataOutput.Download, SfcRecipeDataOutput.DownloadStatus, SfcRecipeDataOutput.ErrorCode, 
                      SfcRecipeDataOutput.ErrorText, SfcRecipeDataOutput.Timing, SfcRecipeDataOutput.MaxTiming, SfcRecipeDataOutput.ActualTiming, SfcRecipeDataOutput.ActualDateTime, 
                      SfcRecipeDataOutput.PVMonitorActive, SfcRecipeDataOutput.PVMonitorStatus, SfcRecipeDataOutput.WriteConfirm, SfcRecipeDataOutput.WriteConfirmed, 
                      SfcRecipeDataOutputRamp.RampTimeMinutes, SfcRecipeDataOutputRamp.UpdateFrequencySeconds, SfcRecipeDataOutput.OutputValueId, 
                      SfcRecipeDataValue.FloatValue AS OutputFloatValue, SfcRecipeDataValue.IntegerValue AS OutputIntegerValue, SfcRecipeDataValue.StringValue AS OutputStringValue, 
                      SfcRecipeDataValue.BooleanValue AS OutputBooleanValue, SfcRecipeDataOutput.TargetValueId, SfcRecipeDataValue_1.FloatValue AS TargetFloatValue, 
                      SfcRecipeDataValue_1.IntegerValue AS TargetIntegerValue, SfcRecipeDataValue_1.StringValue AS TargetStringValue, SfcRecipeDataValue_1.BooleanValue AS TargetBooleanValue, 
                      SfcRecipeDataOutput.PVValueId, SfcRecipeDataValue_2.FloatValue AS PVFloatValue, SfcRecipeDataValue_2.IntegerValue AS PVIntegerValue, 
                      SfcRecipeDataValue_2.StringValue AS PVStringValue, SfcRecipeDataValue_2.BooleanValue AS PVBooleanValue, SfcRecipeData.RecipeDataFolderId AS FolderId, 
                      SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcRecipeData INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcRecipeDataOutput ON SfcRecipeData.RecipeDataId = SfcRecipeDataOutput.RecipeDataId INNER JOIN
                      SfcRecipeDataOutputRamp ON SfcRecipeData.RecipeDataId = SfcRecipeDataOutputRamp.RecipeDataId INNER JOIN
                      SfcValueType ON SfcRecipeDataOutput.ValueTypeId = SfcValueType.ValueTypeId INNER JOIN
                      SfcRecipeDataOutputType ON SfcRecipeDataOutput.OutputTypeId = SfcRecipeDataOutputType.OutputTypeId INNER JOIN
                      SfcStep ON SfcRecipeData.StepId = SfcStep.StepId INNER JOIN
                      SfcChart ON SfcStep.ChartId = SfcChart.ChartId INNER JOIN
                      SfcRecipeDataValue ON SfcRecipeDataOutput.OutputValueId = SfcRecipeDataValue.ValueId INNER JOIN
                      SfcRecipeDataValue AS SfcRecipeDataValue_1 ON SfcRecipeDataOutput.TargetValueId = SfcRecipeDataValue_1.ValueId INNER JOIN
                      SfcRecipeDataValue AS SfcRecipeDataValue_2 ON SfcRecipeDataOutput.PVValueId = SfcRecipeDataValue_2.ValueId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE VIEW SfcRecipeDataMatrixElementView
AS
SELECT     SfcRecipeDataMatrixElement.RecipeDataId, SfcRecipeDataMatrixElement.RowIndex, SfcRecipeDataMatrixElement.ColumnIndex, 
                      SfcRecipeDataValue.FloatValue, SfcRecipeDataValue.IntegerValue, SfcRecipeDataValue.StringValue, SfcRecipeDataValue.BooleanValue
FROM         SfcRecipeDataMatrixElement INNER JOIN
                      SfcRecipeDataValue ON SfcRecipeDataMatrixElement.ValueId = SfcRecipeDataValue.ValueId
ORDER BY SfcRecipeDataMatrixElement.RecipeDataId, SfcRecipeDataMatrixElement.RowIndex, SfcRecipeDataMatrixElement.ColumnIndex
;
CREATE VIEW SfcRecipeDataInputView
AS
SELECT     SfcChart.ChartId, SfcChart.ChartPath, SfcStep.StepId, SfcStep.StepUUID, SfcStep.StepName, SfcRecipeData.RecipeDataId, SfcRecipeData.RecipeDataKey, 
                      SfcRecipeDataType.RecipeDataType, SfcRecipeDataType.JavaClassName, SfcRecipeData.Label, SfcRecipeData.Description, SfcRecipeData.Units, 
                      SfcRecipeDataInput.Tag, SfcRecipeDataInput.ErrorCode, SfcRecipeDataInput.ErrorText, SfcRecipeDataInput.PVMonitorActive, SfcRecipeDataInput.PVMonitorStatus, 
                      SfcValueType.ValueType, SfcRecipeDataInput.PVValueId, SfcRecipeDataValue.FloatValue AS PVFloatValue, SfcRecipeDataValue.IntegerValue AS PVIntegerValue, 
                      SfcRecipeDataValue.StringValue AS PVStringValue, SfcRecipeDataValue.BooleanValue AS PVBooleanValue, SfcRecipeDataInput.TargetValueId, 
                      SfcRecipeDataValue_1.FloatValue AS TargetFloatValue, SfcRecipeDataValue_1.IntegerValue AS TargetIntegerValue, SfcRecipeDataValue_1.StringValue AS TargetStringValue, 
                      SfcRecipeDataValue_1.BooleanValue AS TargetBooleanValue, SfcRecipeDataInput.ValueTypeId, SfcRecipeData.RecipeDataFolderId AS FolderId, 
                      SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         SfcChart INNER JOIN
                      SfcStep ON SfcChart.ChartId = SfcStep.ChartId INNER JOIN
                      SfcRecipeData ON SfcStep.StepId = SfcRecipeData.StepId INNER JOIN
                      SfcRecipeDataType ON SfcRecipeData.RecipeDataTypeId = SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      SfcRecipeDataInput ON SfcRecipeData.RecipeDataId = SfcRecipeDataInput.RecipeDataId INNER JOIN
                      SfcRecipeDataValue ON SfcRecipeDataInput.PVValueId = SfcRecipeDataValue.ValueId INNER JOIN
                      SfcRecipeDataValue AS SfcRecipeDataValue_1 ON SfcRecipeDataInput.TargetValueId = SfcRecipeDataValue_1.ValueId INNER JOIN
                      SfcValueType ON SfcRecipeDataInput.ValueTypeId = SfcValueType.ValueTypeId LEFT OUTER JOIN
                      SfcRecipeDataFolder ON SfcRecipeData.RecipeDataFolderId = SfcRecipeDataFolder.RecipeDataFolderId
;
CREATE VIEW SfcRecipeDataArrayElementView
AS
SELECT     SfcRecipeDataArrayElement.RecipeDataId, SfcRecipeDataArrayElement.ArrayIndex, SfcRecipeDataArrayElement.ValueId, SfcRecipeDataValue.FloatValue, 
                      SfcRecipeDataValue.IntegerValue, SfcRecipeDataValue.StringValue, SfcRecipeDataValue.BooleanValue
FROM         SfcRecipeDataArrayElement INNER JOIN
                      SfcRecipeDataValue ON SfcRecipeDataArrayElement.ValueId = SfcRecipeDataValue.ValueId
;
CREATE TABLE SfcManualDataEntryTable(
	windowId int NOT NULL,
	rowNum int NOT NULL,
	description varchar(900) NOT NULL,
	value varchar(900) NOT NULL,
	units varchar(900) NOT NULL,
	dataKey varchar(900) NOT NULL,
	destination varchar(900) NOT NULL,
	targetStepId int NULL,
	type varchar(900) NOT NULL,
	recipeUnits varchar(900) NOT NULL,
	lowLimit float NULL,
	highLimit float NULL
) ;
CREATE TABLE SfcDownloadGUITable(
	windowId int NOT NULL,
	RecipeDataId int NULL,
	RecipeDataType varchar(50) NULL,
	LabelAttribute varchar(50) NULL,
	RawTiming float NULL,
	Timing float NULL,
	DcsTagId varchar(900) NULL,
	SetPoint varchar(50) NULL,
	Description varchar(900) NULL,
	StepTimestamp varchar(900) NULL,
	PV varchar(50) NULL,
	DownloadStatus varchar(900) NULL,
	PVMonitorStatus varchar(900) NULL,
	SetpointStatus varchar(900) NULL
) ;
CREATE VIEW RtSQCLimitView
AS
SELECT     RtRecipeFamily.RecipeFamilyName, RtSQCParameter.Parameter, RtSQCLimit.Grade, RtSQCLimit.UpperLimit, RtSQCLimit.LowerLimit
FROM         RtSQCParameter INNER JOIN
                      RtSQCLimit ON RtSQCParameter.ParameterId = RtSQCLimit.ParameterId INNER JOIN
                      RtRecipeFamily ON RtSQCParameter.RecipeFamilyId = RtRecipeFamily.RecipeFamilyId
;
CREATE TABLE LtRelatedData(
	RelatedDataId int PRIMARY KEY NOT NULL,
	DerivedValueId int NOT NULL,
	RelatedValueId int NOT NULL
) ;
CREATE VIEW LtPHDValueView
AS
SELECT     LtValue.ValueId, LtValue.ValueName, LtValue.Description, LtValue.DisplayDecimals, LtValue.LastHistoryId, LtPHDValue.ItemId, LtHDAInterface.InterfaceName, 
                      TkPost.Post, TkUnit.UnitName, LtValue.ValidationProcedure, LtHistory.SampleTime, LtPHDValue.AllowManualEntry
FROM         LtValue INNER JOIN
                      LtPHDValue ON LtValue.ValueId = LtPHDValue.ValueId INNER JOIN
                      LtHDAInterface ON LtPHDValue.InterfaceId = LtHDAInterface.InterfaceId INNER JOIN
                      TkUnit ON LtValue.UnitId = TkUnit.UnitId INNER JOIN
                      TkPost ON TkUnit.PostId = TkPost.PostId LEFT OUTER JOIN
                      LtHistory ON LtValue.LastHistoryId = LtHistory.HistoryId
;
CREATE VIEW RtGainView
AS
SELECT     RtRecipeFamily.RecipeFamilyId, RtRecipeFamily.RecipeFamilyName, RtGain.ParameterId, RtGain.Parameter, RtGainGrade.Grade, RtGainGrade.Gain
FROM         RtRecipeFamily INNER JOIN
                      RtGain ON RtRecipeFamily.RecipeFamilyId = RtGain.RecipeFamilyId INNER JOIN
                      RtGainGrade ON RtGain.ParameterId = RtGainGrade.ParameterId
;
CREATE VIEW LtValueView
AS
SELECT     TkUnit.UnitName, LtValue.ValueName, LtValue.ValueId, LtHistory.SampleTime, LtHistory.RawValue, LtHistory.ReportTime, LtHistory.Grade
FROM         TkUnit INNER JOIN
                      LtValue ON TkUnit.UnitId = LtValue.UnitId LEFT OUTER JOIN
                      LtHistory ON LtValue.ValueId = LtHistory.ValueId
;
CREATE VIEW LtLimitView
AS
SELECT     LtLimit.LimitId, LtValue.ValueId, LtValue.ValueName, LtValue.Description, TkUnit.UnitName, TkPost.Post, LtLimit.UpperValidityLimit, LtLimit.LowerValidityLimit, 
                      LtLimit.UpperSQCLimit, LtLimit.LowerSQCLimit, LtLimit.UpperReleaseLimit, LtLimit.LowerReleaseLimit, LtLimit.Target, LtLimit.StandardDeviation, 
                      LtValue.ValidationProcedure, LtLimit.RecipeParameterName, LtLimit.OPCUpperItemId, LtLimit.OPCLowerItemId, Lookup.LookupName AS LimitType, 
                      Lookup_1.LookupName AS LimitSource, LtOPCInterface.InterfaceName
FROM         LtValue INNER JOIN
                      TkUnit ON LtValue.UnitId = TkUnit.UnitId INNER JOIN
                      TkPost ON TkUnit.PostId = TkPost.PostId INNER JOIN
                      LtLimit ON LtValue.ValueId = LtLimit.ValueId INNER JOIN
                      Lookup ON LtLimit.LimitTypeId = Lookup.LookupId INNER JOIN
                      Lookup AS Lookup_1 ON LtLimit.LimitSourceId = Lookup_1.LookupId LEFT OUTER JOIN
                      LtOPCInterface ON LtLimit.OPCInterfaceId = LtOPCInterface.InterfaceId
WHERE     (Lookup.LookupTypeCode = 'RtLimitType') AND (Lookup_1.LookupTypeCode = 'RtLimitSource')
;
CREATE VIEW LtLocalValueView
AS
SELECT     LtValue.ValueId, LtValue.ValueName, TkUnit.UnitName, LtValue.DisplayDecimals, LtValue.Description, LtValue.ValidationProcedure, LtHDAInterface.InterfaceName, 
                      LtLocalValue.ItemId
FROM         LtValue INNER JOIN
                      LtLocalValue ON LtValue.ValueId = LtLocalValue.ValueId INNER JOIN
                      LtHDAInterface ON LtLocalValue.InterfaceId = LtHDAInterface.InterfaceId INNER JOIN
                      TkUnit ON LtValue.UnitId = TkUnit.UnitId
;
CREATE VIEW LtSelectorView
AS
SELECT     LtValue.ValueId, LtValue.ValueName, LtValue.Description, LtHistory.SampleTime, LtValue_1.ValueId AS SourceValueId, LtValue_1.ValueName AS SourceValueName, 
                      LtValue.LastHistoryId
FROM         LtValue INNER JOIN
                      LtSelector ON LtValue.ValueId = LtSelector.ValueId LEFT OUTER JOIN
                      LtValue AS LtValue_1 ON LtSelector.sourceValueId = LtValue_1.ValueId LEFT OUTER JOIN
                      LtHistory ON LtValue.LastHistoryId = LtHistory.HistoryId
;
CREATE VIEW LtLastValueView
AS
SELECT     LtValue.ValueId, LtHistory.HistoryId, LtValue.ValueName, LtHistory.RawValue, LtHistory.SampleTime, LtHistory.ReportTime
FROM         LtHistory RIGHT OUTER JOIN
                      LtValue ON LtHistory.HistoryId = LtValue.LastHistoryId
CREATE VIEW LtDerivedValueView
AS
SELECT     LtValue.ValueName, LtValue.ValueId, LtDerivedValue.DerivedValueId, TkUnit.UnitName, LtValue_1.ValueName AS TriggerValueName, LtValue_1.ValueId AS TriggerValueId, 
                      TkUnit_1.UnitName AS TriggerUnitName, LtDerivedValue.Callback, LtDerivedValue.SampleTimeTolerance, LtDerivedValue.NewSampleWaitTime, 
                      LtHistory.RawValue AS TriggerRawValue, LtHistory.SampleTime AS TriggerSampleTime, LtHistory.ReportTime AS TriggerReportTime, LtHDAInterface.InterfaceName, 
                      LtDerivedValue.ResultItemId
FROM         LtValue INNER JOIN
                      LtDerivedValue ON LtValue.ValueId = LtDerivedValue.ValueId INNER JOIN
                      LtValue AS LtValue_1 ON LtDerivedValue.TriggerValueId = LtValue_1.ValueId INNER JOIN
                      TkUnit ON LtValue.UnitId = TkUnit.UnitId INNER JOIN
                      TkUnit AS TkUnit_1 ON LtValue_1.UnitId = TkUnit_1.UnitId INNER JOIN
                      LtHDAInterface ON LtDerivedValue.ResultInterfaceId = LtHDAInterface.InterfaceId LEFT OUTER JOIN
                      LtHistory ON LtValue_1.LastHistoryId = LtHistory.HistoryId
;
CREATE TABLE DtFinalDiagnosis(
	FinalDiagnosisId int PRIMARY KEY NOT NULL,
	FinalDiagnosisName varchar(250) NOT NULL,
	FinalDiagnosisLabel varchar(250) NULL,
	FamilyId int NOT NULL,
	FinalDiagnosisPriority float NOT NULL,
	CalculationMethod varchar(1000) NULL,
	Constant bit NULL,
	PostTextRecommendation bit NOT NULL,
	PostProcessingCallback varchar(1000) NULL,
	RefreshRate int NOT NULL,
	Comment varchar(1000) NULL,
	TextRecommendation varchar(1000) NULL,
	State bit NULL,
	Active bit NOT NULL,
	Explanation varchar(1000) NULL,
	TrapInsignificantRecommendations bit NULL,
	LastRecommendationTime datetime NULL,
	TimeOfMostRecentRecommendationImplementation datetime NOT NULL,
	FinalDiagnosisUUID varchar(100) NULL,
	DiagramUUID varchar(100) NULL,
	ManualMoveAllowed bit NULL,
	ManualMove float NULL,
	ShowExplanationWithRecommendation bit NOT NULL
) ;
CREATE TABLE DtQuantOutputRamp(
	QuantOutputId int NOT NULL,
	Ramp float NULL,
	RampTypeId int NULL
) ;
CREATE VIEW DtQuantOutputDefinitionView
AS
SELECT     DtApplication.ApplicationName, DtQuantOutput.QuantOutputName, DtQuantOutput.QuantOutputId, DtQuantOutput.TagPath, 
                      DtQuantOutput.MostNegativeIncrement, DtQuantOutput.MostPositiveIncrement, DtQuantOutput.IgnoreMinimumIncrement, DtQuantOutput.MinimumIncrement, 
                      DtQuantOutput.SetpointHighLimit, DtQuantOutput.SetpointLowLimit, DtQuantOutput.IncrementalOutput
FROM         DtApplication INNER JOIN
                      DtQuantOutput ON DtApplication.ApplicationId = DtQuantOutput.ApplicationId
ORDER BY DtApplication.ApplicationName, DtQuantOutput.QuantOutputName
;
CREATE TABLE DtSQCDiagnosis(
	SQCDiagnosisId int PRIMARY KEY NOT NULL,
	SQCDiagnosisName varchar(50) NOT NULL,
	SQCDiagnosisLabel varchar(50) NULL,
	Status varchar(50) NOT NULL,
	FamilyId int NOT NULL,
	SQCDiagnosisUUID varchar(100) NULL,
	DiagramUUID varchar(100) NULL,
	LastResetTime datetime NULL
) ;
CREATE VIEW LtDCSValueView
AS
SELECT     TkPost.Post, TkUnit.UnitName, LtValue.ValueId, LtValue.ValueName, LtValue.DisplayDecimals, LtValue.Description, LtValue.ValidationProcedure, 
                      LtOPCInterface.InterfaceName, LtDCSValue.ItemId, LtValue.LastHistoryId, LtHistory.RawValue, LtHistory.SampleTime, LtDCSValue.AllowManualEntry
FROM         TkPost INNER JOIN
                      TkUnit ON TkPost.PostId = TkUnit.PostId INNER JOIN
                      LtValue ON TkUnit.UnitId = LtValue.UnitId INNER JOIN
                      LtDCSValue ON LtValue.ValueId = LtDCSValue.ValueId INNER JOIN
                      LtOPCInterface ON LtDCSValue.InterfaceId = LtOPCInterface.InterfaceId INNER JOIN
                      LtHistory ON LtValue.LastHistoryId = LtHistory.HistoryId
;
CREATE VIEW DtSQCDiagnosisView
AS
SELECT     DtApplication.ApplicationName, DtFamily.FamilyName, DtSQCDiagnosis.SQCDiagnosisName, DtSQCDiagnosis.SQCDiagnosisLabel, DtSQCDiagnosis.Status, 
                      DtSQCDiagnosis.SQCDiagnosisUUID, DtSQCDiagnosis.DiagramUUID, DtSQCDiagnosis.LastResetTime
FROM         DtApplication INNER JOIN
                      DtFamily ON DtApplication.ApplicationId = DtFamily.ApplicationId RIGHT OUTER JOIN
                      DtSQCDiagnosis ON DtFamily.FamilyId = DtSQCDiagnosis.FamilyId
;
CREATE VIEW DtFinalDiagnosisView
AS
SELECT     DtApplication.ApplicationName, DtFamily.FamilyId, DtFamily.FamilyName, DtFamily.FamilyPriority, DtFinalDiagnosis.FinalDiagnosisId, 
                      DtFinalDiagnosis.FinalDiagnosisName, DtFinalDiagnosis.FinalDiagnosisLabel, DtFinalDiagnosis.FinalDiagnosisPriority, DtFinalDiagnosis.CalculationMethod, 
                      DtFinalDiagnosis.PostProcessingCallback, DtFinalDiagnosis.PostTextRecommendation, DtFinalDiagnosis.RefreshRate, DtFinalDiagnosis.TextRecommendation, 
                      DtFinalDiagnosis.Active, DtFinalDiagnosis.Explanation, DtFinalDiagnosis.TrapInsignificantRecommendations, DtFinalDiagnosis.LastRecommendationTime, 
                      DtFinalDiagnosis.TimeOfMostRecentRecommendationImplementation, DtFinalDiagnosis.Constant, DtFinalDiagnosis.DiagramUUID, DtFinalDiagnosis.FinalDiagnosisUUID, 
                      DtFinalDiagnosis.ManualMove, DtFinalDiagnosis.ManualMoveAllowed, DtFinalDiagnosis.Comment
FROM         DtApplication INNER JOIN
                      DtFamily ON DtApplication.ApplicationId = DtFamily.ApplicationId INNER JOIN
                      DtFinalDiagnosis ON DtFamily.FamilyId = DtFinalDiagnosis.FamilyId
;
CREATE VIEW DtFinalDiagnosisLogView
AS
SELECT     DtFinalDiagnosisLog.LogId, DtFinalDiagnosisLog.Timestamp, DtApplication.ApplicationName, DtFamily.FamilyName, DtFamily.FamilyPriority, 
                      DtFinalDiagnosis.FinalDiagnosisName, DtFinalDiagnosisLog.FinalDiagnosisId, DtFinalDiagnosis.FinalDiagnosisPriority, DtFinalDiagnosisLog.State, 
                      DtFinalDiagnosisLog.Active
FROM         DtFinalDiagnosisLog INNER JOIN
                      DtFinalDiagnosis ON DtFinalDiagnosisLog.FinalDiagnosisId = DtFinalDiagnosis.FinalDiagnosisId INNER JOIN
                      DtFamily ON DtFinalDiagnosis.FamilyId = DtFamily.FamilyId INNER JOIN
                      DtApplication ON DtFamily.ApplicationId = DtApplication.ApplicationId
;
CREATE TABLE DtRecommendationDefinition(
	RecommendationDefinitionId int PRIMARY KEY NOT NULL,
	FinalDiagnosisId int NOT NULL,
	QuantOutputId int NOT NULL
) ;
CREATE TABLE DtRecommendation(
	RecommendationId int PRIMARY KEY NOT NULL,
	RecommendationDefinitionId int NOT NULL,
	DiagnosisEntryId int NOT NULL,
	Recommendation float NOT NULL,
	AutoRecommendation float NOT NULL,
	ManualRecommendation float NULL,
	AutoOrManual varchar(50) NOT NULL,
	RampTime float NULL
) ;
CREATE VIEW DtQuantOutputView
AS
SELECT      DtFinalDiagnosis.FinalDiagnosisName, DtFinalDiagnosis.FinalDiagnosisId, DtQuantOutput.QuantOutputName, DtQuantOutput.QuantOutputId, 
                      DtQuantOutput.TagPath
FROM         DtFinalDiagnosis INNER JOIN
                      DtRecommendationDefinition ON DtFinalDiagnosis.FinalDiagnosisId = DtRecommendationDefinition.FinalDiagnosisId INNER JOIN
                      DtQuantOutput ON DtRecommendationDefinition.QuantOutputId = DtQuantOutput.QuantOutputId
ORDER BY DtFinalDiagnosis.FinalDiagnosisName, DtQuantOutput.QuantOutputName
;
