SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [DtFinalDiagnosisLog](
	[LogId] [int] IDENTITY(1,1) NOT NULL,
	[Timestamp] [datetime] NOT NULL,
	[FinalDiagnosisId] [int] NOT NULL,
	[State] [bit] NULL,
	[Active] [bit] NULL,
 CONSTRAINT [PK_DtFinalDiagnosisLog] PRIMARY KEY CLUSTERED 
(
	[LogId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtDiagnosisEntry](
	[DiagnosisEntryId] [int] IDENTITY(1,1) NOT NULL,
	[FinalDiagnosisId] [int] NOT NULL,
	[Status] [varchar](50) NOT NULL,
	[Timestamp] [datetime] NOT NULL,
	[Grade] [varchar](50) NOT NULL,
	[TextRecommendation] [varchar](1000) NOT NULL,
	[RecommendationStatus] [varchar](50) NOT NULL,
	[Multiplier] [float] NOT NULL,
	[RecommendationErrorText] [varchar](1000) NULL,
 CONSTRAINT [PK_DtDiagnosisEntry] PRIMARY KEY CLUSTERED 
(
	[DiagnosisEntryId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [alarm_events](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[eventid] [varchar](255) NULL,
	[source] [varchar](255) NULL,
	[displaypath] [varchar](255) NULL,
	[priority] [int] NULL,
	[eventtype] [int] NULL,
	[eventflags] [int] NULL,
	[eventtime] [datetime] NULL,
PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [alarm_event_data](
	[id] [int] NULL,
	[propname] [varchar](255) NULL,
	[dtype] [int] NULL,
	[intvalue] [bigint] NULL,
	[floatvalue] [float] NULL,
	[strvalue] [varchar](255) NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE NONCLUSTERED INDEX [alarm_event_dataidndx] ON [alarm_event_data] 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtApplicationManageQueue](
	[ApplicationName] [varchar](250) NOT NULL,
	[Provider] [varchar](50) NOT NULL,
	[Timestamp] [datetime] NOT NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [BtReactor](
	[ReactorId] [int] IDENTITY(1,1) NOT NULL,
	[ReactorName] [varchar](50) NOT NULL,
	[TagName] [varchar](50) NOT NULL,
 CONSTRAINT [PK_BtReactor] PRIMARY KEY CLUSTERED 
(
	[ReactorId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [HBLabReport](
	[Id] [bigint] IDENTITY(1,1) NOT NULL,
	[LabValue] [float] NULL,
	[USpec] [float] NULL,
	[UCtl] [float] NULL,
	[Target] [float] NULL,
	[LCtl] [float] NULL,
	[LSpec] [float] NULL,
	[ReceiveTime] [datetime] NULL,
	[ReceiveText] [text] NULL,
	[SampleTime] [datetime] NULL,
	[Status] [varchar](50) NULL,
	[EventDescription] [varchar](50) NULL,
	[BoxNoteText] [varchar](300) NULL,
	[OCConfirm] [varchar](50) NULL,
	[DeleteFlag] [bit] NULL,
	[ReasonForRejection] [varchar](300) NULL,
	[UIRStatus] [varchar](50) NULL,
	[ActionTaken] [varchar](300) NULL,
	[DescriptionText] [varchar](300) NULL,
	[CurrentGrade] [varchar](50) NULL,
	[ProblemText] [varchar](300) NULL,
	[UIRId] [int] NULL,
	[BackgroundColor] [varchar](20) NULL,
	[ForegroundColor] [varchar](20) NULL,
 CONSTRAINT [PK_HBLabReport] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LookupType](
	[LookupTypeCode] [varchar](15) NOT NULL,
	[LookupTypeName] [varchar](50) NOT NULL,
	[LookupTypeDescription] [varchar](500) NULL,
 CONSTRAINT [PK_LookupType] PRIMARY KEY CLUSTERED 
(
	[LookupTypeCode] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
INSERT [LookupType] ([LookupTypeCode], [LookupTypeName], [LookupTypeDescription]) VALUES (N'FeedbackMethod ', N'Feedback Method', N'Technique used to combine multiple recommandations used for the same output')
INSERT [LookupType] ([LookupTypeCode], [LookupTypeName], [LookupTypeDescription]) VALUES (N'GroupRampMethod', N'Group Ramp Method', NULL)
INSERT [LookupType] ([LookupTypeCode], [LookupTypeName], [LookupTypeDescription]) VALUES (N'LtSelTargetType', N'Lab Selector Target Type', N'Lab Data Toolkit - Type of the selector target')
INSERT [LookupType] ([LookupTypeCode], [LookupTypeName], [LookupTypeDescription]) VALUES (N'RampType', N'RampType', N'IO Ramp Type')
INSERT [LookupType] ([LookupTypeCode], [LookupTypeName], [LookupTypeDescription]) VALUES (N'RtLimitSource', N'Recipe Toolkit Limit Source', N'Values are Constant, Recipe, ?')
INSERT [LookupType] ([LookupTypeCode], [LookupTypeName], [LookupTypeDescription]) VALUES (N'RtLimitType', N'Recipe Toolkit Limit Type', NULL)
INSERT [LookupType] ([LookupTypeCode], [LookupTypeName], [LookupTypeDescription]) VALUES (N'WindowPosition', N'Window Display Position', N'Position for displaying a window')
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtHDAInterface](
	[InterfaceId] [int] IDENTITY(1000,1) NOT NULL,
	[InterfaceName] [varchar](50) NOT NULL,
 CONSTRAINT [PK_LtHDAInterface] PRIMARY KEY CLUSTERED 
(
	[InterfaceId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [LtHDAInterface] ON
INSERT [LtHDAInterface] ([InterfaceId], [InterfaceName]) VALUES (1001, N'PHD-HDA')
SET IDENTITY_INSERT [LtHDAInterface] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtOPCInterface](
	[InterfaceId] [int] IDENTITY(1000,1) NOT NULL,
	[InterfaceName] [varchar](50) NOT NULL,
 CONSTRAINT [PK_LtOPCInterface] PRIMARY KEY CLUSTERED 
(
	[InterfaceId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_LtOPCInterface] ON [LtOPCInterface] 
(
	[InterfaceName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtDownloadDetail](
	[DetailId] [int] IDENTITY(1,1) NOT NULL,
	[MasterId] [int] NOT NULL,
	[Timestamp] [datetime] NOT NULL,
	[Tag] [varchar](max) NOT NULL,
	[OutputValue] [nvarchar](50) NULL,
	[Success] [bit] NOT NULL,
	[StoreValue] [nvarchar](50) NULL,
	[CompareValue] [nvarchar](50) NULL,
	[RecommendedValue] [nvarchar](50) NULL,
	[Reason] [varchar](2000) NULL,
	[Error] [varchar](2000) NULL,
 CONSTRAINT [PK_DownloadDetail] PRIMARY KEY CLUSTERED 
(
	[DetailId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtAllowedFlyingSwitch](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[CurrentGrade] [varchar](50) NOT NULL,
	[NextGrade] [varchar](50) NOT NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtAdhocCatalog](
	[TableName] [varchar](500) NOT NULL,
 CONSTRAINT [PK_RtAdhocCatalog] PRIMARY KEY CLUSTERED 
(
	[TableName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RoleTranslation](
	[IgnitionRole] [varchar](50) NOT NULL,
	[WindowsRole] [varchar](50) NOT NULL,
 CONSTRAINT [PK_RoleTranslation] PRIMARY KEY CLUSTERED 
(
	[IgnitionRole] ASC,
	[WindowsRole] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
INSERT [RoleTranslation] ([IgnitionRole], [WindowsRole]) VALUES (N'Admin', N'Admin')
INSERT [RoleTranslation] ([IgnitionRole], [WindowsRole]) VALUES (N'Admin', N'Administrator')
INSERT [RoleTranslation] ([IgnitionRole], [WindowsRole]) VALUES (N'AE', N'AE')
INSERT [RoleTranslation] ([IgnitionRole], [WindowsRole]) VALUES (N'Operator', N'Operator')
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [QueueMessageStatus](
	[StatusId] [int] IDENTITY(1,1) NOT NULL,
	[Severity] [real] NOT NULL,
	[MessageStatus] [nvarchar](15) NOT NULL,
	[Color] [nvarchar](15) NOT NULL,
 CONSTRAINT [PK_QueueMessageStatus] PRIMARY KEY CLUSTERED 
(
	[StatusId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET IDENTITY_INSERT [QueueMessageStatus] ON
INSERT [QueueMessageStatus] ([StatusId], [Severity], [MessageStatus], [Color]) VALUES (1, 1, N'Info', N'white')
INSERT [QueueMessageStatus] ([StatusId], [Severity], [MessageStatus], [Color]) VALUES (2, 2, N'Warning', N'yellow')
INSERT [QueueMessageStatus] ([StatusId], [Severity], [MessageStatus], [Color]) VALUES (3, 3, N'Error', N'red')
SET IDENTITY_INSERT [QueueMessageStatus] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [QueueMaster](
	[QueueId] [int] IDENTITY(1,1) NOT NULL,
	[QueueKey] [nvarchar](50) NOT NULL,
	[Title] [nvarchar](100) NOT NULL,
	[CheckpointTimestamp] [datetime] NULL,
	[AutoViewSeverityThreshold] [real] NOT NULL,
	[Position] [varchar](50) NOT NULL,
	[AutoViewAdmin] [bit] NOT NULL,
	[AutoViewAE] [bit] NOT NULL,
	[AutoViewOperator] [bit] NOT NULL,
 CONSTRAINT [PK_QueueMaster] PRIMARY KEY CLUSTERED 
(
	[QueueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [IX_QueueMaster] ON [QueueMaster] 
(
	[QueueKey] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET IDENTITY_INSERT [QueueMaster] ON
INSERT [QueueMaster] ([QueueId], [QueueKey], [Title], [CheckpointTimestamp], [AutoViewSeverityThreshold], [Position], [AutoViewAdmin], [AutoViewAE], [AutoViewOperator]) VALUES (1, N'DOWNLOAD', N'Recipe Download LogX', NULL, 2.5, N'bottomLeft', 0, 1, 1)
INSERT [QueueMaster] ([QueueId], [QueueKey], [Title], [CheckpointTimestamp], [AutoViewSeverityThreshold], [Position], [AutoViewAdmin], [AutoViewAE], [AutoViewOperator]) VALUES (4, N'RECOMMENDATIONS', N'Recommendations from the Diagnostic Toolkit ', CAST(0x0000AAAE0161224A AS DateTime), 10, N'center', 0, 0, 0)
INSERT [QueueMaster] ([QueueId], [QueueKey], [Title], [CheckpointTimestamp], [AutoViewSeverityThreshold], [Position], [AutoViewAdmin], [AutoViewAE], [AutoViewOperator]) VALUES (8, N'XO1TEST', N'Test Console Messages', CAST(0x0000AB2700B2713C AS DateTime), 1.5, N'topLeft', 0, 0, 0)
INSERT [QueueMaster] ([QueueId], [QueueKey], [Title], [CheckpointTimestamp], [AutoViewSeverityThreshold], [Position], [AutoViewAdmin], [AutoViewAE], [AutoViewOperator]) VALUES (9, N'LABDATA', N'Lab Data Messages', NULL, 10, N'center', 0, 0, 0)
INSERT [QueueMaster] ([QueueId], [QueueKey], [Title], [CheckpointTimestamp], [AutoViewSeverityThreshold], [Position], [AutoViewAdmin], [AutoViewAE], [AutoViewOperator]) VALUES (15, N'Test', N'A Queue for Testing', CAST(0x0000ABBC0185804E AS DateTime), 2.5, N'topLeft', 0, 0, 0)
INSERT [QueueMaster] ([QueueId], [QueueKey], [Title], [CheckpointTimestamp], [AutoViewSeverityThreshold], [Position], [AutoViewAdmin], [AutoViewAE], [AutoViewOperator]) VALUES (16, N'SFCS', N'A Queue for SFCs', CAST(0x0000AC0C00BEA7EC AS DateTime), 1.5, N'center', 0, 1, 1)
INSERT [QueueMaster] ([QueueId], [QueueKey], [Title], [CheckpointTimestamp], [AutoViewSeverityThreshold], [Position], [AutoViewAdmin], [AutoViewAE], [AutoViewOperator]) VALUES (22, N'LABFEEDBACK', N'Lab Feedback Messages', NULL, 10, N'center', 0, 0, 0)
SET IDENTITY_INSERT [QueueMaster] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcChart](
	[ChartId] [int] IDENTITY(1,1) NOT NULL,
	[ChartPath] [varchar](800) NULL,
	[ChartResourceId] [int] NULL,
	[CreateTime] [datetime] NULL,
	[IsProduction] [bit] NOT NULL,
 CONSTRAINT [PK_SfcCharts] PRIMARY KEY CLUSTERED 
(
	[ChartId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_SfcChart] ON [SfcChart] 
(
	[ChartPath] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [RtWatchdog](
	[Observation] [int] NOT NULL,
	[Timestamp] [datetime] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	[Observation] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtValueType](
	[ValueTypeId] [int] IDENTITY(1,1) NOT NULL,
	[ValueType] [varchar](25) NOT NULL,
 CONSTRAINT [PK_RtValueType] PRIMARY KEY CLUSTERED 
(
	[ValueTypeId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [IX_RtValueType] ON [RtValueType] 
(
	[ValueType] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET IDENTITY_INSERT [RtValueType] ON
INSERT [RtValueType] ([ValueTypeId], [ValueType]) VALUES (4, N' ')
INSERT [RtValueType] ([ValueTypeId], [ValueType]) VALUES (2, N'Float')
INSERT [RtValueType] ([ValueTypeId], [ValueType]) VALUES (3, N'Integer')
INSERT [RtValueType] ([ValueTypeId], [ValueType]) VALUES (1, N'String')
SET IDENTITY_INSERT [RtValueType] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataType](
	[RecipeDataTypeId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeDataType] [varchar](50) NOT NULL,
	[JavaClassName] [varchar](50) NOT NULL,
 CONSTRAINT [PK_SfcRecipeDataTypes] PRIMARY KEY CLUSTERED 
(
	[RecipeDataTypeId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [SfcRecipeDataType] ON
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (1, N'Simple Value', N'Value')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (2, N'Output', N'Output')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (3, N'Array', N'Array')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (4, N'Timer', N'Timer')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (5, N'Input', N'Input')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (6, N'Matrix', N'Matrix')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (7, N'Recipe', N'Recipe')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (10, N'Output Ramp', N'OutputRamp')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (11, N'Group', N'Group')
INSERT [SfcRecipeDataType] ([RecipeDataTypeId], [RecipeDataType], [JavaClassName]) VALUES (12, N'SQC', N'SQC')
SET IDENTITY_INSERT [SfcRecipeDataType] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataOutputType](
	[OutputTypeId] [int] IDENTITY(1,1) NOT NULL,
	[OutputType] [varchar](50) NOT NULL,
 CONSTRAINT [PK_SfcRecipeDataOutputType] PRIMARY KEY CLUSTERED 
(
	[OutputTypeId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [SfcRecipeDataOutputType] ON
INSERT [SfcRecipeDataOutputType] ([OutputTypeId], [OutputType]) VALUES (1, N'Setpoint')
INSERT [SfcRecipeDataOutputType] ([OutputTypeId], [OutputType]) VALUES (2, N'Output')
INSERT [SfcRecipeDataOutputType] ([OutputTypeId], [OutputType]) VALUES (3, N'Mode')
INSERT [SfcRecipeDataOutputType] ([OutputTypeId], [OutputType]) VALUES (4, N'Value')
INSERT [SfcRecipeDataOutputType] ([OutputTypeId], [OutputType]) VALUES (5, N'Setpoint Ramp')
INSERT [SfcRecipeDataOutputType] ([OutputTypeId], [OutputType]) VALUES (6, N'Output Ramp')
SET IDENTITY_INSERT [SfcRecipeDataOutputType] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcNames](
	[SfcName] [varchar](500) NOT NULL,
 CONSTRAINT [PK_SfcNames] PRIMARY KEY CLUSTERED 
(
	[SfcName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataKeyMaster](
	[KeyId] [int] IDENTITY(1,1) NOT NULL,
	[KeyName] [varchar](50) NOT NULL,
 CONSTRAINT [PK_SfcRecipeDataKeyMaster] PRIMARY KEY CLUSTERED 
(
	[KeyId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkAssociationType](
	[AssociationTypeId] [int] IDENTITY(1,1) NOT NULL,
	[AssociationType] [varchar](100) NOT NULL,
 CONSTRAINT [PK_TkAssociationType] PRIMARY KEY CLUSTERED 
(
	[AssociationTypeId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkAssociation](
	[AssociationId] [int] IDENTITY(1,1) NOT NULL,
	[Source] [varchar](250) NOT NULL,
	[Sink] [varchar](250) NOT NULL,
	[AssociationTypeId] [int] NOT NULL,
 CONSTRAINT [PK_TkAssociation] PRIMARY KEY CLUSTERED 
(
	[AssociationId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkLogbook](
	[LogbookId] [int] IDENTITY(1,1) NOT NULL,
	[LogbookName] [varchar](20) NOT NULL,
 CONSTRAINT [PK_TkLogbook] PRIMARY KEY CLUSTERED 
(
	[LogbookId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [UK_TkLogbook] UNIQUE NONCLUSTERED 
(
	[LogbookName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [TkLogbook] ON
INSERT [TkLogbook] ([LogbookId], [LogbookName]) VALUES (3, N'Engineer')
INSERT [TkLogbook] ([LogbookId], [LogbookName]) VALUES (5, N'Test Logbook')
SET IDENTITY_INSERT [TkLogbook] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRunLog](
	[RunId] [int] IDENTITY(1000,1) NOT NULL,
	[ChartPath] [varchar](250) NOT NULL,
	[StepName] [varchar](50) NOT NULL,
	[StepType] [varchar](50) NOT NULL,
	[StartTime] [datetime] NOT NULL,
	[EndTime] [datetime] NULL,
	[Status] [varchar](20) NULL,
	[Notes] [varchar](2000) NULL,
 CONSTRAINT [PK_SfcRunLog] PRIMARY KEY CLUSTERED 
(
	[RunId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkUnitParameter](
	[UnitParameterId] [int] IDENTITY(1,1) NOT NULL,
	[UnitParameterTagName] [varchar](150) NOT NULL,
	[LabValueName] [varchar](150) NULL,
 CONSTRAINT [PK_TkUnitParameter] PRIMARY KEY CLUSTERED 
(
	[UnitParameterId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK1_TkUnitParameter] ON [TkUnitParameter] 
(
	[UnitParameterTagName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcValueType](
	[ValueTypeId] [int] IDENTITY(1,1) NOT NULL,
	[ValueType] [varchar](50) NOT NULL,
 CONSTRAINT [PK_SfcDataType] PRIMARY KEY CLUSTERED 
(
	[ValueTypeId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [SfcValueType] ON
INSERT [SfcValueType] ([ValueTypeId], [ValueType]) VALUES (1, N'Float')
INSERT [SfcValueType] ([ValueTypeId], [ValueType]) VALUES (2, N'Integer')
INSERT [SfcValueType] ([ValueTypeId], [ValueType]) VALUES (3, N'String')
INSERT [SfcValueType] ([ValueTypeId], [ValueType]) VALUES (4, N'Boolean')
SET IDENTITY_INSERT [SfcValueType] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataStash](
	[StashId] [int] IDENTITY(1,1) NOT NULL,
	[RxConfiguration] [varchar](50) NOT NULL,
	[RecipeDataKey] [varchar](50) NOT NULL,
	[RecipeDataAttribute] [varchar](50) NOT NULL,
	[RecipeDataValue] [float] NOT NULL,
 CONSTRAINT [PK_EmRecipeDataStash] PRIMARY KEY CLUSTERED 
(
	[StashId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_EmRecipeDataStash] ON [SfcRecipeDataStash] 
(
	[RxConfiguration] ASC,
	[RecipeDataKey] ASC,
	[RecipeDataAttribute] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcStepType](
	[StepTypeId] [int] IDENTITY(1,1) NOT NULL,
	[StepType] [varchar](50) NOT NULL,
	[FactoryId] [varchar](50) NULL,
 CONSTRAINT [PK_SfcStepType] PRIMARY KEY CLUSTERED 
(
	[StepTypeId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [SfcStepType] ON
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (1, N'Unit Procedure', N'com.ils.procedureStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (2, N'Operation', N'com.ils.operationStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (3, N'Phase', N'com.ils.phaseStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (4, N'Encapsulation', N'enclosing-step')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (13, N'Begin', N'begin-step')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (14, N'Action', N'action-step')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (15, N'End', N'end-step')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (16, N'None', N'None')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (17, N'Delay', N'com.ils.timedDelayStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (18, N'Yes No', N'com.ils.yesNoStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (19, N'Input', N'com.ils.inputStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (20, N'Set Queue', N'com.ils.setQueueStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (21, N'Clear Queue', N'com.ils.clearQueueStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (22, N'Queue Message', N'com.ils.queueMessageStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (23, N'Show Queue', N'com.ils.showQueueStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (24, N'Save Queue', N'com.ils.saveQueueStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (25, N'Data Entry', N'com.ils.manualDataEntryStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (26, N'Show Window', N'com.ils.showWindowStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (27, N'Print Window', N'com.ils.printWindowStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (28, N'Close Window', N'com.ils.closeWindowStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (29, N'Dialog Message', N'com.ils.dialogMessageStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (30, N'Post Delay', N'com.ils.postDelayNotification')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (31, N'Delete Delay', N'com.ils.deleteDelayNotification')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (32, N'Control Panel Message', N'com.ils.controlPanelMessageStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (33, N'Enable/Disable', N'com.ils.enableDisableStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (34, N'Select Input', N'com.ils.selectInputStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (35, N'Write Output', N'com.ils.writeOutputStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (36, N'Monitor Download', N'com.ils.monitorDownloadStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (37, N'PV Monitor', N'com.ils.pvMonitorStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (38, N'Collect Data', N'com.ils.collectDataStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (39, N'Abort', N'com.ils.abortStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (40, N'Pause', N'com.ils.pauseStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (41, N'Review Data', N'com.ils.reviewDataStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (42, N'Review With Advice', N'com.ils.reviewDataWithAdviceStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (43, N'Simple Query', N'com.ils.simpleQueryStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (45, N'Input with Limits', N'com.ils.limitedInputStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (46, N'Review Flows', N'com.ils.reviewFlowsStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (47, N'Save Data', N'com.ils.saveDataStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (48, N'Confirm Controllers', N'com.ils.confirmControllersStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (49, N'Assertion', N'assertion-step')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (50, N'com.ils.rawQueryStep', N'com.ils.rawQueryStep')
INSERT [SfcStepType] ([StepTypeId], [StepType], [FactoryId]) VALUES (51, N'com.ils.printFileStep', N'com.ils.printFileStep')
SET IDENTITY_INSERT [SfcStepType] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkSite](
	[SiteName] [varchar](50) NOT NULL,
	[GatewayStartupScript] [varchar](500) NOT NULL,
	[ClientStartupScript] [varchar](500) NOT NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
INSERT [TkSite] ([SiteName], [GatewayStartupScript], [ClientStartupScript]) VALUES (N'Site', N'ils.demo.startup.gateway', N'ils.demo.startup.gateway')
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkMenuBar](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[Application] [varchar](50) NOT NULL,
	[Menu] [varchar](50) NOT NULL,
	[SubMenu] [varchar](50) NOT NULL,
	[Enabled] [bit] NOT NULL,
 CONSTRAINT [PK_TkMenuBar] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [TkMenuBar] ON
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (1, N'dbManager', N'View', N'Recipe Family', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (2, N'dbManager', N'View', N'Value Definition', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (3, N'dbManager', N'View', N'Grade Master', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (4, N'dbManager', N'View', N'Grade Detail', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (5, N'dbManager', N'View', N'Allowed Flying Switches', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (6, N'dbManager', N'View', N'Download Log', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (7, N'dbManager', N'View', N'Events', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (8, N'dbManager', N'View', N'Gains', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (9, N'dbManager', N'View', N'SQC Limits', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (10, N'dbManager', N'View', N'Adhoc Tables', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (11, N'XOM', N'View', N'Grade History', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (13, N'XOM', N'View', N'Consoles', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (14, N'XOM', N'View', N'Logbooks', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (15, N'XOM', N'View', N'Queues', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (17, N'XOM', N'View', N'Client Isolation Settings', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (18, N'XOM', N'Admin', N'Vistalon', 0)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (19, N'XOM', N'Admin', N'Halobutyl', 0)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (20, N'dbManager', N'View', N'SQC Limits (New)', 1)
INSERT [TkMenuBar] ([Id], [Application], [Menu], [SubMenu], [Enabled]) VALUES (25, N'XOM', N'View', N'Vistalon Product Mooney', 0)
SET IDENTITY_INSERT [TkMenuBar] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkMessageRequest](
	[RequestId] [int] IDENTITY(1,1) NOT NULL,
	[RequestType] [varchar](50) NOT NULL,
	[RequestTime] [datetime] NOT NULL,
 CONSTRAINT [PK_TkMessageRequest] PRIMARY KEY CLUSTERED 
(
	[RequestId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [UIRGline](
	[UIRId] [int] IDENTITY(1,1) NOT NULL,
	[PostId] [int] NULL,
	[UIRTitle] [varchar](100) NULL,
	[Originator] [varchar](100) NULL,
	[ReportDate] [datetime] NULL,
	[ManualEntry] [bit] NULL,
	[IncidentStart] [datetime] NULL,
	[IncidentEnd] [datetime] NULL,
	[Reviewer] [varchar](100) NULL,
	[Grade] [varchar](10) NULL,
	[UnitsAffected] [varchar](2000) NULL,
	[Comments] [varchar](2000) NULL,
	[Summary] [varchar](2000) NULL,
	[UIRNumber] [varchar](100) NULL,
	[Area] [varchar](50) NULL,
	[Category] [varchar](50) NULL,
	[FollowUp] [varchar](50) NULL,
	[TimeBasis] [varchar](10) NULL,
	[TextReport] [nvarchar](max) NULL,
	[XMLReport] [nvarchar](max) NULL,
 CONSTRAINT [PK_UIRGline] PRIMARY KEY CLUSTERED 
(
	[UIRId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkWriteLocation](
	[WriteLocationId] [int] IDENTITY(1,1) NOT NULL,
	[Alias] [varchar](max) NOT NULL,
	[ServerName] [varchar](max) NOT NULL,
	[ScanClass] [varchar](max) NOT NULL,
 CONSTRAINT [PK_RecipeServerMap] PRIMARY KEY CLUSTERED 
(
	[WriteLocationId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [TkWriteLocation] ON
INSERT [TkWriteLocation] ([WriteLocationId], [Alias], [ServerName], [ScanClass]) VALUES (4, N'EPKS', N'PKS-ASYNC', N'OPC-Fast')
INSERT [TkWriteLocation] ([WriteLocationId], [Alias], [ServerName], [ScanClass]) VALUES (5, N'LOCAL', N'LOCAL', N'OPC-Fast')
SET IDENTITY_INSERT [TkWriteLocation] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [UIRVistalon](
	[UIRId] [int] IDENTITY(1,1) NOT NULL,
	[FormName] [varchar](100) NULL,
	[PostId] [int] NULL,
	[Title] [varchar](100) NULL,
	[Originator] [varchar](100) NULL,
	[RecordDate] [datetime] NULL,
	[Operator] [varchar](100) NULL,
	[ManualEntry] [bit] NULL,
	[Type] [varchar](100) NULL,
	[IncidentStart] [datetime] NULL,
	[IncidentEnd] [datetime] NULL,
	[Reviewer] [varchar](100) NULL,
	[Grade] [float] NULL,
	[Quality] [varchar](1000) NULL,
	[Root] [varchar](1000) NULL,
	[IncidentSummary] [varchar](2000) NULL,
	[RootExplanation] [varchar](2000) NULL,
	[GradeSummary] [varchar](2000) NULL,
	[CorrectiveAction] [varchar](2000) NULL,
	[BoxLine] [varchar](100) NULL,
	[BoxStart] [varchar](100) NULL,
	[BoxEnd] [varchar](100) NULL,
	[CloseOut] [bit] NULL,
	[Post] [varchar](100) NULL,
	[UIRNumber] [varchar](100) NULL,
 CONSTRAINT [PK_UIRVistalon] PRIMARY KEY CLUSTERED 
(
	[UIRId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [UIRHB](
	[UIRId] [int] IDENTITY(1,1) NOT NULL,
	[FormName] [varchar](100) NULL,
	[PostId] [int] NULL,
	[Title] [varchar](100) NULL,
	[Originator] [varchar](100) NULL,
	[RecordDate] [datetime] NULL,
	[Operator] [varchar](100) NULL,
	[ManualEntry] [bit] NULL,
	[Type] [varchar](100) NULL,
	[IncidentStart] [datetime] NULL,
	[IncidentEnd] [datetime] NULL,
	[Reviewer] [varchar](100) NULL,
	[Grade] [float] NULL,
	[Quality] [varchar](1000) NULL,
	[Root] [varchar](1000) NULL,
	[IncidentSummary] [varchar](2000) NULL,
	[RootExplanation] [varchar](2000) NULL,
	[GradeSummary] [varchar](2000) NULL,
	[CorrectiveAction] [varchar](2000) NULL,
	[BoxFlagStart] [varchar](100) NULL,
	[BoxFlagEnd] [varchar](100) NULL,
	[FactStart] [datetime] NULL,
	[FactEnd] [datetime] NULL,
	[CloseOut] [bit] NULL,
	[Post] [varchar](100) NULL,
	[UIRNumber] [varchar](100) NULL,
 CONSTRAINT [PK_UIRHB] PRIMARY KEY CLUSTERED 
(
	[UIRId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [Units](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[name] [varchar](64) NOT NULL,
	[isBaseUnit] [bit] NOT NULL,
	[type] [varchar](64) NOT NULL,
	[description] [varchar](2000) NULL,
	[m] [float] NULL,
	[b] [float] NULL,
 CONSTRAINT [PK_Units] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_Units] ON [Units] 
(
	[name] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET IDENTITY_INSERT [Units] ON
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (659, N'MOL/L', 1, N'MOLVOLC', N'(DD SI) Molar Concentration', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (660, N'MKJ/T', 0, N'KBTU/LB', N'(DD SI) Enthalpy per mass', 2.326, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (661, N'BBL', 0, N'VOL', N'(Misc) Barrels', 0.1781190476, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (662, N'LBHR', 0, N'MFLOW', N'(Misc) Pounds per hour', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (663, N'MOL/MOL', 1, N'MOL/MOL', N'(DD US) Molar ratio', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (664, N'Q/R', 1, N'UA', N'(DD US) UA heat transfer coefficient', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (665, N'KBTU/LB', 1, N'KBTU/LB', N'(DD US) Enthalpy per hour', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (667, N'MPAD', 0, N'DPRES', N'(DD SI) Delta pressure', 0.006894733261, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (668, N'PERCENT', 0, N'FRACT', N'(DD US) Percent', 100, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (669, N'KBTU/MR', 1, N'ENTRO/M', N'(DD US) Specific entropy', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (670, N'FRACT', 1, N'FRACT', N'(DD US) Fraction', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (671, N'MBARA', 0, N'PRES', N'Milli Bar Absolute', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (672, N'KG/CM2', 0, N'PRES', N'From Blue Book', 0.070307, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (673, N'SEC', 1, N'TIME', N'Time', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (674, N'FT3/MOL', 1, N'FT3/MOL', N'(DD US) Molar specific volume', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (675, N'MIN', 0, N'TIME', N'', 60, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (676, N'WTPCT', 0, N'FRACT', N'Variant of WtPct', 100, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (677, N'FTLB/LB', 0, N'KBTU/LB', N'(DD US) Polytropic head', 777900, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (678, N'MKJ/H', 0, N'POWER', N'(DD SI) Enthalpy per hour', 1.055056, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (679, N'BTU/HFR', 1, N'Th_Cond', N'(DD US) Thermal conductivity', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (680, N'KG/S', 0, N'MFLOW', N'Kg per second', 0.016666667, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (681, N'G/H', 0, N'MFLOW', N'Grams per hour (SI)', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (682, N'PSIG', 0, N'PRES', N'(Misc) Pounds per square inch gravity', 1, -14.696)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (683, N'LB/FT3', 1, N'DENSITY', N'(DD US) Density', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (684, N'KJ/KG', 0, N'KBTU/LB', N'(DD SI) Polytropic head', 2326, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (685, N'L', 0, N'VOL', N'(DD SI) Volume', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (686, N'PPM', 0, N'FRACT', N'Parts per million', 1000000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (687, N'M', 0, N'LEN', N'(Misc) Meter', 0.3048, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (688, N'KG/MOL', 0, N'MOL_WT', N'(Misc) Thousand MW', 0.001, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (690, N'OFFSET', 1, N'OFFSET', N'(DD US) Offset', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (691, N'UMOL/L', 0, N'MOLVOLC', N'micromoles/liter', 1E-06, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (692, N'KSFT3/H', 1, N'SVFLOW', N'(DD US) Volumetric flow @ 1 ATM, 60^F', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (693, N'MBTU/H', 1, N'POWER', N'(DD US) Enthalpy per mass', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (694, N'KPAD', 0, N'DPRES', N'(DD SI) Delta pressure', 6.894733261, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (697, N'KSFT3/M', 0, N'SVFLOW', N'(Misc) Vol flow per minute', 0.01666667, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (698, N'CP', 1, N'VISCOS', N'(DD US) Dynamic viscosity', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (699, N'MJ/H', 0, N'POWER', N'(Misc) Mega Joules per hour', 1055.056, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (700, N'M^3/H', 0, N'VFLOW', N'Cubic Metres per hour, from ACFM', 1.699, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (701, N'MOLPCT', 0, N'MOL/MOL', N'(Misc) Mole percent', 100, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (702, N'KG/H', 0, N'MFLOW', N'(Misc) Kilograms per hour', 453.5924, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (704, N'ACFM', 0, N'VFLOW', N'(Misc) Actual cubic feet per minute', 16.66666667, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (705, N'PCTREV', 0, N'FRACT', N'(Misc) reverse acting valve', -1, 100)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (706, N'GPD', 0, N'VFLOW', N'(Misc) Gallons per day', 179544, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (707, N'ATM', 0, N'PRES', N'(Misc) Atmosphere (14.696 psia)', 0.06804572673, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (708, N'CAL/MOL', 0, N'KBTU/M', N'(Misc) Calories per mole', 0.5559273423, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (709, N'PSID', 1, N'DPRES', N'(DD US) Delta Pressure', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (710, N'PSIA', 1, N'PRES', N'(DD US) Pressure', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (712, N'GPM', 0, N'VFLOW', N'(Misc) Gallons per minute', 124.6833333, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (713, N'YD3', 0, N'VOL', N'(Misc) Cubic yards', 0.03703703704, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (714, N'FT3/LB', 1, N'FT3/LB', N'(DD US) Mass specific volume', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (716, N'KLB/H', 1, N'MFLOW', N'(DD US) Mass flow', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (717, N'KHP', 0, N'POWER', N'(DD US) Power', 0.3930148039, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (718, N'KG/M^3', 0, N'DENSITY', N'(DD SI) Density', 16.01846, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (719, N'UST/D', 0, N'MFLOW', N'(Misc) 2000 lb (US) tons per hour', 12, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (721, N'WTPPM', 0, N'M_Ratio', N'(Misc) Weight parts per million', 1000000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (723, N'KMOL/H', 1, N'KMOL/H', N'(DD US) Molar flow', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (725, N'BARG', 0, N'PRES', N'(Misc) Bar (gage)', 0.06894733261, -1.01325)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (726, N'LB/LB', 1, N'M_Ratio', N'(DD US) Mass ratio', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (727, N'BARD', 0, N'DPRES', N'(Misc)  Delta pressure', 0.06894733261, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (728, N'BARA', 0, N'PRES', N'(Misc) Bar (absolute)', 0.06894733261, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (729, N'BTU/LBF', 0, N'Cp', N'(Misc) BTU''s per pound per deg F', 0.2388458966, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (730, N'GAL', 0, N'VOL', N'(Misc) Gallons', 7.481, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (731, N'WTPPB', 0, N'M_Ratio', N'(Misc) Weight parts per billion', 1000000000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (732, N'MSCFD', 0, N'SVFLOW', N'(Misc) Millions of standard ft3 per day', 0.024, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (733, N'DRANKINE', 1, N'DTEMP', N'(DD US) Delta temperature', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (734, N'MMHG', 0, N'PRES', N'(Misc) Millimeters of mercury', 51.71474006, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (735, N'KBTU/HR', 0, N'POWER', N'Conversion from MBTU/HR to KBTU/HR', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (736, N'DKELVIN', 0, N'DTEMP', N'(DD SI) Delta temperature', 0.55555556, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (738, N'MOLPPM', 0, N'MOL/MOL', N'(Misc) Mole parts per million', 1000000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (739, N'LB/MOL', 1, N'MOL_WT', N'(DD US) Molecular weight', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (740, N'INHG', 0, N'PRES', N'(Misc) Inches of mercury', 2.04176474, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (741, N'KSM^3/H', 1, N'KSM^3/H', N'(DD SI) Vol Flow at 15C, 1BarA', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (742, N'FT', 1, N'LEN', N'(Misc) Feet', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (743, N'KM^3/H', 0, N'VFLOW', N'(DD SI) Volumetric flow', 0.02831685, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (744, N'Q/K/M^2', 0, N'U', N'(DD SI) U heat transfer coefficient', 20.44175, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (745, N'KCAL/MC', 0, N'ENTRO/M', N'(Misc) Kilo Calories per mole per deg C', 1.000669216, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (746, N'MBARG', 0, N'PRES', N'Milli Bar Gage', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (748, N'CC/GMOL', 0, N'FT3/MOL', N'(DD SI) Molar specific volume', 62.42797373, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (752, N'CAL/GK', 0, N'Cp', N'(Misc) Calories per gram per deg C', 0.2390057361, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (753, N'KLB/D', 0, N'MFLOW', N'Klb per hour', 24, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (754, N'MEGWATT', 0, N'POWER', N'(DD SI) Power', 0.2930711, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (755, N'MOLPPB', 0, N'MOL/MOL', N'(Misc) Mole parts per billion', 1000000000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (760, N'MPA', 0, N'PRES', N'(DD SI) Pressure', 0.006894733261, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (761, N'METER^3', 0, N'VOL', N'(DD SI) Volume', 0.02831685, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (762, N'KT/H', 0, N'MFLOW', N'Kilo Tonne / Hour', 0.001, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (763, N'DEGC', 0, N'TEMP', N'(Misc) Degrees Centigrade', 0.5555555556, -273.15)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (764, N'METER^2', 0, N'AREA', N'(DD SI) Area', 0.09290304, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (766, N'KG/MIN', 0, N'MFLOW', N'Kg per minute', 0.016666667, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (767, N'HP', 0, N'POWER', N'(Misc) Horse Power', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (768, N'MKJ/K', 0, N'ENTROPY', N'Entropy', 0.0018991, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (769, N'G/CC', 0, N'DENSITY', N'(Misc) Grams per cubic centimeter', 0.01601846, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (770, N'HR', 0, N'TIME', N'', 3600, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (771, N'KACFM', 0, N'VFLOW', N'(Misc) Thousands of ACFM', 0.01666666667, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (772, N'KCAL/M', 0, N'KBTU/M', N'(Misc) Kilo Calories per mole', 0.0005559273423, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (773, N'RANKINE', 1, N'TEMP', N'(DD US) Absolute temperature', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (774, N'Q/R/FT2', 1, N'U', N'(DD US) U heat transfer coefficient', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (776, N'NONE', 1, N'NONE', N'For no conversion', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (780, N'FT3', 1, N'VOL', N'(DD US) Volume', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (781, N'KJ/MOLK', 0, N'ENTRO/M', N'(DD SI) Specific entropy', 4.1868, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (782, N'FT2', 1, N'AREA', N'(DD US) Area', 0, 0)
GO
print 'Processed 100 total records'
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (784, N'MOL/M3', 0, N'MOLVOLC', N'(SI) Mols per cubic meter', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (785, N'SEC/10', 0, N'TIME', N'', 10, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (786, N'SCFM', 0, N'SVFLOW', N'(Misc) Std Cu Ft per minute', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (787, N'AMPS', 1, N'CURRENT', N'', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (789, N'CC/GM', 0, N'FT3/LB', N'(DD SI) Mass specific volume', 62.42797373, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (790, N'KELVIN', 0, N'TEMP', N'(DD SI) Absolute temperature', 0.5555555556, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (791, N'SLOPE', 1, N'SLOPE', N'(DD US) Slope', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (793, N'DEGF', 0, N'TEMP', N'(Misc) Degrees Fahrenheit', 1, -459.67)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (794, N'MKJ/H/K', 0, N'UA', N'(DD SI) UA heat transfer coefficient', 1.899100718, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (796, N'INH2O', 0, N'PRES', N'(Misc) Inches of water', 27.70749582, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (797, N'RATIO', 1, N'RATIO', N'(Misc) Ratio''s; e.g., CP/CV ratio', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (798, N'T/H', 0, N'MFLOW', N'(DD SI) Mass flow', 0.4535924, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (802, N'PHR', 1, N'PHR', N'Pounds per Hundred pounds Rubber', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (804, N'KG', 0, N'MASS', N'(Misc) Kilograms', 0.4535924, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (807, N'KCAL/G', 0, N'KBTU/LB', N'(Misc) Kilo Calories per gram', 0.5559273423, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (809, N'KJ/GMOL', 0, N'KBTU/M', N'(DD SI) Specific enthalpy', 2.326, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (812, N'KRPM', 1, N'KRPM', N'(DD US) Rotational speed', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (813, N'MG/MOL', 0, N'MOL_WT', N'(Misc) Million MW', 1E-06, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (815, N'KFT3/H', 1, N'VFLOW', N'(DD US) Volumetric flow', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (817, N'LBM', 1, N'MASS', N'(Misc) Pounds', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (818, N'KJ/KGK', 1, N'Cp', N'(Misc) Kilo Joules per kg per deg K', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (819, N'CAL/MC', 0, N'ENTRO/M', N'(Misc) Calories per mole per deg C', 1000.669216, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (820, N'BTU/LB', 0, N'KBTU/LB', N'BTU per pound', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (823, N'TONS', 0, N'MASS', N'(Misc) US short (2000lb) Tons', 0.0005, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (825, N'IN2', 0, N'AREA', N'(Misc) Square inch', 144, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (826, N'W/M/K', 0, N'Th_Cond', N'(DD SI) Thermal conductivity', 1.730735, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (828, N'BPD', 0, N'VFLOW', N'(Misc) Barrels per day', 4274.857143, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (830, N'KJ/K', 0, N'ENTROPY', N'(DD SI) Entropy', 1E-06, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (833, N'KJ/H', 0, N'POWER', N'(Misc) Kilo Joules per hour', 1055056, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (834, N'BPH', 0, N'VFLOW', N'Barrels per hour', 0.0416666667, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (836, N'RPM', 0, N'KRPM', N'(Misc) Revolutions per minute', 1000, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (837, N'KBTU/M', 1, N'KBTU/M', N'(DD US) Specific enthalpy', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (842, N'INHGV', 0, N'PRES', N'(Misc) Inches of mercury vacuum', -2.04176474, 30.00577461)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (843, N'KWATT', 0, N'POWER', N'(Misc) Kilowatts', 293.0711, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (844, N'KBTU/R', 1, N'ENTROPY', N'(DD US) Entropy', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (845, N'CAL/G', 0, N'KBTU/LB', N'(Misc) Calories per gram', 555.9273423, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (847, N'MMOL/H', 1, N'MMOL/H', N'(DD SI) Molar flow', 0, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (848, N'LB/GAL', 0, N'DENSITY', N'(Misc) Pounds per gallon', 0.133671969, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (851, N'T/DAY', 0, N'MFLOW', N'Tonne / Day', 24, 0)
INSERT [Units] ([id], [name], [isBaseUnit], [type], [description], [m], [b]) VALUES (852, N'MMOL/L', 0, N'MOLVOLC', N'millimoles/liter', 0.001, 0)
SET IDENTITY_INSERT [Units] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [Version](
	[VersionId] [int] NOT NULL,
	[Version] [nchar](10) NOT NULL,
	[ReleaseDate] [datetime] NOT NULL,
	[InstallDate] [datetime] NULL,
 CONSTRAINT [PK_Version] PRIMARY KEY CLUSTERED 
(
	[VersionId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
INSERT [Version] ([VersionId], [Version], [ReleaseDate], [InstallDate]) VALUES (0, N'1.0r0     ', CAST(0x0000AAD900000000 AS DateTime), CAST(0x0000ABE3018408AE AS DateTime))
INSERT [Version] ([VersionId], [Version], [ReleaseDate], [InstallDate]) VALUES (1, N'1.1r0     ', CAST(0x0000AB9000000000 AS DateTime), CAST(0x0000AB9000000000 AS DateTime))
INSERT [Version] ([VersionId], [Version], [ReleaseDate], [InstallDate]) VALUES (2, N'1.2r0     ', CAST(0x0000ABE100000000 AS DateTime), CAST(0x0000ABE100000000 AS DateTime))
INSERT [Version] ([VersionId], [Version], [ReleaseDate], [InstallDate]) VALUES (3, N'1.3r0     ', CAST(0x0000AC3600000000 AS DateTime), CAST(0x0000AC3600000000 AS DateTime))
INSERT [Version] ([VersionId], [Version], [ReleaseDate], [InstallDate]) VALUES (4, N'1.4r0     ', CAST(0x0000AC4B00000000 AS DateTime), CAST(0x0000AC4B00000000 AS DateTime))
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [UIRGlineInvolvedProperty](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[UIRId] [int] NOT NULL,
	[PropertyName] [varchar](50) NOT NULL,
 CONSTRAINT [PK_UIRGlineDetails] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [UnitAliases](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[alias] [varchar](64) NOT NULL,
	[name] [varchar](64) NOT NULL,
 CONSTRAINT [PK_UnitAliases] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_UnitAliases] ON [UnitAliases] 
(
	[alias] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET IDENTITY_INSERT [UnitAliases] ON
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (145, N'GJ/T', N'MKJ/T')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (146, N'LB/HR', N'LBHR')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (147, N'LBS/H', N'LBHR')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (148, N'>MPA', N'MPAD')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (150, N'GJ/H', N'MKJ/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (151, N'G-H', N'G/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (152, N'LBCUFT', N'LB/FT3')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (153, N'MJ/T', N'KJ/KG')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (154, N'>KPA', N'KPAD')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (155, N'LB-HR', N'LBHR')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (156, N'KLB/HR', N'KLB/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (157, N'TONS/D', N'UST/D')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (158, N'GR/HR', N'G/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (159, N'KLBS/H', N'KLB/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (160, N'PSI', N'PSIA')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (161, N'KG/KG', N'LB/LB')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (162, N'DDEGF', N'DRANKINE')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (163, N'DDEGC', N'DKELVIN')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (164, N'PCT', N'PERCENT')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (165, N'KG/KGM', N'LB/MOL')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (166, N'>BAR', N'MPAD')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (167, N'MWATT', N'MEGWATT')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (168, N'WTFRACT', N'FRACT')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (169, N'FRACTN', N'FRACT')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (170, N'KLBPH', N'KLB/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (171, N'KLB-HR', N'KLB/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (172, N'MOLWT', N'LB/MOL')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (173, N'DEGR', N'RANKINE')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (174, N'DEGK', N'KELVIN')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (175, N'KG/GMOL', N'KG/MOL')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (176, N'MBTUHR', N'MBTU/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (177, N'T/T', N'LB/LB')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (178, N'PERCNT', N'PERCENT')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (179, N'#/H', N'LBHR')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (180, N'FT**3', N'FT3')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (181, N'USGAL', N'GAL')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (182, N'M2', N'METER^2')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (183, N'M3', N'METER^3')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (184, N'MG/GMOL', N'MG/MOL')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (185, N'KLBHR', N'KLB/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (186, N'KSCF/M', N'KSFT3/M')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (187, N'GJ/HK', N'MKJ/H/K')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (188, N'KSCF/H', N'KSFT3/H')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (189, N'>PSI', N'PSID')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (190, N'>RANKINE', N'DRANKINE')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (191, N'DEG-F', N'DEGF')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (192, N'>DEGC', N'DKELVIN')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (193, N'G/GMOL', N'LB/MOL')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (194, N'KBTU/H', N'KBTU/HR')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (195, N'>DEGF', N'DRANKINE')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (196, N'>KELVIN', N'DKELVIN')
INSERT [UnitAliases] ([id], [alias], [name]) VALUES (197, N'G/HR', N'G/H')
SET IDENTITY_INSERT [UnitAliases] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkMessageReply](
	[ReplyId] [int] IDENTITY(1,1) NOT NULL,
	[RequestId] [int] NOT NULL,
	[Reply] [varchar](2000) NOT NULL,
	[ReplyTime] [datetime] NOT NULL,
	[ClientId] [varchar](500) NOT NULL,
	[IsolationMode] [bit] NOT NULL,
 CONSTRAINT [PK_TkMessageReply] PRIMARY KEY CLUSTERED 
(
	[ReplyId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkLogbookDetail](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[LogbookId] [int] NULL,
	[Timestamp] [datetime] NULL,
	[Message] [varchar](2000) NULL,
 CONSTRAINT [PK_TkLogbookDetail] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [TkUnitParameterBuffer](
	[UnitParameterId] [int] NOT NULL,
	[BufferIndex] [int] NOT NULL,
	[RawValue] [float] NULL,
	[SampleTime] [datetime] NULL,
	[ReceiptTime] [datetime] NULL,
 CONSTRAINT [PK_TkUnitParameterBuffer] PRIMARY KEY CLUSTERED 
(
	[UnitParameterId] ASC,
	[BufferIndex] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcStep](
	[StepId] [int] IDENTITY(1,1) NOT NULL,
	[StepUUID] [varchar](256) NOT NULL,
	[StepName] [varchar](500) NOT NULL,
	[StepTypeId] [int] NOT NULL,
	[ChartId] [int] NOT NULL,
 CONSTRAINT [PK_SfcStep] PRIMARY KEY CLUSTERED 
(
	[StepId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [UK_ChartId_StepName] UNIQUE NONCLUSTERED 
(
	[ChartId] ASC,
	[StepName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkPost](
	[PostId] [int] IDENTITY(1,1) NOT NULL,
	[Post] [varchar](50) NOT NULL,
	[MessageQueueId] [int] NOT NULL,
	[LogbookId] [int] NOT NULL,
	[DownloadActive] [bit] NOT NULL,
 CONSTRAINT [PK_TkPost] PRIMARY KEY CLUSTERED 
(
	[PostId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [TkPost] ON
INSERT [TkPost] ([PostId], [Post], [MessageQueueId], [LogbookId], [DownloadActive]) VALUES (70, N'Test', 15, 5, 0)
SET IDENTITY_INSERT [TkPost] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataKeyDetail](
	[KeyId] [int] NOT NULL,
	[KeyValue] [varchar](20) NOT NULL,
	[KeyIndex] [int] NOT NULL,
 CONSTRAINT [PK_SfcRecipeDataKeyDetail] PRIMARY KEY CLUSTERED 
(
	[KeyId] ASC,
	[KeyValue] ASC,
	[KeyIndex] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [IX_SfcRecipeDataKeyDetail] ON [SfcRecipeDataKeyDetail] 
(
	[KeyId] ASC,
	[KeyValue] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcHierarchyHandler](
	[HierarchyId] [int] IDENTITY(1,1) NOT NULL,
	[ChartId] [int] NOT NULL,
	[Handler] [varchar](50) NOT NULL,
	[HandlerChartId] [int] NOT NULL,
 CONSTRAINT [PK_SfcChartHandler] PRIMARY KEY CLUSTERED 
(
	[HierarchyId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [QueueDetail](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[QueueId] [int] NOT NULL,
	[Timestamp] [datetime] NOT NULL,
	[StatusId] [int] NOT NULL,
	[Message] [nvarchar](2000) NOT NULL,
 CONSTRAINT [PK_QueueDetail] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtTextRecommendation](
	[TextRecommendationId] [int] IDENTITY(1,1) NOT NULL,
	[DiagnosisEntryId] [int] NOT NULL,
	[TextRecommendation] [varchar](2500) NOT NULL,
 CONSTRAINT [PK_DtTextRecommendation] PRIMARY KEY CLUSTERED 
(
	[TextRecommendationId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [Lookup](
	[LookupId] [int] IDENTITY(1,1) NOT NULL,
	[LookupTypeCode] [varchar](15) NOT NULL,
	[LookupName] [varchar](50) NOT NULL,
	[LookupDescription] [varchar](500) NULL,
	[Active] [bit] NOT NULL,
 CONSTRAINT [PK_Lookup] PRIMARY KEY CLUSTERED 
(
	[LookupId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET IDENTITY_INSERT [Lookup] ON
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (1, N'GroupRampMethod', N'None', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (2, N'GroupRampMethod', N'Shortest', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (5, N'FeedbackMethod', N'Simple Sum', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (6, N'FeedbackMethod', N'Average', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (9, N'FeedbackMethod', N'Most Positive', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (10, N'FeedbackMethod', N'Most Negative', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (12, N'RtLimitType', N'SQC', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (13, N'RtLimitType', N'Validity', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (14, N'RtLimitType', N'Release', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (15, N'RtLimitSource', N'Constant', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (16, N'RtLimitSource', N'Recipe', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (17, N'RtLimitSource', N'DCS', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (18, N'GroupRampMethod', N'Longest', N'None', 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (23, N'LtSelTargetType', N'PHD Lab Value Item Id', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (24, N'LtSelTargetType', N'PHD Lab Value Interface', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (25, N'WindowPosition', N'center', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (26, N'WindowPosition', N'topLeft', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (27, N'WindowPosition', N'topCenter', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (28, N'WindowPosition', N'topRight', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (29, N'WindowPosition', N'bottomLeft', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (30, N'WindowPosition', N'bottomCenter', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (31, N'WindowPosition', N'bottomRight', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (32, N'RampType', N'Time', NULL, 1)
INSERT [Lookup] ([LookupId], [LookupTypeCode], [LookupName], [LookupDescription], [Active]) VALUES (33, N'RampType', N'Rate', NULL, 1)
SET IDENTITY_INSERT [Lookup] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [BtBatchRun](
	[BatchRunId] [int] IDENTITY(1000,1) NOT NULL,
	[ReactorId] [int] NOT NULL,
	[Grade] [float] NOT NULL,
	[BatchCount] [int] NOT NULL,
	[StartDate] [datetime] NOT NULL,
	[EndDate] [datetime] NULL,
 CONSTRAINT [PK_BtBatchRun] PRIMARY KEY CLUSTERED 
(
	[BatchRunId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [BtStripperBatchLog](
	[BatchId] [int] IDENTITY(1,1) NOT NULL,
	[BatchRunId] [int] NULL,
	[BatchNumber] [int] NULL,
	[BatchCount] [int] NULL,
	[Status] [varchar](50) NULL,
	[CreationTime] [datetime] NULL,
	[LabResult] [float] NULL,
	[FillBegin] [datetime] NULL,
	[FillEnd] [datetime] NULL,
	[FillTime] [time](7) NULL,
	[StripBegin] [datetime] NULL,
	[StripEnd] [datetime] NULL,
	[StripTime] [time](7) NULL,
	[JD03Begin] [datetime] NULL,
	[JD03End] [datetime] NULL,
	[JD03Time] [time](7) NULL,
	[TransferBegin] [datetime] NULL,
	[TransferEnd] [datetime] NULL,
	[TransferTime] [time](7) NULL,
	[StandbyBegin] [datetime] NULL,
	[StandbyEnd] [datetime] NULL,
	[StandbyTime] [time](7) NULL,
	[TotalStripperTime] [time](7) NULL,
	[TotalChargeAmount] [float] NULL,
 CONSTRAINT [PK_BtStripperBatchLog] PRIMARY KEY CLUSTERED 
(
	[BatchId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [BtBatchLog](
	[BatchId] [int] IDENTITY(1,1) NOT NULL,
	[BatchRunId] [int] NULL,
	[BatchNumber] [int] NULL,
	[BatchCount] [int] NULL,
	[Status] [varchar](50) NULL,
	[CreationTime] [datetime] NULL,
	[LabResult] [float] NULL,
	[ChargeBegin] [datetime] NULL,
	[ChargeEnd] [datetime] NULL,
	[ChargeTime] [time](7) NULL,
	[HeatUpBegin] [datetime] NULL,
	[HeatUpEnd] [datetime] NULL,
	[HeatUpTime] [time](7) NULL,
	[SoakBegin] [datetime] NULL,
	[SoakEnd] [datetime] NULL,
	[SoakTime] [time](7) NULL,
	[TransferBegin] [datetime] NULL,
	[TransferEnd] [datetime] NULL,
	[TransferTime] [time](7) NULL,
	[StandbyBegin] [datetime] NULL,
	[StandbyEnd] [datetime] NULL,
	[StandbyTime] [time](7) NULL,
	[TotalBatchTime] [time](7) NULL,
	[TotalChargeAmount] [float] NULL,
	[AverageSoakTemp] [float] NULL,
	[SoakTimer] [float] NULL,
 CONSTRAINT [PK_BtBatchLog] PRIMARY KEY CLUSTERED 
(
	[BatchId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtDisplayTable](
	[DisplayTableId] [int] IDENTITY(1000,1) NOT NULL,
	[DisplayTableTitle] [varchar](50) NOT NULL,
	[DisplayPage] [int] NOT NULL,
	[DisplayOrder] [int] NOT NULL,
	[DisplayFlag] [bit] NOT NULL,
	[PostId] [int] NOT NULL,
	[OldTableName] [varchar](50) NULL,
 CONSTRAINT [PK_LtConsoles] PRIMARY KEY CLUSTERED 
(
	[DisplayTableId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_LtConsoles] ON [LtDisplayTable] 
(
	[DisplayTableTitle] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtRecipeFamily](
	[RecipeFamilyId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeFamilyName] [varchar](50) NOT NULL,
	[RecipeUnitPrefix] [varchar](50) NULL,
	[RecipeNameAlias] [varchar](50) NULL,
	[PostId] [int] NULL,
	[CurrentGrade] [varchar](50) NULL,
	[CurrentVersion] [int] NULL,
	[Status] [varchar](50) NULL,
	[ConfirmDownload] [bit] NULL,
	[Timestamp] [datetime] NULL,
	[Comment] [varchar](2000) NULL,
	[HasGains] [bit] NOT NULL,
	[HasSQC] [bit] NOT NULL,
 CONSTRAINT [PK_RtRecipeMap] PRIMARY KEY CLUSTERED 
(
	[RecipeFamilyId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [IX_RtRecipeFamily] ON [RtRecipeFamily] 
(
	[RecipeFamilyName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeData](
	[RecipeDataId] [int] IDENTITY(1,1) NOT NULL,
	[StepId] [int] NOT NULL,
	[RecipeDataKey] [varchar](50) NOT NULL,
	[RecipeDataTypeId] [int] NOT NULL,
	[Label] [varchar](100) NULL,
	[Description] [varchar](1000) NULL,
	[Units] [varchar](50) NULL,
	[RecipeDataFolderId] [int] NULL,
 CONSTRAINT [PK_SfcRecipeData] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [UK_SfcRecipeData] UNIQUE NONCLUSTERED 
(
	[StepId] ASC,
	[RecipeDataKey] ASC,
	[RecipeDataFolderId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcControlPanel](
	[ControlPanelId] [int] IDENTITY(1,1) NOT NULL,
	[ControlPanelName] [varchar](900) NOT NULL,
	[PostId] [int] NULL,
	[ChartPath] [varchar](900) NOT NULL,
	[ChartRunId] [varchar](900) NULL,
	[Operation] [varchar](900) NULL,
	[MsgQueue] [varchar](900) NULL,
	[Originator] [varchar](900) NULL,
	[Project] [varchar](900) NULL,
	[IsolationMode] [bit] NULL,
	[EnableCancel] [bit] NULL,
	[EnablePause] [bit] NULL,
	[EnableReset] [bit] NULL,
	[EnableResume] [bit] NULL,
	[EnableStart] [bit] NULL,
 CONSTRAINT [PK_SfcControlPanelPete] PRIMARY KEY CLUSTERED 
(
	[ControlPanelId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_SfcControlPanel] ON [SfcControlPanel] 
(
	[ControlPanelName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcChartStepView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepName, dbo.SfcStep.StepUUID, dbo.SfcStepType.StepType
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcStepType ON dbo.SfcStep.StepTypeId = dbo.SfcStepType.StepTypeId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcHierarchy](
	[HierarchyId] [int] IDENTITY(200,1) NOT NULL,
	[StepId] [int] NOT NULL,
	[ChartId] [int] NOT NULL,
	[ChildChartId] [int] NOT NULL,
 CONSTRAINT [PK_SfcHierarchy] PRIMARY KEY CLUSTERED 
(
	[HierarchyId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY],
 CONSTRAINT [IX_SfcHierarchy] UNIQUE NONCLUSTERED 
(
	[StepId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataFolder](
	[RecipeDataFolderId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeDataKey] [varchar](50) NOT NULL,
	[StepId] [int] NOT NULL,
	[RecipeDataType] [varchar](50) NOT NULL,
	[ParentRecipeDataFolderId] [int] NULL,
	[Description] [varchar](1000) NULL,
	[Label] [varchar](100) NULL,
 CONSTRAINT [PK_SfcRecipeDataFolder] PRIMARY KEY CLUSTERED 
(
	[RecipeDataFolderId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcHierarchyHandlerView]
AS
SELECT     dbo.SfcHierarchyHandler.HierarchyId, dbo.SfcHierarchyHandler.ChartId, dbo.SfcChart.ChartPath, dbo.SfcHierarchyHandler.Handler, dbo.SfcHierarchyHandler.HandlerChartId, 
                      SfcChart_1.ChartPath AS HandlerChartPath, dbo.SfcChart.IsProduction
FROM         dbo.SfcHierarchyHandler INNER JOIN
                      dbo.SfcChart ON dbo.SfcHierarchyHandler.ChartId = dbo.SfcChart.ChartId INNER JOIN
                      dbo.SfcChart AS SfcChart_1 ON dbo.SfcHierarchyHandler.HandlerChartId = SfcChart_1.ChartId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataKeyView]
AS
SELECT     dbo.SfcRecipeDataKeyMaster.KeyId, dbo.SfcRecipeDataKeyMaster.KeyName, dbo.SfcRecipeDataKeyDetail.KeyValue, dbo.SfcRecipeDataKeyDetail.KeyIndex
FROM         dbo.SfcRecipeDataKeyMaster INNER JOIN
                      dbo.SfcRecipeDataKeyDetail ON dbo.SfcRecipeDataKeyMaster.KeyId = dbo.SfcRecipeDataKeyDetail.KeyId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkUnit](
	[UnitId] [int] IDENTITY(1,1) NOT NULL,
	[UnitName] [varchar](50) NOT NULL,
	[UnitPrefix] [varchar](50) NULL,
	[UnitAlias] [varchar](50) NULL,
	[PostId] [int] NOT NULL,
	[Grade] [varchar](50) NULL,
 CONSTRAINT [PK__UnitRoot__44F5ECB5173876EA] PRIMARY KEY CLUSTERED 
(
	[UnitId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [Uk_TkUnit_UnitName] ON [TkUnit] 
(
	[UnitName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET IDENTITY_INSERT [TkUnit] ON
INSERT [TkUnit] ([UnitId], [UnitName], [UnitPrefix], [UnitAlias], [PostId], [Grade]) VALUES (74, N'Test', NULL, NULL, 70, NULL)
SET IDENTITY_INSERT [TkUnit] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [TkConsole](
	[ConsoleId] [int] IDENTITY(1000,1) NOT NULL,
	[WindowName] [varchar](100) NOT NULL,
	[ConsoleName] [varchar](100) NOT NULL,
	[Priority] [int] NOT NULL,
	[PostId] [int] NOT NULL,
 CONSTRAINT [PK_DtConsole] PRIMARY KEY CLUSTERED 
(
	[ConsoleId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK1_TkConsole] ON [TkConsole] 
(
	[ConsoleName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK2_TkConsole] ON [TkConsole] 
(
	[WindowName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET IDENTITY_INSERT [TkConsole] ON
INSERT [TkConsole] ([ConsoleId], [WindowName], [ConsoleName], [Priority], [PostId]) VALUES (1743, N'Demo/Test Console', N'Test Console', 1, 70)
SET IDENTITY_INSERT [TkConsole] OFF
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcStepView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepName, dbo.SfcStep.StepUUID, dbo.SfcStepType.StepType
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcStepType ON dbo.SfcStep.StepTypeId = dbo.SfcStepType.StepTypeId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [TkPostView]
AS
SELECT     dbo.TkPost.PostId, dbo.TkPost.Post, dbo.QueueMaster.QueueId, dbo.QueueMaster.QueueKey, dbo.TkLogbook.LogbookId, dbo.TkLogbook.logbookName, dbo.TkPost.DownloadActive
FROM         dbo.TkPost INNER JOIN
                      dbo.QueueMaster ON dbo.TkPost.MessageQueueId = dbo.QueueMaster.QueueId LEFT OUTER JOIN
                      dbo.TkLogbook ON dbo.TkPost.LogbookId = dbo.TkLogbook.LogbookId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [UIRGlineView]
AS
SELECT     dbo.UIRGline.UIRId, dbo.UIRGline.UIRNumber, dbo.UIRGline.UIRTitle, dbo.TkPost.Post, dbo.UIRGline.Originator, dbo.UIRGline.ReportDate, dbo.UIRGline.IncidentStart, dbo.UIRGline.IncidentEnd, 
                      dbo.UIRGline.Grade, dbo.UIRGline.Area, dbo.UIRGline.Category, dbo.UIRGline.TimeBasis, dbo.UIRGline.FollowUp, dbo.UIRGline.UnitsAffected, dbo.UIRGline.Summary, dbo.UIRGline.ManualEntry, 
                      dbo.UIRGline.Reviewer, dbo.UIRGline.Comments
FROM         dbo.UIRGline INNER JOIN
                      dbo.TkPost ON dbo.UIRGline.PostId = dbo.TkPost.PostId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [UnitAliasesView]
AS
SELECT     dbo.UnitAliases.id, dbo.UnitAliases.alias AS name, dbo.Units.isBaseUnit, dbo.Units.type, dbo.Units.description, dbo.Units.m, dbo.Units.b
FROM         dbo.UnitAliases INNER JOIN
                      dbo.Units ON dbo.UnitAliases.name = dbo.Units.name
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [UnitsView]
AS
SELECT   * from Units
UNION
SELECT * from UnitAliasesView
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [TkUnitView]
AS
SELECT     dbo.TkUnit.UnitId, dbo.TkUnit.UnitName, dbo.TkUnit.UnitPrefix, dbo.TkUnit.UnitAlias, dbo.TkPost.PostId, dbo.TkPost.Post
FROM         dbo.TkUnit LEFT OUTER JOIN
                      dbo.TkPost ON dbo.TkUnit.PostId = dbo.TkPost.PostId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcWindow](
	[windowId] [int] IDENTITY(1,1) NOT NULL,
	[chartRunId] [varchar](900) NOT NULL,
	[controlPanelId] [int] NOT NULL,
	[windowPath] [varchar](64) NOT NULL,
	[buttonLabel] [varchar](64) NOT NULL,
	[position] [varchar](100) NOT NULL,
	[scale] [float] NOT NULL,
	[title] [varchar](100) NOT NULL,
 CONSTRAINT [PK_SfcWindow_1] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [TkConsoleView]
AS
SELECT     dbo.TkConsole.ConsoleId, dbo.TkConsole.ConsoleName, dbo.TkConsole.WindowName, dbo.TkConsole.Priority, dbo.TkPost.PostId, dbo.TkPost.Post
FROM         dbo.TkConsole INNER JOIN
                      dbo.TkPost ON dbo.TkConsole.PostId = dbo.TkPost.PostId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataTimer](
	[RecipeDataId] [int] NOT NULL,
	[StartTime] [datetime] NULL,
	[StopTime] [datetime] NULL,
	[TimerState] [varchar](10) NULL,
	[CumulativeMinutes] [float] NULL,
 CONSTRAINT [PK_SfcRecipeDataTimer] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcRecipeDataSQC](
	[RecipeDataId] [int] NOT NULL,
	[LowLimit] [float] NULL,
	[TargetValue] [float] NULL,
	[HighLimit] [float] NULL,
 CONSTRAINT [PK_SfcRecipeDataSQC] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataRecipe](
	[RecipeDataId] [int] NOT NULL,
	[PresentationOrder] [int] NOT NULL,
	[StoreTag] [varchar](max) NULL,
	[CompareTag] [varchar](max) NULL,
	[ModeAttribute] [varchar](max) NULL,
	[ModeValue] [varchar](max) NULL,
	[ChangeLevel] [varchar](max) NULL,
	[RecommendedValue] [varchar](max) NULL,
	[LowLimit] [varchar](max) NULL,
	[HighLimit] [varchar](max) NULL,
 CONSTRAINT [PK_SfcRecipeDataRecipe] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcRecipeDataOutputRamp](
	[RecipeDataId] [int] NOT NULL,
	[RampTimeMinutes] [float] NULL,
	[UpdateFrequencySeconds] [float] NULL,
 CONSTRAINT [PK_SfcRecipeDataOutputRamp] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, dbo.SfcRecipeData.RecipeDataFolderId, 
                      dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderName
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcRecipeData INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId ON dbo.SfcStep.StepId = dbo.SfcRecipeData.StepId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataValue](
	[ValueId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeDataId] [int] NOT NULL,
	[FloatValue] [float] NULL,
	[IntegerValue] [int] NULL,
	[StringValue] [varchar](1000) NULL,
	[BooleanValue] [bit] NULL,
 CONSTRAINT [PK_SfcRecipeDataValue] PRIMARY KEY CLUSTERED 
(
	[ValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcRecipeDataMatrix](
	[RecipeDataId] [int] NOT NULL,
	[ValueTypeId] [int] NOT NULL,
	[Rows] [int] NOT NULL,
	[Columns] [int] NOT NULL,
	[RowIndexKeyId] [int] NULL,
	[ColumnIndexKeyId] [int] NULL,
 CONSTRAINT [PK_SfcRecipeDataMatrix] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataFolderView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeDataFolder.RecipeDataFolderId, 
                      dbo.SfcRecipeDataFolder.RecipeDataKey, dbo.SfcRecipeDataFolder.ParentRecipeDataFolderId, dbo.SfcRecipeDataFolder.Description, dbo.SfcRecipeDataFolder.Label, 
                      dbo.SfcRecipeDataFolder.RecipeDataType, SfcRecipeDataFolder_1.RecipeDataKey AS ParentFolderName
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcStep.StepId = dbo.SfcRecipeDataFolder.StepId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder AS SfcRecipeDataFolder_1 ON dbo.SfcRecipeDataFolder.ParentRecipeDataFolderId = SfcRecipeDataFolder_1.RecipeDataFolderId;
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcRecipeDataArray](
	[RecipeDataId] [int] NOT NULL,
	[ValueTypeId] [int] NOT NULL,
	[IndexKeyId] [int] NULL,
 CONSTRAINT [PK_SfcRecipeDataArray] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcHierarchyView]
AS
SELECT     dbo.SfcHierarchy.HierarchyId, dbo.SfcHierarchy.StepId, dbo.SfcStepType.StepType, dbo.SfcHierarchy.ChartId, dbo.SfcHierarchy.ChildChartId, dbo.SfcChart.ChartPath, dbo.SfcChart.ChartResourceId, 
                      SfcChart_1.ChartPath AS ChildChartPath, SfcChart_1.ChartResourceId AS ChildResourceId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcStep.StepTypeId, dbo.SfcChart.IsProduction
FROM         dbo.SfcHierarchy INNER JOIN
                      dbo.SfcChart ON dbo.SfcHierarchy.ChartId = dbo.SfcChart.ChartId INNER JOIN
                      dbo.SfcChart AS SfcChart_1 ON dbo.SfcHierarchy.ChildChartId = SfcChart_1.ChartId INNER JOIN
                      dbo.SfcStep ON dbo.SfcHierarchy.StepId = dbo.SfcStep.StepId AND dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcStepType ON dbo.SfcStep.StepTypeId = dbo.SfcStepType.StepTypeId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcControlPanelMessage](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[controlPanelId] [int] NOT NULL,
	[message] [varchar](256) NOT NULL,
	[priority] [varchar](20) NOT NULL,
	[createTime] [datetime] NOT NULL,
	[ackRequired] [bit] NOT NULL,
 CONSTRAINT [PK_SfcControlPanelMessage] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE NONCLUSTERED INDEX [idx_control_msgs] ON [SfcControlPanelMessage] 
(
	[controlPanelId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtDownloadMaster](
	[MasterId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeFamilyId] [int] NOT NULL,
	[Grade] [varchar](50) NOT NULL,
	[Version] [int] NOT NULL,
	[Type] [varchar](50) NULL,
	[DownloadStartTime] [datetime] NOT NULL,
	[DownloadEndTime] [datetime] NULL,
	[Status] [varchar](50) NULL,
	[TotalDownloads] [int] NULL,
	[PassedDownloads] [int] NULL,
	[FailedDownloads] [int] NULL,
 CONSTRAINT [PK_DownloadMaster] PRIMARY KEY CLUSTERED 
(
	[MasterId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtValueDefinition](
	[RecipeFamilyId] [int] NOT NULL,
	[ValueId] [int] IDENTITY(1,1) NOT NULL,
	[PresentationOrder] [int] NOT NULL,
	[Description] [nvarchar](max) NULL,
	[StoreTag] [varchar](max) NULL,
	[CompareTag] [varchar](max) NULL,
	[ChangeLevel] [nvarchar](max) NULL,
	[ModeAttribute] [varchar](max) NULL,
	[ModeValue] [varchar](max) NULL,
	[WriteLocationId] [int] NULL,
	[ValueTypeId] [int] NULL,
 CONSTRAINT [PK__ValueDef__93364E481B0907CE] PRIMARY KEY CLUSTERED 
(
	[RecipeFamilyId] ASC,
	[ValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtSQCParameter](
	[ParameterId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeFamilyId] [int] NOT NULL,
	[Parameter] [varchar](max) NOT NULL,
 CONSTRAINT [PK_RtSQCParameter] PRIMARY KEY CLUSTERED 
(
	[ParameterId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [RtGain](
	[ParameterId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeFamilyId] [int] NULL,
	[Parameter] [nvarchar](max) NOT NULL,
 CONSTRAINT [PK__Gain__F80C6277286302EC] PRIMARY KEY CLUSTERED 
(
	[ParameterId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtEventParameter](
	[ParameterId] [int] IDENTITY(1,1) NOT NULL,
	[RecipeFamilyId] [int] NOT NULL,
	[Parameter] [varchar](max) NOT NULL,
 CONSTRAINT [PK_RtEventParameter] PRIMARY KEY CLUSTERED 
(
	[ParameterId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtGradeMaster](
	[RecipeFamilyId] [int] NOT NULL,
	[Grade] [varchar](50) NOT NULL,
	[Version] [int] NOT NULL,
	[Timestamp] [datetime] NULL,
	[Active] [bit] NULL,
 CONSTRAINT [PK_RtGradeMaster] PRIMARY KEY CLUSTERED 
(
	[RecipeFamilyId] ASC,
	[Grade] ASC,
	[Version] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtGradeDetail](
	[RecipeFamilyId] [int] NOT NULL,
	[Grade] [varchar](50) NOT NULL,
	[Version] [int] NOT NULL,
	[ValueId] [int] NOT NULL,
	[RecommendedValue] [varchar](max) NULL,
	[LowLimit] [varchar](max) NULL,
	[HighLimit] [varchar](max) NULL,
 CONSTRAINT [PK_RtGradeDetail] PRIMARY KEY CLUSTERED 
(
	[RecipeFamilyId] ASC,
	[Grade] ASC,
	[Version] ASC,
	[ValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtValue](
	[ValueId] [int] IDENTITY(1000,1) NOT NULL,
	[ValueName] [nvarchar](50) NOT NULL,
	[UnitId] [int] NOT NULL,
	[DisplayDecimals] [int] NOT NULL,
	[Description] [nvarchar](500) NULL,
	[ValidationProcedure] [varchar](250) NULL,
	[LastHistoryId] [int] NULL,
 CONSTRAINT [PK_LtLabValue] PRIMARY KEY CLUSTERED 
(
	[ValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_LtLabValue] ON [LtValue] 
(
	[ValueName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtApplication](
	[ApplicationId] [int] IDENTITY(1,1) NOT NULL,
	[ApplicationName] [varchar](250) NOT NULL,
	[UnitId] [int] NULL,
	[ApplicationUUID] [varchar](100) NULL,
	[Description] [varchar](2000) NULL,
	[IncludeInMainMenu] [bit] NULL,
	[MessageQueueId] [int] NULL,
	[GroupRampMethodId] [int] NULL,
	[DownloadAction] [varchar](50) NULL,
	[NotificationStrategy] [varchar](50) NOT NULL,
	[ClientId] [varchar](50) NULL,
	[Managed] [bit] NOT NULL,
 CONSTRAINT [PK_DtApplication] PRIMARY KEY CLUSTERED 
(
	[ApplicationId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_Application] ON [DtApplication] 
(
	[ApplicationName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [BtStripperBatchView]
AS
SELECT     dbo.BtReactor.ReactorId, dbo.BtReactor.ReactorName, dbo.BtReactor.TagName, dbo.BtBatchRun.BatchRunId, dbo.BtBatchRun.Grade, dbo.BtBatchRun.StartDate, dbo.BtBatchRun.EndDate, 
                      dbo.BtStripperBatchLog.BatchId, dbo.BtStripperBatchLog.BatchNumber, dbo.BtStripperBatchLog.BatchCount, dbo.BtStripperBatchLog.Status, dbo.BtStripperBatchLog.CreationTime, 
                      dbo.BtStripperBatchLog.LabResult, dbo.BtStripperBatchLog.FillBegin, dbo.BtStripperBatchLog.FillEnd, dbo.BtStripperBatchLog.FillTime, dbo.BtStripperBatchLog.StripBegin, 
                      dbo.BtStripperBatchLog.StripEnd, dbo.BtStripperBatchLog.StripTime, dbo.BtStripperBatchLog.JD03Begin, dbo.BtStripperBatchLog.JD03End, dbo.BtStripperBatchLog.JD03Time, 
                      dbo.BtStripperBatchLog.TransferBegin, dbo.BtStripperBatchLog.TransferEnd, dbo.BtStripperBatchLog.TransferTime, dbo.BtStripperBatchLog.StandbyBegin, dbo.BtStripperBatchLog.StandbyEnd, 
                      dbo.BtStripperBatchLog.StandbyTime, dbo.BtStripperBatchLog.TotalStripperTime, dbo.BtStripperBatchLog.TotalChargeAmount
FROM         dbo.BtReactor INNER JOIN
                      dbo.BtBatchRun ON dbo.BtReactor.ReactorId = dbo.BtBatchRun.ReactorId INNER JOIN
                      dbo.BtStripperBatchLog ON dbo.BtBatchRun.BatchRunId = dbo.BtStripperBatchLog.BatchRunId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [BtBatchView]
AS
SELECT     dbo.BtReactor.ReactorId, dbo.BtReactor.ReactorName, dbo.BtReactor.TagName, dbo.BtBatchRun.BatchRunId, dbo.BtBatchRun.Grade, dbo.BtBatchRun.StartDate, dbo.BtBatchRun.EndDate, 
                      dbo.BtBatchLog.BatchId, dbo.BtBatchLog.BatchNumber, dbo.BtBatchLog.BatchCount, dbo.BtBatchLog.Status, dbo.BtBatchLog.CreationTime, dbo.BtBatchLog.LabResult, 
                      dbo.BtBatchLog.ChargeBegin, dbo.BtBatchLog.ChargeEnd, dbo.BtBatchLog.ChargeTime, dbo.BtBatchLog.HeatUpBegin, dbo.BtBatchLog.HeatUpEnd, dbo.BtBatchLog.HeatUpTime, 
                      dbo.BtBatchLog.SoakBegin, dbo.BtBatchLog.SoakEnd, dbo.BtBatchLog.SoakTime, dbo.BtBatchLog.TransferBegin, dbo.BtBatchLog.TransferEnd, dbo.BtBatchLog.TransferTime, 
                      dbo.BtBatchLog.StandbyBegin, dbo.BtBatchLog.StandbyEnd, dbo.BtBatchLog.StandbyTime, dbo.BtBatchLog.TotalBatchTime, dbo.BtBatchLog.TotalChargeAmount, 
                      dbo.BtBatchLog.AverageSoakTemp, dbo.BtBatchLog.SoakTimer
FROM         dbo.BtReactor INNER JOIN
                      dbo.BtBatchRun ON dbo.BtReactor.ReactorId = dbo.BtBatchRun.ReactorId INNER JOIN
                      dbo.BtBatchLog ON dbo.BtBatchRun.BatchRunId = dbo.BtBatchLog.BatchRunId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtDCSValue](
	[DCSValueId] [int] IDENTITY(1,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[ItemId] [varchar](100) NOT NULL,
	[InterfaceId] [int] NOT NULL,
	[MinimumSampleIntervalSeconds] [int] NOT NULL,
	[AllowManualEntry] [bit] NOT NULL,
 CONSTRAINT [PK_LtDCSValue] PRIMARY KEY CLUSTERED 
(
	[DCSValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtDerivedValue](
	[DerivedValueId] [int] IDENTITY(1,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[TriggerValueId] [int] NOT NULL,
	[Callback] [varchar](250) NOT NULL,
	[SampleTimeTolerance] [int] NOT NULL,
	[NewSampleWaitTime] [int] NOT NULL,
	[ResultItemId] [varchar](100) NULL,
	[ResultInterfaceId] [int] NULL,
 CONSTRAINT [PK_LtDerivedValue] PRIMARY KEY CLUSTERED 
(
	[DerivedValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtFamily](
	[FamilyId] [int] IDENTITY(1,1) NOT NULL,
	[ApplicationId] [int] NOT NULL,
	[FamilyName] [varchar](250) NOT NULL,
	[FamilyPriority] [float] NOT NULL,
	[FamilyUUID] [varchar](100) NULL,
	[Description] [varchar](2000) NULL,
 CONSTRAINT [PK_DtFamily] PRIMARY KEY CLUSTERED 
(
	[FamilyId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_DtFamily] ON [DtFamily] 
(
	[FamilyName] ASC,
	[ApplicationId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtQuantOutput](
	[QuantOutputId] [int] IDENTITY(1,1) NOT NULL,
	[QuantOutputName] [varchar](750) NOT NULL,
	[ApplicationId] [int] NOT NULL,
	[TagPath] [varchar](1000) NOT NULL,
	[MostNegativeIncrement] [float] NOT NULL,
	[MostPositiveIncrement] [float] NOT NULL,
	[IgnoreMinimumIncrement] [bit] NOT NULL,
	[MinimumIncrement] [float] NOT NULL,
	[SetpointHighLimit] [float] NOT NULL,
	[SetpointLowLimit] [float] NOT NULL,
	[FeedbackMethodId] [int] NOT NULL,
	[IncrementalOutput] [bit] NOT NULL,
	[OutputLimitedStatus] [varchar](50) NULL,
	[OutputLimited] [bit] NULL,
	[OutputPercent] [float] NULL,
	[FeedbackOutput] [float] NULL,
	[FeedbackOutputManual] [float] NULL,
	[FeedbackOutputConditioned] [float] NULL,
	[ManualOverride] [bit] NULL,
	[Active] [bit] NULL,
	[DownloadAction] [varchar](25) NULL,
	[DownloadStatus] [varchar](100) NULL,
	[CurrentSetpoint] [float] NULL,
	[FinalSetpoint] [float] NULL,
	[DisplayedRecommendation] [float] NULL,
 CONSTRAINT [PK_DtQuantOutput] PRIMARY KEY CLUSTERED 
(
	[QuantOutputId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_DtQuantOutput] ON [DtQuantOutput] 
(
	[QuantOutputName] ASC,
	[ApplicationId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [DtApplicationView]
AS
SELECT     dbo.DtApplication.ApplicationId, dbo.TkPost.Post, dbo.TkUnit.UnitName, dbo.DtApplication.ApplicationName, dbo.DtApplication.Description, dbo.DtApplication.IncludeInMainMenu, 
                      dbo.QueueMaster.QueueKey, dbo.DtApplication.DownloadAction, dbo.DtApplication.NotificationStrategy, dbo.DtApplication.Managed, dbo.Lookup.LookupName AS GroupRampMethodName, 
                      dbo.Lookup.LookupTypeCode AS GroupRampMethodCode
FROM         dbo.DtApplication INNER JOIN
                      dbo.TkUnit ON dbo.DtApplication.UnitId = dbo.TkUnit.UnitId INNER JOIN
                      dbo.QueueMaster ON dbo.DtApplication.MessageQueueId = dbo.QueueMaster.QueueId INNER JOIN
                      dbo.TkPost ON dbo.TkUnit.PostId = dbo.TkPost.PostId INNER JOIN
                      dbo.Lookup ON dbo.DtApplication.GroupRampMethodId = dbo.Lookup.LookupId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [LtDisplayTableDetails](
	[Id] [int] IDENTITY(1,1) NOT NULL,
	[DisplayTableId] [int] NOT NULL,
	[ValueId] [int] NOT NULL,
	[DisplayOrder] [int] NOT NULL,
 CONSTRAINT [PK_LtDisplayTableDetails] PRIMARY KEY CLUSTERED 
(
	[Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtHistory](
	[HistoryId] [int] IDENTITY(1000,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[RawValue] [float] NOT NULL,
	[SampleTime] [datetime] NOT NULL,
	[ReportTime] [datetime] NOT NULL,
	[Grade] [varchar](50) NULL,
 CONSTRAINT [PK_LtHistory] PRIMARY KEY CLUSTERED 
(
	[HistoryId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_LtHistory] ON [LtHistory] 
(
	[ValueId] ASC,
	[RawValue] ASC,
	[SampleTime] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtLimit](
	[LimitId] [int] IDENTITY(1000,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[LimitTypeId] [int] NOT NULL,
	[LimitSourceId] [int] NOT NULL,
	[RecipeParameterName] [varchar](100) NULL,
	[UpperValidityLimit] [float] NULL,
	[LowerValidityLimit] [float] NULL,
	[UpperSQCLimit] [float] NULL,
	[LowerSQCLimit] [float] NULL,
	[UpperReleaseLimit] [float] NULL,
	[LowerReleaseLimit] [float] NULL,
	[Target] [float] NULL,
	[StandardDeviation] [float] NULL,
	[OPCUpperItemId] [varchar](50) NULL,
	[OPCLowerItemId] [varchar](50) NULL,
	[OPCInterfaceId] [int] NULL,
 CONSTRAINT [PK_LtLimit] PRIMARY KEY CLUSTERED 
(
	[LimitId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtPHDValue](
	[PHDValueId] [int] IDENTITY(1,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[ItemId] [varchar](50) NOT NULL,
	[InterfaceId] [int] NOT NULL,
	[AllowManualEntry] [bit] NOT NULL,
 CONSTRAINT [PK_LtPHDLabValue] PRIMARY KEY CLUSTERED 
(
	[PHDValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtLocalValue](
	[LocalValueId] [int] IDENTITY(1,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[ItemId] [varchar](500) NULL,
	[InterfaceId] [int] NULL,
 CONSTRAINT [PK_LtLocalValue] PRIMARY KEY CLUSTERED 
(
	[LocalValueId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [LtValueViewed](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[Username] [varchar](25) NOT NULL,
	[ViewTime] [datetime] NOT NULL,
 CONSTRAINT [PK_LtValueViewed] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcBusyNotification](
	[windowId] [int] NOT NULL,
	[message] [varchar](900) NULL,
 CONSTRAINT [PK_SfcBusyNotification] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtEvent](
	[ParameterId] [int] NOT NULL,
	[Grade] [varchar](50) NOT NULL,
	[Value] [float] NULL,
 CONSTRAINT [PK_RtEvent] PRIMARY KEY CLUSTERED 
(
	[ParameterId] ASC,
	[Grade] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [LtSelector](
	[SelectorId] [int] IDENTITY(1000,1) NOT NULL,
	[ValueId] [int] NOT NULL,
	[hasSQCLimit] [bit] NOT NULL,
	[hasValidityLimit] [bit] NOT NULL,
	[hasReleaseLimit] [bit] NOT NULL,
	[sourceValueId] [int] NULL,
 CONSTRAINT [PK_LtSelector] PRIMARY KEY CLUSTERED 
(
	[SelectorId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtSQCLimit](
	[ParameterId] [int] NOT NULL,
	[Grade] [varchar](50) NOT NULL,
	[UpperLimit] [float] NULL,
	[LowerLimit] [float] NULL,
 CONSTRAINT [PK_RtSQCLimit] PRIMARY KEY CLUSTERED 
(
	[ParameterId] ASC,
	[Grade] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [RtRecipeView]
AS
SELECT     dbo.RtRecipeFamily.RecipeFamilyId, dbo.RtRecipeFamily.RecipeFamilyName, dbo.RtGradeMaster.Grade, dbo.RtGradeMaster.Version, dbo.RtGradeDetail.ValueId, 
                      dbo.RtValueDefinition.PresentationOrder, dbo.RtValueDefinition.Description, dbo.RtValueDefinition.StoreTag, dbo.RtValueDefinition.CompareTag, dbo.RtValueDefinition.ChangeLevel, 
                      dbo.RtValueDefinition.ModeAttribute, dbo.RtValueDefinition.ModeValue, dbo.RtGradeDetail.RecommendedValue, dbo.RtGradeDetail.LowLimit, dbo.RtGradeDetail.HighLimit, 
                      dbo.RtValueType.ValueType
FROM         dbo.RtRecipeFamily INNER JOIN
                      dbo.RtGradeMaster ON dbo.RtRecipeFamily.RecipeFamilyId = dbo.RtGradeMaster.RecipeFamilyId INNER JOIN
                      dbo.RtGradeDetail ON dbo.RtGradeMaster.RecipeFamilyId = dbo.RtGradeDetail.RecipeFamilyId AND dbo.RtGradeMaster.Grade = dbo.RtGradeDetail.Grade AND 
                      dbo.RtGradeMaster.Version = dbo.RtGradeDetail.Version INNER JOIN
                      dbo.RtValueDefinition ON dbo.RtGradeDetail.RecipeFamilyId = dbo.RtValueDefinition.RecipeFamilyId AND dbo.RtGradeDetail.ValueId = dbo.RtValueDefinition.ValueId INNER JOIN
                      dbo.RtValueType ON dbo.RtValueDefinition.ValueTypeId = dbo.RtValueType.ValueTypeId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [RtGainGrade](
	[ParameterId] [int] NOT NULL,
	[Grade] [varchar](50) NOT NULL,
	[Gain] [float] NULL,
 CONSTRAINT [PK_RtGainGrade] PRIMARY KEY CLUSTERED 
(
	[ParameterId] ASC,
	[Grade] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcDownloadGUI](
	[WindowId] [int] NOT NULL,
	[State] [varchar](25) NOT NULL,
	[TimerRecipeDataId] [int] NOT NULL,
	[LastUpdated] [datetime] NULL,
	[StartTime] [datetime] NULL,
 CONSTRAINT [PK_SfcDownloadGUI] PRIMARY KEY CLUSTERED 
(
	[WindowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcDialogMessage](
	[windowId] [int] NOT NULL,
	[message] [varchar](900) NOT NULL,
	[ackRequired] [bit] NOT NULL,
	[acknowledged] [bit] NULL,
 CONSTRAINT [PK_SfcDialogMsg] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcManualDataEntry](
	[windowId] [int] NOT NULL,
	[requireAllInputs] [bit] NOT NULL,
	[complete] [bit] NOT NULL,
	[header] [varchar](1024) NULL,
 CONSTRAINT [PK_SfcManualDataEntry] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcInput](
	[windowId] [int] NOT NULL,
	[prompt] [varchar](900) NOT NULL,
	[lowLimit] [float] NULL,
	[highLimit] [float] NULL,
	[responseLocation] [varchar](25) NULL,
	[targetStepId] [int] NULL,
	[keyAndAttribute] [varchar](255) NULL,
	[defaultValue] [varchar](255) NULL,
	[chartId] [varchar](200) NULL,
	[stepId] [varchar](200) NULL,
 CONSTRAINT [PK_SfcInput] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataArrayView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepName, dbo.SfcStep.StepUUID, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, dbo.SfcValueType.ValueType, dbo.SfcRecipeDataArray.ValueTypeId, 
                      dbo.SfcRecipeDataArray.IndexKeyId, dbo.SfcRecipeDataKeyMaster.KeyName, dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcRecipeData INNER JOIN
                      dbo.SfcRecipeDataArray ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataArray.RecipeDataId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcValueType ON dbo.SfcRecipeDataArray.ValueTypeId = dbo.SfcValueType.ValueTypeId INNER JOIN
                      dbo.SfcStep ON dbo.SfcRecipeData.StepId = dbo.SfcStep.StepId INNER JOIN
                      dbo.SfcChart ON dbo.SfcStep.ChartId = dbo.SfcChart.ChartId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId LEFT OUTER JOIN
                      dbo.SfcRecipeDataKeyMaster ON dbo.SfcRecipeDataArray.IndexKeyId = dbo.SfcRecipeDataKeyMaster.KeyId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataInput](
	[RecipeDataId] [int] NOT NULL,
	[ValueTypeId] [int] NULL,
	[Tag] [varchar](500) NULL,
	[ErrorCode] [varchar](50) NULL,
	[ErrorText] [varchar](5000) NULL,
	[PVMonitorActive] [bit] NULL,
	[PVMonitorStatus] [varchar](25) NULL,
	[PVValueId] [int] NULL,
	[TargetValueId] [int] NULL,
 CONSTRAINT [PK_SfcRecipeDataInput] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcRecipeDataArrayElement](
	[RecipeDataId] [int] NOT NULL,
	[ArrayIndex] [int] NOT NULL,
	[ValueId] [int] NULL,
 CONSTRAINT [PK_SfcRecipeDataArrayElement] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC,
	[ArrayIndex] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataTimerView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeDataType.JavaClassName, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, 
                      dbo.SfcRecipeDataTimer.StartTime, dbo.SfcRecipeDataTimer.StopTime, dbo.SfcRecipeDataTimer.TimerState, dbo.SfcRecipeDataTimer.CumulativeMinutes, 
                      dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcRecipeData ON dbo.SfcStep.StepId = dbo.SfcRecipeData.StepId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcRecipeDataTimer ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataTimer.RecipeDataId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcRecipeDataOutput](
	[RecipeDataId] [int] NOT NULL,
	[ValueTypeId] [int] NOT NULL,
	[OutputTypeId] [int] NOT NULL,
	[Tag] [varchar](500) NULL,
	[Download] [bit] NULL,
	[DownloadStatus] [varchar](50) NULL,
	[ErrorCode] [varchar](50) NULL,
	[ErrorText] [varchar](1000) NULL,
	[Timing] [float] NULL,
	[MaxTiming] [float] NULL,
	[ActualTiming] [float] NULL,
	[ActualDateTime] [datetime] NULL,
	[OutputValueId] [int] NOT NULL,
	[TargetValueId] [int] NOT NULL,
	[PVValueId] [int] NOT NULL,
	[PVMonitorActive] [bit] NULL,
	[PVMonitorStatus] [varchar](50) NULL,
	[SetpointStatus] [varchar](50) NULL,
	[WriteConfirm] [bit] NULL,
	[WriteConfirmed] [bit] NULL,
 CONSTRAINT [PK_SfcRecipeDataOutput] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataMatrixView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcValueType.ValueType, dbo.SfcRecipeDataMatrix.Rows, 
                      dbo.SfcRecipeDataMatrix.Columns, dbo.SfcRecipeDataMatrix.RowIndexKeyId, dbo.SfcRecipeDataKeyMaster.KeyName AS RowIndexKeyName, dbo.SfcRecipeDataMatrix.ColumnIndexKeyId, 
                      SfcRecipeDataKeyMaster_1.KeyName AS ColumnIndexKeyName, dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcRecipeData ON dbo.SfcStep.StepId = dbo.SfcRecipeData.StepId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcRecipeDataMatrix ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataMatrix.RecipeDataId INNER JOIN
                      dbo.SfcValueType ON dbo.SfcRecipeDataMatrix.ValueTypeId = dbo.SfcValueType.ValueTypeId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId LEFT OUTER JOIN
                      dbo.SfcRecipeDataKeyMaster ON dbo.SfcRecipeDataMatrix.RowIndexKeyId = dbo.SfcRecipeDataKeyMaster.KeyId LEFT OUTER JOIN
                      dbo.SfcRecipeDataKeyMaster AS SfcRecipeDataKeyMaster_1 ON dbo.SfcRecipeDataMatrix.ColumnIndexKeyId = SfcRecipeDataKeyMaster_1.KeyId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcRecipeDataMatrixElement](
	[RecipeDataId] [int] NOT NULL,
	[RowIndex] [int] NOT NULL,
	[ColumnIndex] [int] NOT NULL,
	[ValueId] [int] NOT NULL,
 CONSTRAINT [PK_SfcRecipeDataMatrixElement] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC,
	[RowIndex] ASC,
	[ColumnIndex] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [SfcRecipeDataSimpleValue](
	[RecipeDataId] [int] NOT NULL,
	[ValueTypeId] [int] NULL,
	[ValueId] [int] NULL,
 CONSTRAINT [PK_SfcRecipeDataSimpleValue] PRIMARY KEY CLUSTERED 
(
	[RecipeDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataRecipeView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, dbo.SfcRecipeDataRecipe.PresentationOrder, 
                      dbo.SfcRecipeDataRecipe.StoreTag, dbo.SfcRecipeDataRecipe.CompareTag, dbo.SfcRecipeDataRecipe.ModeAttribute, dbo.SfcRecipeDataRecipe.ModeValue, 
                      dbo.SfcRecipeDataRecipe.ChangeLevel, dbo.SfcRecipeDataRecipe.RecommendedValue, dbo.SfcRecipeDataRecipe.LowLimit, dbo.SfcRecipeDataRecipe.HighLimit, 
                      dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcRecipeData ON dbo.SfcStep.StepId = dbo.SfcRecipeData.StepId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcRecipeDataRecipe ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataRecipe.RecipeDataId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcSelectInput](
	[windowId] [int] NOT NULL,
	[prompt] [varchar](900) NOT NULL,
	[choicesStepId] [int] NOT NULL,
	[choicesKey] [varchar](500) NOT NULL,
	[responseLocation] [varchar](25) NULL,
	[targetStepId] [int] NULL,
	[keyAndAttribute] [varchar](255) NULL,
	[chartId] [varchar](200) NULL,
	[stepId] [varchar](200) NULL,
 CONSTRAINT [PK_SfcSelectInput] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcSaveData](
	[windowId] [int] NOT NULL,
	[textData] [varchar](max) NULL,
	[binaryData] [varbinary](max) NULL,
	[fileLocation] [varchar](25) NOT NULL,
	[printText] [bit] NOT NULL,
	[viewText] [bit] NOT NULL,
	[showPrintDialog] [bit] NOT NULL,
	[filePath] [varchar](900) NULL,
 CONSTRAINT [PK_SfcSaveData] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcTimeDelayNotification](
	[windowId] [int] NOT NULL,
	[message] [varchar](900) NOT NULL,
	[endTime] [datetime] NOT NULL,
 CONSTRAINT [PK_SfcTimeDelayNotification] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataSQCView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, dbo.SfcRecipeDataSQC.LowLimit, dbo.SfcRecipeDataSQC.TargetValue, dbo.SfcRecipeDataSQC.HighLimit, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeDataType.JavaClassName, dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, 
                      dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcRecipeData ON dbo.SfcStep.StepId = dbo.SfcRecipeData.StepId INNER JOIN
                      dbo.SfcRecipeDataSQC ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataSQC.RecipeDataId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcReviewData](
	[windowId] [int] NOT NULL,
	[showAdvice] [bit] NOT NULL,
	[targetStepUUID] [varchar](255) NULL,
	[responseKey] [varchar](255) NULL,
	[primaryTabLabel] [varchar](100) NULL,
	[secondaryTabLabel] [varchar](100) NULL,
 CONSTRAINT [PK_SfcManualDataEntryTable] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcReviewFlows](
	[windowId] [int] NOT NULL,
	[heading1] [varchar](900) NOT NULL,
	[heading2] [varchar](900) NOT NULL,
	[heading3] [varchar](900) NOT NULL,
	[targetStepUUID] [varchar](255) NOT NULL,
	[responseKey] [varchar](255) NOT NULL,
	[primaryTabLabel] [varchar](100) NULL,
	[secondaryTabLabel] [varchar](100) NULL,
 CONSTRAINT [PK_SfcReviewFlows] PRIMARY KEY CLUSTERED 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcReviewFlowsTable](
	[windowId] [int] NOT NULL,
	[rowNum] [int] NOT NULL,
	[configKey] [varchar](150) NOT NULL,
	[advice] [varchar](900) NOT NULL,
	[units] [varchar](900) NOT NULL,
	[prompt] [varchar](900) NOT NULL,
	[data1] [varchar](150) NOT NULL,
	[data2] [varchar](150) NOT NULL,
	[data3] [varchar](150) NOT NULL,
	[isPrimary] [bit] NOT NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE NONCLUSTERED INDEX [idx_SfcReviewFlowsTable] ON [SfcReviewFlowsTable] 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcReviewDataTable](
	[windowId] [int] NOT NULL,
	[rowNum] [int] NOT NULL,
	[configKey] [varchar](150) NULL,
	[prompt] [varchar](150) NULL,
	[value] [varchar](150) NULL,
	[units] [varchar](20) NULL,
	[isPrimary] [bit] NOT NULL,
	[advice] [varchar](150) NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE NONCLUSTERED INDEX [idx_SfcReviewDataTable] ON [SfcReviewDataTable] 
(
	[windowId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataSimpleValueView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeDataType.RecipeDataType, 
                      dbo.SfcRecipeDataType.JavaClassName, dbo.SfcRecipeData.RecipeDataKey, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Units, dbo.SfcValueType.ValueType, 
                      dbo.SfcRecipeDataSimpleValue.ValueId, dbo.SfcRecipeDataValue.FloatValue, dbo.SfcRecipeDataValue.IntegerValue, dbo.SfcRecipeDataValue.StringValue, dbo.SfcRecipeDataValue.BooleanValue, 
                      dbo.SfcRecipeDataSimpleValue.ValueTypeId, dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcRecipeData INNER JOIN
                      dbo.SfcRecipeDataSimpleValue ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataSimpleValue.RecipeDataId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcValueType ON dbo.SfcRecipeDataSimpleValue.ValueTypeId = dbo.SfcValueType.ValueTypeId INNER JOIN
                      dbo.SfcStep ON dbo.SfcRecipeData.StepId = dbo.SfcStep.StepId INNER JOIN
                      dbo.SfcChart ON dbo.SfcStep.ChartId = dbo.SfcChart.ChartId INNER JOIN
                      dbo.SfcRecipeDataValue ON dbo.SfcRecipeDataSimpleValue.ValueId = dbo.SfcRecipeDataValue.ValueId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataOutputView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepName, dbo.SfcStep.StepUUID, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeDataType.JavaClassName, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeDataOutput.Tag, 
                      dbo.SfcRecipeData.Units, dbo.SfcValueType.ValueType, dbo.SfcRecipeDataOutputType.OutputType, dbo.SfcRecipeDataOutput.Download, dbo.SfcRecipeDataOutput.DownloadStatus, 
                      dbo.SfcRecipeDataOutput.ErrorCode, dbo.SfcRecipeDataOutput.ErrorText, dbo.SfcRecipeDataOutput.Timing, dbo.SfcRecipeDataOutput.MaxTiming, dbo.SfcRecipeDataOutput.ActualTiming, 
                      dbo.SfcRecipeDataOutput.ActualDateTime, dbo.SfcRecipeDataOutput.PVMonitorActive, dbo.SfcRecipeDataOutput.PVMonitorStatus, dbo.SfcRecipeDataOutput.WriteConfirm, 
                      dbo.SfcRecipeDataOutput.WriteConfirmed, dbo.SfcRecipeDataOutput.OutputValueId, dbo.SfcRecipeDataValue.FloatValue AS OutputFloatValue, 
                      dbo.SfcRecipeDataValue.IntegerValue AS OutputIntegerValue, dbo.SfcRecipeDataValue.StringValue AS OutputStringValue, dbo.SfcRecipeDataValue.BooleanValue AS OutputBooleanValue, 
                      dbo.SfcRecipeDataOutput.TargetValueId, SfcRecipeDataValue_1.FloatValue AS TargetFloatValue, SfcRecipeDataValue_1.IntegerValue AS TargetIntegerValue, 
                      SfcRecipeDataValue_1.StringValue AS TargetStringValue, SfcRecipeDataValue_1.BooleanValue AS TargetBooleanValue, dbo.SfcRecipeDataOutput.PVValueId, 
                      SfcRecipeDataValue_2.FloatValue AS PVFloatValue, SfcRecipeDataValue_2.IntegerValue AS PVIntegerValue, SfcRecipeDataValue_2.StringValue AS PVStringValue, 
                      SfcRecipeDataValue_2.BooleanValue AS PVBooleanValue, dbo.SfcRecipeDataOutput.SetpointStatus, dbo.SfcRecipeDataOutput.ValueTypeId, dbo.SfcRecipeDataOutput.OutputTypeId, 
                      dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcRecipeDataOutput INNER JOIN
                      dbo.SfcRecipeDataOutputType ON dbo.SfcRecipeDataOutput.OutputTypeId = dbo.SfcRecipeDataOutputType.OutputTypeId INNER JOIN
                      dbo.SfcRecipeData ON dbo.SfcRecipeDataOutput.RecipeDataId = dbo.SfcRecipeData.RecipeDataId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcValueType ON dbo.SfcRecipeDataOutput.ValueTypeId = dbo.SfcValueType.ValueTypeId INNER JOIN
                      dbo.SfcStep ON dbo.SfcRecipeData.StepId = dbo.SfcStep.StepId INNER JOIN
                      dbo.SfcChart ON dbo.SfcStep.ChartId = dbo.SfcChart.ChartId INNER JOIN
                      dbo.SfcRecipeDataValue ON dbo.SfcRecipeDataOutput.OutputValueId = dbo.SfcRecipeDataValue.ValueId INNER JOIN
                      dbo.SfcRecipeDataValue AS SfcRecipeDataValue_1 ON dbo.SfcRecipeDataOutput.TargetValueId = SfcRecipeDataValue_1.ValueId INNER JOIN
                      dbo.SfcRecipeDataValue AS SfcRecipeDataValue_2 ON dbo.SfcRecipeDataOutput.PVValueId = SfcRecipeDataValue_2.ValueId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
WHERE     (dbo.SfcRecipeDataType.RecipeDataType = 'Output')
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataOutputRampView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, dbo.SfcRecipeDataOutput.Tag, 
                      dbo.SfcRecipeDataOutputType.OutputType, dbo.SfcValueType.ValueType, dbo.SfcRecipeDataOutput.Download, dbo.SfcRecipeDataOutput.DownloadStatus, dbo.SfcRecipeDataOutput.ErrorCode, 
                      dbo.SfcRecipeDataOutput.ErrorText, dbo.SfcRecipeDataOutput.Timing, dbo.SfcRecipeDataOutput.MaxTiming, dbo.SfcRecipeDataOutput.ActualTiming, dbo.SfcRecipeDataOutput.ActualDateTime, 
                      dbo.SfcRecipeDataOutput.PVMonitorActive, dbo.SfcRecipeDataOutput.PVMonitorStatus, dbo.SfcRecipeDataOutput.WriteConfirm, dbo.SfcRecipeDataOutput.WriteConfirmed, 
                      dbo.SfcRecipeDataOutputRamp.RampTimeMinutes, dbo.SfcRecipeDataOutputRamp.UpdateFrequencySeconds, dbo.SfcRecipeDataOutput.OutputValueId, 
                      dbo.SfcRecipeDataValue.FloatValue AS OutputFloatValue, dbo.SfcRecipeDataValue.IntegerValue AS OutputIntegerValue, dbo.SfcRecipeDataValue.StringValue AS OutputStringValue, 
                      dbo.SfcRecipeDataValue.BooleanValue AS OutputBooleanValue, dbo.SfcRecipeDataOutput.TargetValueId, SfcRecipeDataValue_1.FloatValue AS TargetFloatValue, 
                      SfcRecipeDataValue_1.IntegerValue AS TargetIntegerValue, SfcRecipeDataValue_1.StringValue AS TargetStringValue, SfcRecipeDataValue_1.BooleanValue AS TargetBooleanValue, 
                      dbo.SfcRecipeDataOutput.PVValueId, SfcRecipeDataValue_2.FloatValue AS PVFloatValue, SfcRecipeDataValue_2.IntegerValue AS PVIntegerValue, 
                      SfcRecipeDataValue_2.StringValue AS PVStringValue, SfcRecipeDataValue_2.BooleanValue AS PVBooleanValue, dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, 
                      dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcRecipeData INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcRecipeDataOutput ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataOutput.RecipeDataId INNER JOIN
                      dbo.SfcRecipeDataOutputRamp ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataOutputRamp.RecipeDataId INNER JOIN
                      dbo.SfcValueType ON dbo.SfcRecipeDataOutput.ValueTypeId = dbo.SfcValueType.ValueTypeId INNER JOIN
                      dbo.SfcRecipeDataOutputType ON dbo.SfcRecipeDataOutput.OutputTypeId = dbo.SfcRecipeDataOutputType.OutputTypeId INNER JOIN
                      dbo.SfcStep ON dbo.SfcRecipeData.StepId = dbo.SfcStep.StepId INNER JOIN
                      dbo.SfcChart ON dbo.SfcStep.ChartId = dbo.SfcChart.ChartId INNER JOIN
                      dbo.SfcRecipeDataValue ON dbo.SfcRecipeDataOutput.OutputValueId = dbo.SfcRecipeDataValue.ValueId INNER JOIN
                      dbo.SfcRecipeDataValue AS SfcRecipeDataValue_1 ON dbo.SfcRecipeDataOutput.TargetValueId = SfcRecipeDataValue_1.ValueId INNER JOIN
                      dbo.SfcRecipeDataValue AS SfcRecipeDataValue_2 ON dbo.SfcRecipeDataOutput.PVValueId = SfcRecipeDataValue_2.ValueId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataMatrixElementView]
AS
SELECT     TOP (100) PERCENT dbo.SfcRecipeDataMatrixElement.RecipeDataId, dbo.SfcRecipeDataMatrixElement.RowIndex, dbo.SfcRecipeDataMatrixElement.ColumnIndex, 
                      dbo.SfcRecipeDataMatrixElement.ValueId, dbo.SfcRecipeDataValue.FloatValue, dbo.SfcRecipeDataValue.IntegerValue, dbo.SfcRecipeDataValue.StringValue, 
                      dbo.SfcRecipeDataValue.BooleanValue
FROM         dbo.SfcRecipeDataMatrixElement INNER JOIN
                      dbo.SfcRecipeDataValue ON dbo.SfcRecipeDataMatrixElement.ValueId = dbo.SfcRecipeDataValue.ValueId
ORDER BY dbo.SfcRecipeDataMatrixElement.RecipeDataId, dbo.SfcRecipeDataMatrixElement.RowIndex, dbo.SfcRecipeDataMatrixElement.ColumnIndex
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataInputView]
AS
SELECT     dbo.SfcChart.ChartId, dbo.SfcChart.ChartPath, dbo.SfcStep.StepId, dbo.SfcStep.StepUUID, dbo.SfcStep.StepName, dbo.SfcRecipeData.RecipeDataId, dbo.SfcRecipeData.RecipeDataKey, 
                      dbo.SfcRecipeDataType.RecipeDataType, dbo.SfcRecipeDataType.JavaClassName, dbo.SfcRecipeData.Label, dbo.SfcRecipeData.Description, dbo.SfcRecipeData.Units, 
                      dbo.SfcRecipeDataInput.Tag, dbo.SfcRecipeDataInput.ErrorCode, dbo.SfcRecipeDataInput.ErrorText, dbo.SfcRecipeDataInput.PVMonitorActive, dbo.SfcRecipeDataInput.PVMonitorStatus, 
                      dbo.SfcValueType.ValueType, dbo.SfcRecipeDataInput.PVValueId, dbo.SfcRecipeDataValue.FloatValue AS PVFloatValue, dbo.SfcRecipeDataValue.IntegerValue AS PVIntegerValue, 
                      dbo.SfcRecipeDataValue.StringValue AS PVStringValue, dbo.SfcRecipeDataValue.BooleanValue AS PVBooleanValue, dbo.SfcRecipeDataInput.TargetValueId, 
                      SfcRecipeDataValue_1.FloatValue AS TargetFloatValue, SfcRecipeDataValue_1.IntegerValue AS TargetIntegerValue, SfcRecipeDataValue_1.StringValue AS TargetStringValue, 
                      SfcRecipeDataValue_1.BooleanValue AS TargetBooleanValue, dbo.SfcRecipeDataInput.ValueTypeId, dbo.SfcRecipeData.RecipeDataFolderId AS FolderId, 
                      dbo.SfcRecipeDataFolder.RecipeDataKey AS FolderKey
FROM         dbo.SfcChart INNER JOIN
                      dbo.SfcStep ON dbo.SfcChart.ChartId = dbo.SfcStep.ChartId INNER JOIN
                      dbo.SfcRecipeData ON dbo.SfcStep.StepId = dbo.SfcRecipeData.StepId INNER JOIN
                      dbo.SfcRecipeDataType ON dbo.SfcRecipeData.RecipeDataTypeId = dbo.SfcRecipeDataType.RecipeDataTypeId INNER JOIN
                      dbo.SfcRecipeDataInput ON dbo.SfcRecipeData.RecipeDataId = dbo.SfcRecipeDataInput.RecipeDataId INNER JOIN
                      dbo.SfcRecipeDataValue ON dbo.SfcRecipeDataInput.PVValueId = dbo.SfcRecipeDataValue.ValueId INNER JOIN
                      dbo.SfcRecipeDataValue AS SfcRecipeDataValue_1 ON dbo.SfcRecipeDataInput.TargetValueId = SfcRecipeDataValue_1.ValueId INNER JOIN
                      dbo.SfcValueType ON dbo.SfcRecipeDataInput.ValueTypeId = dbo.SfcValueType.ValueTypeId LEFT OUTER JOIN
                      dbo.SfcRecipeDataFolder ON dbo.SfcRecipeData.RecipeDataFolderId = dbo.SfcRecipeDataFolder.RecipeDataFolderId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [SfcRecipeDataArrayElementView]
AS
SELECT     dbo.SfcRecipeDataArrayElement.RecipeDataId, dbo.SfcRecipeDataArrayElement.ArrayIndex, dbo.SfcRecipeDataArrayElement.ValueId, dbo.SfcRecipeDataValue.FloatValue, 
                      dbo.SfcRecipeDataValue.IntegerValue, dbo.SfcRecipeDataValue.StringValue, dbo.SfcRecipeDataValue.BooleanValue
FROM         dbo.SfcRecipeDataArrayElement INNER JOIN
                      dbo.SfcRecipeDataValue ON dbo.SfcRecipeDataArrayElement.ValueId = dbo.SfcRecipeDataValue.ValueId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcManualDataEntryTable](
	[windowId] [int] NOT NULL,
	[rowNum] [int] NOT NULL,
	[description] [varchar](900) NOT NULL,
	[value] [varchar](900) NOT NULL,
	[units] [varchar](900) NOT NULL,
	[dataKey] [varchar](900) NOT NULL,
	[destination] [varchar](900) NOT NULL,
	[targetStepId] [int] NULL,
	[type] [varchar](900) NOT NULL,
	[recipeUnits] [varchar](900) NOT NULL,
	[lowLimit] [float] NULL,
	[highLimit] [float] NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [SfcDownloadGUITable](
	[windowId] [int] NOT NULL,
	[RecipeDataId] [int] NULL,
	[RecipeDataType] [varchar](50) NULL,
	[LabelAttribute] [varchar](50) NULL,
	[RawTiming] [float] NULL,
	[Timing] [float] NULL,
	[DcsTagId] [varchar](900) NULL,
	[SetPoint] [varchar](50) NULL,
	[Description] [varchar](900) NULL,
	[StepTimestamp] [varchar](900) NULL,
	[PV] [varchar](50) NULL,
	[DownloadStatus] [varchar](900) NULL,
	[PVMonitorStatus] [varchar](900) NULL,
	[SetpointStatus] [varchar](900) NULL
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [RtSQCLimitView]
AS
SELECT     dbo.RtRecipeFamily.RecipeFamilyName, dbo.RtSQCParameter.Parameter, dbo.RtSQCLimit.Grade, dbo.RtSQCLimit.UpperLimit, dbo.RtSQCLimit.LowerLimit
FROM         dbo.RtSQCParameter INNER JOIN
                      dbo.RtSQCLimit ON dbo.RtSQCParameter.ParameterId = dbo.RtSQCLimit.ParameterId INNER JOIN
                      dbo.RtRecipeFamily ON dbo.RtSQCParameter.RecipeFamilyId = dbo.RtRecipeFamily.RecipeFamilyId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [LtRelatedData](
	[RelatedDataId] [int] IDENTITY(1,1) NOT NULL,
	[DerivedValueId] [int] NOT NULL,
	[RelatedValueId] [int] NOT NULL,
 CONSTRAINT [PK_LtRelatedData] PRIMARY KEY CLUSTERED 
(
	[RelatedDataId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtPHDValueView]
AS
SELECT     dbo.LtValue.ValueId, dbo.LtValue.ValueName, dbo.LtValue.Description, dbo.LtValue.DisplayDecimals, dbo.LtValue.LastHistoryId, dbo.LtPHDValue.ItemId, dbo.LtHDAInterface.InterfaceName, 
                      dbo.TkPost.Post, dbo.TkUnit.UnitName, dbo.LtValue.ValidationProcedure, dbo.LtHistory.SampleTime, dbo.LtPHDValue.AllowManualEntry
FROM         dbo.LtValue INNER JOIN
                      dbo.LtPHDValue ON dbo.LtValue.ValueId = dbo.LtPHDValue.ValueId INNER JOIN
                      dbo.LtHDAInterface ON dbo.LtPHDValue.InterfaceId = dbo.LtHDAInterface.InterfaceId INNER JOIN
                      dbo.TkUnit ON dbo.LtValue.UnitId = dbo.TkUnit.UnitId INNER JOIN
                      dbo.TkPost ON dbo.TkUnit.PostId = dbo.TkPost.PostId LEFT OUTER JOIN
                      dbo.LtHistory ON dbo.LtValue.LastHistoryId = dbo.LtHistory.HistoryId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [RtGainView]
AS
SELECT     dbo.RtRecipeFamily.RecipeFamilyId, dbo.RtRecipeFamily.RecipeFamilyName, dbo.RtGain.ParameterId, dbo.RtGain.Parameter, dbo.RtGainGrade.Grade, dbo.RtGainGrade.Gain
FROM         dbo.RtRecipeFamily INNER JOIN
                      dbo.RtGain ON dbo.RtRecipeFamily.RecipeFamilyId = dbo.RtGain.RecipeFamilyId INNER JOIN
                      dbo.RtGainGrade ON dbo.RtGain.ParameterId = dbo.RtGainGrade.ParameterId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtValueView]
AS
SELECT     dbo.TkUnit.UnitName, dbo.LtValue.ValueName, dbo.LtValue.ValueId, dbo.LtHistory.SampleTime, dbo.LtHistory.RawValue, dbo.LtHistory.ReportTime, dbo.LtHistory.Grade
FROM         dbo.TkUnit INNER JOIN
                      dbo.LtValue ON dbo.TkUnit.UnitId = dbo.LtValue.UnitId LEFT OUTER JOIN
                      dbo.LtHistory ON dbo.LtValue.ValueId = dbo.LtHistory.ValueId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtLimitView]
AS
SELECT     dbo.LtLimit.LimitId, dbo.LtValue.ValueId, dbo.LtValue.ValueName, dbo.LtValue.Description, dbo.TkUnit.UnitName, dbo.TkPost.Post, dbo.LtLimit.UpperValidityLimit, dbo.LtLimit.LowerValidityLimit, 
                      dbo.LtLimit.UpperSQCLimit, dbo.LtLimit.LowerSQCLimit, dbo.LtLimit.UpperReleaseLimit, dbo.LtLimit.LowerReleaseLimit, dbo.LtLimit.Target, dbo.LtLimit.StandardDeviation, 
                      dbo.LtValue.ValidationProcedure, dbo.LtLimit.RecipeParameterName, dbo.LtLimit.OPCUpperItemId, dbo.LtLimit.OPCLowerItemId, dbo.Lookup.LookupName AS LimitType, 
                      Lookup_1.LookupName AS LimitSource, dbo.LtOPCInterface.InterfaceName
FROM         dbo.LtValue INNER JOIN
                      dbo.TkUnit ON dbo.LtValue.UnitId = dbo.TkUnit.UnitId INNER JOIN
                      dbo.TkPost ON dbo.TkUnit.PostId = dbo.TkPost.PostId INNER JOIN
                      dbo.LtLimit ON dbo.LtValue.ValueId = dbo.LtLimit.ValueId INNER JOIN
                      dbo.Lookup ON dbo.LtLimit.LimitTypeId = dbo.Lookup.LookupId INNER JOIN
                      dbo.Lookup AS Lookup_1 ON dbo.LtLimit.LimitSourceId = Lookup_1.LookupId LEFT OUTER JOIN
                      dbo.LtOPCInterface ON dbo.LtLimit.OPCInterfaceId = dbo.LtOPCInterface.InterfaceId
WHERE     (dbo.Lookup.LookupTypeCode = 'RtLimitType') AND (Lookup_1.LookupTypeCode = 'RtLimitSource')
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtLocalValueView]
AS
SELECT     dbo.LtValue.ValueId, dbo.LtValue.ValueName, dbo.TkUnit.UnitName, dbo.LtValue.DisplayDecimals, dbo.LtValue.Description, dbo.LtValue.ValidationProcedure, dbo.LtHDAInterface.InterfaceName, 
                      dbo.LtLocalValue.ItemId
FROM         dbo.LtValue INNER JOIN
                      dbo.LtLocalValue ON dbo.LtValue.ValueId = dbo.LtLocalValue.ValueId INNER JOIN
                      dbo.LtHDAInterface ON dbo.LtLocalValue.InterfaceId = dbo.LtHDAInterface.InterfaceId INNER JOIN
                      dbo.TkUnit ON dbo.LtValue.UnitId = dbo.TkUnit.UnitId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtSelectorView]
AS
SELECT     dbo.LtValue.ValueId, dbo.LtValue.ValueName, dbo.LtValue.Description, dbo.LtHistory.SampleTime, LtValue_1.ValueId AS SourceValueId, LtValue_1.ValueName AS SourceValueName, 
                      dbo.LtValue.LastHistoryId
FROM         dbo.LtValue INNER JOIN
                      dbo.LtSelector ON dbo.LtValue.ValueId = dbo.LtSelector.ValueId LEFT OUTER JOIN
                      dbo.LtValue AS LtValue_1 ON dbo.LtSelector.sourceValueId = LtValue_1.ValueId LEFT OUTER JOIN
                      dbo.LtHistory ON dbo.LtValue.LastHistoryId = dbo.LtHistory.HistoryId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtLastValueView]
AS
SELECT     dbo.LtValue.ValueId, dbo.LtHistory.HistoryId, dbo.LtValue.ValueName, dbo.LtHistory.RawValue, dbo.LtHistory.SampleTime, dbo.LtHistory.ReportTime
FROM         dbo.LtHistory RIGHT OUTER JOIN
                      dbo.LtValue ON dbo.LtHistory.HistoryId = dbo.LtValue.LastHistoryId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtDerivedValueView]
AS
SELECT     dbo.LtValue.ValueName, dbo.LtValue.ValueId, dbo.LtDerivedValue.DerivedValueId, dbo.TkUnit.UnitName, LtValue_1.ValueName AS TriggerValueName, LtValue_1.ValueId AS TriggerValueId, 
                      TkUnit_1.UnitName AS TriggerUnitName, dbo.LtDerivedValue.Callback, dbo.LtDerivedValue.SampleTimeTolerance, dbo.LtDerivedValue.NewSampleWaitTime, 
                      dbo.LtHistory.RawValue AS TriggerRawValue, dbo.LtHistory.SampleTime AS TriggerSampleTime, dbo.LtHistory.ReportTime AS TriggerReportTime, dbo.LtHDAInterface.InterfaceName, 
                      dbo.LtDerivedValue.ResultItemId
FROM         dbo.LtValue INNER JOIN
                      dbo.LtDerivedValue ON dbo.LtValue.ValueId = dbo.LtDerivedValue.ValueId INNER JOIN
                      dbo.LtValue AS LtValue_1 ON dbo.LtDerivedValue.TriggerValueId = LtValue_1.ValueId INNER JOIN
                      dbo.TkUnit ON dbo.LtValue.UnitId = dbo.TkUnit.UnitId INNER JOIN
                      dbo.TkUnit AS TkUnit_1 ON LtValue_1.UnitId = TkUnit_1.UnitId INNER JOIN
                      dbo.LtHDAInterface ON dbo.LtDerivedValue.ResultInterfaceId = dbo.LtHDAInterface.InterfaceId LEFT OUTER JOIN
                      dbo.LtHistory ON LtValue_1.LastHistoryId = dbo.LtHistory.HistoryId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtFinalDiagnosis](
	[FinalDiagnosisId] [int] IDENTITY(1,1) NOT NULL,
	[FinalDiagnosisName] [varchar](250) NOT NULL,
	[FinalDiagnosisLabel] [varchar](250) NULL,
	[FamilyId] [int] NOT NULL,
	[FinalDiagnosisPriority] [float] NOT NULL,
	[CalculationMethod] [varchar](1000) NULL,
	[Constant] [bit] NULL,
	[PostTextRecommendation] [bit] NOT NULL,
	[PostProcessingCallback] [varchar](1000) NULL,
	[RefreshRate] [int] NOT NULL,
	[Comment] [varchar](1000) NULL,
	[TextRecommendation] [varchar](1000) NULL,
	[State] [bit] NULL,
	[Active] [bit] NOT NULL,
	[Explanation] [varchar](1000) NULL,
	[TrapInsignificantRecommendations] [bit] NULL,
	[LastRecommendationTime] [datetime] NULL,
	[TimeOfMostRecentRecommendationImplementation] [datetime] NOT NULL,
	[FinalDiagnosisUUID] [varchar](100) NULL,
	[DiagramUUID] [varchar](100) NULL,
	[ManualMoveAllowed] [bit] NULL,
	[ManualMove] [float] NULL,
	[ShowExplanationWithRecommendation] [bit] NOT NULL,
 CONSTRAINT [PK_DtFinalDiagnosis] PRIMARY KEY CLUSTERED 
(
	[FinalDiagnosisId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_DtFinalDiagnosis] ON [DtFinalDiagnosis] 
(
	[FinalDiagnosisName] ASC,
	[FamilyId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtSQCDiagnosis](
	[SQCDiagnosisId] [int] IDENTITY(1,1) NOT NULL,
	[SQCDiagnosisName] [varchar](50) NOT NULL,
	[SQCDiagnosisLabel] [varchar](50) NULL,
	[Status] [varchar](50) NOT NULL,
	[FamilyId] [int] NOT NULL,
	[SQCDiagnosisUUID] [varchar](100) NULL,
	[DiagramUUID] [varchar](100) NULL,
	[LastResetTime] [datetime] NULL,
 CONSTRAINT [PK_DtSQCDiagnosis] PRIMARY KEY CLUSTERED 
(
	[SQCDiagnosisId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
CREATE UNIQUE NONCLUSTERED INDEX [UK_DtSQCDiagnosis] ON [DtSQCDiagnosis] 
(
	[SQCDiagnosisName] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [DtQuantOutputRamp](
	[QuantOutputId] [int] NOT NULL,
	[Ramp] [float] NULL,
	[RampTypeId] [int] NULL,
 CONSTRAINT [PK_DtQuantOutputRamp] PRIMARY KEY CLUSTERED 
(
	[QuantOutputId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [DtQuantOutputDefinitionView]
AS
SELECT     TOP (100) PERCENT dbo.DtApplication.ApplicationName, dbo.DtQuantOutput.QuantOutputName, dbo.DtQuantOutput.QuantOutputId, dbo.DtQuantOutput.TagPath, 
                      dbo.DtQuantOutput.MostNegativeIncrement, dbo.DtQuantOutput.MostPositiveIncrement, dbo.DtQuantOutput.IgnoreMinimumIncrement, dbo.DtQuantOutput.MinimumIncrement, 
                      dbo.DtQuantOutput.SetpointHighLimit, dbo.DtQuantOutput.SetpointLowLimit, dbo.DtQuantOutput.IncrementalOutput
FROM         dbo.DtApplication INNER JOIN
                      dbo.DtQuantOutput ON dbo.DtApplication.ApplicationId = dbo.DtQuantOutput.ApplicationId
ORDER BY dbo.DtApplication.ApplicationName, dbo.DtQuantOutput.QuantOutputName
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [LtDCSValueView]
AS
SELECT     dbo.TkPost.Post, dbo.TkUnit.UnitName, dbo.LtValue.ValueId, dbo.LtValue.ValueName, dbo.LtValue.DisplayDecimals, dbo.LtValue.Description, dbo.LtValue.ValidationProcedure, 
                      dbo.LtOPCInterface.InterfaceName, dbo.LtDCSValue.ItemId, dbo.LtValue.LastHistoryId, dbo.LtHistory.RawValue, dbo.LtHistory.SampleTime, dbo.LtDCSValue.AllowManualEntry
FROM         dbo.TkPost INNER JOIN
                      dbo.TkUnit ON dbo.TkPost.PostId = dbo.TkUnit.PostId INNER JOIN
                      dbo.LtValue ON dbo.TkUnit.UnitId = dbo.LtValue.UnitId INNER JOIN
                      dbo.LtDCSValue ON dbo.LtValue.ValueId = dbo.LtDCSValue.ValueId INNER JOIN
                      dbo.LtOPCInterface ON dbo.LtDCSValue.InterfaceId = dbo.LtOPCInterface.InterfaceId INNER JOIN
                      dbo.LtHistory ON dbo.LtValue.LastHistoryId = dbo.LtHistory.HistoryId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TRIGGER [FinalDiagnosisUpdateTrigger] 
   ON  [DtFinalDiagnosis] 
   FOR UPDATE
AS 
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	IF UPDATE(Active) or UPDATE(State)
	BEGIN
		insert into DtFinalDiagnosisLog (Timestamp, FinalDiagnosisId, State, Active) 
		select getdate(), FinalDiagnosisId, State, Active from inserted
    END

END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [DtFinalDiagnosisView]
AS
SELECT     dbo.DtApplication.ApplicationName, dbo.DtFamily.FamilyId, dbo.DtFamily.FamilyName, dbo.DtFamily.FamilyPriority, dbo.DtFinalDiagnosis.FinalDiagnosisId, 
                      dbo.DtFinalDiagnosis.FinalDiagnosisName, dbo.DtFinalDiagnosis.FinalDiagnosisLabel, dbo.DtFinalDiagnosis.FinalDiagnosisPriority, dbo.DtFinalDiagnosis.CalculationMethod, 
                      dbo.DtFinalDiagnosis.PostProcessingCallback, dbo.DtFinalDiagnosis.PostTextRecommendation, dbo.DtFinalDiagnosis.RefreshRate, dbo.DtFinalDiagnosis.TextRecommendation, 
                      dbo.DtFinalDiagnosis.Active, dbo.DtFinalDiagnosis.Explanation, dbo.DtFinalDiagnosis.TrapInsignificantRecommendations, dbo.DtFinalDiagnosis.LastRecommendationTime, 
                      dbo.DtFinalDiagnosis.TimeOfMostRecentRecommendationImplementation, dbo.DtFinalDiagnosis.Constant, dbo.DtFinalDiagnosis.DiagramUUID, dbo.DtFinalDiagnosis.FinalDiagnosisUUID, 
                      dbo.DtFinalDiagnosis.ManualMove, dbo.DtFinalDiagnosis.ManualMoveAllowed, dbo.DtFinalDiagnosis.Comment
FROM         dbo.DtApplication INNER JOIN
                      dbo.DtFamily ON dbo.DtApplication.ApplicationId = dbo.DtFamily.ApplicationId INNER JOIN
                      dbo.DtFinalDiagnosis ON dbo.DtFamily.FamilyId = dbo.DtFinalDiagnosis.FamilyId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [DtFinalDiagnosisLogView]
AS
SELECT     dbo.DtFinalDiagnosisLog.LogId, dbo.DtFinalDiagnosisLog.Timestamp, dbo.DtApplication.ApplicationName, dbo.DtFamily.FamilyName, dbo.DtFamily.FamilyPriority, 
                      dbo.DtFinalDiagnosis.FinalDiagnosisName, dbo.DtFinalDiagnosisLog.FinalDiagnosisId, dbo.DtFinalDiagnosis.FinalDiagnosisPriority, dbo.DtFinalDiagnosisLog.State, 
                      dbo.DtFinalDiagnosisLog.Active
FROM         dbo.DtFinalDiagnosisLog INNER JOIN
                      dbo.DtFinalDiagnosis ON dbo.DtFinalDiagnosisLog.FinalDiagnosisId = dbo.DtFinalDiagnosis.FinalDiagnosisId INNER JOIN
                      dbo.DtFamily ON dbo.DtFinalDiagnosis.FamilyId = dbo.DtFamily.FamilyId INNER JOIN
                      dbo.DtApplication ON dbo.DtFamily.ApplicationId = dbo.DtApplication.ApplicationId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [DtSQCDiagnosisView]
AS
SELECT     dbo.DtApplication.ApplicationName, dbo.DtFamily.FamilyName, dbo.DtSQCDiagnosis.SQCDiagnosisName, dbo.DtSQCDiagnosis.SQCDiagnosisLabel, dbo.DtSQCDiagnosis.Status, 
                      dbo.DtSQCDiagnosis.SQCDiagnosisUUID, dbo.DtSQCDiagnosis.DiagramUUID, dbo.DtSQCDiagnosis.LastResetTime
FROM         dbo.DtApplication INNER JOIN
                      dbo.DtFamily ON dbo.DtApplication.ApplicationId = dbo.DtFamily.ApplicationId RIGHT OUTER JOIN
                      dbo.DtSQCDiagnosis ON dbo.DtFamily.FamilyId = dbo.DtSQCDiagnosis.FamilyId
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [DtRecommendationDefinition](
	[RecommendationDefinitionId] [int] IDENTITY(1,1) NOT NULL,
	[FinalDiagnosisId] [int] NOT NULL,
	[QuantOutputId] [int] NOT NULL,
 CONSTRAINT [PK_DtRecommendationDefinition] PRIMARY KEY CLUSTERED 
(
	[RecommendationDefinitionId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [DtRecommendation](
	[RecommendationId] [int] IDENTITY(1,1) NOT NULL,
	[RecommendationDefinitionId] [int] NOT NULL,
	[DiagnosisEntryId] [int] NOT NULL,
	[Recommendation] [float] NOT NULL,
	[AutoRecommendation] [float] NOT NULL,
	[ManualRecommendation] [float] NULL,
	[AutoOrManual] [varchar](50) NOT NULL,
	[RampTime] [float] NULL,
 CONSTRAINT [PK_DtRecommendation] PRIMARY KEY CLUSTERED 
(
	[RecommendationId] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
SET ANSI_PADDING OFF
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [DtQuantOutputView]
AS
SELECT     TOP (100) PERCENT dbo.DtFinalDiagnosis.FinalDiagnosisName, dbo.DtFinalDiagnosis.FinalDiagnosisId, dbo.DtQuantOutput.QuantOutputName, dbo.DtQuantOutput.QuantOutputId, 
                      dbo.DtQuantOutput.TagPath
FROM         dbo.DtFinalDiagnosis INNER JOIN
                      dbo.DtRecommendationDefinition ON dbo.DtFinalDiagnosis.FinalDiagnosisId = dbo.DtRecommendationDefinition.FinalDiagnosisId INNER JOIN
                      dbo.DtQuantOutput ON dbo.DtRecommendationDefinition.QuantOutputId = dbo.DtQuantOutput.QuantOutputId
ORDER BY dbo.DtFinalDiagnosis.FinalDiagnosisName, dbo.DtQuantOutput.QuantOutputName
GO
ALTER TABLE [DtApplication] ADD  CONSTRAINT [DF_DtApplication_NotificationStrategy]  DEFAULT ('ocAlert') FOR [NotificationStrategy]
GO
ALTER TABLE [DtApplication] ADD  CONSTRAINT [DF_DtApplication_Managed]  DEFAULT ((1)) FOR [Managed]
GO
ALTER TABLE [DtFinalDiagnosis] ADD  CONSTRAINT [DF_DtFinalDiagnosis_Active]  DEFAULT ((0)) FOR [Active]
GO
ALTER TABLE [DtFinalDiagnosis] ADD  CONSTRAINT [DF_DtFinalDiagnosis_TimeOfMostRecentRecommendationImplementation]  DEFAULT (getdate()) FOR [TimeOfMostRecentRecommendationImplementation]
GO
ALTER TABLE [DtFinalDiagnosis] ADD  CONSTRAINT [DF_DtFinalDiagnosis_ShowExplanationWithRecommendation]  DEFAULT ((0)) FOR [ShowExplanationWithRecommendation]
GO
ALTER TABLE [DtQuantOutput] ADD  CONSTRAINT [DF_DtQuantOutput_IgnoreMinimumIncrement]  DEFAULT ((0)) FOR [IgnoreMinimumIncrement]
GO
ALTER TABLE [LtDCSValue] ADD  CONSTRAINT [DF_LtDCSValue_MinimumSampleIntervalSeconds]  DEFAULT ((300)) FOR [MinimumSampleIntervalSeconds]
GO
ALTER TABLE [LtDCSValue] ADD  CONSTRAINT [DF_LtDCSValue_AllowManualEntry]  DEFAULT ((1)) FOR [AllowManualEntry]
GO
ALTER TABLE [LtDisplayTableDetails] ADD  CONSTRAINT [DF_LtDisplayTableDetails_DisplayOrder]  DEFAULT ((0)) FOR [DisplayOrder]
GO
ALTER TABLE [LtPHDValue] ADD  CONSTRAINT [DF_LtPHDValue_AllowManualEntry]  DEFAULT ((1)) FOR [AllowManualEntry]
GO
ALTER TABLE [QueueMaster] ADD  CONSTRAINT [DF_QueueMaster_AutoViewSeverityThreshold]  DEFAULT ((10.0)) FOR [AutoViewSeverityThreshold]
GO
ALTER TABLE [QueueMaster] ADD  CONSTRAINT [DF_QueueMaster_Position]  DEFAULT ('center') FOR [Position]
GO
ALTER TABLE [QueueMaster] ADD  CONSTRAINT [DF_QueueMaster_AutoViewAdmin]  DEFAULT ((0)) FOR [AutoViewAdmin]
GO
ALTER TABLE [QueueMaster] ADD  CONSTRAINT [DF_QueueMaster_AutoViewAE]  DEFAULT ((0)) FOR [AutoViewAE]
GO
ALTER TABLE [QueueMaster] ADD  CONSTRAINT [DF_QueueMaster_AutoViewOperator]  DEFAULT ((0)) FOR [AutoViewOperator]
GO
ALTER TABLE [QueueMessageStatus] ADD  CONSTRAINT [DF_QueueMessageStatus_Severity]  DEFAULT ((0.0)) FOR [Severity]
GO
ALTER TABLE [RtGradeMaster] ADD  CONSTRAINT [DF__RtGradeMa__Times__7A672E12]  DEFAULT (getdate()) FOR [Timestamp]
GO
ALTER TABLE [RtGradeMaster] ADD  CONSTRAINT [DF__RtGradeMa__Activ__7B5B524B]  DEFAULT ((0)) FOR [Active]
GO
ALTER TABLE [RtRecipeFamily] ADD  CONSTRAINT [DF_RtRecipeFamily_HasGains]  DEFAULT ((1)) FOR [HasGains]
GO
ALTER TABLE [RtRecipeFamily] ADD  CONSTRAINT [DF_RtRecipeFamily_HasSQC]  DEFAULT ((1)) FOR [HasSQC]
GO
ALTER TABLE [SfcChart] ADD  CONSTRAINT [DF_SfcChart_IsProduction]  DEFAULT ((1)) FOR [IsProduction]
GO
ALTER TABLE [SfcRecipeDataFolder] ADD  CONSTRAINT [DF_SfcRecipeDataFolder_RecipeDataType]  DEFAULT ('Folder') FOR [RecipeDataType]
GO
ALTER TABLE [TkConsole] ADD  CONSTRAINT [DF_TkConsole_Priority]  DEFAULT ((1)) FOR [Priority]
GO
ALTER TABLE [Units] ADD  CONSTRAINT [DF__Units__m__178D7CA5]  DEFAULT ((0)) FOR [m]
GO
ALTER TABLE [Units] ADD  CONSTRAINT [DF__Units__b__1881A0DE]  DEFAULT ((0)) FOR [b]
GO
ALTER TABLE [BtBatchLog]  WITH CHECK ADD  CONSTRAINT [FK_BtBatchLog_BtBatchRun] FOREIGN KEY([BatchRunId])
REFERENCES [BtBatchRun] ([BatchRunId])
ON DELETE CASCADE
GO
ALTER TABLE [BtBatchLog] CHECK CONSTRAINT [FK_BtBatchLog_BtBatchRun]
GO
ALTER TABLE [BtBatchRun]  WITH CHECK ADD  CONSTRAINT [FK_BtBatchRun_BtReactor] FOREIGN KEY([ReactorId])
REFERENCES [BtReactor] ([ReactorId])
GO
ALTER TABLE [BtBatchRun] CHECK CONSTRAINT [FK_BtBatchRun_BtReactor]
GO
ALTER TABLE [BtStripperBatchLog]  WITH CHECK ADD  CONSTRAINT [FK_BtStripperBatchLog_BtBatchRun] FOREIGN KEY([BatchRunId])
REFERENCES [BtBatchRun] ([BatchRunId])
ON DELETE CASCADE
GO
ALTER TABLE [BtStripperBatchLog] CHECK CONSTRAINT [FK_BtStripperBatchLog_BtBatchRun]
GO
ALTER TABLE [DtApplication]  WITH CHECK ADD  CONSTRAINT [FK_DtApplication_Lookup] FOREIGN KEY([GroupRampMethodId])
REFERENCES [Lookup] ([LookupId])
GO
ALTER TABLE [DtApplication] CHECK CONSTRAINT [FK_DtApplication_Lookup]
GO
ALTER TABLE [DtApplication]  WITH CHECK ADD  CONSTRAINT [FK_DtApplication_QueueMaster] FOREIGN KEY([MessageQueueId])
REFERENCES [QueueMaster] ([QueueId])
GO
ALTER TABLE [DtApplication] CHECK CONSTRAINT [FK_DtApplication_QueueMaster]
GO
ALTER TABLE [DtApplication]  WITH CHECK ADD  CONSTRAINT [FK_DtApplication_TkUnit] FOREIGN KEY([UnitId])
REFERENCES [TkUnit] ([UnitId])
GO
ALTER TABLE [DtApplication] CHECK CONSTRAINT [FK_DtApplication_TkUnit]
GO
ALTER TABLE [DtFamily]  WITH CHECK ADD  CONSTRAINT [FK_DtFamily_DtApplication] FOREIGN KEY([ApplicationId])
REFERENCES [DtApplication] ([ApplicationId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [DtFamily] CHECK CONSTRAINT [FK_DtFamily_DtApplication]
GO
ALTER TABLE [DtFinalDiagnosis]  WITH CHECK ADD  CONSTRAINT [FK_DtFinalDiagnosis_DtFamily] FOREIGN KEY([FamilyId])
REFERENCES [DtFamily] ([FamilyId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [DtFinalDiagnosis] CHECK CONSTRAINT [FK_DtFinalDiagnosis_DtFamily]
GO
ALTER TABLE [DtQuantOutput]  WITH CHECK ADD  CONSTRAINT [FK_DtQuantOutput_DtApplication] FOREIGN KEY([ApplicationId])
REFERENCES [DtApplication] ([ApplicationId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [DtQuantOutput] CHECK CONSTRAINT [FK_DtQuantOutput_DtApplication]
GO
ALTER TABLE [DtQuantOutput]  WITH CHECK ADD  CONSTRAINT [FK_DtQuantOutput_Lookup] FOREIGN KEY([FeedbackMethodId])
REFERENCES [Lookup] ([LookupId])
GO
ALTER TABLE [DtQuantOutput] CHECK CONSTRAINT [FK_DtQuantOutput_Lookup]
GO
ALTER TABLE [DtQuantOutputRamp]  WITH CHECK ADD  CONSTRAINT [FK_DtQuantOutputRamp_DtQuantOutput] FOREIGN KEY([QuantOutputId])
REFERENCES [DtQuantOutput] ([QuantOutputId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [DtQuantOutputRamp] CHECK CONSTRAINT [FK_DtQuantOutputRamp_DtQuantOutput]
GO
ALTER TABLE [DtRecommendation]  WITH CHECK ADD  CONSTRAINT [FK_DtRecommendation_DtDiagnosisEntry] FOREIGN KEY([DiagnosisEntryId])
REFERENCES [DtDiagnosisEntry] ([DiagnosisEntryId])
GO
ALTER TABLE [DtRecommendation] CHECK CONSTRAINT [FK_DtRecommendation_DtDiagnosisEntry]
GO
ALTER TABLE [DtRecommendation]  WITH CHECK ADD  CONSTRAINT [FK_DtRecommendation_DtRecommendationDefinition] FOREIGN KEY([RecommendationDefinitionId])
REFERENCES [DtRecommendationDefinition] ([RecommendationDefinitionId])
GO
ALTER TABLE [DtRecommendation] CHECK CONSTRAINT [FK_DtRecommendation_DtRecommendationDefinition]
GO
ALTER TABLE [DtRecommendationDefinition]  WITH CHECK ADD  CONSTRAINT [FK_DtRecommendationDefinition_DtFinalDiagnosis] FOREIGN KEY([FinalDiagnosisId])
REFERENCES [DtFinalDiagnosis] ([FinalDiagnosisId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [DtRecommendationDefinition] CHECK CONSTRAINT [FK_DtRecommendationDefinition_DtFinalDiagnosis]
GO
ALTER TABLE [DtRecommendationDefinition]  WITH CHECK ADD  CONSTRAINT [FK_DtRecommendationDefinition_DtQuantOutput] FOREIGN KEY([QuantOutputId])
REFERENCES [DtQuantOutput] ([QuantOutputId])
GO
ALTER TABLE [DtRecommendationDefinition] CHECK CONSTRAINT [FK_DtRecommendationDefinition_DtQuantOutput]
GO
ALTER TABLE [DtSQCDiagnosis]  WITH CHECK ADD  CONSTRAINT [FK_DtSQCDiagnosis_DtFamily] FOREIGN KEY([FamilyId])
REFERENCES [DtFamily] ([FamilyId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [DtSQCDiagnosis] CHECK CONSTRAINT [FK_DtSQCDiagnosis_DtFamily]
GO
ALTER TABLE [DtTextRecommendation]  WITH CHECK ADD  CONSTRAINT [FK_DtTextRecommendation_DtDiagnosisEntry] FOREIGN KEY([DiagnosisEntryId])
REFERENCES [DtDiagnosisEntry] ([DiagnosisEntryId])
ON DELETE CASCADE
GO
ALTER TABLE [DtTextRecommendation] CHECK CONSTRAINT [FK_DtTextRecommendation_DtDiagnosisEntry]
GO
ALTER TABLE [Lookup]  WITH CHECK ADD  CONSTRAINT [FK_Lookup_LookupType] FOREIGN KEY([LookupTypeCode])
REFERENCES [LookupType] ([LookupTypeCode])
ON DELETE CASCADE
GO
ALTER TABLE [Lookup] CHECK CONSTRAINT [FK_Lookup_LookupType]
GO
ALTER TABLE [LtDCSValue]  WITH CHECK ADD  CONSTRAINT [FK_LtDCSValue_LtOPCInterface] FOREIGN KEY([InterfaceId])
REFERENCES [LtOPCInterface] ([InterfaceId])
GO
ALTER TABLE [LtDCSValue] CHECK CONSTRAINT [FK_LtDCSValue_LtOPCInterface]
GO
ALTER TABLE [LtDCSValue]  WITH CHECK ADD  CONSTRAINT [FK_LtDCSValue_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtDCSValue] CHECK CONSTRAINT [FK_LtDCSValue_LtValue]
GO
ALTER TABLE [LtDerivedValue]  WITH CHECK ADD  CONSTRAINT [FK_LtDerivedValue_LtHDAInterface] FOREIGN KEY([ResultInterfaceId])
REFERENCES [LtHDAInterface] ([InterfaceId])
GO
ALTER TABLE [LtDerivedValue] CHECK CONSTRAINT [FK_LtDerivedValue_LtHDAInterface]
GO
ALTER TABLE [LtDerivedValue]  WITH CHECK ADD  CONSTRAINT [FK_LtDerivedValue_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtDerivedValue] CHECK CONSTRAINT [FK_LtDerivedValue_LtValue]
GO
ALTER TABLE [LtDerivedValue]  WITH CHECK ADD  CONSTRAINT [FK_LtDerivedValue_LtValue1] FOREIGN KEY([TriggerValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtDerivedValue] CHECK CONSTRAINT [FK_LtDerivedValue_LtValue1]
GO
ALTER TABLE [LtDisplayTable]  WITH CHECK ADD  CONSTRAINT [FK_LtDisplayTable_TkPost] FOREIGN KEY([PostId])
REFERENCES [TkPost] ([PostId])
GO
ALTER TABLE [LtDisplayTable] CHECK CONSTRAINT [FK_LtDisplayTable_TkPost]
GO
ALTER TABLE [LtDisplayTableDetails]  WITH CHECK ADD  CONSTRAINT [FK_LtDisplayTableDetails_LtDisplayTable] FOREIGN KEY([DisplayTableId])
REFERENCES [LtDisplayTable] ([DisplayTableId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [LtDisplayTableDetails] CHECK CONSTRAINT [FK_LtDisplayTableDetails_LtDisplayTable]
GO
ALTER TABLE [LtDisplayTableDetails]  WITH CHECK ADD  CONSTRAINT [FK_LtDisplayTableDetails_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [LtDisplayTableDetails] CHECK CONSTRAINT [FK_LtDisplayTableDetails_LtValue]
GO
ALTER TABLE [LtHistory]  WITH CHECK ADD  CONSTRAINT [FK_LtHistory_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtHistory] CHECK CONSTRAINT [FK_LtHistory_LtValue]
GO
ALTER TABLE [LtLimit]  WITH CHECK ADD  CONSTRAINT [FK_LtLimit_Lookup] FOREIGN KEY([LimitTypeId])
REFERENCES [Lookup] ([LookupId])
GO
ALTER TABLE [LtLimit] CHECK CONSTRAINT [FK_LtLimit_Lookup]
GO
ALTER TABLE [LtLimit]  WITH CHECK ADD  CONSTRAINT [FK_LtLimit_Lookup1] FOREIGN KEY([LimitSourceId])
REFERENCES [Lookup] ([LookupId])
GO
ALTER TABLE [LtLimit] CHECK CONSTRAINT [FK_LtLimit_Lookup1]
GO
ALTER TABLE [LtLimit]  WITH CHECK ADD  CONSTRAINT [FK_LtLimit_LtOPCInterface] FOREIGN KEY([OPCInterfaceId])
REFERENCES [LtOPCInterface] ([InterfaceId])
GO
ALTER TABLE [LtLimit] CHECK CONSTRAINT [FK_LtLimit_LtOPCInterface]
GO
ALTER TABLE [LtLimit]  WITH CHECK ADD  CONSTRAINT [FK_LtLimit_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtLimit] CHECK CONSTRAINT [FK_LtLimit_LtValue]
GO
ALTER TABLE [LtLocalValue]  WITH CHECK ADD  CONSTRAINT [FK_LtLocalValue_LtHDAInterface] FOREIGN KEY([InterfaceId])
REFERENCES [LtHDAInterface] ([InterfaceId])
GO
ALTER TABLE [LtLocalValue] CHECK CONSTRAINT [FK_LtLocalValue_LtHDAInterface]
GO
ALTER TABLE [LtLocalValue]  WITH CHECK ADD  CONSTRAINT [FK_LtLocalValue_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtLocalValue] CHECK CONSTRAINT [FK_LtLocalValue_LtValue]
GO
ALTER TABLE [LtPHDValue]  WITH CHECK ADD  CONSTRAINT [FK_LtPHDLabValue_LtLabValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtPHDValue] CHECK CONSTRAINT [FK_LtPHDLabValue_LtLabValue]
GO
ALTER TABLE [LtPHDValue]  WITH CHECK ADD  CONSTRAINT [FK_LtPHDValue_LtHDAInterface] FOREIGN KEY([InterfaceId])
REFERENCES [LtHDAInterface] ([InterfaceId])
GO
ALTER TABLE [LtPHDValue] CHECK CONSTRAINT [FK_LtPHDValue_LtHDAInterface]
GO
ALTER TABLE [LtRelatedData]  WITH CHECK ADD  CONSTRAINT [FK_LtRelatedData_LtDerivedValue] FOREIGN KEY([DerivedValueId])
REFERENCES [LtDerivedValue] ([DerivedValueId])
GO
ALTER TABLE [LtRelatedData] CHECK CONSTRAINT [FK_LtRelatedData_LtDerivedValue]
GO
ALTER TABLE [LtRelatedData]  WITH CHECK ADD  CONSTRAINT [FK_LtRelatedData_LtValue] FOREIGN KEY([RelatedValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtRelatedData] CHECK CONSTRAINT [FK_LtRelatedData_LtValue]
GO
ALTER TABLE [LtSelector]  WITH CHECK ADD  CONSTRAINT [FK_LtSelector_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtSelector] CHECK CONSTRAINT [FK_LtSelector_LtValue]
GO
ALTER TABLE [LtValue]  WITH CHECK ADD  CONSTRAINT [FK_LtValue_TkUnit] FOREIGN KEY([UnitId])
REFERENCES [TkUnit] ([UnitId])
GO
ALTER TABLE [LtValue] CHECK CONSTRAINT [FK_LtValue_TkUnit]
GO
ALTER TABLE [LtValueViewed]  WITH CHECK ADD  CONSTRAINT [FK_LtValueViewed_LtValue] FOREIGN KEY([ValueId])
REFERENCES [LtValue] ([ValueId])
GO
ALTER TABLE [LtValueViewed] CHECK CONSTRAINT [FK_LtValueViewed_LtValue]
GO
ALTER TABLE [QueueDetail]  WITH CHECK ADD  CONSTRAINT [FK_QueueDetail_QueueMaster] FOREIGN KEY([QueueId])
REFERENCES [QueueMaster] ([QueueId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [QueueDetail] CHECK CONSTRAINT [FK_QueueDetail_QueueMaster]
GO
ALTER TABLE [QueueDetail]  WITH CHECK ADD  CONSTRAINT [FK_QueueDetail_QueueMessageStatus] FOREIGN KEY([StatusId])
REFERENCES [QueueMessageStatus] ([StatusId])
GO
ALTER TABLE [QueueDetail] CHECK CONSTRAINT [FK_QueueDetail_QueueMessageStatus]
GO
ALTER TABLE [QueueMaster]  WITH CHECK ADD  CONSTRAINT [UK_QueueMaster_QueueMaster] FOREIGN KEY([QueueId])
REFERENCES [QueueMaster] ([QueueId])
GO
ALTER TABLE [QueueMaster] CHECK CONSTRAINT [UK_QueueMaster_QueueMaster]
GO
ALTER TABLE [RtDownloadMaster]  WITH CHECK ADD  CONSTRAINT [FK_RtDownloadMaster_RtRecipeFamily] FOREIGN KEY([RecipeFamilyId])
REFERENCES [RtRecipeFamily] ([RecipeFamilyId])
ON DELETE CASCADE
GO
ALTER TABLE [RtDownloadMaster] CHECK CONSTRAINT [FK_RtDownloadMaster_RtRecipeFamily]
GO
ALTER TABLE [RtEvent]  WITH CHECK ADD  CONSTRAINT [FK_RtEvent_RtEventParameter] FOREIGN KEY([ParameterId])
REFERENCES [RtEventParameter] ([ParameterId])
ON DELETE CASCADE
GO
ALTER TABLE [RtEvent] CHECK CONSTRAINT [FK_RtEvent_RtEventParameter]
GO
ALTER TABLE [RtEventParameter]  WITH CHECK ADD  CONSTRAINT [FK_RtEventParameter_RtRecipeFamily] FOREIGN KEY([RecipeFamilyId])
REFERENCES [RtRecipeFamily] ([RecipeFamilyId])
ON DELETE CASCADE
GO
ALTER TABLE [RtEventParameter] CHECK CONSTRAINT [FK_RtEventParameter_RtRecipeFamily]
GO
ALTER TABLE [RtGain]  WITH CHECK ADD  CONSTRAINT [FK_RtGain_RtRecipeFamily] FOREIGN KEY([RecipeFamilyId])
REFERENCES [RtRecipeFamily] ([RecipeFamilyId])
ON DELETE CASCADE
GO
ALTER TABLE [RtGain] CHECK CONSTRAINT [FK_RtGain_RtRecipeFamily]
GO
ALTER TABLE [RtGainGrade]  WITH CHECK ADD  CONSTRAINT [FK_RtGainGrade_RtGain] FOREIGN KEY([ParameterId])
REFERENCES [RtGain] ([ParameterId])
GO
ALTER TABLE [RtGainGrade] CHECK CONSTRAINT [FK_RtGainGrade_RtGain]
GO
ALTER TABLE [RtGradeDetail]  WITH CHECK ADD  CONSTRAINT [FK_RtGradeDetail_RtRecipeFamily] FOREIGN KEY([RecipeFamilyId])
REFERENCES [RtRecipeFamily] ([RecipeFamilyId])
ON DELETE CASCADE
GO
ALTER TABLE [RtGradeDetail] CHECK CONSTRAINT [FK_RtGradeDetail_RtRecipeFamily]
GO
ALTER TABLE [RtGradeMaster]  WITH CHECK ADD  CONSTRAINT [FK_RtGradeMaster_RtRecipeFamily] FOREIGN KEY([RecipeFamilyId])
REFERENCES [RtRecipeFamily] ([RecipeFamilyId])
ON DELETE CASCADE
GO
ALTER TABLE [RtGradeMaster] CHECK CONSTRAINT [FK_RtGradeMaster_RtRecipeFamily]
GO
ALTER TABLE [RtRecipeFamily]  WITH CHECK ADD  CONSTRAINT [FK_RtRecipeFamily_TkPost] FOREIGN KEY([PostId])
REFERENCES [TkPost] ([PostId])
GO
ALTER TABLE [RtRecipeFamily] CHECK CONSTRAINT [FK_RtRecipeFamily_TkPost]
GO
ALTER TABLE [RtSQCLimit]  WITH CHECK ADD  CONSTRAINT [FK_RtSQCLimit_RtSQCParameter] FOREIGN KEY([ParameterId])
REFERENCES [RtSQCParameter] ([ParameterId])
ON DELETE CASCADE
GO
ALTER TABLE [RtSQCLimit] CHECK CONSTRAINT [FK_RtSQCLimit_RtSQCParameter]
GO
ALTER TABLE [RtSQCParameter]  WITH CHECK ADD  CONSTRAINT [FK_RtSQCParameter_RtRecipeFamily] FOREIGN KEY([RecipeFamilyId])
REFERENCES [RtRecipeFamily] ([RecipeFamilyId])
ON DELETE CASCADE
GO
ALTER TABLE [RtSQCParameter] CHECK CONSTRAINT [FK_RtSQCParameter_RtRecipeFamily]
GO
ALTER TABLE [RtValueDefinition]  WITH CHECK ADD  CONSTRAINT [FK_RtValueDefinition_RtValueDefinition] FOREIGN KEY([RecipeFamilyId])
REFERENCES [RtRecipeFamily] ([RecipeFamilyId])
ON DELETE CASCADE
GO
ALTER TABLE [RtValueDefinition] CHECK CONSTRAINT [FK_RtValueDefinition_RtValueDefinition]
GO
ALTER TABLE [RtValueDefinition]  WITH CHECK ADD  CONSTRAINT [FK_RtValueDefinition_RtValueType] FOREIGN KEY([ValueTypeId])
REFERENCES [RtValueType] ([ValueTypeId])
GO
ALTER TABLE [RtValueDefinition] CHECK CONSTRAINT [FK_RtValueDefinition_RtValueType]
GO
ALTER TABLE [RtValueDefinition]  WITH CHECK ADD  CONSTRAINT [FK_RtValueDefinition_RtWriteLocation] FOREIGN KEY([WriteLocationId])
REFERENCES [TkWriteLocation] ([WriteLocationId])
GO
ALTER TABLE [RtValueDefinition] CHECK CONSTRAINT [FK_RtValueDefinition_RtWriteLocation]
GO
ALTER TABLE [SfcBusyNotification]  WITH CHECK ADD  CONSTRAINT [FK_SfcBusyNotification_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcBusyNotification] CHECK CONSTRAINT [FK_SfcBusyNotification_SfcWindow]
GO
ALTER TABLE [SfcControlPanel]  WITH CHECK ADD  CONSTRAINT [FK_SfcControlPanel_TkPost] FOREIGN KEY([PostId])
REFERENCES [TkPost] ([PostId])
GO
ALTER TABLE [SfcControlPanel] CHECK CONSTRAINT [FK_SfcControlPanel_TkPost]
GO
ALTER TABLE [SfcControlPanelMessage]  WITH CHECK ADD  CONSTRAINT [FK_SfcControlPanelMessage_SfcControlPanel] FOREIGN KEY([controlPanelId])
REFERENCES [SfcControlPanel] ([ControlPanelId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcControlPanelMessage] CHECK CONSTRAINT [FK_SfcControlPanelMessage_SfcControlPanel]
GO
ALTER TABLE [SfcDialogMessage]  WITH CHECK ADD  CONSTRAINT [FK_SfcDialogMessage_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcDialogMessage] CHECK CONSTRAINT [FK_SfcDialogMessage_SfcWindow]
GO
ALTER TABLE [SfcDownloadGUI]  WITH CHECK ADD  CONSTRAINT [FK_SfcDownloadGUI_SfcWindow] FOREIGN KEY([WindowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcDownloadGUI] CHECK CONSTRAINT [FK_SfcDownloadGUI_SfcWindow]
GO
ALTER TABLE [SfcDownloadGUITable]  WITH CHECK ADD  CONSTRAINT [FK_SfcDownloadGUITable_SfcDownloadGUI] FOREIGN KEY([windowId])
REFERENCES [SfcDownloadGUI] ([WindowId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [SfcDownloadGUITable] CHECK CONSTRAINT [FK_SfcDownloadGUITable_SfcDownloadGUI]
GO
ALTER TABLE [SfcHierarchy]  WITH CHECK ADD  CONSTRAINT [FK_SfcHierarchy_SfcChart] FOREIGN KEY([ChartId])
REFERENCES [SfcChart] ([ChartId])
GO
ALTER TABLE [SfcHierarchy] CHECK CONSTRAINT [FK_SfcHierarchy_SfcChart]
GO
ALTER TABLE [SfcHierarchy]  WITH CHECK ADD  CONSTRAINT [FK_SfcHierarchy_SfcChart1] FOREIGN KEY([ChildChartId])
REFERENCES [SfcChart] ([ChartId])
GO
ALTER TABLE [SfcHierarchy] CHECK CONSTRAINT [FK_SfcHierarchy_SfcChart1]
GO
ALTER TABLE [SfcHierarchy]  WITH CHECK ADD  CONSTRAINT [FK_SfcHierarchy_SfcStep] FOREIGN KEY([StepId])
REFERENCES [SfcStep] ([StepId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcHierarchy] CHECK CONSTRAINT [FK_SfcHierarchy_SfcStep]
GO
ALTER TABLE [SfcHierarchyHandler]  WITH CHECK ADD  CONSTRAINT [FK_SfcChartHandler_SfcChart] FOREIGN KEY([ChartId])
REFERENCES [SfcChart] ([ChartId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [SfcHierarchyHandler] CHECK CONSTRAINT [FK_SfcChartHandler_SfcChart]
GO
ALTER TABLE [SfcInput]  WITH CHECK ADD  CONSTRAINT [FK_SfcInput_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcInput] CHECK CONSTRAINT [FK_SfcInput_SfcWindow]
GO
ALTER TABLE [SfcManualDataEntry]  WITH CHECK ADD  CONSTRAINT [FK_SfcManualDataEntry_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
GO
ALTER TABLE [SfcManualDataEntry] CHECK CONSTRAINT [FK_SfcManualDataEntry_SfcWindow]
GO
ALTER TABLE [SfcManualDataEntryTable]  WITH CHECK ADD  CONSTRAINT [FK__SfcManual__windo__5AB9788F] FOREIGN KEY([windowId])
REFERENCES [SfcManualDataEntry] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcManualDataEntryTable] CHECK CONSTRAINT [FK__SfcManual__windo__5AB9788F]
GO
ALTER TABLE [SfcRecipeData]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeData_SfcRecipeDataType] FOREIGN KEY([RecipeDataTypeId])
REFERENCES [SfcRecipeDataType] ([RecipeDataTypeId])
GO
ALTER TABLE [SfcRecipeData] CHECK CONSTRAINT [FK_SfcRecipeData_SfcRecipeDataType]
GO
ALTER TABLE [SfcRecipeData]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeData_SfcStep] FOREIGN KEY([StepId])
REFERENCES [SfcStep] ([StepId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeData] CHECK CONSTRAINT [FK_SfcRecipeData_SfcStep]
GO
ALTER TABLE [SfcRecipeDataArray]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataArray_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataArray] CHECK CONSTRAINT [FK_SfcRecipeDataArray_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataArray]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataArray_SfcRecipeDataKeyMaster] FOREIGN KEY([IndexKeyId])
REFERENCES [SfcRecipeDataKeyMaster] ([KeyId])
GO
ALTER TABLE [SfcRecipeDataArray] CHECK CONSTRAINT [FK_SfcRecipeDataArray_SfcRecipeDataKeyMaster]
GO
ALTER TABLE [SfcRecipeDataArray]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataArray_SfcValueType] FOREIGN KEY([ValueTypeId])
REFERENCES [SfcValueType] ([ValueTypeId])
GO
ALTER TABLE [SfcRecipeDataArray] CHECK CONSTRAINT [FK_SfcRecipeDataArray_SfcValueType]
GO
ALTER TABLE [SfcRecipeDataArrayElement]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataArrayElement_SfcRecipeDataArray] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeDataArray] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataArrayElement] CHECK CONSTRAINT [FK_SfcRecipeDataArrayElement_SfcRecipeDataArray]
GO
ALTER TABLE [SfcRecipeDataArrayElement]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataArrayElement_SfcRecipeDataValue] FOREIGN KEY([ValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataArrayElement] CHECK CONSTRAINT [FK_SfcRecipeDataArrayElement_SfcRecipeDataValue]
GO
ALTER TABLE [SfcRecipeDataFolder]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataFolder_SfcStep] FOREIGN KEY([StepId])
REFERENCES [SfcStep] ([StepId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataFolder] CHECK CONSTRAINT [FK_SfcRecipeDataFolder_SfcStep]
GO
ALTER TABLE [SfcRecipeDataInput]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataInput_PVValue] FOREIGN KEY([PVValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataInput] CHECK CONSTRAINT [FK_SfcRecipeDataInput_PVValue]
GO
ALTER TABLE [SfcRecipeDataInput]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataInput_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataInput] CHECK CONSTRAINT [FK_SfcRecipeDataInput_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataInput]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataInput_SfcValueType] FOREIGN KEY([ValueTypeId])
REFERENCES [SfcValueType] ([ValueTypeId])
GO
ALTER TABLE [SfcRecipeDataInput] CHECK CONSTRAINT [FK_SfcRecipeDataInput_SfcValueType]
GO
ALTER TABLE [SfcRecipeDataInput]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataInput_TargetValue] FOREIGN KEY([TargetValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataInput] CHECK CONSTRAINT [FK_SfcRecipeDataInput_TargetValue]
GO
ALTER TABLE [SfcRecipeDataKeyDetail]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataKeyDetail_SfcRecipeDataKeyMaster] FOREIGN KEY([KeyId])
REFERENCES [SfcRecipeDataKeyMaster] ([KeyId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataKeyDetail] CHECK CONSTRAINT [FK_SfcRecipeDataKeyDetail_SfcRecipeDataKeyMaster]
GO
ALTER TABLE [SfcRecipeDataMatrix]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataMatrix] CHECK CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataMatrix]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeDataKeyMaster] FOREIGN KEY([RowIndexKeyId])
REFERENCES [SfcRecipeDataKeyMaster] ([KeyId])
GO
ALTER TABLE [SfcRecipeDataMatrix] CHECK CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeDataKeyMaster]
GO
ALTER TABLE [SfcRecipeDataMatrix]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeDataKeyMaster1] FOREIGN KEY([ColumnIndexKeyId])
REFERENCES [SfcRecipeDataKeyMaster] ([KeyId])
GO
ALTER TABLE [SfcRecipeDataMatrix] CHECK CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeDataKeyMaster1]
GO
ALTER TABLE [SfcRecipeDataMatrix]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeDataMatrix] FOREIGN KEY([ValueTypeId])
REFERENCES [SfcValueType] ([ValueTypeId])
GO
ALTER TABLE [SfcRecipeDataMatrix] CHECK CONSTRAINT [FK_SfcRecipeDataMatrix_SfcRecipeDataMatrix]
GO
ALTER TABLE [SfcRecipeDataMatrixElement]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataMatrixElement_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataMatrixElement] CHECK CONSTRAINT [FK_SfcRecipeDataMatrixElement_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataMatrixElement]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataMatrixElement_SfcRecipeDataValue] FOREIGN KEY([ValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataMatrixElement] CHECK CONSTRAINT [FK_SfcRecipeDataMatrixElement_SfcRecipeDataValue]
GO
ALTER TABLE [SfcRecipeDataOutput]  WITH CHECK ADD  CONSTRAINT [FK_Output_Value] FOREIGN KEY([OutputValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataOutput] CHECK CONSTRAINT [FK_Output_Value]
GO
ALTER TABLE [SfcRecipeDataOutput]  WITH CHECK ADD  CONSTRAINT [FK_PV_Value] FOREIGN KEY([PVValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataOutput] CHECK CONSTRAINT [FK_PV_Value]
GO
ALTER TABLE [SfcRecipeDataOutput]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataOutput_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataOutput] CHECK CONSTRAINT [FK_SfcRecipeDataOutput_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataOutput]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataOutput_SfcValueType] FOREIGN KEY([ValueTypeId])
REFERENCES [SfcValueType] ([ValueTypeId])
GO
ALTER TABLE [SfcRecipeDataOutput] CHECK CONSTRAINT [FK_SfcRecipeDataOutput_SfcValueType]
GO
ALTER TABLE [SfcRecipeDataOutput]  WITH CHECK ADD  CONSTRAINT [FK_Target_Value] FOREIGN KEY([TargetValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataOutput] CHECK CONSTRAINT [FK_Target_Value]
GO
ALTER TABLE [SfcRecipeDataOutputRamp]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataOutputRamp_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataOutputRamp] CHECK CONSTRAINT [FK_SfcRecipeDataOutputRamp_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataRecipe]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataRecipe_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataRecipe] CHECK CONSTRAINT [FK_SfcRecipeDataRecipe_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataSimpleValue]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataSimpleValue_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataSimpleValue] CHECK CONSTRAINT [FK_SfcRecipeDataSimpleValue_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataSimpleValue]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataSimpleValue_SfcRecipeDataValue] FOREIGN KEY([ValueId])
REFERENCES [SfcRecipeDataValue] ([ValueId])
GO
ALTER TABLE [SfcRecipeDataSimpleValue] CHECK CONSTRAINT [FK_SfcRecipeDataSimpleValue_SfcRecipeDataValue]
GO
ALTER TABLE [SfcRecipeDataSQC]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataSQC_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataSQC] CHECK CONSTRAINT [FK_SfcRecipeDataSQC_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataTimer]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataTimer_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataTimer] CHECK CONSTRAINT [FK_SfcRecipeDataTimer_SfcRecipeData]
GO
ALTER TABLE [SfcRecipeDataValue]  WITH CHECK ADD  CONSTRAINT [FK_SfcRecipeDataValue_SfcRecipeData] FOREIGN KEY([RecipeDataId])
REFERENCES [SfcRecipeData] ([RecipeDataId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcRecipeDataValue] CHECK CONSTRAINT [FK_SfcRecipeDataValue_SfcRecipeData]
GO
ALTER TABLE [SfcReviewData]  WITH CHECK ADD  CONSTRAINT [FK_SfcReviewData_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcReviewData] CHECK CONSTRAINT [FK_SfcReviewData_SfcWindow]
GO
ALTER TABLE [SfcReviewDataTable]  WITH CHECK ADD  CONSTRAINT [FK__SfcReview__windo__5F7E2DAC] FOREIGN KEY([windowId])
REFERENCES [SfcReviewData] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcReviewDataTable] CHECK CONSTRAINT [FK__SfcReview__windo__5F7E2DAC]
GO
ALTER TABLE [SfcReviewFlows]  WITH CHECK ADD  CONSTRAINT [FK_SfcReviewFlows_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcReviewFlows] CHECK CONSTRAINT [FK_SfcReviewFlows_SfcWindow]
GO
ALTER TABLE [SfcReviewFlowsTable]  WITH CHECK ADD  CONSTRAINT [FK__SfcReview__windo__6442E2C9] FOREIGN KEY([windowId])
REFERENCES [SfcReviewFlows] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcReviewFlowsTable] CHECK CONSTRAINT [FK__SfcReview__windo__6442E2C9]
GO
ALTER TABLE [SfcSaveData]  WITH CHECK ADD  CONSTRAINT [FK_SfcSaveData_SfcSaveData] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcSaveData] CHECK CONSTRAINT [FK_SfcSaveData_SfcSaveData]
GO
ALTER TABLE [SfcSelectInput]  WITH CHECK ADD  CONSTRAINT [FK_SfcSelectInput_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcSelectInput] CHECK CONSTRAINT [FK_SfcSelectInput_SfcWindow]
GO
ALTER TABLE [SfcStep]  WITH CHECK ADD  CONSTRAINT [FK_SfcStep_SfcChart] FOREIGN KEY([ChartId])
REFERENCES [SfcChart] ([ChartId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcStep] CHECK CONSTRAINT [FK_SfcStep_SfcChart]
GO
ALTER TABLE [SfcStep]  WITH CHECK ADD  CONSTRAINT [FK_SfcStep_SfcStepType] FOREIGN KEY([StepTypeId])
REFERENCES [SfcStepType] ([StepTypeId])
GO
ALTER TABLE [SfcStep] CHECK CONSTRAINT [FK_SfcStep_SfcStepType]
GO
ALTER TABLE [SfcTimeDelayNotification]  WITH CHECK ADD  CONSTRAINT [FK_SfcTimeDelayNotification_SfcWindow] FOREIGN KEY([windowId])
REFERENCES [SfcWindow] ([windowId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcTimeDelayNotification] CHECK CONSTRAINT [FK_SfcTimeDelayNotification_SfcWindow]
GO
ALTER TABLE [SfcWindow]  WITH CHECK ADD  CONSTRAINT [FK_SfcWindow_SfcControlPanel] FOREIGN KEY([controlPanelId])
REFERENCES [SfcControlPanel] ([ControlPanelId])
ON DELETE CASCADE
GO
ALTER TABLE [SfcWindow] CHECK CONSTRAINT [FK_SfcWindow_SfcControlPanel]
GO
ALTER TABLE [TkConsole]  WITH CHECK ADD  CONSTRAINT [FK_TkConsole_TkPost] FOREIGN KEY([PostId])
REFERENCES [TkPost] ([PostId])
GO
ALTER TABLE [TkConsole] CHECK CONSTRAINT [FK_TkConsole_TkPost]
GO
ALTER TABLE [TkLogbookDetail]  WITH CHECK ADD  CONSTRAINT [FK_TkLogbookDetail_TkLogbook] FOREIGN KEY([LogbookId])
REFERENCES [TkLogbook] ([LogbookId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [TkLogbookDetail] CHECK CONSTRAINT [FK_TkLogbookDetail_TkLogbook]
GO
ALTER TABLE [TkMessageReply]  WITH CHECK ADD  CONSTRAINT [FK_TkMessageReply_TkMessageRequest] FOREIGN KEY([RequestId])
REFERENCES [TkMessageRequest] ([RequestId])
ON DELETE CASCADE
GO
ALTER TABLE [TkMessageReply] CHECK CONSTRAINT [FK_TkMessageReply_TkMessageRequest]
GO
ALTER TABLE [TkPost]  WITH CHECK ADD  CONSTRAINT [FK_TkPost_QueueMaster] FOREIGN KEY([MessageQueueId])
REFERENCES [QueueMaster] ([QueueId])
GO
ALTER TABLE [TkPost] CHECK CONSTRAINT [FK_TkPost_QueueMaster]
GO
ALTER TABLE [TkPost]  WITH CHECK ADD  CONSTRAINT [FK_TkPost_TkLogbook] FOREIGN KEY([LogbookId])
REFERENCES [TkLogbook] ([LogbookId])
GO
ALTER TABLE [TkPost] CHECK CONSTRAINT [FK_TkPost_TkLogbook]
GO
ALTER TABLE [TkUnit]  WITH CHECK ADD  CONSTRAINT [FK_TkUnit_TkPost] FOREIGN KEY([PostId])
REFERENCES [TkPost] ([PostId])
GO
ALTER TABLE [TkUnit] CHECK CONSTRAINT [FK_TkUnit_TkPost]
GO
ALTER TABLE [TkUnitParameterBuffer]  WITH CHECK ADD  CONSTRAINT [FK_TkUnitParameterBuffer_TkUnitParameter] FOREIGN KEY([UnitParameterId])
REFERENCES [TkUnitParameter] ([UnitParameterId])
ON UPDATE CASCADE
ON DELETE CASCADE
GO
ALTER TABLE [TkUnitParameterBuffer] CHECK CONSTRAINT [FK_TkUnitParameterBuffer_TkUnitParameter]
GO
ALTER TABLE [UIRGlineInvolvedProperty]  WITH CHECK ADD  CONSTRAINT [FK_UIRGlineDetails_UIRGline] FOREIGN KEY([UIRId])
REFERENCES [UIRGline] ([UIRId])
ON DELETE CASCADE
GO
ALTER TABLE [UIRGlineInvolvedProperty] CHECK CONSTRAINT [FK_UIRGlineDetails_UIRGline]
GO
ALTER TABLE [UnitAliases]  WITH CHECK ADD  CONSTRAINT [FK_UnitAliases_Units] FOREIGN KEY([name])
REFERENCES [Units] ([name])
GO
ALTER TABLE [UnitAliases] CHECK CONSTRAINT [FK_UnitAliases_Units]
GO
ALTER TABLE [Units]  WITH CHECK ADD  CONSTRAINT [FK_Units_Units] FOREIGN KEY([id])
REFERENCES [Units] ([id])
GO
ALTER TABLE [Units] CHECK CONSTRAINT [FK_Units_Units]
GO
