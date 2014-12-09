-- These are class-dependent attributes for classes implemented in Python
-- Columns are:  className, shape,embedded icon,embedded text, icon path, text size, width, height 
-- NOTE: The icon path is an icon path to use if the entire rendering is an icon.
insert into PythonPrototypes values ('emc.block.Action.Action','SQUARE','Block/icons/embedded/gear.png','','',24,70,70);
insert into PythonPrototypes values ('emc.block.Arithmetic.Arithmetic','SQUARE','Block/icons/embedded/fx.png','','',24,150,100);
insert into PythonPrototypes values ('emc.block.FinalDiagnosis.FinalDiagnosis','SQUARE','','Final\nDiagnosis','',24,100,80);
insert into PythonPrototypes values ('emc.block.SQCDiagnosis.SQCDiagnosis','SQUARE','','SQC\nDiagnosis','',24,100,80);
insert into PythonPrototypes values ('emc.block.SubDiagnosis.SubDiagnosis','SQUARE','','Sub\nDiagnosis','',24,100,80);

-- Columns are:  className, propertyName, propertyType, editable
insert into PythonBlockProperties values ('emc.block.Action.Action','Script','STRING',1);
insert into PythonBlockProperties values ('emc.block.Arithmetic.Arithmetic','Function','STRING',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','CalculationMethod','SCRIPTREF',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','Explanation','STRING',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','Label','STRING',1);
--insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','LogToDatabase','BOOLEAN',1);
--insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','ManualMove','BOOLEAN',1);
--insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','ManualMoveValue','DOUBLE',1);
--insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','ManualTextRequired','BOOLEAN',1);
--insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','Multiplier','DOUBLE',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','PostTextRecommendation','BOOLEAN',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','Priority','DOUBLE',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','TextRecommendation','STRING',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','TextRecommendationCallback','SCRIPTREF',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','RecommendationRefreshInterval','DOUBLE',1);
insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','Targets','LIST',1);
--insert into PythonBlockProperties values ('emc.block.FinalDiagnosis.FinalDiagnosis','TrapInsignificantConditions','BOOLEAN',1);
insert into PythonBlockProperties values ('emc.block.SQCDiagnosis.SQCDiagnosis','Label','STRING',1);
insert into PythonBlockProperties values ('emc.block.SubDiagnosis.SubDiagnosis','Label','STRING',1);
