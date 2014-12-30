-- These are class-dependent attributes for classes implemented in Python
-- Columns are:  className, shape,embedded icon,embedded text, icon path, text size, width, height 
-- NOTE: The icon path is an icon path to use if the entire rendering is an icon.
insert into PythonPrototypes values ('xom.block.action.Action','SQUARE','Block/icons/embedded/gear.png','','',24,70,70);
insert into PythonPrototypes values ('xom.block.arithmetic.Arithmetic','SQUARE','Block/icons/embedded/fx.png','','',24,150,100);
insert into PythonPrototypes values ('xom.block.finaldiagnosis.FinalDiagnosis','SQUARE','','Final Diagnosis','',24,100,80);
insert into PythonPrototypes values ('xom.block.sqcdiagnosis.SQCDiagnosis','SQUARE','','SQC Diagnosis','',24,100,80);
insert into PythonPrototypes values ('xom.block.subdiagnosis.SubDiagnosis','SQUARE','','Sub Diagnosis','',24,100,80);

-- Columns are:  className, propertyName, propertyType, editable
insert into PythonBlockProperties values ('xom.block.action.Action','Script','STRING',1);
insert into PythonBlockProperties values ('xom.block.arithmetic.Arithmetic','Function','STRING',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','CalculationMethod','SCRIPTREF',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','Explanation','STRING',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','Label','STRING',1);
-- insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','LogToDatabase','BOOLEAN',1);
-- insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','ManualMove','BOOLEAN',1);
-- insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','ManualMoveValue','DOUBLE',1);
-- insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','ManualTextRequired','BOOLEAN',1);
-- insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','Multiplier','DOUBLE',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','PostRecommendation','BOOLEAN',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','Priority','DOUBLE',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','Recommendation','STRING',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','RecommendationCallback','SCRIPTREF',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','RecommendationRefreshInterval','DOUBLE',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','Targets','LIST',1);
-- insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','TrapInsignificantConditions','BOOLEAN',1);
insert into PythonBlockProperties values ('xom.block.sqcdiagnosis.SQCDiagnosis','Label','STRING',1);
insert into PythonBlockProperties values ('xom.block.subdiagnosis.SubDiagnosis','Label','STRING',1);
