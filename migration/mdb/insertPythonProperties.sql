-- These are class-dependent attributes for classes implemented in Python
-- Columns are:  className, Key, Value  for the block descriptors.
--    keys: blockStyle,editorClass,iconPath,label,nameDisplayed,nameOffsetX,nameOffsetY,tooltip,tabName,
--          viewBackgroundColor,viewBlockIcon,viewFontSize,viewHeight,viewIcon,viewLabel,viewWidth
-- Columns are:  className, shape,embedded icon,embedded text, icon path, text size, width, height 
insert into PythonPrototypes values ('xom.block.action.Action','blockstyle','SQUARE');
insert into PythonPrototypes values ('xom.block.action.Action','viewIcon','Block/icons/embedded/gear.png');
insert into PythonPrototypes values ('xom.block.action.Action','viewFontSize','24');
insert into PythonPrototypes values ('xom.block.action.Action','viewHeight','70');
insert into PythonPrototypes values ('xom.block.action.Action','viewWidth','70');
insert into PythonPrototypes values ('xom.block.arithmetic.Arithmetic','blockstyle','SQUARE');
insert into PythonPrototypes values ('xom.block.arithmetic.Arithmetic','viewIcon','Block/icons/embedded/gear.png');
insert into PythonPrototypes values ('xom.block.arithmetic.Arithmetic','viewFontSize','24');
insert into PythonPrototypes values ('xom.block.arithmetic.Arithmetic','viewHeight','100');
insert into PythonPrototypes values ('xom.block.arithmetic.Arithmetic','viewWidth','150');
insert into PythonPrototypes values ('xom.block.finaldiagnosis.FinalDiagnosis','blockstyle','SQUARE');
insert into PythonPrototypes values ('xom.block.finaldiagnosis.FinalDiagnosis','editorClass','com.ils.blt.designer.config.FinalDiagnosisConfiguration');
insert into PythonPrototypes values ('xom.block.finaldiagnosis.FinalDiagnosis','viewLabel','Final Diagnosis');
insert into PythonPrototypes values ('xom.block.finaldiagnosis.FinalDiagnosis','viewFontSize','24');
insert into PythonPrototypes values ('xom.block.finaldiagnosis.FinalDiagnosis','viewHeight','80');
insert into PythonPrototypes values ('xom.block.finaldiagnosis.FinalDiagnosis','viewWidth','100');

insert into PythonPrototypes values ('xom.block.sqcdiagnosis.SQCDiagnosis','blockstyle','SQUARE');
insert into PythonPrototypes values ('xom.block.sqcdiagnosis.SQCDiagnosis','viewLabel','SQC Diagnosis');
insert into PythonPrototypes values ('xom.block.sqcdiagnosis.SQCDiagnosis','viewFontSize','24');
insert into PythonPrototypes values ('xom.block.sqcdiagnosis.SQCDiagnosis','viewHeight','80');
insert into PythonPrototypes values ('xom.block.sqcdiagnosis.SQCDiagnosis','viewWidth','100');
insert into PythonPrototypes values ('xom.block.subdiagnosis.SubDiagnosis','blockstyle','SQUARE');
insert into PythonPrototypes values ('xom.block.subdiagnosis.SubDiagnosis','viewLabel','Sub Diagnosis');
insert into PythonPrototypes values ('xom.block.subdiagnosis.SubDiagnosis','viewFontSize','24');
insert into PythonPrototypes values ('xom.block.subdiagnosis.SubDiagnosis','viewHeight','80');
insert into PythonPrototypes values ('xom.block.subdiagnosis.SubDiagnosis','viewWidth','100');

-- Columns are:  className, propertyName, propertyType, editable
insert into PythonBlockProperties values ('xom.block.action.Action','Script','STRING',1);
insert into PythonBlockProperties values ('xom.block.arithmetic.Arithmetic','Function','STRING',1);
insert into PythonBlockProperties values ('xom.block.finaldiagnosis.FinalDiagnosis','Label','STRING',1);
insert into PythonBlockProperties values ('xom.block.sqcdiagnosis.SQCDiagnosis','Label','STRING',1);
insert into PythonBlockProperties values ('xom.block.subdiagnosis.SubDiagnosis','Label','STRING',1);
