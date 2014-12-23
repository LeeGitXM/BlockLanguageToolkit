insert into ClassMap values ('EM-GDA-ABSOLUTE-COMPARE','com.ils.block.CompareAbsolute');
insert into ClassMap values ('EM-GDA-BAD-DATA-HANDLER','com.ils.block.DataConditioner');
insert into ClassMap values ('EM-GDA-CLOCK-TIMER','com.ils.block.Clock');
insert into ClassMap values ('EM-GDA-COMPARE','com.ils.block.Compare');
insert into ClassMap values ('EM-GDA-DEADBAND-COMPARE','com.ils.block.CompareDeadband');
insert into ClassMap values ('EM-GDA-ELAPSED-TIME-VARIABLE','com.ils.block.Junction');
insert into ClassMap values ('EM-GDA-EXPIRATION-FILTER','com.ils.block.Junction');
insert into ClassMap values ('EM-GDA-FINAL-DIAGNOSIS','xom.block.finaldiagnosis.FinalDiagnosis');
insert into ClassMap values ('EM-GDA-HIGH-LIMIT','com.ils.block.HighLimitObservation');
insert into ClassMap values ('EM-GDA-HIGH-LIMIT-WITH-DEADBAND','com.ils.block.HighLimitObservation');
insert into ClassMap values ('EM-GDA-LATCH','com.ils.block.EdgeTrigger');
insert into ClassMap values ('EM-GDA-LOGIC-FILTER','com.ils.block.LogicFilter');
insert into ClassMap values ('EM-GDA-LOGICAL-VARIABLE','com.ils.block.Parameter');
insert into ClassMap values ('EM-GDA-LOW-LIMIT','com.ils.block.LowLimitObservation');
insert into ClassMap values ('EM-GDA-LOW-LIMIT-WITH-DEADBAND','com.ils.block.LowLimitObservation');
insert into ClassMap values ('EM-GDA-SET-VAR-FALSE','com.ils.block.Junction');
insert into ClassMap values ('EM-GDA-SIMPLE-TREND-OBSERVATION','com.ils.block.TrendObservation');
insert into ClassMap values ('EM-GDA-SQC-DIAGNOSIS','xom.block.sqcdiagnosis.SQCDiagnosis');
insert into ClassMap values ('EM-GDA-SQC-LIMIT-OBSERVATION','com.ils.block.SQC');
insert into ClassMap values ('EM-GDA-SUBDIAGNOSIS','xom.block.subdiagnosis.SubDiagnosis');
insert into ClassMap values ('EM-GDA-SYMBOLIC-VARIABLE','com.ils.block.Parameter');
insert into ClassMap values ('EM-GDA-TEST-POINT','com.ils.block.TestPoint');
insert into ClassMap values ('GDL-AND-GATE','com.ils.block.And');
insert into ClassMap values ('GDL-ARITHMETIC-FUNCTION','xom.block.Arithmetic.Arithmetic');
insert into ClassMap values ('GDL-BLOCK-EVALUATION','com.ils.block.Clock');
insert into ClassMap values ('GDL-BLOCK-RESET','com.ils.block.Reset');
insert into ClassMap values ('GDL-CONCLUSION','com.ils.block.Junction');
insert into ClassMap values ('GDL-D.D-DISPLAY','com.ils.block.Readout');
insert into ClassMap values ('GDL-DATA-PATH-DISPLAY','com.ils.block.Readout');
insert into ClassMap values ('GDL-DATA-SHIFT','com.ils.block.DataShift');
insert into ClassMap values ('GDL-DATA-TIME-STAMP','com.ils.block.Junction');
insert into ClassMap values ('GDL-DIFFERENCE','com.ils.block.Difference');
insert into ClassMap values ('GDL-ENCAPSULATION','com.ils.block.Encapsulation');
insert into ClassMap values ('GDL-ENCAPSULATION-BLOCK','com.ils.block.Encapsulation');
insert into ClassMap values ('GDL-EQUALITY-OBSERVATION','com.ils.block.EqualityObservation');
insert into ClassMap values ('GDL-INFERENCE-DELAY','com.ils.block.Delay');
insert into ClassMap values ('GDL-INFERENCE-EVENT','com.ils.block.EdgeTrigger');
insert into ClassMap values ('GDL-GENERIC-ACTION','xom.block.Action.Action');
insert into ClassMap values ('GDL-HIGH-VALUE-OBSERVATION','com.ils.block.HighLimitObservation');
insert into ClassMap values ('GDL-IN-RANGE-OBSERVATION','com.ils.block.RangeObservation');
insert into ClassMap values ('GDL-LOW-VALUE-OBSERVATION','com.ils.block.LowLimitObservation');
insert into ClassMap values ('GDL-MOVING-AVERAGE','com.ils.block.MovingAverageSample');
insert into ClassMap values ('GDL-NOT-GATE','com.ils.block.Not');
insert into ClassMap values ('GDL-NUMERIC-ENTRY-POINT','com.ils.block.Input');
insert into ClassMap values ('GDL-OR-GATE','com.ils.block.Or');
insert into ClassMap values ('GDL-PERSISTENCE-GATE','com.ils.block.PersistenceGate');
insert into ClassMap values ('GDL-SYMBOLIC-ENTRY-POINT','com.ils.block.Input');
insert into ClassMap values ('GDL-TIMER','com.ils.block.Clock');
insert into ClassMap values ('INTEGER-PARAMETER','com.ils.block.Parameter');
insert into ClassMap values ('LOGICAL-PARAMETER','com.ils.block.Parameter');
insert into ClassMap values ('LOGICAL-VARIABLE','com.ils.block.Parameter');
insert into ClassMap values ('SYMBOLIC-PARAMETER','com.ils.block.Parameter');
-- NOTE: We convert all connnection posts to "sinks"
--       later on we analyze and determine which are "sources"
insert into ClassMap values ('GDL-INFERENCE-PATH-CONNECTION-POST','com.ils.block.SinkConnection');
insert into ClassMap values ('GDL-DATA-PATH-CONNECTION-POST','com.ils.block.SinkConnection');
