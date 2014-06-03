insert into ClassMap values ('EM-GDA-FINAL-DIAGNOSIS','emc.block.FinalDiagnosis.FinalDiagnosis');
insert into ClassMap values ('EM-GDA-LATCH','emc.block.EdgeTrigger');
insert into ClassMap values ('EM-GDA-SET-VAR-FALSE','com.ils.block.Junction');
insert into ClassMap values ('EM-GDA-SQC-LIMIT-OBSERVATION','com.ils.block.SQC');
insert into ClassMap values ('EM-GDA-TEST-POINT','com.ils.block.TestPoint');
insert into ClassMap values ('GDL-AND-GATE','com.ils.block.And');
insert into ClassMap values ('GDL-BLOCK-RESET','com.ils.block.Reset');
insert into ClassMap values ('GDL-CONCLUSION','com.ils.block.Junction');
insert into ClassMap values ('GDL-DATA-PATH-DISPLAY','com.ils.block.Readout');
insert into ClassMap values ('GDL-DATA-TIME-STAMP','com.ils.block.Junction');
insert into ClassMap values ('GDL-ENCAPSULATION','com.ils.block.Encapsulation');
insert into ClassMap values ('GDL-EQUALITY-OBSERVATION','com.ils.block.EqualityObservation');
insert into ClassMap values ('GDL-INFERENCE-EVENT','com.ils.block.EdgeTrigger');
insert into ClassMap values ('GDL-HIGH-VALUE-OBSERVATION','com.ils.block.HighLimitObservation');
insert into ClassMap values ('GDL-LOW-VALUE-OBSERVATION','com.ils.block.LowLimitObservation');
insert into ClassMap values ('GDL-NOT-GATE','com.ils.block.Not');
insert into ClassMap values ('GDL-NUMERIC-ENTRY-POINT','com.ils.block.Input');
insert into ClassMap values ('GDL-OR-GATE','com.ils.block.Or');
insert into ClassMap values ('GDL-SYMBOLIC-ENTRY-POINT','com.ils.block.Input');
-- NOTE: We convert all connnection posts to "sinks"
--       later on we analyze and determine which are "sources"
insert into ClassMap values ('GDL-INFERENCE-PATH-CONNECTION-POST','com.ils.block.SinkConnection');
