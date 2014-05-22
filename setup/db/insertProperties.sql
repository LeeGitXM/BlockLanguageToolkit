-- Map G2 block properties into Ignition block properties
-- Columns are: G2Class, G2Property, ignition propertyName, datatype, editable, binding type
insert into PropertyMap values ('GDL-AND-GATE','logic','','STRING',1,'NONE');
insert into PropertyMap values ('GDL-AND-GATE','maximumUnknownInputs','','INTEGER',1,'NONE');
insert into PropertyMap values ('GDL-AND-GATE','outputUncertainty','','DOUBLE',1,'NONE');
insert into PropertyMap values ('GDL-AND-GATE','useExpiredInputs','','STRING',1,'NONE');
insert into PropertyMap values ('GDL-HIGH-VALUE-OBSERVATION','hysteresisWhen','','DOUBLE',1,'NONE');
insert into PropertyMap values ('GDL-HIGH-VALUE-OBSERVATION','outputUncertainty','','DOUBLE',1,'NONE');
insert into PropertyMap values ('GDL-HIGH-VALUE-OBSERVATION','statusOnInitialization','','STRING',1,'NONE');
insert into PropertyMap values ('GDL-HIGH-VALUE-OBSERVATION','threshold','Limit','DOUBLE',1,'NONE');
insert into PropertyMap values ('GDL-HIGH-VALUE-OBSERVATION','thresholdUncertainty','deadband','DOUBLE',1,'NONE');
insert into PropertyMap values ('GDL-NUMERIC-ENTRY-POINT','nameOfSensor','Input','STRING',1,'TAG');
insert into PropertyMap values ('GDL-OR-GATE','logic','','STRING',1,'NONE');
insert into PropertyMap values ('GDL-OR-GATE','maximumUnknownInputs','','INTEGER',1,'NONE');
insert into PropertyMap values ('GDL-OR-GATE','outputUncertainty','','DOUBLE',1,'NONE');
insert into PropertyMap values ('GDL-OR-GATE','useExpiredInputs','','STRING',1,'NONE');
