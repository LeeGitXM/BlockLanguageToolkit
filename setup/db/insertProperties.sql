-- Map G2 block properties into Ignition block properties
-- Columns are: G2Class, G2Property, ignition propertyName, datatype, editable, binding type
insert into PropertyMap values ('GDL-NUMERIC-ENTRY-POINT','nameOfSensor','Input','DOUBLE',1,'TAG');
insert into PropertyMap values ('GDL-HIGH-VALUE-OBSERVATION','threshold','Limit','DOUBLE',1,'NONE');
