-- Map G2 block properties into Ignition block properties within procedures
-- Columns are: G2 class, G2 propertyName, property, mode
--     where mode = 1 => wholesale replacement
--                  2 => rename property
--                  3 => constant
-- chart - ignore debug-mode
insert into ProcPropertyMap values ('chart','debug-mode','log.isTraceEnabled()',1);
-- step
insert into ProcPropertyMap values ('s88-recipe-entity','debug-mode','log.isTraceEnabled()',1);
insert into ProcPropertyMap values ('s88-recipe-entity','class','step',3);
insert into ProcPropertyMap values ('s88-recipe-entity','label','name',2);
