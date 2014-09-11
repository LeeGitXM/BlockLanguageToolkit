-- Map a G2 class to an Ignition python class
CREATE TABLE ClassMap(
    G2Class text PRIMARY KEY ,
    IgnitionClass text NOT NULL
);

-- Map a G2 procedure to a python module
CREATE TABLE ProcedureMap(
    G2Procedure text PRIMARY KEY ,
    IgnitionProcedure text NOT NULL
);

-- Set properties of Ignition blocks based on a 
-- G2 class. These properties rely only on the G2 class.
CREATE TABLE ClassProperty(
    G2Class    text NOT NULL,
	Name     text NOT NULL,
	DataType text NOT NULL,
	Editable integer,
	BindingType text NOT NULL,
	Value text
);

-- Map a G2 block class to an Ignition block class
-- Map properties of G2 blocks to properties of
-- Ignition blocks
CREATE TABLE PropertyMap(
    G2Class    text NOT NULL,
    G2Property text NOT NULL,
	Name     text NOT NULL,
	DataType text NOT NULL,
	Editable integer,
	BindingType text NOT NULL
);
-- These are values of symbolic constants
CREATE TABLE ConstantMap(
   G2Name text NOT NULL,
   Value  text NOT NULL
);
-- These are values of globals
-- by the procedures that need them.
CREATE TABLE GlobalMap(
   PyProc      text NOT NULL,
   GlobalName  text NOT NULL
);
