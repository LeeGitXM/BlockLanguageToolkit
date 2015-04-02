-- Map stub to anchor names
-- Key is G2 class and port
CREATE TABLE AnchorMap(
	G2Class text NOT NULL,
	G2Port  text NOT NULL,
	Port    text NOT NULL,
	Annotation text NULL
);
-- Map a G2 block class to an Ignition block class
CREATE TABLE ClassMap(
    G2Class text PRIMARY KEY ,
    IgnitionClass text NOT NULL
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
-- Global preferences
CREATE TABLE PreferenceMap(
    Name  text NOT NULL,
    Value text NOT NULL
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

-- These are properties of blocks that are fixed
-- no matter what. Applicable to python blocks.
CREATE TABLE PythonBlockProperties(
    BlockClass text NOT NULL,
    PropertyName text NOT NULL,
	PropertyType text NOT NULL,
	Editable integer
);
-- This table is used to define attributes that
-- are fixed for a given class of block. Only
-- blocks that are defined in Python should be
-- described. Attributes of Java-defined blocks
-- come directly from the class definitions.
CREATE TABLE PythonPrototypes(
    BlockClass text PRIMARY KEY ,
	Style text,
    EmbeddedIconPath text,
    EmbeddedLabel text,
    IconPath text,
    FontSize integer,
    Width integer,
    Height integer
);
-- Map G2 GSI names to Ignition tags
-- Key is G2 class and port
CREATE TABLE TagMap(
	GSIName  text NOT NULL,
	TagPath  text NOT NULL,
	DataType text NOT NULL
);

-- The following are for use during procedure translation
-- Map a G2 class to an Ignition python class
CREATE TABLE ProcClassMap(
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
CREATE TABLE ProcClassProperty(
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
CREATE TABLE ProcPropertyMap(
    G2Class    text NOT NULL,
    G2Property text NOT NULL,
	Name     text NOT NULL,
	DataType text NOT NULL,
	Editable integer,
	BindingType text NOT NULL
);
-- These are values of symbolic constants
CREATE TABLE ProcEnumerationMap(
   G2Name text      NOT NULL,
   EnumerationName  text NOT NULL,
   Value            text NOT NULL
);
-- These are values of globals
-- by the procedures that need them.
CREATE TABLE ProcGlobalMap(
   PyProc      text NOT NULL,
   GlobalName  text NOT NULL
);
