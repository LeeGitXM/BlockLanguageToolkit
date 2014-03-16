CREATE TABLE BlockProperties(
    BlockClass text NOT NULL,
    PropertyName text NOT NULL,
	Type text NOT NULL,
	Editable integer,
	Maximum float,
    Minimum float

);

CREATE TABLE ClassMap(
    G2Class text PRIMARY KEY ,
    IgnitionClass text NOT NULL
);
CREATE TABLE ViewAttributesMap(
    BlockClass text PRIMARY KEY ,
	Style text,
    EmbeddedIconPath text,
    EmbeddedLabel text,
    IconPath text,
    FontSize integer,
    Width integer,
    Height integer
);
