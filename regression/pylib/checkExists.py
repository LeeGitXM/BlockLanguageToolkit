# Copyright 2015. ILS Automation. All rights reserved.

import system.ils.blt.application as application

# Place a result of "true" in the common dictionary
# if the application is found in the default database.
# Argument is the application name
def application(name):
	handler = application.getHandler()
	db = handler.getDefaultDatabase(name)
	SQL = "SELECT ApplicationId FROM DtApplication "\
          " WHERE Application = '%s';" % name
	val = system.db.runScalarQuery(SQL,db)
	if val != None:
		common['result'] = True
	else:
		common['result'] = False

# Argument is the family name
def family(app,family):
	handler = application.getHandler()
	db = handler.getDefaultDatabase(name)
	result = system.# Fetch the list of consoles
	SQL = "SELECT FamilyId FROM DtFamily "\
          " WHERE Application = '%s' "\
		  "  AND  Family = '%s';" % app,family
	val = system.db.runScalarQuery(SQL,db)
	if val != None:
		common['result'] = True
	else:
		common['result'] = False
