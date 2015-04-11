# Copyright 2015. ILS Automation. All rights reserved.

import system.ils.blt.application as application

# Place a result of "true" in the common dictionary
# if the family is found in the default database.
# Argument is the family name
def family(common,app,family):
	handler = application.getHandler()
	db = handler.getDefaultDatabase(name)
	SQL = "SELECT FamilyId FROM DtFamily "\
          " WHERE Application = '%s' "\
		  "  AND  Family = '%s';" % app,family
	val = system.db.runScalarQuery(SQL,db)
	if val != None:
		common['result'] = True
	else:
		common['result'] = False
