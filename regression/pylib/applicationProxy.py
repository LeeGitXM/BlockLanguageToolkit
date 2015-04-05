# Copyright 2015. ILS Automation. All rights reserved.

import system.ils.blt.application as application

# Place a result of "true" in the common dictionary
# if the application is found in the default database.
# Argument is the application name
def exists(common,name):
	handler = application.getHandler()
	db = handler.getDefaultDatabase(name)
	SQL = "SELECT ApplicationId FROM DtApplication "\
          " WHERE Application = '%s';" % name
	val = system.db.runScalarQuery(SQL,db)
	if val != None:
		common['result'] = True
	else:
		common['result'] = False

# Args: application node name
#       state
# NOTE: The above is an Application in the database
#       This method refers to an application node
def setState(common,name,state):
	print name,":",state
	application.setApplicationState(name,state)
