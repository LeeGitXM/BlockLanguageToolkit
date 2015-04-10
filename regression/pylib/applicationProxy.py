# Copyright 2015. ILS Automation. All rights reserved.

import system.ils.blt.application as application

def configureForTest(common,name):
	console = 'VFU'
	grade = '28'

	handler = application.getHandler()
	db = handler.getDefaultDatabase(name)

	vfuConsoleId=insertConsole(console,db)
	app1Id=insertApplication(name,vfuConsoleId,db)
	return app1Id

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


#  This method refers to an application node in the Nav tree
def setState(common,name,state):
	print name,":",state
	application.setApplicationState(name,state)
# 
# =============================== Helper Methods =========================
# These are not directly callable from a test script
# 
def insertConsole(console,db):
	SQL = "delete from DtConsole where console='%s'" % (console)
	system.db.runUpdateQuery(SQL,db)
	SQL = "insert into DtConsole (console) values ('%s')" % (console)
	consoleId = system.db.runUpdateQuery(SQL,db, getKey=True)
	return consoleId
			
# Define an application
def insertApplication(application, consoleId):
	SQL = "insert into DtApplication (application, ConsoleId) values ('%s', %i)" % (application, consoleId)
	applicationId = system.db.runUpdateQuery(SQL, getKey=True)
	return applicationIdNOTE: The above is an Application in the database

