# Copyright 2015. ILS Automation. All rights reserved.
import system.ils.blt.application as application
# Args: application node name
#       state
# NOTE: Don't confuse application with an Application node
def setAll(name,state):
	print name,":",state
	application.setApplicationState(name,state)
