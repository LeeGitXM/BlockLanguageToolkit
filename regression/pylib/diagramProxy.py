# Copyright 2015. ILS Automation. All rights reserved.
# Operations on a diagram
import system.ils.blt.application as application

# Argument is the diagram path
def reset(common,name):
	# The descriptor paths are :-separated, the input uses /
	# the descriptor path starts with ":root:", the input stars with the application
	descriptors = application.getDiagramDescriptors()
	for desc in descriptors:
		path = desc.path[6:]
		path = path.replace(":","/")
		#print desc.id, path
		if name == path:
			application.resetDiagram(desc.id)
			break

