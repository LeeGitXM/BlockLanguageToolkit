# Copyright 2015. ILS Automation. All rights reserved.
# Argument is the diagram path
import system.ils.blt.application as application
def reset(name):
	print name
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

