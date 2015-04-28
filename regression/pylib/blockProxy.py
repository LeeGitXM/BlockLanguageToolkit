# Copyright 2015. ILS Automation. All rights reserved.
# OPerations on a block
import system.ils.blt.application as application

# Argument is the diagram path
def internalValue(common,diagramName,blockName,attribute):
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

