# Copyright 2015. ILS Automation. All rights reserved.
# Argument is the diagram path
import system.ils.blt.application as application
def construct(common,diagramPath,blockName,port):
	handler = application.getHandler()     # PythonRequestHandler
	notification = ""
	# The descriptor paths are :-separated, the input uses /
	# the descriptor path starts with ":root:", the input stars with the application
	descriptors = application.getDiagramDescriptors()
	for desc in descriptors:
		path = desc.path[6:]
		path = path.replace(":","/")
		#print desc.id, path
		if diagramPath == path:
			diagram = handler.getDiagram(desc.id)
			blockId = handler.getBlockId(diagram,blockName)
			print blockId
			notification = "C:"+blockId+":"+port
			break
	print "notificationKey.construct=="+notification
	common['result'] = notification 
