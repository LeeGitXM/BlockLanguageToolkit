# Copyright 2015. ILS Automation. All rights reserved.
# Test the client/designer "toolkit" scripting interfaces
import system.ils.blt.application as application
# Return a list of block names that match the class criterion
def listBlocksOfClass(common,dpath,classname):
	diagid = getDiagram(dpath).getSelf().toString()
	print "DIAGRAM=",diagid
# blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listDiagramBlocksOfClass(diagid,classname)
	lst = []
	for block in blocks:
		lst.append(block.getName())
	common['result'] = lst 

# -------------------------- Helper methods ----------------------
# Return the id of the diagram at the specified path
def getDiagram(dpath):
	diagram = None
	# The descriptor paths are :-separated, the input uses /
	# the descriptor path starts with ":root:", 
	# the input starts with the application
	descriptors = application.getDiagramDescriptors()
	handler = application.getHandler()
	for desc in descriptors:
		path = desc.path[6:]
		path = path.replace(":","/")
		#print desc.id, path
		if dpath == path:
			diagram = handler.getDiagram(desc.id)
	# Diagram is a ProcessDiagram
	return diagram

