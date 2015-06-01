# Copyright 2015. ILS Automation. All rights reserved.
# Operations on a block
import system.ils.blt.application as application

# Internal status is a SerializableBlockStateDescriptor
# -- the descriptor has methods getAttributes(), getBuffer()
# -- both return lists of dictionaries.
# dpath - the diagram path
# blockName - name of the block within the diagram
# attName - name of the desired parameter in the internal structure
def internalValue(common,dpath,blockName,attName):
	diagram = getDiagram(dpath)
	for block in diagram.getProcessBlocks:
		if block.getName == blockName:
			attributes = block.getInternalStatus().getAttributes()
			attribute = attributes.get(attName)
			common['result'] = attribute
			return 
			
# Return the value of a property of a block
def getBlockProperty(common,dpath,blockName,propName):
	diagram = getDiagram(dpath)
	for block in diagram.getProcessBlocks():
		if block.getName() == blockName:
			prop = block.getProperty(propName)
			if prop != None:
				common['result'] = prop.getValue()
			return

# Set the value of a property of a block
def setBlockProperty(common,dpath,blockName,propName,value):
	diagram = getDiagram(dpath)
	for block in diagram.getProcessBlocks():
		if block.getName() == blockName:
			prop = block.getProperty(propName)
			if prop != None:
				prop.setValue(value)
			return

# Return the state of a block
def getBlockState(common,dpath,blockName):
	diagram = getDiagram(dpath)
	for block in diagram.getProcessBlocks():
		if block.getName() == blockName:
			common['result'] = block.getState()
			return

# Reset an individual block
def resetBlock(common,dpath,blockName):
	diagid = getDiagram(dpath).getSelf().toString()
	application.resetBlock(diagid,blockName)

# -------------------------- Helper methods ----------------------
# Return a ProcessDiagram at the specified path
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
	return diagram

