# Copyright 2015. ILS Automation. All rights reserved.
# Test the client/designer "toolkit" scripting interfaces

import system.ils.blt.application as application

# Return a list of name of blocks that are downstream of the 
# specified block - and in the same diagram
def listBlocksDownstreamOf(common,dpath,blockName):
	diagid = getDiagram(dpath).getSelf().toString()
    # blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listBlocksDownStreamOf(diagid,blockName)
	lst = []
	for block in blocks:
		print block.getName()
		lst.append(block.getName())
	common['result'] = lst 

# Return a list of names of blocks that are downstream of the 
# specified block - and in the same diagram
def listBlocksUpstreamOf(common,dpath,blockName):
	diagid = getDiagram(dpath).getSelf().toString()
    # blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listBlocksUpstreamOf(diagid,blockName)
	lst = []
	for block in blocks:
		print block.getName()
		lst.append(block.getName())
	common['result'] = lst 

# Return a list of block names that match the class criterion
def listBlocksOfClass(common,dpath,classname):
	diagid = getDiagram(dpath).getSelf().toString()
    # blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listDiagramBlocksOfClass(diagid,classname)
	lst = []
	for block in blocks:
		print block.getName()
		lst.append(block.getName())
	common['result'] = lst 

# Return a list of names of all blocks in the specified diagram
def listBlocksInDiagram(common,dpath):
	diagid = getDiagram(dpath).getSelf().toString()
    # blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listBlocksInDiagram(diagid)
	lst = []
	for block in blocks:
		print block.getName()
		lst.append(block.getName())
	common['result'] = lst 

# Return a list of names of blocks that with one or more properties
# that are bound to the specified tag. The search is across all
# diagrams.
def listBlocksForTag(common,tagpath):
    # blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listBlocksForTag(tagpath)
	lst = []
	for block in blocks:
		print block.getName()
		lst.append(block.getName())
	common['result'] = lst 

# Return a list of sink blocks that are "connected" to the
# input of the specified source. All blocks in the 
# gateway are considered.
def listSinksForSource(common,dpath,blockName):
	diagid = getDiagram(dpath).getSelf().toString()
    # blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listBlocksInDiagram(diagid)
	lst = []
	for source in blocks:
		if source.getName()==blockName:
			blocks = application.listSinksForSource(source.getId().toString())
			for block in blocks:
				print block.getName()
				lst.append(block.getName())
			break

	common['result'] = lst 

# Return a list of source blocks that are "connected" to
# the output of the specified sink. All blocks in the 
# gateway are considered.
def listSourcesForSink(common,dpath,blockName):
	diagid = getDiagram(dpath).getSelf().toString()
    # blocks is a list of SerializableBlockStateDescriptor
	blocks = application.listBlocksInDiagram(diagid)
	lst = []
	for sink in blocks:
		if sink.getName()==blockName:
			blocks = application.listSourcesForSink(sink.getId().toString())
			for block in blocks:
				print block.getName()
				lst.append(block.getName())
			break

	common['result'] = lst 

# Return the navigation path to a block or diagram
def pathForNode(common,nodeId):
	path = application.pathForNode(nodeId)
	common['result'] = path 

# Propagate a signal to any receivers on the diagra
def sendLocalSignal(common,dpath,command,message,arg):
	diagid = getDiagram(dpath).getSelf().toString()
	application.sendLocalSignal(diagid,command,message,arg)

# -------------------------- Helper methods ----------------------
# Return the ProcessDiagram at the specified path
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

