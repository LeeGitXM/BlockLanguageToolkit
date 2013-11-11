#  Copyright 2013 ILS Automation
#
# This class an abstract base class for all blocks in
# diagnostics diagrams.
#
class BasicBlock():
	# Properties are a dictionaty of attributes keyed by name
	properties = {'class':{'value':'app.diagnostics.classes.BasicBlock','readonly':True}}
	
	# Input ports are named stubs for incoming connections
	inports = []
	# Outports are named stubs for outgoing connections
	outports = []
	
	# Return a list of property names. 
	def getPropertyNames():
		return properties.getKeys()
		
	# Return a specified property. The property
	# is a dictionary guaranteed to have a "value". 
	def getPropertyName(name):
		properties.get(name,{})
		
	# Accept notification that a value has arrived on an input
	# The default implementation does nothing
	def setValue(value,port):
		pass
	
	# Evaluate the block. This default implementation
	# does nothing.
	def evaluate():
		pass
		