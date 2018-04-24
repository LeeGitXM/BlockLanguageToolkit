#  Copyright 2014 ILS Automation
#
# Utility functions for dealing with Python classes for
# the block language toolkit. Each of these functions
# expects a variable shared with the caller named: share.
#
#  ============== Externally callable ======
# Create an instance of a particular class.
# The shared dictionary contains:
#     class - incoming class name
#     parent - UUID of enclosing diagram
#     id     - UUID of the block itself
#     result - shared dictionary.
def createBlockInstance(className,parent,id):
	import app, system
	
	obj = app.lib.util.getNewBlockInstance(className)
	obj.setUUID(id)
	obj.setParentUUID(parent)
	result['instance'] = obj
#
# Given an instance of an executable block
# Call its evaluate() method. There is no
# shared dictionary.
def evaluate(block):
	import app, system
	print 'app.block.util.evaluate(block) ...'

	if block!=None:
		block.evluate()
		
# Given an instance of an executable block,
# write its properties to the supplied list (properties)
# as specified in the Gateway startup script.
# 
def getBlockProperties(block):
	import app, system
	print 'app.block.util.getBlockProperties(block) ...'

	if block!=None:
		print block.__class__
		print block.getProperties()
		dict = block.getProperties()
		for key in dict:
			prop = dict[key]
			prop['name'] = key
			properties.append(prop)
	else:
		print "getBlockProperties: argument ",block," not defined"

	

#
# Return a new instance of each class of block.
# This works as long as all the block definitions are 
# in the "app.block" package. Our convention is that only
# executable blocks appear in this package -- and that
# the class has the same name as its file.
def getNewBlockInstances():
	import app.block
	
	instances = []
	for name in dir(app.block):
		if not name.startswith('__') and not name == 'BasicBlock':
			constructor = "app.block."+name+"."+name+"()"
			obj = eval(constructor)
			instances.append(obj)	
	return instances
#
# Return a new instance of each class of block.
# This works as long as all the block definitions are 
# in the "app.block" package. Our convention is that only
# executable blocks appear in this package -- and that
# the class has the same name as its file.
def getNewBlockInstance(className):
	import app.block
	
	print 'getNewBlockInstance: ',className
	obj = None
	for name in dir(app.block):
		if "app.block."+name+"."+name == className:
			constructor = "app.block."+name+"."+name+"()"
			obj = eval(constructor)
			break
	return obj
	
#
# Obtain a list of all subclasses of BasicBlock,
# then create a dictionary of prototype attributes from each. 
# Communicate back in 'prototypes', a list, as set
# in Gateway startup script. 
def getBlockPrototypes():
	import app

	instances = app.lib.util.getNewBlockInstances()
	for obj in instances:
		print 'getBlockPrototypes:',obj.getProperty('class').get('value','--')
		prototypes.append(obj.getPrototype())

#

# Given an instance of an executable block,
# set one of its properties. The shared variable
# is a dictionary named "property"
# as specified in the Gateway startup script.
# 
def setBlockProperty(block):
	import app, system
	print 'app.block.util.setBlockProperty(block) ...'

	if block!=None:
		print block.__class__
		block.setProperties(property.get("name"),property)
	
# We've received a value on our input (there is only one)
# We expect a truth value.
#  block - the python block object
#  port  - the input port name
#  value - the new value
#  quality - the quality of the new value
def setValue(block,port,value,quality):
	import app, system
	print 'app.block.util.evaluate(block) ...'
	
	if block!=None:
		print block.__class__," received ",value,",",quality," on ",port
		block.setValue(value,quality,port)
	
