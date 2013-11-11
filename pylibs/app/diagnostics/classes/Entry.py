#  Copyright 2013 ILS Automation
#
# This class an abstract base class for all blocks in
# diagnostics diagrams.
#
class Entry():
	def __init__(self, quadratic, linear, constant):
		super()
		properties['class'] = {'value':'app.diagnostics.classes.ENTRY','editable':False}
	
	# Accept notification that a value has arrived on an input
	# The default implementation does nothing
	def setValue(value,port):
		pass
	
	# Evaluate the block. This default implementation
	# does nothing.
	def evaluate():
		pass
		