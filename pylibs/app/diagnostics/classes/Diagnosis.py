#  Copyright 2013 ILS Automation
#
# This class computes a diagnosis.
#
import app
class Dia(app.diagnostics.classes.BasicBlock):
	def __init__(self):
		super().__init__()
		properties['class'] = {'value':'app.diagnostics.classes.Diagnosis','editable':'False'}
		properties['diagnosis'] = {'value':''}
	
	# Accept notification that a value has arrived on an input
	# The default implementation does nothing
	def setValue(value,port):
		pass
	
	# Evaluate the block. This default implementation
	# does nothing.
	def evaluate():
		pass
		