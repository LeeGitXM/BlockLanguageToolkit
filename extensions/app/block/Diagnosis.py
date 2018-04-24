#  Copyright 2014 ILS Automation
#
# A Diagnosis block receives a truth-value from an SQC
# or other block upstream and deduces the reason for the
# issue.
#
import app
class Diagnosis(app.block.BasicBlock.BasicBlock):
	def __init__(self):
		import app
		app.block.BasicBlock.BasicBlock.__init__(self)
		self.initialize()
	
	# Set attributes custom to this class
	def initialize(self):
		self.className = 'app.block.Diagnosis.Diagnosis'
		self.properties['Diagnosis'] = {'value':'','editable':'True'}
		self.inports = [{'name':'in','type':'truthvalue'}]
		self.outports= [{'name':'out','type':'text'},{'name':'send','type':'signal'}]
		
	# Return a dictionary describing how to draw an icon
	# in the palette and how to create a view from it.
	def getPrototype(self):
		proto = {}
		proto['iconPath']= "Block/icons/large/diagnosis.png"
		proto['label']   = "Diagnosis"
		proto['tooltip']        = "Conclude a diagnosis based on input"
		proto['tabName']        = 'Control'
		proto['viewLabel']      = 'Diag'
		proto['blockClass']     = self.getClassName()
		proto['blockStyle']     = 'square'
		proto['viewHeight']     = 100
		proto['inports']        = self.getInputPorts()
		proto['outports']       = self.getOutputPorts()
		#proto['viewWidth']      = 150
		return proto
			
	# Called when a value has arrived on one of our input ports
	# It is our diagnosis. Set the property then evaluate.
	def setValue(self,value,quality,port):
		import system.ils.blt.report as reporter
		diagnosis = self.properties.get('Diagnosis',{})
		text = str(value).lower()
		if text == 'true':
			msg = "Fix it"
			reporter.send(self.parentuuid,self.uuid,"send","inhibit","good")
			reporter.send(self.parentuuid,self.uuid,"send","reset","good")
		else:
			msg = "Leave things alone"
			
		diagnosis['value'] = msg
		print "Diagnosis.setValue: ",msg
		reporter.send(self.parentuuid,self.uuid,"out",msg,"good")