#  Copyright 2014 ILS Automation
#
# Implement a block that can execute custom functions. These
# functions are Python modules.
#
import project.block
import system.ils.blt.block as block
class Function(project.block.BasicBlock.BasicBlock):
    def __init__(self):
        project.block.BasicBlock.BasicBlock.__init__(self)
        self.initialize()
        
    # Set attributes custom to this class
    def initialize(self):
        self.className = 'project.block.Function.Function'
        self.properties['Function'] = {'value':'','editable':'True'}
        self.inports = [{'name':'in','type':'data'}]
        self.outports= [{'name':'out','type':'data'}]
        
    # Return a dictionary describing how to draw an icon
    # in the palette and how to create a view from it.
    def getPrototype(self):
        proto = {}
        proto['iconPath']= "Block/icons/palette/function.png"
        proto['label']   = "Function"
        proto['tooltip']        = "Execute a user-defined function on the input"
        proto['tabName']        = 'Arithmetic'
        proto['viewBackgroundColor'] = '0xF0F0F0'
        proto['viewIcon']      = "Block/icons/embedded/fx.png"
        proto['blockClass']     = self.getClassName()
        proto['blockStyle']     = 'diamond'
        proto['viewHeight']     = 70
        proto['viewWidth']      = 70
        proto['inports']        = self.getInputPorts()
        proto['outports']       = self.getOutputPorts()
        proto['receiveEnabled']  = 'false'
        proto['transmitEnabled'] = 'false'
        return proto
            
    # Called when a value has arrived on one of our input ports
    # It is our diagnosis. Set the property then evaluate.
    def acceptValue(self,value,quality,port):
        diagnosis = self.properties.get('Diagnosis',{})
        text = str(value).lower()
        if text == 'true':
            msg = "Fix it"
            block.postValue(self.parentuuid,self.uuid,"send","inhibit","good")
            block.postValue(self.parentuuid,self.uuid,"send","reset","good")
        else:
            msg = "Leave things alone"
            
        diagnosis['value'] = msg
        print "FinalDiagnosis.setValue: ",msg
        block.postValue(self.parentuuid,self.uuid,"out",msg,"good")