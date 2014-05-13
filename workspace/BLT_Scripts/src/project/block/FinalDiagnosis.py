#  Copyright 2014 ILS Automation
#
# A FinalDiagnosis block receives a truth-value from an SQC
# or other block upstream and deduces the reason for the
# issue.
#
import project.block
import system.ils.blt.block as block
class FinalDiagnosis(project.block.BasicBlock.BasicBlock):
    def __init__(self):
        project.block.BasicBlock.BasicBlock.__init__(self)
        self.initialize()
    
    # Set attributes custom to this class
    def initialize(self):
        self.className = 'project.block.FinalDiagnosis.FinalDiagnosis'
        self.properties['Diagnosis'] = {'value':'','editable':'True'}
        self.inports = [{'name':'in','type':'truthvalue'}]
        self.outports= [{'name':'out','type':'text'}]
        
    # Return a dictionary describing how to draw an icon
    # in the palette and how to create a view from it.
    def getPrototype(self):
        proto = {}
        proto['iconPath']= "Block/icons/palette/final_diagnosis.png"
        proto['label']   = "Diagnosis"
        proto['tooltip']        = "Conclude a diagnosis based on input"
        proto['tabName']        = 'Analysis'
        proto['viewBackgroundColor'] = '0xC3E1F0'
        proto['viewLabel']      = "FinDiagnosis"
        proto['blockClass']     = self.getClassName()
        proto['blockStyle']     = 'square'
        proto['viewHeight']     = 100
        proto['inports']        = self.getInputPorts()
        proto['outports']       = self.getOutputPorts()
        proto['receiveEnabled']  = 'false'
        proto['transmitEnabled'] = 'true'
        proto['viewWidth']      = 120
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