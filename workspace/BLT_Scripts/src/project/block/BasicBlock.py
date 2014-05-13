#  Copyright 2014 ILS Automation
#
# An abstract base class for all executable blocks written
# in tbe Python style.
#
class BasicBlock():
    # Properties are a dictionary of attributes keyed by name
    # - property values are cached in the Gateway java proxy
    # - values should not be changed except by call from the proxy
    properties = {}
    # Input ports are named stubs for incoming connections
    # Ports have properties: name,connectionType
    inports = []
    # Outports are named stubs for outgoing connections
    # Connection types are: data,information,signal,truthvalue
    outports = []
    # Each block has a unique id that matches its proxy object
    uuid = ""
    parentuuid=""

    
    def __init__(self):
        self.initialize()
    
    # Set the default properties and connection ports
    # For the super class there are none
    def initialize(self):    
        self.className = 'project.block.BasicBlock.BasicBlock'

    # Return the class name. This is a fully qualified
    # path, including the module path 
    def getClassName(self):
        return self.className
            
    # Return a list of property names. 
    def getPropertyNames(self):
        return self.properties.keys()
        
    # Return a specified property. The property
    # is a dictionary guaranteed to have a "value". 
    def getProperty(self,name):
        return self.properties.get(name,{})
        
    # Return all properties
    def getProperties(self):
        return self.properties
    # Return a list of all input ports
    def getInputPorts(self):
        return self.inports
    # Return a list of all output ports
    def getOutputPorts(self):
        return self.outports
    # Set the block's UUID (a string)
    def setUUID(self,id):
        self.uuid = id
    # Set the block's parent's UUID (a string)
    def setParentUUID(self,id):
        self.parentuuid = id
    # Replace or add a property
    # We expect the dictionary to have the proper attributes
    def setProperty(self,name,dict):
        self.properties[name] = dict
        
    # Called when a value has arrived on one of our input ports
    # By default, we do nothing
    def setValue(self,value,quality,port):
        self.value = value
        self.quality = quality
    
    # Evaluate the block. This is called on expiration
    # of a timer. This default implementation
    # does nothing.
    def evaluate(self):
        pass