'''
  Designer/client scope extension functions dealing with FinalDiagnosis instances.
'''
import system
log = system.util.getLogger("com.ils.diagToolkit.extensions")

'''
Gateway Scope Functions
'''

def delete(finalDiagnosisUUID):
    '''    Even though a delete is initiated from Designer scope, this runs in gateway scope!  '''
    log.infof("%s.delete()", __name__)
    

def save(uuid, aux):
    '''
    This method IS called when they do a save from the Designer.  
    It should really insert a new record into the DB for a new application, but I don't have enough info here to
    do anything (and I don't know how to get it).  This isn't really a show stopper because the engineer needs to
    open the big configuration popup Swing dialog which will insert a record if it doesn't already exist.
    '''
    log.infof("%s.save(), doing nothing", __name__)


'''
Designer Scope Functions
'''

def rename(uuid,oldName,newName):
    '''
    The production and isolation databases need to be kept structurally in-synch.
    Apply these changes against both instances
    '''
    log.infof("%s.rename()", __name__)


def getAux(uuid, aux, db):
    '''
    NOTE: The UUID supplied is from the parent, a diagram. The database interactions
           are all based on a the block name which is  the data structure.
    
    The aux data structure is a Python list of three dictionaries. These are:
    properties, lists and maplists.
     
    Fill the aux structure with values from the database.
    '''
    log.infof("%s.getAux()", __name__)

def setAux(uuid,aux,db):
    log.infof("%s.setAux with %s", __name__, db)
