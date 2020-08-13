'''
Created on Jun 14, 2019

@author: phass

Designer/client scope extension functions dealing with sqcDiagnosis instances.

These methods are usually called in Designer scope. However, we may be using either the
production or isolation databases. The Gateway makes this call when converting into
isolation mode.
'''
import system
log = system.util.getLogger("com.ils.diagToolkit.extensions")

def delete(uuid):
    log.infof("%s.delete()", __name__)

# The production an isolation databases need to be kept structurally in-synch.
# Apply these changes against both instances
def rename(uuid,oldName,newName):
    log.infof("%s.rename()", __name__)
    

def save(uuid,aux):
    '''
    This method IS called when they do a save from the Designer.  
    It should really insert a new record into the DB for a new application, but I don't have enough info here to
    do anything (and I don't know how to get it).  This isn't really a show stopper because the engineer needs to
    open the big configuration popup Swing dialog which will insert a record if it doesn't already exist.
    '''
    log.infof("%s.save()", __name__)

'''
NOTE: The UUID supplied is from the parent, a diagram. The database interactions
       are all based on a the block name which is  the data structure.

The aux data structure is a Python list of three dictionaries. These are:
properties, lists and maplists.
 
Fill the aux structure with values from the database.
'''
def getAux(uuid,aux,db):
    log.infof("In %s.getAux", __name__)

def setAux(uuid,aux,db):
    log.infof("In %s.setAux", __name__)
