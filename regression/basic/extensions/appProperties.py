'''
  Designer/client scope extension functions dealing with Application instances.
'''
import system
log = system.util.getLogger("com.ils.diagToolkit.extensions")
def delete(applicationUUID):
    '''
    This is called when the user deletes an application from the designer.  I hope that it already called the delete method for the diagrams and families in the application
    from the bottom up so that we don't hit a FK constraint here.
    
    I'd like to use the application name, which is guarenteed to be unique by the database, but I think that the gateway has already deleted the application so the getApplicationName()
    call fails - at least that is the only explanation I can come up with!  So instead use the UUID to delete the application.
    '''
    log.infof("%s.delete()", __name__)
    

def rename(uuid,oldName,newName):
    '''
    TODO: It appears that this is NOT called when I rename an application.  
    I think I can handle this case in the save method, especially if I can figure out how to get the name.
    '''
    log.infof("%s.rename()", __name__)
    
def save(applicationUUID, aux):
    '''
    This method IS called when they do a save from the Designer.  
    It should really insert a new record into the DB for a new application, but I don't have enough info here to
    do anything (and I don't know how to get it).  This isn't really a show stopper because the engineer needs to
    open the big configuration popup Swing dialog which will insert a record if it doesn't already exist.
    '''
    log.infof("%s.save()", __name__)
    
'''
These methods are usually called in Designer scope. However, we may be using either the
production or isolation databases. The Gateway makes this call when converting into
isolation mode. 

The aux data structure is a Python list of three dictionaries. These are:
properties, lists and maplists.
 
Fill the aux structure with values from the database
The caller must supply either the production or isolation database name
'''
def getAux(uuid,aux,db):
    log.infof("%s.getAux()", __name__)

# Set values in the database from contents of the aux container
# The caller must supply either the production or isolation database name
def setAux(uuid, aux, db):
    log.infof("%s.setAux()", __name__)
