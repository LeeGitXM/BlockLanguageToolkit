'''
  Designer/client scope extension functions dealing with Family instances.
'''
import system
log = system.util.getLogger("com.ils.diagToolkit.extensions")

'''
These run in Gateway scope
'''
def delete(familyUUID):
    '''
    This is called when the user deletes an family from the designer.  I hope that it already called the delete method for the diagrams in the family
    from the bottom up so that we don't hit a FK constraint here.
    
    I'd like to use the application name, which is guarenteed to be unique by the database, but I think that the gateway has already deleted the application so the getApplicationName()
    call fails - at least that is the only explanation I can come up with!  So instead use the UUID to delete the application.
    '''
    log.infof("%s.delete() with family uuid: %s", __name__, familyUUID)
    
# The production an isolation databases need to be kept structurally in-synch.
# Apply these changes against both instances.
def rename(uuid,oldName,newName):
    
    def renameInDatabase(uuid,oldName,newName,db):
        SQL = "UPDATE DtFamily SET FamilyName= '%s' WHERE FamilyName = '%s'" % (newName,oldName)
        system.db.runUpdateQuery(SQL,db)
    
    log.infof("In %s.rename()", __name__)
    db = applicationRequestHandler.getProductionDatabase()
    renameInDatabase(uuid,oldName,newName,db)
    db = applicationRequestHandler.getIsolationDatabase()
    renameInDatabase(uuid,oldName,newName,db)
    

   
def save(familyUUID, aux):
    '''
    This method IS called when they do a save from the Designer. 
    Although this is initiated from Designer - this runs in Gateway scope!
     
    It should really insert a new record into the DB for a new application, but I don't have enough info here to
    do anything (and I don't know how to get it).  This isn't really a show stopper because the engineer needs to
    open the big configuration popup Swing dialog which will insert a record if it doesn't already exist.
    '''
    log.infof("%s.save()", __name__)
    
'''
These methods are  called in Designer scope. However, we may be using either the
production or isolation databases. The Gateway makes this call when converting into
 isolation mode. 
'''

# The aux data structure is a Python list of three dictionaries. These are:
# properties, lists and maplists. Of these, the family only uses properties.
# 
# Fill the aux structure with values from the database
def getAux(uuid,aux,db):
    log.infof("%s.getAux()", __name__)

def setAux(uuid,aux,db):
    log.infof("%s.setAux()", __name__)
