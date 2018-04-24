'''
  Designer/client scope extension functions dealing with Application instances.
'''
import system

def delete(uuid):
    print "famProperties.delete: "+str(uuid)

def rename(uuid,oldName,newName):
    print "famProperties.rename: "+str(uuid)+" from:"+oldName+" to:"+newName
    
def renameInDatabase(uuid,oldName,newName,db):
    print "famProperties.renameInDatabase: "+str(uuid)+" from:"+oldName+" to:"+newName
    
def save(uuid,aux):
    print "famProperties,save: "+str(uuid)+" aux:"+str(aux)
  
def getAux(uuid,aux,db):
    print "famProperties.getAux: "+str(uuid)+ ", database is: ",db

def setAux(uuid,aux,db):
    print "famProperties.setAux: "+str(uuid)+",aux: "+str(aux)+ ", database is: ",db
