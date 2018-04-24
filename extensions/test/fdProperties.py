'''
  Designer/client scope extension functions dealing with Application instances.
'''
import system

def delete(uuid):
    print "fdProperties.delete: "+str(uuid)

def rename(uuid,oldName,newName):
    print "fdProperties.rename: "+str(uuid)+" from:"+oldName+" to:"+newName
    
def renameInDatabase(uuid,oldName,newName,db):
    print "fdProperties.renameInDatabase: "+str(uuid)+" from:"+oldName+" to:"+newName
    
def save(uuid,aux):
    print "fdProperties,save: "+str(uuid)+" aux:"+str(aux)
  
def getAux(uuid,aux,db):
    print "fdProperties.getAux: "+str(uuid)+ ", database is: ",db

def setAux(uuid,aux,db):
    print "fdProperties.setAux: "+str(uuid)+",aux: "+str(aux)+ ", database is: ",db
