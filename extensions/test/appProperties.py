'''
  Designer/client scope extension functions dealing with Application instances.
'''
import system

def delete(uuid):
    print "appProperties.delete: "+str(uuid)

def rename(uuid,oldName,newName):
    print "appProperties.rename: "+str(uuid)+" from:"+oldName+" to:"+newName
    
def renameInDatabase(uuid,oldName,newName,db):
    print "appProperties.renameInDatabase: "+uuid+" from:"+oldName+" to:"+newName
    
def save(uuid,aux):
    print "appProperties,save: "+str(uuid)+" aux:"+str(aux)
  
def getAux(uuid,aux,db):
    print "appProperties.getAux: "+str(uuid)+ ", database is: ",db

def setAux(uuid,aux,db):
    print "appProperties.setAux: "+str(uuid)+",aux: "+str(aux)+ ", database is: ",db
