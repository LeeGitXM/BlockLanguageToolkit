'''
  Designer/client scope extension functions dealing with Application instances.
'''
import system

def delete(uuid):
    print "fdProperties.delete: "+uuid

def rename(uuid,oldName,newName):
    print "fdProperties.rename: "+uuid+" from:"+oldName+" to:"+newName
    
def renameInDatabase(uuid,oldName,newName,db):
    print "fdProperties.renameInDatabase: "+uuid+" from:"+oldName+" to:"+newName
    
def save(uuid,aux):
    print "fdProperties,save: "+uuid+" aux:"+aux
  
def getAux(uuid,aux,db):
    print "fdProperties.getAux: "+uuid+ ", database is: ",db

def setAux(uuid,aux,db):
    print "fdProperties.setAux: "+uuid+",aux: "+aux+ ", database is: ",db
