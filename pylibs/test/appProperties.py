'''
  Designer/client scope extension functions dealing with Application instances.
'''
import system

def delete(uuid):
    print "appProperties.delete: "+uuid

def rename(uuid,oldName,newName):
    print "appProperties.rename: "+uuid+" from:"+oldName+" to:"+newName
    
def renameInDatabase(uuid,oldName,newName,db):
    print "appProperties.renameInDatabase: "+uuid+" from:"+oldName+" to:"+newName
    
def save(uuid,aux):
    print "appProperties,save: "+uuid+" aux:"+aux
  
def getAux(uuid,aux,db):
    print "appProperties.getAux: "+uuid+ ", database is: ",db

def setAux(uuid,aux,db):
    print "appProperties.setAux: "+uuid+",aux: "+aux+ ", database is: ",db
