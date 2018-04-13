'''
  Designer/client scope extension functions dealing with Application instances.
'''
import system

def delete(uuid):
    print "famProperties.delete: "+uuid

def rename(uuid,oldName,newName):
    print "famProperties.rename: "+uuid+" from:"+oldName+" to:"+newName
    
def renameInDatabase(uuid,oldName,newName,db):
    print "famProperties.renameInDatabase: "+uuid+" from:"+oldName+" to:"+newName
    
def save(uuid,aux):
    print "famProperties,save: "+uuid+" aux:"+aux
  
def getAux(uuid,aux,db):
    print "famProperties.getAux: "+uuid+ ", database is: ",db

def setAux(uuid,aux,db):
    print "famProperties.setAux: "+uuid+",aux: "+aux+ ", database is: ",db
