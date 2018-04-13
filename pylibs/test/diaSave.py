'''
  Copyright 2018. ILS Automation. All rights reserved.
  Gateway scope extension function that is called whenever a diagram is saved.
  We use this as an opportunity to synchronize our database perception of
  what exists in the diagnostic toolkit world.
'''
import system

def delete(diagramId):
    print "diaSave.delete: "+diagramId

def save(diagramId):
    print "diaSave.save: "+diagramId
