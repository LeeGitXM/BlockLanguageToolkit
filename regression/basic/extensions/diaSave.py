'''
  Copyright 2016. ILS Automation. All rights reserved.
  Gateway scope extension function that is called whenever a diagram is saved.
  We use this as an opportunity to synchronize our database perception of
  what exists in the Symbolic AI world.
'''
import system
log = system.util.getLogger("com.ils.diagToolkit.extensions")

def delete(diagramUUID):
    log.infof("%s.delete() - %s", __name__, diagramUUID)

def save(diagramUUID, aux):
    log.infof("%s.save() - %s", __name__, diagramUUID)
