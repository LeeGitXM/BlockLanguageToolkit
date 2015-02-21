# Copyright 2015. ILS Automation. All rights reserved.
# Argument is the diagram path
import system.ils.blt.application as application
def setFactor(factor):
	application.setTimeFactor(float(factor))
