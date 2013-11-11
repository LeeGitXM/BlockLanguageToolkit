# Copyright 2013 ILS Automation
#
# Collection of useful methods for debugging and exploration
#
# Given a component, traverse the hierarchy of its parents
# until we find a JFrame. Return its menubar
def getMenuBar(window):
	parent = window
	bar = None
	while parent!=None:
		clazz = parent.getClass()
		name = clazz.getSimpleName()
		if name == 'JFrame':
			bar = parent.getJMenuBar()
			break
		parent = parent.getParent()
	return bar