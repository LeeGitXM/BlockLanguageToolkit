# Copyright 2013 ILS Automation
#
# Enable or disable menuitems per the user role.
# The argument is a menubar component.
#
def enableMenuItemsPerUserRole(bar):
	import app
	#
	# For our purposes all we care about is engineer vs operator
	isEngineer = app.ils.user.hasRole('DiagnosticsToolkit','engineer')
	count = bar.getMenuCount()
	index = 0
	while index<count:
		menu = bar.getMenu(index)
		name = menu.getText()
		# print "Menu:",name
		if name== 'View':
			if isEngineer==False:
				app.diagnostics.menu.delete(menu,'Palette')
	
		index=index+1
		
# Given a menu, delete a menuitem of a given name
def delete(menu,childName):
	count = menu.getItemCount()
	index = 0
	while index<count:
		submenu = menu.getItem(index)
		name = submenu.getText()
		if name==childName:
			menu.remove(submenu)
			break;
		index=index+1