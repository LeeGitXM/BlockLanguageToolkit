# Copyright 2013 ILS Automation
#
# Methods associated with users and permissions
#
# Return true if the current user has the specified role.
# The check is made inside a source of users.
def hasRole(source,role):
	import system
	username = system.tag.getTagValue("[System]Client/User/UserName")
	user = system.user.getUser(source,username)
	roles = user.getRoles()
	
	result = False
	for r in roles:
		if r == role:
			result = True
			break
	return result