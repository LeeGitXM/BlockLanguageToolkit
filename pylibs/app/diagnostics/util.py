#  Copyright 2013 ILS Automation
#
# Utility functions for dealing with Python classes for
# the Diagnostics block language. Each of these modules
# expects a variable shared with the caller named: share.
# This is both an input and an output.
#
def classLauncher():
	import system.ils.diag.report as report
	repo = report.getRepository
# Return true if there is an object instance corresponding
# to the specified key. We expect the key format to be:
#   projectId:tree-path:blockId
def instanceExists():
	import system.ils.diag.report as report
	dict = shared
	key  = shared.get('input')
	repo = report.getRepository()
	code = repo.get(key)
	if code==None:
		shared['output'] = 'false'
	else:
		shared['output'] = 'true'

