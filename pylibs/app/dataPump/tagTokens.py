# This takes a comma delimited text string, does some formatting and adds it to tha tag list

def tagTokens(txt):
	import system, string
	
	tokens = txt.split(',')

	# The first token should be blamk because the first column is always the datetime
	tags = []
	
	for token in tokens:
		if (token != "" and token != "\n"):
			token = string.replace(token, '.', '/')
			token = string.replace(token, '\r\n', '')
			tags.append(token)
	
	return tags
	
		
	# Determine if the tag exists.  Try to read a value, if we can't read a value
	# then the tag probably does not exist so skip the tag by leaving a hole in the 
	# sequence
#	try:
#		val = system.tag.getTagValue(tagName)
#		tags.append(token)
#	except:
#		print "Tag <" + tagName + "> does not exist or is not healthy"
#		tags.append("")