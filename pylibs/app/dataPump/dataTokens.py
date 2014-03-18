# This takes a comma delimited text string and validates each of the tokens as tags.
# If a token is a tag then the tagname is written to the array/sequence

def dataTokens(txt, data, maxColumns):
	import system, string

	# Ignore lines that begin with a # because it is a comment
	if (txt[0] != "#"):
		tokens = txt.split(',')
		record = []
		i = 0
		for token in tokens:
			if (token != "" and token != "\n" and token!="\r\n" and i <= maxColumns):
				token = string.replace(token,'\r\n', '')
				record.append(token)
			i = i + 1

		# If the number of tokens doesn't match the number of columns then the record is of no use.
		if len(record) == maxColumns:
			data.append(record)

	return data