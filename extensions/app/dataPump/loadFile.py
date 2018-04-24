def loadFile(rootContainer, filename):
	import system, app
	
	print "Filename: " + filename
	
	i = 0
	data = []
	for line in open(filename):
		if (i == 0):
			tags = app.dataPump.tagTokens.tagTokens(line)
			cols = len(tags)
			print "Tags: ", tags
			print "*** There are %i columns ***" % (cols)
		else:
			data = app.dataPump.dataTokens.dataTokens(line, data, cols)			

		i = i + 1

#	print "At the end"
#	print "Data: ", data
	
	dataset = system.dataset.toDataSet(tags, data)
	rootContainer.getComponent("Table").data = dataset