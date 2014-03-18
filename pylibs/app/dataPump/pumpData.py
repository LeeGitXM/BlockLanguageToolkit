# This is called asynchronously so that it will run in its own thread, therefore
# Ignition can't (won't) pass any arguments.  We can assume that immediately after it 
# is called that the simulator window is still open, but we can't assume it stays open
# for the duration of the simulation.

def pumpData():
	import system, string, time

	# Get the simulation window and such the data out of the table
	window = system.gui.getWindow("Data Pump/Data Pump")
	table = window.rootContainer.getComponent("Data Pump Container").getComponent("Table")
	ds = table.data
	pds = system.dataset.toPyDataSet(ds)
	
	tagPath = window.rootContainer.getComponent("Data Pump Container").getComponent("TagPrefixField").text
	dataPumpPath = "[default]DataPump/"

	system.tag.writeToTag(dataPumpPath + "command", "Run")
	system.tag.writeToTag(dataPumpPath + "simulationState", "Running")
	
	# We have to give these writes a chance to get there
	time.sleep(1)	
	
	i = 0
	for row in pds:
		print "Concluding at ", time.strftime("%H:%M:%S")
		command = system.tag.getTagValue(dataPumpPath + 'command')
		if command == "Abort":
			break

		system.tag.writeToTag(dataPumpPath + "lineNumber", i)
		j = 0
		for val in row:
			tagname = ds.getColumnName(j)
#			print tagname, " = ", val
			fullTagPath = tagPath + tagname
			status = system.tag.write(fullTagPath, val)
			print "Tag: %s, Value: %s, Status: %s" % (fullTagPath, str(val), str(status) )
			j = j + 1
		
		timeDelay = window.rootContainer.getComponent("Data Pump Container").getComponent("TimeDelaySlider").value	
		print "Waiting for ", timeDelay, " seconds."
		time.sleep(timeDelay)	
		
		i = i + 1
	
	print "Done Pumping!"
	system.tag.writeToTag(dataPumpPath + "simulationState", "Idle")