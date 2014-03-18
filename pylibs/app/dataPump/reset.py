def hardReset():
	import system, string, time

	system.tag.writeToTag("[default]DataPump/simulationState", "Idle")
	system.tag.writeToTag("[default]DataPump/command", "Abort")
	system.tag.writeToTag("[default]DataPump/lineNumber", -1)