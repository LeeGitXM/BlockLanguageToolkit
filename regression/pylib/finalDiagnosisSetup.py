# Copyright 2015. ILS Automation. All rights reserved.
# Configure the database for different FianlDiagnosis configurations
# and calculation methods.
# Calculation methods are of the form:
#    xom.vistalon.diagToolkit.crxProductQuality.fd1_2_1
#
global params, sqlparams, dict, pyResults
from ils.diagToolkit.finalDiagnosis import postDiagnosisEntry

def test01():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)
				
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_1', 'FD1_1_1', 'FD_UUID','DIAGRAM_UUID')

def test02():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)

	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

def test03a():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)

	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_3', 'FD_UUID','DIAGRAM_UUID')
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

def test03b():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_3', 'FD_UUID','DIAGRAM_UUID')

def test04():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_2', 'FD_UUID','DIAGRAM_UUID')
	
def test05():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, feedbackMethod='Most Negative')
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5, feedbackMethod='Most Negative')
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3, feedbackMethod='Most Negative')
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_2', 'FD_UUID','DIAGRAM_UUID')
	
def test06():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, feedbackMethod='Average')
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5, feedbackMethod='Average')
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3, feedbackMethod='Average')
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_2', 'FD_UUID','DIAGRAM_UUID')
	
def test07():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, feedbackMethod='Simple Sum')
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5, feedbackMethod='Simple Sum')
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3, feedbackMethod='Simple Sum')
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_2', 'FD_UUID','DIAGRAM_UUID')

def test08():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, incrementalOutput=False)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5, incrementalOutput=False)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3, incrementalOutput=False)
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')
	
def test09():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, mostPositiveIncrement=10.0)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)

	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

def test10():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, incrementalOutput=False, setpointHighLimit=15.0)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5, incrementalOutput=False)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3, incrementalOutput=False)
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

def test11a():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, minimumIncrement=20.0)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)
	
	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_1', 'FD1_1_1', 'FD_UUID','DIAGRAM_UUID')

def test11b():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, minimumIncrement=40.0)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)

	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

def test12a():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Implement")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, mostPositiveIncrement=10.0)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)

	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

def test12b():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Advise")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, mostPositiveIncrement=10.0)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)

	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

def test12c():
	system.tag.write("[XOM]Configuration/DiagnosticToolkit/vectorClampMode", "Disabled")
	# Set up the Application/family/final diagnosis
	appId=insertApp1()
	T1Id=insertQuantOutput(appId, 'Q1', '[XOM]Tags/T1', 9.6, mostPositiveIncrement=10.0)
	T2Id=insertQuantOutput(appId, 'Q2', '[XOM]Tags/T2', 23.5)
	T3Id=insertQuantOutput(appId, 'Q3', '[XOM]Tags/T3', 46.3)
	insertApp1Families(appId,T1Id,T2Id,T3Id)

	# Insert a diagnosis Entry - This simulates the FD becoming True
	postDiagnosisEntry('APP1', 'Family1_2', 'FD1_2_1', 'FD_UUID','DIAGRAM_UUID')

# This is called from a button
def clear(rootContainer):
	import system, os

	table = rootContainer.getComponent("Table")
	ds = table.data
	# Clear the results column for every selected row
	for row in range(ds.rowCount):
		selected = ds.getValueAt(row, 'selected')
		if selected:
			ds = system.dataset.setValue(ds, row, 'result', '')
			table.data = ds

# This is called from a button
def run(rootContainer):
	import system, os
		
			#-------------------------------------------
def initializeDatabase():
	import system
	print "Initializing..."
	for SQL in ["delete from DtRecommendation", "delete from DtDiagnosisEntry", \
		"delete from DtRecommendationDefinition", "delete from DtQuantOutput",\
		"delete from DtFinalDiagnosis","delete from DtFamily", \
		"delete from DtApplication", "delete from DtConsole"]:
		print SQL	  
		system.db.runUpdateQuery(SQL)
	print "Done"
	print "---------------------"
	#-------------------------------------------
		
	path = rootContainer.getComponent("Path").text
	table = rootContainer.getComponent("Table")
	ds = table.data
	pds = system.dataset.toPyDataSet(ds)

	# Clear the results column for selected rows
	cnt = 0
	for row in range(ds.rowCount):
		selected = ds.getValueAt(row, 'selected')
		if selected:
			cnt = cnt + 1
		table.setValue(row, 'result', 'Running')
		
	functionName = ds.getValueAt(row, 'function')
	fileRoot = ds.getValueAt(row, 'fileRoot')
	description = ds.getValueAt(row, 'description')
																																																						# Start with a clean slate			
																																																						initializeDatabase()
																																																						
																																																					# Run a specific test
																																																						func = getattr(project.test.diagToolkit, functionName)
	print "Starting ", functionName, "..."
	func()
	print "   .. finished", functionName

	# Define the path to the results file in an O/S neutral way
	outputFilename = os.path.join(path,functionName + "-out.csv")
	goldFilename = os.path.join(path,functionName + "-gold.csv")

	# Fetch the results from the database
	fetchResults(outputFilename)

	# Compare the results of this run to the Master results
	compareResults(outputFilename, goldFilename, table, row)

	# If we get all of the way through, and there is nothing left to run, then stop the timer.
	print "Done"

# There are two sets of Data here, When I compare them I load them into a single datset.
# So make sure that the recommendation records have the same nimber of columns as the 
# QuantOutput dataset.
def fetchResults(filename):

#----------------
# Fetch Recommendations
def logRecommendations():
	
	SQL = "select F.Family, F.FamilyPriority, FD.FinalDiagnosis, FD.FinalDiagnosisPriority, DE.Status, "\
			" DE.RecommendationStatus, DE.TextRecommendation, QO.QuantOutput, QO.TagPath, R.Recommendation, "\
			" R.AutoRecommendation, R.ManualRecommendation "\
			" from DtFamily F, DtFinalDiagnosis FD, DtDiagnosisEntry DE, DtRecommendationDefinition RD, "\
			"      DtQuantOutput QO, DtRecommendation R"\
			" where F.FamilyId = FD.FamilyId "\
			"   and FD.FinalDiagnosisId = DE.FinalDiagnosisId "\
			"   and DE.DiagnosisEntryId = R.DiagnosisEntryId "\
			"   and FD.FinalDiagnosisId = RD.FinalDiagnosisId "\
			"   and RD.QuantOutputId = QO.QuantOutputId "\
			"   and RD.RecommendationDefinitionId = R.RecommendationDefinitionId "\
			" order by Family, FinalDiagnosis"
																																									
	pds = system.db.runQuery(SQL)
	print "   fetched ", len(pds), " recommendation..."

	header = 'Family,FamilyPriority,FinalDiagnosis,FinalDiagnosisPriority,Status,'\
			'RecommendationStatus,TextRecommendation,QuantOutput, TagPath,Recommendation,'\
			'AutoRecommendation,ManualRecommendation,A,B,C,D,E,F,G,H,I'

																																																						system.file.writeFile(filename, header, False)
	for record in pds:
		txt = "\n%s,%s,%s, %s,%s,%s, %s,%s,%s, %s,%s,%s,0,0,0,0,0,0,0,0,0" % \
			(record['Family'], str(record['FamilyPriority']), record['FinalDiagnosis'], \
			str(record['FinalDiagnosisPriority']), record['Status'], record['RecommendationStatus'], \ 
			record['TextRecommendation'], record['QuantOutput'], record['TagPath'],\
			str(record['Recommendation']), str(record['AutoRecommendation']), str(record['ManualRecommendation']))
		system.file.writeFile(filename, txt, True)
			
#----------------------------------------------------
# Fetch Quant Outputs
def logQuantOutputs():
	SQL = "select QuantOutput, TagPath, MostNegativeIncrement, MostPositiveIncrement, "\
		" MinimumIncrement, SetpointHighLimit, SetpointLowLimit, FeedbackMethod, "\
		" OutputLimitedStatus, OutputLimited, OutputPercent, IncrementalOutput, "\
		" FeedbackOutput, FeedbackOutputManual, FeedbackOutputConditioned, "\
		" DisplayedRecommendation, ManualOverride, Active, CurrentSetpoint,  "\
		" FinalSetpoint, DisplayedRecommendation"\
		" from DtQuantOutput "\
		" order by QuantOutput"

	pds = system.db.runQuery(SQL)
	print "   fetched ", len(pds), " QuantOutputs..."
																																																					header = "\nQuantOutput,TagPath,MostNegativeIncrement,MostPositiveIncrement,"\
		"MinimumIncrement,SetpointHighLimit,SetpointLowLimit,FeedbackMethod,"\
		"OutputLimitedStatus,OutputLimited,OutputPercent,IncrementalOutput,"\
		"FeedbackOutput,FeedbackOutputManual,FeedbackOutputConditioned,"\
		"DisplayedRecommendation,ManualOverride,Active,CurrentSetpoint,"\
		"FinalSetpoint,DisplayedRecommendation"
		system.file.writeFile(filename, header, True)

		for record in pds:
			txt = "\n%s,%s,%f, %f,%f,%f, %f,%s,%s, %s,%s,%s, %s,%s, %s,%s, %s,%s,%s,%s,%s" % \
			(record['QuantOutput'], record['TagPath'], record['MostNegativeIncrement'], \
			record['MostPositiveIncrement'], record['MinimumIncrement'], record['SetpointHighLimit'], \
			record['SetpointLowLimit'], record['FeedbackMethod'], record['OutputLimitedStatus'], \
			str(record['OutputLimited']), str(record['OutputPercent']), str(record['IncrementalOutput']), \
			str(record['FeedbackOutput']), str(record['FeedbackOutputManual']), \
			str(record['FeedbackOutputConditioned']), str(record['DisplayedRecommendation']), \
			str(record['ManualOverride']), str(record['Active']), str(record['CurrentSetpoint']), \
			str(record['FinalSetpoint']),str(record['DisplayedRecommendation']) )
			
			system.file.writeFile(filename, txt, True)

#----------------------------------------------------
# Fetch Diagnosis
def logDiagnosis():
	
	SQL = "select FD.FinalDiagnosis, DE.Status, DE.TextRecommendation, DE.RecommendationStatus, "\
		" DE.ManualMove, DE.ManualMoveValue, DE.RecommendationMultiplier "\
		" from DtFinalDiagnosis FD, DtDiagnosisEntry DE "\
		" where FD.FinalDiagnosisId = DE.FinalDiagnosisId"\
		" order by FD.FinalDiagnosis"

	pds = system.db.runQuery(SQL)
	print "   fetched ", len(pds), " Diagnosis..."

	header = "\nFinalDiagnosis,Status,TextRecommendation,RecommendationStatus, "\
			"ManualMove,ManualMoveValue,RecommendationMultiplier,A,B,C,D,E,F,G,H,I,J,K,L,M,N"
	system.file.writeFile(filename, header, True)

	for record in pds:
		txt = "\n%s,%s,%s,%s,%s,%s,%s,0,0,0,0,0,0,0,0,0,0,0,0,0,0" % \
			(record['FinalDiagnosis'], record['Status'], record['TextRecommendation'], \
			record['RecommendationStatus'], str(record['ManualMove']), str(record['ManualMoveValue']), \
			str(record['RecommendationMultiplier']))
				
		system.file.writeFile(filename, txt, True)
#---------------
		print "...fetching results..."
		logRecommendations()
		logQuantOutputs()
		logDiagnosis()


def compareResults(outputFilename, goldFilename, table, row):
	print "...analyzing the results..."

	# Check if the Gold file exists
	if not(system.file.fileExists(goldFilename)):
		print "  The gold file <%s> does not exist!" % (goldFilename)
		print "Complete ........................... FAILED"
		table.setValue(row, 'result', 'Failed')
		return
		
# Check if the output file exists
	if not(system.file.fileExists(outputFilename)):
		print "  The output file <%s> does not exist!" % (outputFilename)
		print "Complete ........................... FAILED"
		table.setValue(row, 'result', 'Failed')
		return

		# Check if the two files are identical
		result, explanation = project.test.diff.diff(outputFilename, goldFilename)
		
		if result:
			txt = 'Passed'
			print "Complete ........................... Passed"
		else:
		txt = 'Failed'
			print "Complete ........................... FAILED"
			
			# Try to update the status row of the table
			table.setValue(row, 'result', txt)
			
# Create everything required for the APP1 test
def insertApp1():
	console = 'VFU'
	grade = '28'
	application='APP1'

	vfuConsoleId=insertConsole(console)
	app1Id=insertApplication(application, vfuConsoleId)
	return app1Id

def insertApp1Families(appId,T1Id,T2Id,T3Id):
	family = 'Family1_1'
	familyPriority=5.4
	familyId=insertFamily(family, appId, familyPriority)

	finalDiagnosis = 'FD1_1_1'
	finalDiagnosisPriority=2.3
	calculationMethod='xom.vistalon.diagToolkit.crxProductQuality.fd1_1_1'
	textRecommendation = "Final Diagnosis 1.1.1"
	finalDiagnosisId=insertFinalDiagnosis(finalDiagnosis, familyId, finalDiagnosisPriority, calculationMethod, textRecommendation)
	insertRecommendationDefinition(finalDiagnosisId, T1Id)

	family = 'Family1_2'
	familyPriority=7.6
	familyId=insertFamily(family, appId, familyPriority)
	
	finalDiagnosis = 'FD1_2_1'
	finalDiagnosisPriority=4.5
	calculationMethod='xom.vistalon.diagToolkit.crxProductQuality.fd1_2_1'
	textRecommendation = "Final Diagnosis 1.2.1"
	finalDiagnosisId=insertFinalDiagnosis(finalDiagnosis, familyId, finalDiagnosisPriority, calculationMethod, textRecommendation)
	insertRecommendationDefinition(finalDiagnosisId, T1Id)
	insertRecommendationDefinition(finalDiagnosisId, T2Id)

	finalDiagnosis = 'FD1_2_2'
	finalDiagnosisPriority=4.5
	calculationMethod='xom.vistalon.diagToolkit.crxProductQuality.fd1_2_2'
	textRecommendation = "Final Diagnosis 1.2.2"
	finalDiagnosisId=insertFinalDiagnosis(finalDiagnosis, familyId, finalDiagnosisPriority, calculationMethod, textRecommendation)
	insertRecommendationDefinition(finalDiagnosisId, T2Id)
	insertRecommendationDefinition(finalDiagnosisId, T3Id)
	
	finalDiagnosis = 'FD1_2_3'
	finalDiagnosisPriority=9.8
	calculationMethod='xom.vistalon.diagToolkit.crxProductQuality.fd1_2_3'
	textRecommendation = "Final Diagnosis 1.2.3"
	finalDiagnosisId=insertFinalDiagnosis(finalDiagnosis, familyId, finalDiagnosisPriority, calculationMethod, textRecommendation)
	insertRecommendationDefinition(finalDiagnosisId, T1Id)
	insertRecommendationDefinition(finalDiagnosisId, T2Id)
	insertRecommendationDefinition(finalDiagnosisId, T3Id)

# Insert a Quant Output
def insertQuantOutput(appId, quantOutput, tagPath, tagValue, mostNegativeIncrement=-500.0, mostPositiveIncrement=500.0, minimumIncrement=0.0001,
	setpointHighLimit=1000.0, setpointLowLimit=-1000.0, feedbackMethod='Most Positive', incrementalOutput=True):
	SQL = "insert into DtQuantOutput (QuantOutput, ApplicationId, TagPath, MostNegativeIncrement, MostPositiveIncrement, MinimumIncrement, "\
			"SetpointHighLimit, SetpointLowLimit, FeedbackMethod, IncrementalOutput) values "\
			"('%s', %i, '%s', %f, %f, %f, %f, %f, '%s', '%s')" % (quantOutput, appId, tagPath, mostNegativeIncrement, mostPositiveIncrement, minimumIncrement,
			setpointHighLimit, setpointLowLimit, feedbackMethod, incrementalOutput)
	id = system.db.runUpdateQuery(SQL, getKey=True)
	print "Writing ", tagValue, " to ", tagPath
	system.tag.write(tagPath, tagValue, 60)
	return id
			
# Define a console
def insertConsole(console):
	SQL = "insert into DtConsole (console) values ('%s')" % (console)
	consoleId = system.db.runUpdateQuery(SQL, getKey=True)
	return consoleId
																
# Define an application
def insertApplication(application, consoleId):
	SQL = "insert into DtApplication (application, ConsoleId) values ('%s', %i)" % (application, consoleId)
	applicationId = system.db.runUpdateQuery(SQL, getKey=True)
	return applicationId

# Create all of the families in this Application
def insertFamily(familyName, applicationId, familyPriority):
	SQL = "insert into DtFamily (Family, ApplicationId, FamilyPriority) values ('%s', %i, %f)" % (familyName, applicationId, familyPriority)
	familyId = system.db.runUpdateQuery(SQL, getKey=True)
	return familyId

# Create a final diagnosis
def insertFinalDiagnosis(finalDiagnosis, familyId, finalDiagnosisPriority, calculationMethod, textRecommendation, postTextRecommendation=0, textRecommendationCallback='bar.py', refreshRate=300):
	SQL = "insert into DtFinalDiagnosis (FinalDiagnosis, FamilyId, FinalDiagnosisPriority, CalculationMethod, TextRecommendation, PostTextRecommendation, " \
			"TextRecommendationCallback, RefreshRate, Active) "\
					" values ('%s', %i, %f, '%s', '%s', %i, '%s', %i, 0)"\
							% (finalDiagnosis, familyId, finalDiagnosisPriority, calculationMethod, textRecommendation, postTextRecommendation, textRecommendationCallback, refreshRate)
								finalDiagnosisId = system.db.runUpdateQuery(SQL, getKey=True)
	return finalDiagnosisId
										
# Create the recommendationDefinitions
def insertRecommendationDefinition(finalDiagnosisId, quantOutputId):
	SQL = "insert into DtRecommendationDefinition (FinalDiagnosisId, QuantOutputId) "\
		" values (%i, %i)" % (finalDiagnosisId, quantOutputId)
	system.db.runUpdateQuery(SQL)
