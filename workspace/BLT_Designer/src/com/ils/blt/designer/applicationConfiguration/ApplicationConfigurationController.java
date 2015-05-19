package com.ils.blt.designer.applicationConfiguration;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.SortedListModel;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/** A controller for all the sliding panes that are involved in editing the application. */
public class ApplicationConfigurationController {	
	public static java.awt.Color background = new java.awt.Color(238,238,238);
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private final SerializableApplication application;       // The application that we update
	protected final DesignerContext context;
	private final ApplicationConfigurationDialog dialog;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private final SortedListModel<String> outputListModel;
	protected final LoggerEx log;
	
	interface EditorPane {
		/** show yourself, after doing any necessary preparation. */
		public void activate();
	}
	
	
	
	// The constructor
	public ApplicationConfigurationController(DesignerContext ctx,ApplicationConfigurationDialog dlg,SerializableApplication app) {
		this.context = ctx;
		this.dialog = dlg;
		this.log = dlg.getLog();
		this.application = app;    // The real thing
		this.model = new GeneralPurposeDataContainer();
		this.outputListModel = new SortedListModel<>();
		
		initializeModel();
		log.infof("   ...leaving ApplicationConfigurationController constructor!");
	}
	
	// Create an empty model, then initialize from the database ...
	private void initializeModel(){
		log.infof("   ...initializing the Application %s data model...",application.getName());
		
		// Fetch data from the database and store in the model
		model.setProperties(new HashMap<String,String>());
		model.setLists(new HashMap<>());
		model.setMapLists(new HashMap<>());
		model.getProperties().put("Name", application.getName());   // Use as a key when fetching
		try {
			extensionManager.runScript(context.getScriptManager(),ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				this.application.getId().toString(),model);
		
		// If trace is enabled, then dump contents of the database query.
		if( log.isTraceEnabled() ) model.dump();
	
		// Build the list of output names that goes into the list widget
		buildOutputListModel();
		
		}
		catch( Exception ex ) {
			log.errorf("ApplicationConfigurationController.initializeModel: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}
	}
	
	public SerializableApplication getApplication() { return this.application; }
	public GeneralPurposeDataContainer getModel() { return this.model; }
	public SortedListModel<String> getOutputListModel() { return this.outputListModel; }
	
	public void doOK(){
		save();
		dialog.dispose();
	}

	public void doCancel(){
		dialog.setCancelled(true);
		dialog.dispose();
	}
	
	
	// This is generally called after an output has been edited and the output list widget 
	// needs to be refreshed
	public void refreshOutputs(){
		clearOutputListModel();
		buildOutputListModel();
	}
	
	public void slideTo(int index) {
		dialog.slideTo(index);	
	}
	
	// Build the outputListModel from the list of QuantOutput maps
	private void buildOutputListModel(){
		List< Map<String,String> > outputList = model.getMapLists().get("QuantOutputs");
		log.tracef("OutputList: " + outputList);
		if( outputList!=null ) {
			for(Map<String,String> outMap : outputList) {
				log.tracef("Adding " + outMap + " a " + outMap.getClass().getName());
				outputListModel.add(outMap.get("QuantOutput"));
			}
		}
	}
	private void clearOutputListModel(){
		outputListModel.clear();
	}
	
	// Copy the Application auxiliary data back into the database
	private void save(){
		try {
			// Save values back to the database
			extensionManager.runScript(context.getScriptManager(), ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT, 
					this.application.getId().toString(),model);
			// Replace the aux data structure in our serializable application
			// NOTE: The Nav tree node that calls the dialog saves the application resource.
			application.setAuxiliaryData(model);
		}
		catch( Exception ex ) {
			log.errorf("ApplicationConfigurationController.save: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}
	}
	
}