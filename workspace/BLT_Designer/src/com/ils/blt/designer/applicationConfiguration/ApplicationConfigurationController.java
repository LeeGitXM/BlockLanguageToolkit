package com.ils.blt.designer.applicationConfiguration;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.common.serializable.SerializableAuxiliaryData;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/** A controller for all the sliding panes that are involved in editing the application. */
public class ApplicationConfigurationController {	
	public static java.awt.Color background = new java.awt.Color(238,238,238);
	private final SerializableAuxiliaryData model;           // Data container operated on by panels
	private final SerializableApplication application;       // The application that we update
	protected final DesignerContext context;
	private final ApplicationConfigurationDialog dialog;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private final SortedListModel outputListModel;
	private Map<String,String> outputMap;
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
		this.model = new SerializableAuxiliaryData();
		this.outputListModel = new SortedListModel();
		
		initializeModel();
		log.infof("   ...leaving ApplicationConfigurationController constructor!");
	}
	
	// Create an empty model, then initialize from the database ...
	private void initializeModel(){
		log.infof("   ...initializing the Application data model...");
		
		// Fetch data from the database and store in the model
		model.setProperties(new HashMap<String,String>());
		model.setLists(new HashMap<>());
		model.setMapLists(new HashMap<>());
		model.getProperties().put("Name", application.getName());   // Use as a key when fetching
		extensionManager.runScript(context.getScriptManager(),ScriptConstants.APPLICATION_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
				this.application.getId().toString(),model);
		
		// If trace is enabled, then dump contents of the database query.
		if( log.isTraceEnabled() ) {
			Map<String,String> properties = application.getAuxiliaryData().getProperties();
			for (String key:properties.keySet()) {
				log.tracef("Properties: key = %s, value = %s", key, properties.get(key));
			}
			Map<String,List<String>> lists = application.getAuxiliaryData().getLists();
			for (String key:lists.keySet()) {
				List<String> list = lists.get(key);
				for(String val:list) {
					log.tracef("Lists: key = %s, value = %s", key, val);
				}
			}
			 Map<String,List<Map<String,String>>> maplists = application.getAuxiliaryData().getMapLists();
			for (String key:maplists.keySet()) {
				List<Map<String,String>> maplist = maplists.get(key);
				for (Map<String,String> map:maplist) {
					for (String prop:map.keySet()) {
						log.tracef("MapList(%s): name = %s, value = %s", key, prop, map.get(prop));
					}
				}
			}
		}
		
		/*
		appDataContainer.setName(application.getName());
		appDataContainer.setDescription(aux.getProperties().get("Description"));
		appDataContainer.setPost(aux.getProperties().get("Post"));
		appDataContainer.setPosts(aux.getLists().get("Posts"));
		appDataContainer.setQueue(aux.getProperties().get("MessageQueue"));
		appDataContainer.setQueues(aux.getLists().get("MessageQueues"));
		appDataContainer.setGroupRampMethod(aux.getProperties().get("GroupRampMethod"));
		appDataContainer.setGroupRampMethods(aux.getLists().get("GroupRampMethods"));
		appDataContainer.setUnit(aux.getProperties().get("Unit"));
		appDataContainer.setUnits(aux.getLists().get("Units"));
		appDataContainer.setFeedbackMethods(aux.getLists().get("FeedbackMethods"));
		*/
	
		// Build the list of output names that goes into the list widget
		buildOutputListModel();
	}
	public SerializableApplication getApplication() { return this.application; }
	public SerializableAuxiliaryData getModel() { return this.model; }
	public SortedListModel getOutputListModel() { return this.outputListModel; }
	
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
		log.tracef("In buildOutputListModel()");
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
	
	// Convert the Application object to the property dictionary
	private void save(){
		log.infof("In ApplicationConfigurationController:save()");
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
	
	/*
	
	public void setContext(DesignerContext context) {
//PH		tagBrowser.setContext(context);
	}
	
	
	
	
	public SlidingPane getSlidingPane() {
		return slidingPane;
	}

	public HomePane getHome() {
		return home;
	}

	public OutputsPane getOutputs() {
		return outputs;
	}
	
	public void addOutputs(ListModel newValue){
		fillListModel(outputListModel, newValue);
	}	
	
	public void addOutputs(Object newValue[]){
		fillListModel(outputListModel, newValue);
	}
	
	public void setOutputs(ListModel newValue){
		System.out.println("In setOutputs()");
		clearOutputListModel();
		addOutputs(newValue);
	}
	



	
	private void fillListModel(SortedListModel model, ListModel newValues){
		int size = newValues.getSize();
		for (int i=0; i<size; i++) {
			model.add(newValues.getElementAt(i));
		}
	}
	
	private void fillListModel(SortedListModel model, Object newValues[]){
		model.addAll(newValues);
	}


	/*
	public static void main(String[] args) {
		ApplicationConfigurationController controller = new ApplicationConfigurationController();		
		javax.swing.JFrame frame = new javax.swing.JFrame();
		frame.setContentPane(controller.getSlidingPane());
		frame.setSize(300,200);
		frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);
		frame.setVisible(true);
	}
	*/
}