package com.ils.blt.designer.applicationConfiguration;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.ListModel;

import com.inductiveautomation.ignition.client.util.gui.SlidingPane;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/** A controller for all the sliding panes that are involved in editing the application. */
public class ApplicationConfigurationController {
	public static java.awt.Color background = new java.awt.Color(238,238,238);
	private Application application;
	private ApplicationConfigurationDialog dialog;
	private SortedListModel outputListModel;
	private Map<String,Object> properties;
	private Map<String,Object> outputMap;
	protected final LoggerEx log;
	private double mostNegativeIncrement;
	
	interface EditorPane {
		/** show yourself, after doing any necessary preparation. */
		public void activate();
	}
	
	private SlidingPane slidingPane = new SlidingPane();
	
	// indices for the sub-panes:
	static final int HOME = 0;
	static final int OUTPUTS = 1;
	static final int EDITOR = 2;
	
	// The sub-panes:
	private HomePane home;
	private OutputsPane outputs;
	private OutputEditorPane outputEditor;
	
	// The constructor
	public ApplicationConfigurationController(ApplicationConfigurationDialog diag) {
		this.log = diag.log;
		dialog = diag;
		log.infof("In ApplicationConfigurationController constructor for %s...", dialog.getApplication().getName() );
		
		properties=dialog.properties;
		
		application = new Application();
		initializeApplication();
		
//PH		tagBrowser = new TagBrowserPane(this);
		
		// Create the sub-panes
		home = new HomePane(this, application);
		outputs = new OutputsPane(this, application, outputListModel);
		outputEditor = new OutputEditorPane(this, application, outputListModel);

		// sub-panes added according to the indexes above:
		slidingPane.add(home);
		slidingPane.add(outputs);
		slidingPane.add(outputEditor);
		slidingPane.add(new JPanel());  // a blank pane
		slideTo(HOME);

		log.infof("   ...leaving ApplicationConfigurationController constructor!");
	}
	
	private void initializeApplication(){
		log.infof("   ...initializing the Application data model...");
		
		// The data from the database has already been fetched and is in the properties dictionary.
		// Transfer the data from the dictionary to the application object.
		log.tracef("Properties: " + properties);

		application.setName(dialog.getApplication().getName());
		application.setDescription((String) properties.get("Description"));
		application.setConsole((String) properties.get("Console"));
		application.setConsoles((ArrayList<String>) properties.get("Consoles"));
		application.setQueue((String) properties.get("MessageQueue"));
		application.setQueues((ArrayList<String>) properties.get("MessageQueues"));
		application.setGroupRampMethod((String) properties.get("GroupRampMethod"));
		application.setPost((String) properties.get("Post"));
		application.setUnit((String) properties.get("Unit"));
		
		outputListModel = new SortedListModel();
		
		// Process the list of outputs (A list of dictionaries)
		List<?> outputList = (List<?>) properties.get("QuantOutputs");
		log.tracef("OutputList: " + outputList);
		if( outputList!=null ) {
			for(Object obj : outputList) {
				if (obj instanceof Hashtable){ 
					outputMap = (Map<String,Object>) obj;
					application.addQuantOutput(outputMap);
				}
			}
		}
		
		// Build the list of output names that goes into the list widget
		buildOutputListModel();
		
		// Process the list of consoles
		List<?> consoleList = (List<?>) properties.get("Consoles");
	}
	
	public void setContext(DesignerContext context) {
//PH		tagBrowser.setContext(context);
	}
	
	
	public void slideTo(int index) {
		slidingPane.setSelectedPane(index);	
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

	public OutputEditorPane getOutputEditor() {
		return outputEditor;
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
	
	public void clearOutputListModel(){
		outputListModel.clear();
	}
	
	// Build the outputListModel from the list of QuantOutput maps
	private void buildOutputListModel(){
		log.tracef("In buildOutputListModel()");
		List<?> outputList = (List<?>) application.getOutputs();
		log.tracef("OutputList: " + outputList);
		if( outputList!=null ) {
			for(Object obj : outputList) {
				if (obj instanceof Hashtable){ 
					outputMap = (Map<String,Object>) obj;
					log.tracef("Adding " + outputMap + " a " + obj.getClass().getName());
					outputListModel.add(outputMap.get("QuantOutput"));
				}
			}
		}
	}
		
	// This is generally called after an output has been edited and the output list widget 
	// needs to be refreshed
	public void refreshOutputs(){
		clearOutputListModel();
		buildOutputListModel();
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
	
	public void doOK(){
		save();
		dialog.save();
		dialog.dispose();
	}

	public void doCancel(){
		dialog.dispose();
	}

	// Convert the Application object to the property dictionary
	public void save(){
		properties.put("Console", application.getConsole());
		properties.put("Description", application.getDescription());
		properties.put("MessageQueue", application.getQueue());
		properties.put("GroupRampMethod", application.getGroupRampMethod());
		properties.put("Post", application.getPost());
		properties.put("Unit", application.getUnit());
		properties.put("QuantOutputs", application.getOutputs());
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