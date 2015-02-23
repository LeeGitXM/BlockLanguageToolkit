package com.ils.blt.designer.applicationConfiguration;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.ListModel;

import org.python.core.PyList;

import com.inductiveautomation.ignition.client.util.gui.SlidingPane;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/** A controller for all the sliding panes that are involved in editing the application. */
public class ApplicationConfigurationController {
	public static java.awt.Color background = new java.awt.Color(238,238,238);
	private Application application;
	private ApplicationConfigurationDialog dialog;
	private SortedListModel outputListModel;
	private Map<String,Object> properties;
	protected final LoggerEx log;
	
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
	
	public ApplicationConfigurationController(ApplicationConfigurationDialog diag) {
		this.log = diag.log;
		dialog = diag;
		log.infof("In ApplicationConfigurationController constructor for %s...", dialog.getApplication().getName() );
		
		properties=dialog.properties;
		
		initializeApplication();
		
//PH		tagBrowser = new TagBrowserPane(this);
		
//		application.setConsole((String) properties.get("Console"));
//		System.out.printf("Application Console: %s %n", application.getConsole());
		
		
		// This would be a good time to go out and query the database...
		System.out.println("Properties: " + properties);
		
		outputListModel = new SortedListModel();
		outputListModel.addAll(new String[] {"FC100.PV", "FC1001.PV", "Three", "FC1002.PV", "FC1003.PV", "FC1004.PV",
				"FC1005.PV", "FC1006.PV", "FC1007.PV", "FC1008.PV", "FC1009.PV", "FC1010.PV"});
		
		application = new Application();
		application.setName(dialog.getApplication().getName());
		application.setDescription((String) properties.get("Description"));
		application.setConsole((String) properties.get("Console"));
		application.setConsoles((ArrayList<String>) properties.get("Consoles"));
		application.setQueue((String) properties.get("MessageQueue"));
		application.setQueues((ArrayList<String>) properties.get("MessageQueues"));
		
		// Create the sub-panes
		home = new HomePane(this, application);
		outputs = new OutputsPane(this, outputListModel);
		outputEditor = new OutputEditorPane(this);

		// sub-panes added according to the indexes above:
		slidingPane.add(home);
		slidingPane.add(outputs);
		slidingPane.add(outputEditor);
		slidingPane.add(new JPanel());  // a blank pane
		slideTo(HOME);
		
		// hook into the property editor's string edit action so that
		// we can do our own thing:
/*PH	
		editor.getPropertyEditor().setStringEditListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {doStringEdit();}
		});
*/
		log.infof("   ...leaving ApplicationConfigurationController constructor!");
	}
	
	private void initializeApplication(){
		log.infof("   ...initializing the Application data model...");
		String post = (String) properties.get("Post");
		System.out.printf("Application Post: %s %n", post);
		
		// Process the list of outputs (A list of dictionaries)
		List<?> outputList = (List<?>) properties.get("QuantOutputs");
		if( outputList!=null ) {
			for(Object output : outputList) {
				System.out.println("Output: " + output);
			}
		}
		
		// Process the list of consoles
		List<?> consoleList = (List<?>) properties.get("Consoles");
		if( consoleList!=null ) {
			for(Object console : consoleList) {
				System.out.println("Console: " + console);
			}
		}

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
		clearOutputListModel();
		addOutputs(newValue);
	}
	
	public void clearOutputListModel(){
		outputListModel.clear();
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
		System.out.println("In ApplicationConfigurationController doOK");
		//TODO Now would be a really good time to save things to the database
		dialog.dispose();
	}

	public void doCancel(){
		System.out.println("In ApplicationConfigurationController doCancel");
		dialog.dispose();
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