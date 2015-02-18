package com.ils.blt.designer.applicationConfiguration;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.ListModel;

import com.inductiveautomation.ignition.client.util.gui.SlidingPane;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/** A controller for all the sliding panes that are involved in editing the application. */
public class ApplicationConfigurationController {
	public static java.awt.Color background = new java.awt.Color(238,238,238);
	private ApplicationConfigurationDialog dialog;
	private Application application;
	private SortedListModel outputListModel;
	
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
		dialog = diag;
//PH		tagBrowser = new TagBrowserPane(this);
		
		System.out.println("In ApplicationConfigurationController constructor");
		
		//TODO This would be a good time to go out and query the database...
		outputListModel = new SortedListModel();
		outputListModel.addAll(new String[] {"FC100.PV", "FC1001.PV", "Three", "FC1002.PV", "FC1003.PV", "FC1004.PV",
				"FC1005.PV", "FC1006.PV", "FC1007.PV", "FC1008.PV", "FC1009.PV", "FC1010.PV"});
		
		application = new Application();
		application.setName("My APP #1");
		application.setConsole("VFU Console1");
		application.setDescription("This is my first hard-coded application");
		
		// Create the sub-panes
		home = new HomePane(this, application);
		outputs = new OutputsPane(this, outputListModel);
		outputEditor = new OutputEditorPane(this);
		
		
		// sub-panes added according to the indexes above:
		System.out.println("In ApplicationConfigurationController constructor");
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