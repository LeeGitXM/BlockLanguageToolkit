/**
 *   (c) 2015-2018  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import com.ils.blt.client.ClientScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.ui.DualListBox;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a dialog to configure the outputs available for a Final Diagnosis.
 * The constants contained herein are defined in designer.properties (I'm not sure how it knows that...)
 *                                                                  (*It's defined in ConfigurationDialog*)
 */
public class FinalDiagnosisConfiguration extends ConfigurationDialog {
	private static final long serialVersionUID = 7211480530910862375L;
	private static final String TAG = "FinalDiagnosisConfiguration";
	private final int DIALOG_HEIGHT = 700;
	private final int DIALOG_WIDTH = 600;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final ClientScriptExtensionManager extensionManager = ClientScriptExtensionManager.getInstance();
	private JPanel mainPanel = null;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	protected DualListBox dual;
	protected JTextField finalDiagnosisLabelField;
	protected JTextField calculationMethodField;
	protected JTextArea textRecommendationArea;
	protected JCheckBox postTextRecommendationCheckBox;
	protected JCheckBox showExplanationWithRecommendationCheckBox;
	protected JTextField priorityField;
	protected JTextField refreshRateField;
	protected JTextField postProcessingCallbackField;
	protected JCheckBox constantCheckBox;
	protected JCheckBox trapBox;
	protected JCheckBox manualMoveAllowedCheckBox;
	protected JTextArea explanationArea;
	protected JTextArea commentArea;
	protected static final Dimension EXPLANATIION_AREA_SIZE  = new Dimension(280,300);
	protected static final Dimension TEXT_RECOMMENDATION_AREA_SIZE  = new Dimension(280,300);
	protected static final Dimension COMMENT_AREA_SIZE  = new Dimension(280,300);

	public FinalDiagnosisConfiguration(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView view) {
		super(wksp.getContext());
		this.model = new GeneralPurposeDataContainer();

		this.diagram = diag;
		this.block = view;
		this.setTitle(rb.getString("FinalDiagnosisEditor.Title"));
		
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
        
        // This isn't the right place to set the location.  See DiagramWorkspace
//		this.setLocation(-500,-500);
        
	}
	/**
	 * The super class takes care of making a central tabbed pane --- but
	 * we don't want it. Simply put our mainPanel as the content pane.
	 * Here we add the tabs ...
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		retrieveAuxiliaryData();
		mainPanel = createMainPanel();
		contentPanel.add(mainPanel,BorderLayout.CENTER);
		setOKActions();
	}
	
	private JPanel createMainPanel() {	
		// The internal panel has two panes
		// - one for the dual list box, the other for the remaining attributes
		//setLayout(new BorderLayout());
		mainPanel = new JPanel();
		mainPanel.setLayout(new MigLayout("ins 2,fill","[][]","[][growprio 50,200:1000:2000][]"));
		
		addSeparator(mainPanel,"FinalDiagnosis.QuantOutputs");
		
		dual = new DualListBox();
		List<String> q1 = model.getLists().get("OutputsInUse");
		if( q1==null ) q1 = new ArrayList<>();
		dual.setDestinationElements(q1);
		// The outputs are ALL possibilities. Subtract 
		// those already being used.
		List<String> q0 = model.getLists().get("QuantOutputs");
		if( q0!=null ) {
			for( String inUse:q1) {
				q0.remove(inUse);
			}
		}
		else {
			q0 = new ArrayList<>();
		}
		dual.setSourceElements(q0);
		mainPanel.add(dual, "gapx 50 40,grow,wrap");
		mainPanel.add(createPropertiesPanel(),"growx,wrap");
		return mainPanel;
	}
	
	/**
	 * Read the database and fill the model
	 */
	private void retrieveAuxiliaryData() {
		// Fetch properties of the diagnosis associated with the database and not serialized.
		// Fetch from the database and store in the model
		model.setProperties(new HashMap<String,String>());
		model.setLists(new HashMap<>());
		model.setMapLists(new HashMap<>());
		model.getProperties().put("Name", block.getName());   // Use as a key when fetching
		try {
			String db = requestHandler.getDatabaseForUUID(diagram.getId().toString());
			extensionManager.runScript(context.getScriptManager(),block.getClassName(), ScriptConstants.PROPERTY_GET_SCRIPT, 
					diagram.getId().toString(),model,db);
		}
		catch( Exception ex ) {
			log.errorf(TAG+".retrieveAuxiliaryData: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}
	}
	
	private void setOKActions() {
		// The button panel is already added by the base class.
		okButton.setText(rb.getString("FinalDiagnosisEditor.Save"));
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
				dispose();
			}
		});
	}
	
	/**
	 * This panel holds the "simple" attributes of the block
	 * @return
	 */
	/**
	 * Create the main data pane as a grid 4 columns wide:
	 *     label | value | label | value
	 *     label | value -- span 3
	 */
	private JPanel createPropertiesPanel() {
		Map<String,String> properties = model.getProperties();
		JPanel panel = new JPanel();
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 2,gapy 1,gapx 5,fillx,filly";
		final String rowConstraints = "para [][][][growprio 100,48:72:96][growprio 100,48:72:96][growprio 100,48:72:96][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		panel.add(createLabel("FinalDiagnosis.Name"),"");
		nameField = createTextField("FinalDiagnosis.Name.Desc",block.getName());
		nameField.setEditable(false);
		panel.add(nameField,"spanx 3,growx,wrap");

		panel.add(createLabel("FinalDiagnosis.UUID"),"gaptop 2,aligny top");
		JTextField uuidField = createTextField("FinalDiagnosis.UUID.Desc",block.getId().toString());
		uuidField.setEditable(false);
		panel.add(uuidField,"spanx 3,growx,wrap");
		
		panel.add(createLabel("FinalDiagnosis.Label"),"gaptop 2,aligny top");
		String method = properties.get("FinalDiagnosisLabel");
		if( method==null) method="";
		finalDiagnosisLabelField = createTextField("FinalDiagnosis.Label.Desc",method);
		panel.add(finalDiagnosisLabelField,"spanx 3,growx,wrap");
		
		panel.add(createLabel("FinalDiagnosis.Comment"),"gaptop 2,aligny top");
		String comment = (String)properties.get("Comment");
		if( comment==null) comment="";
		commentArea = createTextArea("FinalDiagnosis.Comment.Desc",comment);
		JScrollPane commentScrollPane = new JScrollPane(commentArea);
		commentScrollPane.setPreferredSize(COMMENT_AREA_SIZE);
		panel.add(commentScrollPane,"spanx 3,growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.Explanation"),"gaptop 2,aligny top");
		String explanation = (String)properties.get("Explanation");
		if( explanation==null) explanation="";
		explanationArea = createTextArea("FinalDiagnosis.Explanation.Desc",explanation);
		JScrollPane explanationScrollPane = new JScrollPane(explanationArea);
		explanationScrollPane.setPreferredSize(EXPLANATIION_AREA_SIZE);
		panel.add(explanationScrollPane,"spanx 3,growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.TextRecommendation"),"gaptop 2,aligny top");
		String recommendation = (String)properties.get("TextRecommendation");
		if( recommendation==null) recommendation="";
		textRecommendationArea = createTextArea("FinalDiagnosis.TextRecommendation.Desc",recommendation);
		JScrollPane textRecommendationScrollPane = new JScrollPane(textRecommendationArea);
		textRecommendationScrollPane.setPreferredSize(TEXT_RECOMMENDATION_AREA_SIZE);
		panel.add(textRecommendationScrollPane,"spanx 3,growx,growy,wrap");
		
		panel.add(createLabel("FinalDiagnosis.CalcMethod"),"gaptop 2,aligny top");
		String calculationMethod = properties.get("CalculationMethod");
		if( calculationMethod==null) calculationMethod="";
		calculationMethodField = createTextField("FinalDiagnosis.CalcMethod.Desc",calculationMethod);
		panel.add(calculationMethodField,"spanx 3,growx,wrap");

		panel.add(createLabel("FinalDiagnosis.Priority"),"gaptop 2,aligny top");
		String priority = (String)properties.get("Priority");
		if( priority==null) priority="";
		priorityField = createTextField("FinalDiagnosis.Priority.Desc",priority);
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(priorityField,"span 2,wrap");
		
		panel.add(createLabel("FinalDiagnosis.RefreshRate"),"gaptop 2,aligny top");
		String rate = (String)properties.get("RefreshRate");
		if( rate==null) rate="";
		refreshRateField = createTextField("FinalDiagnosis.RefreshRate.Desc",rate);
		refreshRateField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(refreshRateField,"span 2,alignx left,wrap");
		
		panel.add(createLabel("FinalDiagnosis.PostProcessingCallback"),"gaptop 2,aligny top");
		method = (String)properties.get("PostProcessingCallback");
		if( method==null) method="";
		postProcessingCallbackField = createTextField("FinalDiagnosis.PostProcessingCallback.Desc",method);
		panel.add(postProcessingCallbackField,"span 3,growx,wrap");
		
		panel.add(createLabel("FinalDiagnosis.Constant"),"gaptop 2,aligny top");
		String constantValue = properties.get("Constant");
		if( constantValue==null) constantValue="0";
		constantCheckBox = createCheckBox("FinalDiagnosis.Constant.Desc",(constantValue.equalsIgnoreCase("1")));
		//panel.add(constantCheckBox,"alignx left,wrap");
		panel.add(constantCheckBox,"alignx left");
		
		//panel.add(createLabel("FinalDiagnosis.PostTextRecommendation"),"gaptop 2,aligny top");
		panel.add(createLabel("FinalDiagnosis.PostTextRecommendation"),"alignx left");
		String postTextRec = (String)properties.get("PostTextRecommendation");
		if( postTextRec==null) postTextRec="0";
		postTextRecommendationCheckBox = createCheckBox("FinalDiagnosis.PostTextRecommendation.Desc",(postTextRec.equals("0")?false:true));
		panel.add(postTextRecommendationCheckBox,"alignx left,wrap");
		
		panel.add(createLabel("FinalDiagnosis.ShowExplanationWithRecommendation"),"gaptop 2,aligny top");
		String showExplanation = (String)properties.get("ShowExplanationWithRecommendation");
		if( showExplanation==null) showExplanation="0";
		showExplanationWithRecommendationCheckBox = createCheckBox("FinalDiagnosis.ShowExplanationWithRecommendation.Desc",(showExplanation.equals("0")?false:true));
		panel.add(showExplanationWithRecommendationCheckBox,"alignx left");
		
		panel.add(createLabel("FinalDiagnosis.ManualMoveAllowed"),"gaptop 2,aligny top");
		String manualMoveAllowed = properties.get("ManualMoveAllowed");
		if( manualMoveAllowed==null) manualMoveAllowed="0";
		manualMoveAllowedCheckBox = createCheckBox("FinalDiagnosis.ManualMoveAllowed.Desc",(manualMoveAllowed.equalsIgnoreCase("1")));
		panel.add(manualMoveAllowedCheckBox,"alignx left,wrap");
		
		panel.add(createLabel("FinalDiagnosis.TrapInsignificant"),"gaptop 2,aligny top");
		String tf = (String)properties.get("TrapInsignificantRecommendations");
		if( tf==null) tf="0";
		trapBox = createCheckBox("FinalDiagnosis.TrapInsignificant.Desc",(tf.equals("0")?false:true));
		panel.add(trapBox,"alignx left");
		return panel;
	}
	
	// Copy the FinalDiagnosis auxiliary data back into the database
	private void save(){
		model.getProperties().put("Constant", (constantCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("ManualMoveAllowed", (manualMoveAllowedCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("CalculationMethod",calculationMethodField.getText());
		model.getProperties().put("FinalDiagnosisLabel",finalDiagnosisLabelField.getText());
		model.getProperties().put("TextRecommendation", textRecommendationArea.getText());
		model.getProperties().put("Comment", commentArea.getText());
		model.getProperties().put("Explanation", explanationArea.getText());
		model.getProperties().put("PostTextRecommendation", (postTextRecommendationCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("ShowExplanationWithRecommendation", (showExplanationWithRecommendationCheckBox.isSelected()?"1":"0"));
		model.getProperties().put("Priority", priorityField.getText());
		model.getProperties().put("RefreshRate", refreshRateField.getText());
		model.getProperties().put("PostProcessingCallback", postProcessingCallbackField.getText());
		model.getProperties().put("TrapInsignificantRecommendations", (trapBox.isSelected()?"1":"0"));
		
		List<String> inUseList = dual.getDestinations();
		model.getLists().put("OutputsInUse",inUseList);
		
		// Save values back to the database
		try {
			String db = requestHandler.getDatabaseForUUID(diagram.getId().toString());
			extensionManager.runScript(context.getScriptManager(),block.getClassName(), ScriptConstants.PROPERTY_SET_SCRIPT, 
					diagram.getId().toString(),model,db);
			// Replace the aux data structure in our serializable application
			// NOTE: The Nav tree node that calls the dialog saves the application resource.
			block.setAuxiliaryData(model);
			block.setDirty(true);
		}
		catch( Exception ex ) {
			log.errorf(TAG+".save: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}
	}
}
