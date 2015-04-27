/**
 *   (c) 2015  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.ui.DualListBox;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Display a dialog to configure the outputs available for a Final Diagnosis.
 */
public class FinalDiagnosisConfiguration extends ConfigurationDialog {
	private static final long serialVersionUID = 7211480530910862375L;
	private static final String TAG = "FinalDiagnosisConfiguration";
	private final int DIALOG_HEIGHT = 520;
	private final int DIALOG_WIDTH = 480;
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	private final ApplicationRequestHandler requestHandler;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private JPanel mainPanel = null;
	protected JTextField calculationMethodField;
	protected JTextArea textRecommendationArea;
	protected JTextArea postTextRecommendationArea;
	protected JTextField priorityField;
	protected JTextField refreshRateField;
	protected JTextField recommendationMethodField;
	protected JCheckBox  trapBox;
	
	public FinalDiagnosisConfiguration(DesignerContext ctx,ProcessDiagramView diag,ProcessBlockView view) {
		super(ctx);
		this.diagram = diag;
		this.block = view;
		this.setTitle(rb.getString("FinalDiagnosisEditor.Title"));
		setAlwaysOnTop(true);
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		this.requestHandler = new ApplicationRequestHandler();
        initialize();
	}
	/**
	 * The super class takes care of making a central tabbed pane --- but
	 * we don't want it. Simply put our mainPanel as the content pane.
	 * Here we add the tabs ...
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		// Fetch properties of the family associated with the database and not serialized.

		mainPanel = createMainPanel();
		contentPanel.add(mainPanel,BorderLayout.CENTER);
		populatePanel();
		setOKActions();
	}
	
	private JPanel createMainPanel() {	
		try {
			// Fetch properties of this block which associated with the database and not serialized.
			extensionManager.runOneTimeScript(context.getScriptManager(), block.getClassName(), 
					ScriptConstants.PROPERTY_GET_SCRIPT, block.getId().toString(),properties);
		}
		catch(Exception ex) {
			log.error(TAG+".initialize: Exception getting properties ("+ex.getMessage()+")",ex);
		}



		
		// The internal panel has two panes
		// - one for the dual list box, the other for the remaining attributes
		//setLayout(new BorderLayout());
		mainPanel = new JPanel();
		mainPanel.setLayout(new MigLayout("ins 2","",""));
		
		
		addSeparator(mainPanel,"FinalDiagnosis.QuantOutputs");
		
		DualListBox dual = new DualListBox();
		dual.addSourceElements(new String[] {"One", "Two", "Three"});
		mainPanel.add(dual, "gapx 50 40,wrap");
		mainPanel.add(createPropertiesPanel(),"wrap");
		return mainPanel;
	}
	
	/**
	 * Read the database and fill fields in the dialog
	 */
	private void populatePanel() {
		// Search block properties for the getter script
		properties.clear();

		try {
			// Fetch properties of this block which associated with the database and not serialized.
			extensionManager.runOneTimeScript(context.getScriptManager(), block.getClassName(), 
					ScriptConstants.PROPERTY_GET_SCRIPT, block.getId().toString(),properties);
		}
		catch(Exception ex) {
			log.error(TAG+".actionPerformed: Exception getting properties ("+ex.getMessage()+")",ex);
			return;
		}


		// Now set the component values
		calculationMethodField.setText(properties.getOrDefault(ScriptConstants.PROPERTY_CALCULATION_METHOD,"").toString());
		postTextRecommendationArea.setText(properties.getOrDefault(ScriptConstants.PROPERTY_POST_TEXT_RECOMMENDATION,"").toString());
		priorityField.setText(properties.getOrDefault(ScriptConstants.PROPERTY_PRIORITY,"0").toString());
		refreshRateField.setText(properties.getOrDefault(ScriptConstants.PROPERTY_REFRESH_RATE,"0.0").toString());
		textRecommendationArea.setText(properties.getOrDefault(ScriptConstants.PROPERTY_TEXT_RECOMMENDATION,"").toString());
		recommendationMethodField.setText(properties.getOrDefault(ScriptConstants.PROPERTY_TEXT_RECOMMENDATION_CALLBACK,"").toString());
		String val = properties.getOrDefault(ScriptConstants.PROPERTY_TRAP_INSIGNITFICANT_RECOMMENDATIONS,"1").toString();
		trapBox.setSelected((val.equals("0")?false:true));
	}
	private void setOKActions() {
		// The button panel is already added by the base class.
		okButton.setText(rb.getString("FinalDiagnosisEditor.Save"));
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Loop over the values and place in the target list
				// Set attributes from fields
				block.setName(nameField.getText());
				properties.put(ScriptConstants.PROPERTY_NAME,nameField.getText());
				
				try {
					int pri = Integer.parseInt(priorityField.getText());
					properties.put(ScriptConstants.PROPERTY_PRIORITY, String.valueOf(pri));
				} 
				catch (NumberFormatException nfe) {
					log.warnf("%s.initialize: Priority (%s) must be an integer (%s)",
							TAG, priorityField.getText(), nfe.getMessage());
				}
				properties.put(ScriptConstants.PROPERTY_CALCULATION_METHOD,calculationMethodField.getText());
				properties.put(ScriptConstants.PROPERTY_POST_TEXT_RECOMMENDATION,postTextRecommendationArea.getText());
				properties.put(ScriptConstants.PROPERTY_PRIORITY,priorityField.getText());
				properties.put(ScriptConstants.PROPERTY_REFRESH_RATE,refreshRateField.getText());
				properties.put(ScriptConstants.PROPERTY_TEXT_RECOMMENDATION,textRecommendationArea.getText());
				properties.put(ScriptConstants.PROPERTY_TEXT_RECOMMENDATION_CALLBACK,recommendationMethodField.getText());
				properties.put(ScriptConstants.PROPERTY_TRAP_INSIGNITFICANT_RECOMMENDATIONS,(trapBox.isSelected()?"1":"0"));

				try {
					// Fetch properties of this block which associated with the database and not serialized.
					extensionManager.runOneTimeScript(context.getScriptManager(), block.getClassName(), 
							ScriptConstants.PROPERTY_SET_SCRIPT, block.getId().toString(),properties);
				}
				catch(Exception ex) {
					log.error(TAG+".actionPerformed: Exception setting properties ("+ex.getMessage()+")",ex);
				}

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
		JPanel panel = new JPanel();
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		panel.add(createLabel("FinalDiagnosis.Name"),"");
		nameField = createTextField("FinalDiagnosis.Name.Desc",block.getName());
		nameField.setPreferredSize(NAME_BOX_SIZE);
		panel.add(nameField,"span,wrap");

		panel.add(createLabel("FinalDiagnosis.UUID"),"gaptop 2,aligny top");
		JTextField uuidField = createTextField("FinalDiagnosis.UUID.Desc",block.getId().toString());
		uuidField.setPreferredSize(NAME_BOX_SIZE);
		uuidField.setEditable(false);
		panel.add(uuidField,"span,wrap");
		
		panel.add(createLabel("FinalDiagnosis.CalcMethod"),"");
		String method = (String)properties.get(ScriptConstants.PROPERTY_CALCULATION_METHOD);
		if( method==null) method="";
		calculationMethodField = createTextField("FinalDiagnosis.CalcMethod.Desc",method);
		calculationMethodField.setPreferredSize(NAME_BOX_SIZE);
		panel.add(calculationMethodField,"span,wrap");
		
		panel.add(createLabel("FinalDiagnosis.TextRecommendation"),"gaptop 2,aligny top");
		String recommendation = (String)properties.get(ScriptConstants.PROPERTY_TEXT_RECOMMENDATION);
		if( recommendation==null) recommendation="";
		textRecommendationArea = createTextArea("FinalDiagnosis.TextRecommendation.Desc",recommendation);
		panel.add(textRecommendationArea,"gaptop 2,aligny top,span,wrap");
		
		panel.add(createLabel("FinalDiagnosis.PostTextRecommendation"),"gaptop 2,aligny top");
		recommendation = (String)properties.get(ScriptConstants.PROPERTY_POST_TEXT_RECOMMENDATION);
		if( recommendation==null) recommendation="";
		postTextRecommendationArea = createTextArea("FinalDiagnosis.PostTextRecommendation.Desc",recommendation);
		panel.add(postTextRecommendationArea,"gaptop 2,aligny top,span,wrap");

		panel.add(createLabel("FinalDiagnosis.Priority"),"");
		String priority = (String)properties.get(ScriptConstants.PROPERTY_PRIORITY);
		if( priority==null) priority="";
		priorityField = createTextField("FinalDiagnosis.Priority.Desc",priority);
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(priorityField,"span,wrap");
		
		panel.add(createLabel("FinalDiagnosis.RefreshRate"),"");
		String rate = (String)properties.get(ScriptConstants.PROPERTY_REFRESH_RATE);
		if( rate==null) rate="";
		refreshRateField = createTextField("FinalDiagnosis.RefreshRate.Desc",rate);
		refreshRateField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(refreshRateField,"span,wrap");
		
		panel.add(createLabel("FinalDiagnosis.RecommendationMethod"),"");
		method = (String)properties.get(ScriptConstants.PROPERTY_TEXT_RECOMMENDATION_CALLBACK);
		if( method==null) method="";
		recommendationMethodField = createTextField("FinalDiagnosis.RecommendationMethod.Desc",method);
		recommendationMethodField.setPreferredSize(NAME_BOX_SIZE);
		panel.add(recommendationMethodField,"span,wrap");
		
		panel.add(createLabel("FinalDiagnosis.TrapInsignificant"),"");
		String tf = (String)properties.get(ScriptConstants.PROPERTY_TRAP_INSIGNITFICANT_RECOMMENDATIONS);
		if( tf==null) tf="0";
		trapBox = createCheckBox("FinalDiagnosis.TrapInsignificant.Desc",(tf.equals("0")?false:true));
		panel.add(trapBox,"span,wrap");
		return panel;
	}
}
