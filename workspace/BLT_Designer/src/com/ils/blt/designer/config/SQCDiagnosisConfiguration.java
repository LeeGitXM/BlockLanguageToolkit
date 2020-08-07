/**
 *   (c) 2015-2018  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.config;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import com.ils.blt.common.script.CommonScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.ils.common.GeneralPurposeDataContainer;

import net.miginfocom.swing.MigLayout;

/**
 * Display a dialog to configure the outputs available for an SQC Diagnosis.
 */
public class SQCDiagnosisConfiguration  extends ConfigurationDialog  {

	private static final long serialVersionUID = 2002388376824434423L;
	private final int DIALOG_HEIGHT = 220;
	private final int DIALOG_WIDTH = 500;
	private final GeneralPurposeDataContainer model;           // Data container operated on by controls
	private final ProcessDiagramView diagram;
	private final ProcessBlockView block;
	protected JTextField sQCLabelField;
	private final CommonScriptExtensionManager extensionManager = CommonScriptExtensionManager.getInstance();
	private static final String TAG = "SQCDiagnosisConfiguration";
	
	public SQCDiagnosisConfiguration(DiagramWorkspace wksp,ProcessDiagramView diag,ProcessBlockView view) {
		super(wksp.getContext());
		this.model = new GeneralPurposeDataContainer();

		this.diagram = diag;
		this.block = view;
		this.setTitle(rb.getString("SQCDiagnosisEditor.Title"));
		setModal(false);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
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
		retrieveAuxiliaryData();
		JPanel mainPanel = createMainPanel();
		contentPanel.add(mainPanel,BorderLayout.CENTER);
		setOKActions();
	}

	private JPanel createMainPanel() {
		
//		setLayout(new BorderLayout());
		JPanel internalPanel = new JPanel();
		internalPanel.setLayout(new MigLayout());
		internalPanel.add(createPropertiesPanel(),"growx,wrap");

		return internalPanel;
	}
	
	/**
	 * Create a fixed size text field. 
	 * 
	 * Initialize it with the block text property.
	 * @return
	 */
	private JPanel createPropertiesPanel()  {
		JPanel panel = new JPanel();
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 2,gapy 1,gapx 5,fillx,filly";
		final String rowConstraints = "para [][][][growprio 100,48:72:96][growprio 100,48:72:96][growprio 100,48:72:96][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		
		
		
		
		Map<String,String> properties = model.getProperties();
		
		
		panel.add(createLabel("SQCDiagnosis.Name"),"");
		nameField = createTextField("SQCDiagnosis.Name.Desc",block.getName());
		nameField.setEditable(false);
		panel.add(nameField,"spanx 3,growx,wrap");

		panel.add(createLabel("SQCDiagnosis.UUID"),"gaptop 2,aligny top");
		JTextField uuidField = createTextField("SQCDiagnosis.UUID.Desc",block.getId().toString());
		uuidField.setEditable(false);
		panel.add(uuidField,"spanx 3,growx,wrap");
		
		panel.add(createLabel("SQCDiagnosis.Label"),"gaptop 2,aligny top");
		String method = properties.get("SQCDiagnosisLabel");
		if( method==null) method="";
		sQCLabelField = createTextField("SQCDiagnosis.Label.Desc",method);
		panel.add(sQCLabelField,"spanx 3,growx,wrap");


		
		
		return panel;
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
		okButton.setText(rb.getString("SQCDiagnosisEditor.Save"));
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
				dispose();
			}
		});
	}
	
	// Copy the SQC auxiliary data back into the database
	private void save(){
		model.getProperties().put("SQCDiagnosisLabel",sQCLabelField.getText());
		
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
