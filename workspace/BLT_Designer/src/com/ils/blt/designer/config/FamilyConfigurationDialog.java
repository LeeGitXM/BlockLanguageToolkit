/**
 *   (c) 2015-2106  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.config;


import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;

import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import com.ils.blt.client.ClientScriptExtensionManager;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a dialog to configure a Family node
 */
public class FamilyConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "FamilyConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 280;
	private final int DIALOG_WIDTH = 400;
	private final ClientScriptExtensionManager extensionManager = ClientScriptExtensionManager.getInstance();
	private final SerializableFamily family;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private JPanel mainPanel = null;
	protected JTextArea descriptionArea;
	protected JComboBox<String> stateBox;
	protected JFormattedTextField priorityField;
	
	public FamilyConfigurationDialog(Frame frame,DesignerContext ctx,SerializableFamily fam) {
		super(ctx);
		this.family = fam;
		this.model = new GeneralPurposeDataContainer();
		this.setTitle(rb.getString("Family.Title"));
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
		// Fetch data from the database and store in the model
		model.setProperties(new HashMap<String,String>());
		model.setLists(new HashMap<>());
		model.setMapLists(new HashMap<>());
		model.getProperties().put("Name", family.getName());   // Use as a key when fetching
		try {
			extensionManager.runScript(context.getScriptManager(),ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_GET_SCRIPT, 
					this.family.getId().toString(),model);
		}
		catch( Exception ex ) {
			log.errorf("FamilyConfigurationController.initialize: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}
		
		mainPanel = createMainPanel();
		contentPanel.add(mainPanel,BorderLayout.CENTER);
		setOKActions();
	}

	/**
	 * Create the main data pane as a grid 4 columns wide:
	 *     label | value | label | value
	 *     label | value -- span 3
	 */
	private JPanel createMainPanel() {
		JPanel panel = new JPanel();
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fill";
		final String rowConstraints = "para[][][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		panel.add(createLabel("Family.Name"),"");
		nameField = createTextField("Family.Name.Desc",family.getName());
		nameField.setEditable(false);
		panel.add(nameField,"span,growx,wrap");

		panel.add(createLabel("Family.UUID"),"gaptop 2,aligny top");
		JTextField uuidField = createTextField("Family.UUID.Desc",family.getId().toString());
		uuidField.setEditable(false);
		panel.add(uuidField,"span,growx,wrap");
		
		panel.add(createLabel("Family.Description"),"gaptop 2,aligny top");
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionArea = createTextArea("Family.Description.Desc",description);
		JScrollPane scrollPane = new JScrollPane(descriptionArea);
		scrollPane.setPreferredSize(DESCRIPTION_AREA_SIZE);
		panel.add(scrollPane,"gaptop 2,aligny top,spanx,growx,growy,wrap");

		panel.add(createLabel("Family.Priority"),"gaptop 2,aligny top");
		String priority = model.getProperties().get("Priority");
		if( priority==null) priority="0.0";
		priorityField = createDoubleField("Family.Priority.Desc",priority);
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(priorityField,"");
		
		panel.add(createLabel("Family.State"),"gapleft 20");
		stateBox = createActiveStateCombo("Family.State",family.getState());
		panel.add(stateBox,"wrap 20");
		return panel;
	}

	// The OK button copies data from the components and sets the property
	// properties. The super class already created the button and placed it 
	// in the panel. We just need to add the action listener.
	// NOTE: The only database-resident properties are: desc and priority.
	private void setOKActions() {
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
				dispose();
			}
		});
	}

	/**
	 * @return the family that we are editing. 
	 */
	public SerializableFamily getFamily() { return family; }

	// Copy the Family auxiliary data back into the database
	private void save(){
		log.infof("%s.save()",TAG);
		model.getProperties().put("Description",descriptionArea.getText());
		model.getProperties().put("Priority", priorityField.getText());
		try {
			// Save values back to the database
			extensionManager.runScript(context.getScriptManager(),ScriptConstants.FAMILY_CLASS_NAME, ScriptConstants.PROPERTY_SET_SCRIPT,
								       family.getId().toString(),model);
			// Replace the aux data structure in our serializable application
			// NOTE: The Nav tree node that calls the dialog saves the application resource.
			family.setAuxiliaryData(model);
		}
		catch( Exception ex ) {
			log.errorf(TAG+".save: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}
	}
}
