/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.config;


import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableFamily;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Display a dialog to configure a Family node
 */
public class FamilyConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "FamilyConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 280;
	private final int DIALOG_WIDTH = 400;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private final SerializableFamily family;
	private JPanel mainPanel = null;
	protected JTextArea descriptionArea;
	protected JComboBox<String> stateBox;
	protected JTextField priorityField;
	
	public FamilyConfigurationDialog(Frame frame,DesignerContext ctx,SerializableFamily fam) {
		super(ctx);
		this.family = fam;
		this.setTitle(rb.getString("Family.Title"));
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}
	/**
	 * The super class takes care of making a central tabbed pane --- but
	 * we don't wantit. Simply put our mainPanel as the content pane.
	 * Here we add the tabs ...
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		// Fetch properties of the family associated with the database and not serialized.
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAM_GET_AUX_SCRIPT, 
				this.family.getId().toString(),properties);
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
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

		panel.add(createLabel("Family.Name"),"");
		nameField = createTextField("Family.Name.Desc",family.getName());
		panel.add(nameField,"span,wrap");

		panel.add(createLabel("Family.UUID"),"gaptop 2,aligny top");
		JTextField uuidField = createTextField("Family.UUID.Desc",family.getId().toString());
		uuidField.setEditable(false);
		panel.add(uuidField,"span,wrap");
		
		panel.add(createLabel("Family.Description"),"gaptop 2,aligny top");
		String description = (String)properties.get(PROPERTY_DESCRIPTION);
		if( description==null) description="";
		descriptionArea = createTextArea("Family.Description.Desc",description);
		panel.add(descriptionArea,"gaptop 2,aligny top,span,wrap");

		panel.add(createLabel("Family.Priority"),"");
		String priority = (String)properties.get(ScriptConstants.PROPERTY_PRIORITY);
		if( priority==null) priority="";
		priorityField = createTextField("Family.Priority.Desc",priority);
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
				// Set attributes from fields
				family.setName(nameField.getText());
				properties.put(PROPERTY_DESCRIPTION, descriptionArea.getText());
				try {
					int pri = Integer.parseInt(priorityField.getText());
					properties.put(ScriptConstants.PROPERTY_PRIORITY, String.valueOf(pri));
				} 
				catch (NumberFormatException nfe) {
					log.warnf("%s.initialize: Priority (%s) must be an integer (%s)",
							TAG, priorityField.getText(), nfe.getMessage());
				}
				String activeState = (String) stateBox.getSelectedItem();
				family.setState(ActiveState.valueOf(activeState));
				extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAM_SET_AUX_SCRIPT, 
						family.getId().toString(),properties);
				dispose();
			}
		});
	}

	/**
	 * @return the family that we are editing. 
	 */
	public SerializableFamily getFamily() { return family; }

}
