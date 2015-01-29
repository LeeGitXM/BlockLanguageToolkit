/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.serializable.SerializableFamily;

/**
 * Display a dialog to configure a Family node
 */
public class FamilyConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "FamilyConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 280;
	private final int DIALOG_WIDTH = 400;
	private final SerializableFamily family;
	private JPanel mainPanel = null;
	protected JTextField priorityField;
	
	// These are the keys to the map of properties that are unique to applications
	public final static String PROPERTY_PRIORITY    = "priority";
	
	public FamilyConfigurationDialog(Frame frame,SerializableFamily fam) {
		super(frame);
		this.family = fam;
		this.setTitle(rb.getString("Family.Title"));
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}
	/**
	 * The super class takes care of making a central tabbed pane.
	 * Here we add the tabs ...
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		// TODO: Call the getAuxData script
		mainPanel = createMainPanel();
		// Tab label,?,panel, tooltip
		parentTabPanel.addTab(rb.getString("Family.Core.Tab"),null,mainPanel,rb.getString("Family.Core.Tab.Desc"));
		parentTabPanel.setSelectedIndex(0);
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

		panel.add(createLabel("Family.Description"),"gaptop 2,aligny top");
		String description = (String)properties.get(PROPERTY_DESCRIPTION);
		if( description==null) description="";
		descriptionArea = createTextArea("Family.Description.Desc",description);
		panel.add(descriptionArea,"gaptop 2,aligny top,span,wrap");

		panel.add(createLabel("Family.Priority"),"");
		String priority = (String)properties.get(PROPERTY_PRIORITY);
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
	// properties.
	// The super class already created the button and placed it in the panel. We
	// just
	// need to add the action listener.
	private void setOKActions() {
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Set attributes from fields
				family.setName(nameField.getText());
				properties.put(PROPERTY_DESCRIPTION, descriptionArea.getText());
				try {
					int pri = Integer.parseInt(priorityField.getText());
					properties.put(PROPERTY_PRIORITY, String.valueOf(pri));
				} 
				catch (NumberFormatException nfe) {
					log.warnf("%s.initialize: Priority (%s) must be an integer (%s)",
							TAG, priorityField.getText(), nfe.getMessage());
				}
				String activeState = (String) stateBox.getSelectedItem();
				family.setState(ActiveState.valueOf(activeState));
				// TODO: Call the setAuxData script
				dispose();
			}
		});
	}

	/**
	 * @return the family that we are editing. 
	 */
	public SerializableFamily getFamily() { return family; }

}
