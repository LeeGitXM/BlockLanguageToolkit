/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.serializable.SerializableFamily;
/**
 * Display a dialog to export a diagram.
 *    ExportDialog ed = new ExportDialog("Attribute Editor");
 *    bad.pack();
 *    bad.setVisible(true);   // Terminates when dialog closed.
 *    result = bad.getModel();
 */

public class FamilyConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "FamilyConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 240;
	private final int DIALOG_WIDTH = 400;
	private final SerializableFamily family;
	protected JTextField priorityField;

	
	
	public FamilyConfigurationDialog(Frame frame,SerializableFamily fam) {
		super(frame);
		this.family = fam;
		this.setTitle(PREFIX+".Family.Title");
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		add(createLabel(PREFIX+".Family.Name"),"");
		nameField = createTextField(PREFIX+".Family.Name",family.getName());
		add(nameField,"span,wrap");
	
		add(createLabel(PREFIX+".Family.Description"),"gaptop 2,aligny top");
		descriptionArea = createTextArea(PREFIX+".Family.Description",family.getDescription());
		add(descriptionArea,"gaptop 2,aligny top,span,wrap");
		
		add(createLabel(PREFIX+".Family.Priority"),"");
		priorityField = createTextField(PREFIX+".Family.Priority",String.valueOf(family.getPriority()));
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		add(priorityField,"");
		add(createLabel(PREFIX+".Family.State"),"gapleft 20");
		stateBox = createActiveStateCombo(PREFIX+".Family.State",family.getState());
		add(stateBox,"wrap");


		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {			
				// Set attributes from fields
				family.setName(nameField.getText());
				family.setDescription(descriptionArea.getText());
				try {
					int pri = Integer.parseInt(priorityField.getText());
					family.setPriority(pri);
				}
				catch (NumberFormatException nfe) {
					log.warnf("%s.initialize: Priority (%s) must be an integer (%s)",TAG,priorityField.getText(),nfe.getMessage()); 
				}
				String activeState = (String)stateBox.getSelectedItem();
				family.setState(ActiveState.valueOf(activeState));
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				cancelled = true;
				dispose();
			}			
		});
	}
	/**
	 * @return the family that we are editing. If the operation was
	 *         a "cancel", return null.
	 */
	public SerializableFamily getFamily() { return family; }

}
