/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

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
	private final int DIALOG_HEIGHT = 300;
	private final int DIALOG_WIDTH = 600;
	private final SerializableFamily family;
	protected JTextField priorityField;
	protected JComboBox<String> stateBox;
	
	
	public FamilyConfigurationDialog(SerializableFamily fam) {
		super();
		this.family = fam;
		this.setTitle(PREFIX+".Family.Title");
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	private void initialize() {
		JPanel namePanel = new JPanel(new MigLayout("fillx","para[:80:]0[]",""));
		namePanel.add(createLabel(PREFIX+".Family.Name"),"");
		nameField = createTextField(PREFIX+".Family.Name","");
		namePanel.add(nameField,"");
		add(namePanel,"wrap");
		
		JPanel descriptionPanel = new JPanel(new MigLayout("fillx","para[:80:]0[]","[:100:]"));
		descriptionPanel.add(createLabel(PREFIX+".Family.Description"),"gaptop 2,aligny top");
		descriptionArea = createTextArea(PREFIX+".Family.Description","");
		descriptionPanel.add(descriptionArea,"gaptop 2,aligny top");
		add(descriptionPanel,"wrap");
		
		JPanel priorityStatePanel = new JPanel(new MigLayout("fillx","para[:80:]0[]20[:80:]0[]",""));
		priorityStatePanel.add(createLabel(PREFIX+".Family.Priority"),"");
		priorityField = createTextField(PREFIX+".Family.Priority","");
		priorityStatePanel.add(priorityField,"");
		priorityStatePanel.add(createLabel(PREFIX+".Family.State"),"");
		stateBox = createActiveStateCombo(PREFIX+".Family.State");
		priorityStatePanel.add(stateBox,"");
		add(priorityStatePanel,"wrap");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel(new MigLayout("", "60[center]5[center]",""));
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(family!=null) {
					// Set attributes from fields
				}
				dispose();
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dispose();
			}			
		});
	}
	/**
	 * @return the family that we are editing
	 */
	public SerializableFamily getFamily() { return family; }

}
