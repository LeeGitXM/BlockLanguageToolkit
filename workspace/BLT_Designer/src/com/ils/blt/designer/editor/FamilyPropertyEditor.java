/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.editor;


import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a sliding pane in the property edit window to configure a Family node
 */
public class FamilyPropertyEditor extends AbstractPropertyEditor  { 
	private final static String CLSS = "FamilyPropertyEditor";
	private static final long serialVersionUID = 2882399376824334427L;
	protected final DesignerContext context;
	private final SerializableFamily family;
	protected final ILSLogger log;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private JPanel mainPanel = null;
	private JTextField nameField = new JTextField();
	private JTextField priorityField = new JTextField();
	protected JTextArea descriptionArea;

	
	public FamilyPropertyEditor(DesignerContext ctx,SerializableFamily fam) {
		this.context = ctx;
		this.family = fam;
		this.model = new GeneralPurposeDataContainer();
		this.log = LogMaker.getLogger(this);
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
			family.setAuxiliaryData(model);
		}
		catch( Exception ex ) {
			log.errorf("FamilyConfigurationController.initialize: Exception ("+ex.getMessage()+")",ex); // Throw stack trace
		}
		
		mainPanel = createMainPanel();
		add(mainPanel,BorderLayout.CENTER);
	}

	public void shutdown() {}
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

		panel.add(new JLabel("Name"),"");
		nameField.setEditable(false);
		panel.add(nameField,"span,growx,wrap");

		panel.add(new JLabel("UUID"),"gaptop 2,aligny top");
		JTextField uuidField = new JTextField(family.getId().toString());
		uuidField.setEditable(false);
		panel.add(uuidField,"span,growx,wrap");
		
		panel.add(new JLabel("Description"),"gaptop 2,aligny top");
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionArea = new JTextArea(description);
		JScrollPane scrollPane = new JScrollPane(descriptionArea);
		scrollPane.setPreferredSize(ApplicationEditConstants.DESCRIPTION_AREA_SIZE);
		panel.add(scrollPane,"gaptop 2,aligny top,spanx,growx,growy,wrap");

		panel.add(new JLabel("Priority"),"gaptop 2,aligny top");
		String priority = model.getProperties().get("Priority");
		if( priority==null) priority="0.0";
		priorityField = new JTextField(priority);
		priorityField.setPreferredSize(ApplicationEditConstants.NUMBER_BOX_SIZE);
		panel.add(priorityField,"");
		
		return panel;
	}
	/**
	 * @return the family that we are editing. 
	 */
	public SerializableFamily getFamily() { return family; }

	// Call extension functions to save values.
	private void save(){
		log.infof("%s.save()",CLSS);
		model.getProperties().put("Description",descriptionArea.getText());
		model.getProperties().put("Priority", priorityField.getText());
	}
}
