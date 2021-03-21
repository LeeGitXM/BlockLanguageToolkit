/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.editor;


import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.HashMap;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.serializable.SerializableFamily;
import com.ils.blt.designer.ResourceUpdateManager;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.execution.ExecutionManager;
import com.inductiveautomation.ignition.common.execution.impl.BasicExecutionEngine;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a sliding pane in the property edit window to configure a Family node
 */
public class FamilyPropertyEditor extends AbstractPropertyEditor  { 
	private final static String CLSS = "FamilyPropertyEditor";
	private static final long serialVersionUID = 2882399376824334427L;
	private static final Dimension BUTTON_SIZE  = new Dimension(80,36);
	private static final Dimension COMBO_SIZE  = new Dimension(180,24);
	private static final Dimension DESCRIPTION_AREA_SIZE  = new Dimension(200,160);
	private static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	private static final Dimension PANEL_SIZE = new Dimension(250,300);
	protected final DesignerContext context;
	private final SerializableFamily family;
	protected final ILSLogger log;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private final ExecutionManager executionEngine;
	private JPanel mainPanel = null;
	private JComboBox<String> stateBox;
	private JTextArea descriptionArea;
	private JTextField nameField;
	private JTextField priorityField;
	private JButton saveButton;
	
	public FamilyPropertyEditor(DesignerContext ctx,SerializableFamily fam,ProjectResource res) {
		super(res);
		this.context = ctx;
		this.family = fam;
		this.model = new GeneralPurposeDataContainer();
		executionEngine = new BasicExecutionEngine(1,CLSS);
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
		validate();
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
		panel.setPreferredSize(PANEL_SIZE);

		panel.add(new JLabel("Name"),"");
		nameField = new JTextField(family.getName());
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
		scrollPane.setPreferredSize(DESCRIPTION_AREA_SIZE);
		panel.add(scrollPane,"gaptop 2,aligny top,spanx,growx,growy,wrap");

		panel.add(new JLabel("Priority"),"gaptop 2,aligny top");
		String priority = model.getProperties().get("Priority");
		if( priority==null) priority="0.0";
		priorityField = new JTextField(priority);
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		panel.add(priorityField,"");
		
		panel.add(new JLabel("State"),"gapleft 20");
		stateBox = new JComboBox<String>();
		for(ActiveState s:ActiveState.values()) {
			stateBox.addItem(s.name());
		}
		stateBox.setSelectedItem(family.getState());
		stateBox.setPreferredSize(COMBO_SIZE);
		panel.add(stateBox,"wrap 20");
		
		saveButton = new JButton("Save");
		saveButton.setPreferredSize(BUTTON_SIZE);
		panel.add(saveButton,"skip,gaptop 20,alignx center,wrap");
		return panel;
	}
	/**
	 * @return the family that we are editing. 
	 */
	public SerializableFamily getFamily() { return family; }

	// On save we serialize values back into resource
	private void save(){
		log.infof("%s.save()",CLSS);
		family.setState(ActiveState.valueOf(stateBox.getSelectedItem().toString()));
		model.getProperties().put("Description",descriptionArea.getText());
		model.getProperties().put("Priority", priorityField.getText());
		family.setAuxiliaryData(model);
		ObjectMapper mapper = new ObjectMapper();
		try{
			byte[] bytes = mapper.writeValueAsBytes(family);
			resource.setData(bytes);
			executionEngine.executeOnce(new ResourceUpdateManager(resource));
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s.run: Exception serializing family %s, resource %d (%s)",CLSS,family.getName(),resource.getResourceId(),jpe.getMessage());
		}
	}
}
