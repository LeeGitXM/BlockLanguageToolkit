/**
 *   (c) 2015-2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.editor;


import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

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
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * Display a sliding pane in the property edit window to configure a Family node
 */
public class FamilyPropertyEditor extends AbstractPropertyEditor implements ActionListener, FocusListener { 
	private final static String CLSS = "FamilyPropertyEditor";
	private static final long serialVersionUID = 2882399376824334427L;
	private static final Dimension COMBO_SIZE  = new Dimension(180,24);
	private static final Dimension DESCRIPTION_AREA_SIZE  = new Dimension(200,160);
	private static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	private static final Dimension PANEL_SIZE = new Dimension(250,300);
	protected final DesignerContext context;
	private final SerializableFamily family;
	protected final ILSLogger log;
	private final GeneralPurposeDataContainer model;           // Data container operated on by panels
	private JPanel mainPanel = null;
	private JComboBox<String> stateBox;
	private JTextArea descriptionArea;
	private JTextField nameField;
	private JTextField priorityField;
	
	public FamilyPropertyEditor(DesignerContext ctx,SerializableFamily fam,ProjectResource res) {
		super(res);
		this.context = ctx;
		this.family = fam;
		this.model = family.getAuxiliaryData();;
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
		model.getProperties().put("Name", family.getName());   // Use as a key when fetching
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
		nameField.addFocusListener(this);
		panel.add(nameField,"span,growx,wrap");

		panel.add(new JLabel("UUID"),"gaptop 2,aligny top");
		JTextField uuidField = new JTextField(family.getId().toString());
		uuidField.setEditable(false);
		uuidField.addFocusListener(this);
		panel.add(uuidField,"span,growx,wrap");
		
		panel.add(new JLabel("Description"),"gaptop 2,aligny top");
		String description = model.getProperties().get("Description");
		if( description==null) description="";
		descriptionArea = new JTextArea(description);
		descriptionArea.addFocusListener(this);
		JScrollPane scrollPane = new JScrollPane(descriptionArea);
		scrollPane.setPreferredSize(DESCRIPTION_AREA_SIZE);
		panel.add(scrollPane,"gaptop 2,aligny top,spanx,growx,growy,wrap");

		panel.add(new JLabel("Priority"),"gaptop 2,aligny top");
		String priority = model.getProperties().get("Priority");
		if( priority==null) priority="0.0";
		priorityField = new JTextField(priority);
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		priorityField.addFocusListener(this);
		panel.add(priorityField,"");
		
		panel.add(new JLabel("State"),"gapleft 20");
		stateBox = new JComboBox<String>();
		for(ActiveState s:ActiveState.values()) {
			stateBox.addItem(s.name());
		}
		stateBox.setSelectedItem(family.getState().name());
		stateBox.setPreferredSize(COMBO_SIZE);
		stateBox.addActionListener(this);
		panel.add(stateBox,"wrap 20");
		return panel;
	}
	/**
	 * @return the family that we are editing. 
	 */
	public SerializableFamily getFamily() { return family; }

	// On save we get values from the widgets and place back into the model
	private void save(){
		family.setState(ActiveState.valueOf(stateBox.getSelectedItem().toString()));
		model.getProperties().put("Description",descriptionArea.getText());
		model.getProperties().put("Priority", priorityField.getText());
		log.infof("%s.save(): state = %s",CLSS,family.getState().name());
	}
	
	@Override
	public void saveResource() {
		ObjectMapper mapper = new ObjectMapper();
		try{
			byte[] bytes = mapper.writeValueAsBytes(family);
			//log.tracef("%s.run JSON = %s",CLSS,new String(bytes));
			resource.setData(bytes);
			if( context.requestLockQuietly(resource.getResourceId()) )  {
				context.updateResource(resource.getResourceId(),resource.getData());   // Force an update
			}
			else {
				log.infof("%s.save: Failed to lock resource",CLSS);
			}
		}
		catch(JsonProcessingException jpe) {
			log.warnf("%s.run: Exception serializing family, resource %d (%s)",CLSS,resource.getResourceId(),jpe.getMessage());
		}
	}
	// ============================================== Action listener ==========================================
	@Override
	public void actionPerformed(ActionEvent event) {
		save();
		saveResource();
	}	
	// ============================================== Focus listener ==========================================
	@Override
	public void focusGained(FocusEvent event) {
	}
	@Override
	public void focusLost(FocusEvent event) {
		save();
		saveResource();
	}

}
