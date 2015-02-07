/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.block.RampMethod;
import com.ils.blt.common.serializable.SerializableApplication;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Display a dialog to configure an Application node
 */
public class ApplicationConfigurationDialog extends ConfigurationDialog { 
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 420;
	private final int DIALOG_WIDTH = 440;
	
	private final SerializableApplication application;
	private JPanel mainPanel = null;
	private JPanel quantPanel = null;
	private JTextField consoleField;
	private JCheckBox menuCheckBox;
	private JComboBox<String> methodBox;
	private JTextField queueField;
	private JTextField unitField;
	
	// These are the keys to the map of properties that are unique to applications
	public final static String PROPERTY_CONSOLE          = "console";
	public final static String PROPERTY_HIGHEST_PRIORITY = "highestPriority";
	public final static String PROPERTY_INCLUDE_IN_MENU  = "includeInMenu";
	public final static String PROPERTY_MESSAGE_QUEUE    = "messageQueue";
	public final static String PROPERTY_RAMP_METHOD      = "rampMethod";
	public final static String PROPERTY_UNIT             = "unit";
	
	public ApplicationConfigurationDialog(Frame frame,DesignerContext ctx,SerializableApplication app) {
		super(frame,ctx);
		this.application = app;
		this.setTitle(rb.getString("Application.Title"));
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        initialize();
	}

	/**
	 * The super class takes care of making a central tabbed pane.
	 * Here we add the tabs ...
	 * 1) Core attributes
	 * 2) Quant output definitions
	 * 3) Python hook definitions.
	 */
	private void initialize() {
		// TODO: Call the getAuxData script
		mainPanel = createMainPanel();
		// Tab label,?,panel, tooltip
		parentTabPanel.addTab(rb.getString("Application.Core.Tab"),null,mainPanel,rb.getString("Application.Core.Tab.Desc"));
		quantPanel = createQuantOutputPanel();
		parentTabPanel.addTab(rb.getString("Application.Quant.Tab"),null,quantPanel,rb.getString("Application.Quant.Tab.Desc"));
		parentTabPanel.setSelectedIndex(0);
		setOKActions();
	}
	
	/**
	 * Create the content pane and initialize layout.
	 */
	private JPanel createMainPanel() {
		JPanel panel = new JPanel();
		
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";
		
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		panel.add(createLabel("Application.Name"),"");
		nameField = createTextField("Application.Name.Desc",application.getName());
		panel.add(nameField,"span,wrap");
		
		panel.add(createLabel("Application.UUID"),"gaptop 2,aligny top");
		JTextField uuidField = createTextField("Application.UUID.Desc",application.getId().toString());
		uuidField.setEditable(false);
		panel.add(uuidField,"span,wrap");
		
		panel.add(createLabel("Application.Description"),"gaptop 2,aligny top");
		String description = (String)properties.get(PROPERTY_DESCRIPTION);
		if( description==null) description="";
		descriptionArea = createTextArea("Application.Description.Desc",description);
		panel.add(descriptionArea,"gaptop 2,aligny top,span,wrap");
		
		panel.add(createLabel("Application.Console"),"");
		String console = (String)properties.get(PROPERTY_CONSOLE);
		if( console==null) console="";
		consoleField = createTextField("Application.Console.Desc",console);
		panel.add(consoleField,"span,wrap");
		
		panel.add(createLabel("Application.Queue"),"");
		String queue = (String)properties.get(PROPERTY_MESSAGE_QUEUE);
		if( queue==null) queue="";
		queueField = createTextField("Application.Queue.Desc",queue);
		panel.add(queueField,"span,wrap");
		
		panel.add(createLabel("Application.Unit"),"");
		String unit = (String)properties.get(PROPERTY_UNIT);
		if( unit==null) unit="";
		unitField = createTextField("Application.Unit.Desc",unit);
		panel.add(unitField,"span,wrap");
		
		panel.add(createLabel("Application.Menu"),"");
		String include = (String)properties.get(PROPERTY_INCLUDE_IN_MENU);
		boolean shouldInclude = false;
		if( include!=null && include.equalsIgnoreCase("true")) shouldInclude = true;
		menuCheckBox = createCheckBox("Application.Menu.Desc",shouldInclude);
		panel.add(menuCheckBox,"");
		
		panel.add(createLabel("Application.Ramp"),"gapleft 10");
		String method = (String)properties.get(PROPERTY_RAMP_METHOD);
		if( method==null) method="";
		methodBox = createRampMethodCombo("Application.Ramp.Desc",method);
		panel.add(methodBox,"wrap");
		
		panel.add(createLabel("Application.Priority"),"");
		String priority = (String)properties.get(PROPERTY_HIGHEST_PRIORITY);
		if( priority==null) priority="";
		// Highest priority is read-only
		JTextField priorityField = createTextField("Application.Priority.Desc",priority);
		priorityField.setPreferredSize(NUMBER_BOX_SIZE);
		priorityField.setEnabled(false);
		panel.add(priorityField,"");
		panel.add(createLabel("Application.State"),"gapleft 10");
		stateBox = createActiveStateCombo("Application.State",application.getState());
		panel.add(stateBox,"wrap 20");
		return panel;
	}
	/**
	 * Create the content pane and initialize layout.
	 */
	private JPanel createQuantOutputPanel() {
		JPanel panel = new JPanel();
		return panel;
	}

		
		
		
		// The OK button copies data from the components and sets properties.
		// The super class already created the button and placed it in the panel. 
		// We just add the action listener here.
		private void setOKActions() {
			// The OK button copies data from the components and sets the property properties.
			// It then returns to the main tab

			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					// Set attributes from fields
					properties.put(PROPERTY_DESCRIPTION, descriptionArea.getText());
					// TODO: fill the properties structure with all the other attributes
					String activeState = (String)stateBox.getSelectedItem();

					// TODO: Call the setAuxData script
					dispose();
				}
			});
		}
	/**
	 * Create a combo box for ramp method
	 */
	protected JComboBox<String> createRampMethodCombo(String bundle,String method) {
		
		JComboBox<String> box = new JComboBox<String>();
		for(RampMethod as : RampMethod.values()) {
			box.addItem(as.name());
		}
		box.setToolTipText(BundleUtil.get().getString(bundle));
		box.setSelectedItem(method.toUpperCase());
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	/**
	 * @return the application that we are configuring.
	 */
	public SerializableApplication getApplication() { return application; }
	
}
