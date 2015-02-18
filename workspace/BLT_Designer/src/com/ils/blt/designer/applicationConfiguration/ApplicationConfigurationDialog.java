/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.applicationConfiguration;


import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.ListModel;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.block.RampMethod;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.designer.navtree.ConfigurationDialog;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.jidesoft.docking.DockContext;
/**
 * Display a dialog to configure an Application node
 */
public class ApplicationConfigurationDialog extends JDialog { 
	private static String KEY = "ILS Application Editor";
	private static String TITLE = "Application Editor";
	protected final DesignerContext context;
	private ApplicationConfigurationController controller;
	protected final LoggerEx log;
	protected boolean cancelled = false;
	protected final Map<String,Object> properties;
	
	public ApplicationConfigurationDialog(Frame frame,DesignerContext ctx,SerializableApplication app) {
//PH		super(KEY);
/*PH
       	setInitIndex(10);

*/
		System.out.println("In ApplicationConfigurationDialog constructor");
		setTitle(TITLE);
		this.application = app;
		this.context = ctx;
		this.properties = new HashMap<>();
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		
		System.out.printf("Application: %s %n", application.getName());
		
		controller = new ApplicationConfigurationController(this);
       	setContentPane(controller.getSlidingPane());
//PH       	super(frame,ctx);
		
        initialize();
	}


	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 420;
	private final int DIALOG_WIDTH = 440;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private final SerializableApplication application;
	private JTextField consoleField;
	private JCheckBox menuCheckBox;
	private JComboBox<String> methodBox;
	private JTextField queueField;
/*
	private JTextField unitField;
*/	
	// These are the keys to the map of properties that are unique to applications
	public final static String PROPERTY_CONSOLE          = "console";
	public final static String PROPERTY_HIGHEST_PRIORITY = "highestPriority";
	public final static String PROPERTY_INCLUDE_IN_MENU  = "includeInMenu";
	public final static String PROPERTY_MESSAGE_QUEUE    = "messageQueue";
	public final static String PROPERTY_RAMP_METHOD      = "rampMethod";
/*
	public final static String PROPERTY_UNIT             = "unit";
*/

	/**
	 * The super class takes care of making a central tabbed pane.
	 * Here we add the tabs ...
	 * 1) Core attributes
	 * 2) Quant output definitions
	 * 3) Python hook definitions.
	 */
	private void initialize() {
		log.infof("Initializing the application dialog, calling the get extension function...");
			
		
		// Fetch properties of the family associated with the database and not serialized.

		extensionManager.runScript(context.getScriptManager(), ScriptConstants.FAM_GET_AUX_SCRIPT, 
				this.application.getId().toString(),properties);

		log.infof("...back in Java land!");
		// TODO: Call the getAuxData script
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
/*
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
*/
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
//		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	/**
	 * @return the application that we are configuring.
	 */
	public SerializableApplication getApplication() { return application; }
	
	/*
	 * @return true if the user has selected the "Cancel" button.
	 */
	public boolean isCancelled() { return cancelled; }
	
	
}
