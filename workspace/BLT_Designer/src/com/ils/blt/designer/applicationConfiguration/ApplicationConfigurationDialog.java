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

import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.ils.blt.common.serializable.SerializableApplication;
import com.ils.blt.designer.config.ConfigurationDialog;
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
	
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 500;
	private final int DIALOG_WIDTH = 440;
	private final ScriptExtensionManager extensionManager = ScriptExtensionManager.getInstance();
	private final SerializableApplication application;
	
	// Getters
	public LoggerEx getLog() { return log; }

	public ApplicationConfigurationDialog(Frame frame,DesignerContext ctx,SerializableApplication app) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		setTitle(TITLE);
		this.application = app;
		this.context = ctx;
		this.properties = new HashMap<>();
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		
		log.infof("Application: %s", application.getName());
		log.tracef("Calling the get extension function...");
		
		// Fetch properties of the application associated with the database and not serialized.
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.APP_GET_AUX_SCRIPT, 
				this.application.getId().toString(),properties);

		log.tracef("...back in Java land!");
		for (String key:properties.keySet()){
			log.tracef("Properties: key = %s, value = %s", key, properties.get(key).getClass().getName());
		}
		log.tracef("Properties: " + properties);
		
		controller = new ApplicationConfigurationController(this);
       	setContentPane(controller.getSlidingPane());
        
        log.infof("   ...leaving ApplicationConfigurationDialog constructor!");
	}
	
	public void save(){
		log.infof("In ApplicationConfigurationDialog:save()");
		log.tracef("Properties: " + properties);
		
		// Fetch properties of the application associated with the database and not serialized.
		extensionManager.runScript(context.getScriptManager(), ScriptConstants.APP_SET_AUX_SCRIPT, 
				this.application.getId().toString(),properties);
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