/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.classic.config;

import java.awt.Dimension;
import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.JPanel;

import com.ils.blt.common.serializable.SerializableApplication;
import com.inductiveautomation.ignition.client.util.gui.SlidingPane;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
/**
 * Display a dialog to configure an Application node
 */
public class ApplicationConfigurationDialog extends JDialog { 
	private static String KEY = "ILS Application Editor";
	private static String TITLE = "Application Editor";

	protected final DesignerContext context;
	protected final LoggerEx log;
	private final ApplicationConfigurationController controller;
	protected boolean cancelled = false;
	private static final long serialVersionUID = 2882399376824334427L;
	private final SlidingPane slidingPane;
	
	// Getters
	public LoggerEx getLog() { return log; }

	public ApplicationConfigurationDialog(Frame frame,DesignerContext ctx,SerializableApplication app) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		setTitle(TITLE);
		this.context = ctx;
		this.setPreferredSize(new Dimension(ApplicationConfigurationConstants.DIALOG_WIDTH,ApplicationConfigurationConstants.DIALOG_HEIGHT));
		controller = new ApplicationConfigurationController(context,this,app);
		
		slidingPane = new SlidingPane();
		initialize();
       	setContentPane(slidingPane);
        
        log.infof("   ...leaving ApplicationConfigurationDialog constructor!");
	}
	
	// Add all the tabs to the sliding pane. The controller holds the model data.
	private void initialize() {
		// sub-panes added according to the indexes above:
		slidingPane.add(new HomePane(controller));
		OutputEditorPane outputEditor = new OutputEditorPane(controller);
		slidingPane.add(new OutputsPane(controller,outputEditor));
		slidingPane.add(outputEditor);
		slidingPane.add(new TagSelectorPane(controller, outputEditor));
		slidingPane.add(new JPanel());  // a blank pane
		slideTo(ApplicationConfigurationConstants.HOME);
	}

	public void slideTo(int tab) {
		slidingPane.setSelectedPane(tab);
	}
	
	public void setCancelled(boolean flag) { this.cancelled = flag; }
	/*
	 * @return true if the user has selected the "Cancel" button.
	 */
	public boolean isCancelled() { return cancelled; }
	
	public SerializableApplication getApplication() { return controller.getApplication(); }
}