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

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.script.ScriptConstants;

/**
 * Display a dialog to configure attributes that are toolkit-wide.
 */
public class ToolkitConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "ToolkitConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 280;
	private final int DIALOG_WIDTH = 400;
	private JPanel applicationScriptPanel = null;
	private JPanel familyScriptPanel = null;
	private final ApplicationRequestHandler handler;
	// These are the text fields that hold script paths.
	protected JTextField addAppHookField;
	protected JTextField cloneAppHookField;
	protected JTextField deleteAppHookField;
	protected JTextField getAppAuxDataHookField;
	protected JTextField setAppAuxDataHookField;
	protected JTextField updateAppHookField;
	protected JTextField addFamHookField;
	protected JTextField cloneFamHookField;
	protected JTextField deleteFamHookField;
	protected JTextField getFamAuxDataHookField;
	protected JTextField setFamAuxDataHookField;
	protected JTextField updateFamHookField;

	public ToolkitConfigurationDialog(Frame frame) {
		super(frame);
		this.handler = new ApplicationRequestHandler();
		this.setTitle(rb.getString("Toolkit.Title"));
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
		applicationScriptPanel = createApplicationScriptPanel();
		// Tab label,?,panel, tooltip
		parentTabPanel.addTab(rb.getString("Application.Script.Tab"),null,applicationScriptPanel,rb.getString("Application.Script.Tab.Desc"));
		familyScriptPanel = createFamilyScriptPanel();
		parentTabPanel.addTab(rb.getString("Family.Script.Tab"),null,familyScriptPanel,rb.getString("Family.Script.Tab.Desc"));
		parentTabPanel.setSelectedIndex(0);
		setOKActions();
	}

	/**
	 * Create the content pane as a grid 4 columns wide:
	 *     label | value | label | value
	 *     label | value -- span 3
	 */
	private JPanel createApplicationScriptPanel() {
		JPanel panel = new JPanel();
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		
		
		addSeparator(panel,"Application.Hooks");
		panel.add(createLabel("Application.AddHook"),"");
		addAppHookField = createTextField("Application.AddHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_ADD_TYPE));
		panel.add(addAppHookField,"span,wrap");
		
		panel.add(createLabel("Application.CloneHook"),"");
		cloneAppHookField = createTextField("Family.CloneHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_CLONE_TYPE));
		panel.add(cloneAppHookField,"span,wrap");
		
		panel.add(createLabel("Application.DeleteHook"),"");
		deleteAppHookField = createTextField("Application.DeleteHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_DELETE_TYPE));
		panel.add(deleteAppHookField,"span,wrap");
		
		panel.add(createLabel("Application.GetAuxDataHook"),"");
		getAppAuxDataHookField = createTextField("Application.GetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_GET_AUX_TYPE));
		panel.add(getAppAuxDataHookField,"span,wrap");
		
		panel.add(createLabel("Application.SetAuxDataHook"),"");
		setAppAuxDataHookField = createTextField("Application.SetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_SET_AUX_TYPE));
		panel.add(setAppAuxDataHookField,"span,wrap");
		
		panel.add(createLabel("Application.UpdateHook"),"");
		updateAppHookField = createTextField("Application.UpdateHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_UPDATE_TYPE));
		panel.add(updateAppHookField,"span,wrap");
		return panel;
	}


	/**
	 * Create the content pane as a grid 4 columns wide:
	 *     label | value | label | value
	 *     label | value -- span 3
	 */
	private JPanel createFamilyScriptPanel() {
		JPanel panel = new JPanel();
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";
		panel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));


		addSeparator(panel,"Family.Hooks");
		panel.add(createLabel("Family.AddHook"),"");
		addFamHookField = createTextField("Family.AddHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_ADD_TYPE));
		panel.add(addFamHookField,"span,wrap");

		panel.add(createLabel("Family.CloneHook"),"");
		cloneFamHookField = createTextField("Family.CloneHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_CLONE_TYPE));
		panel.add(cloneFamHookField,"span,wrap");

		panel.add(createLabel("Family.DeleteHook"),"");
		deleteFamHookField = createTextField("Family.DeleteHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_DELETE_TYPE));
		panel.add(deleteFamHookField,"span,wrap");

		panel.add(createLabel("Family.GetAuxDataHook"),"");
		getFamAuxDataHookField = createTextField("Family.GetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_GET_AUX_TYPE));
		panel.add(getFamAuxDataHookField,"span,wrap");

		panel.add(createLabel("Family.SetAuxDataHook"),"");
		setFamAuxDataHookField = createTextField("Family.SetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_SET_AUX_TYPE));
		panel.add(setFamAuxDataHookField,"span,wrap");

		panel.add(createLabel("Family.UpdateHook"),"");
		updateFamHookField = createTextField("Family.UpdateHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_UPDATE_TYPE));
		panel.add(updateFamHookField,"span,wrap");
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
				handler.setToolkitProperty(ScriptConstants.APP_ADD_TYPE,addAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_CLONE_TYPE,cloneAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_DELETE_TYPE,deleteAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_GET_AUX_TYPE,getAppAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_SET_AUX_TYPE,setAppAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_UPDATE_TYPE,updateAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_ADD_TYPE,addFamHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_CLONE_TYPE,cloneFamHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_DELETE_TYPE,deleteFamHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_GET_AUX_TYPE,getFamAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_SET_AUX_TYPE,setFamAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_UPDATE_TYPE,updateFamHookField.getText());
				dispose();
			}
		});
	}

}
