/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.prefs.Preferences;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.designer.config.ConfigurationDialog;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Display a dialog to configure attributes that are toolkit-wide.
 */
public class ToolkitConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "ToolkitConfigurationDialog";
	private static final long serialVersionUID = 2882399376824334427L;
	private final static String FILE_CHOOSER_NAME = "FileChoser";
	private final int DIALOG_HEIGHT = 280;
	private final int DIALOG_WIDTH = 400;
	private JPanel applicationScriptPanel = null;
	private JPanel familyScriptPanel = null;
	protected JTabbedPane parentTabPanel = null;
	private final ApplicationRequestHandler handler;
	private final Preferences prefs;
	private JFileChooser fc = null;
	private JButton importButton = null;
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

	public ToolkitConfigurationDialog(Frame frame,DesignerContext ctx) {
		super(ctx);
		this.handler = new ApplicationRequestHandler();
		this.setTitle(rb.getString("Toolkit.Title"));
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		this.prefs = Preferences.userRoot().node(BLTProperties.PREFERENCES_NAME);
        initialize();
	}
	/**
	 * We ddd a central tabbed pane central tabbed pane.
	 * then add tabs for ...
	 * 1) Core attributes
	 * 2) Python hook definitions.
	 */
	private void initialize() {
		applicationScriptPanel = createApplicationScriptPanel();
		// Tab label,?,panel, tooltip
		parentTabPanel = new JTabbedPane(SwingConstants.BOTTOM);
		parentTabPanel.setBorder(BorderFactory.createEtchedBorder());
		parentTabPanel.addTab(rb.getString("Application.Script.Tab"),null,applicationScriptPanel,rb.getString("Application.Script.Tab.Desc"));
		familyScriptPanel = createFamilyScriptPanel();
		parentTabPanel.addTab(rb.getString("Family.Script.Tab"),null,familyScriptPanel,rb.getString("Family.Script.Tab.Desc"));
		parentTabPanel.setSelectedIndex(0);
		contentPanel.add(parentTabPanel,BorderLayout.CENTER);
		addImportButton(this);
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
		addAppHookField = createTextField("Application.AddHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_ADD_SCRIPT));
		panel.add(addAppHookField,"span,wrap");
		
		panel.add(createLabel("Application.CloneHook"),"");
		cloneAppHookField = createTextField("Application.CloneHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_CLONE_SCRIPT));
		panel.add(cloneAppHookField,"span,wrap");
		
		panel.add(createLabel("Application.DeleteHook"),"");
		deleteAppHookField = createTextField("Application.DeleteHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_DELETE_SCRIPT));
		panel.add(deleteAppHookField,"span,wrap");
		
		panel.add(createLabel("Application.GetAuxDataHook"),"");
		getAppAuxDataHookField = createTextField("Application.GetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_GET_AUX_SCRIPT));
		panel.add(getAppAuxDataHookField,"span,wrap");
		
		panel.add(createLabel("Application.SetAuxDataHook"),"");
		setAppAuxDataHookField = createTextField("Application.SetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_SET_AUX_SCRIPT));
		panel.add(setAppAuxDataHookField,"span,wrap");
		
		panel.add(createLabel("Application.UpdateHook"),"");
		updateAppHookField = createTextField("Application.UpdateHook.Desc",handler.getToolkitProperty(ScriptConstants.APP_UPDATE_SCRIPT));
		panel.add(updateAppHookField,"span,wrap");
		return panel;
	}

	private void updateApplicationScriptPanel() {
		addAppHookField.setText(handler.getToolkitProperty(ScriptConstants.APP_ADD_SCRIPT));
		cloneAppHookField.setText(handler.getToolkitProperty(ScriptConstants.APP_CLONE_SCRIPT));
		deleteAppHookField.setText(handler.getToolkitProperty(ScriptConstants.APP_DELETE_SCRIPT));
		getAppAuxDataHookField.setText(handler.getToolkitProperty(ScriptConstants.APP_GET_AUX_SCRIPT));
		setAppAuxDataHookField.setText(handler.getToolkitProperty(ScriptConstants.APP_SET_AUX_SCRIPT));
		updateAppHookField.setText(handler.getToolkitProperty(ScriptConstants.APP_UPDATE_SCRIPT));
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
		addFamHookField = createTextField("Family.AddHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_ADD_SCRIPT));
		panel.add(addFamHookField,"span,wrap");

		panel.add(createLabel("Family.CloneHook"),"");
		cloneFamHookField = createTextField("Family.CloneHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_CLONE_SCRIPT));
		panel.add(cloneFamHookField,"span,wrap");

		panel.add(createLabel("Family.DeleteHook"),"");
		deleteFamHookField = createTextField("Family.DeleteHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_DELETE_SCRIPT));
		panel.add(deleteFamHookField,"span,wrap");

		panel.add(createLabel("Family.GetAuxDataHook"),"");
		getFamAuxDataHookField = createTextField("Family.GetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_GET_AUX_SCRIPT));
		panel.add(getFamAuxDataHookField,"span,wrap");

		panel.add(createLabel("Family.SetAuxDataHook"),"");
		setFamAuxDataHookField = createTextField("Family.SetAuxDataHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_SET_AUX_SCRIPT));
		panel.add(setFamAuxDataHookField,"span,wrap");

		panel.add(createLabel("Family.UpdateHook"),"");
		updateFamHookField = createTextField("Family.UpdateHook.Desc",handler.getToolkitProperty(ScriptConstants.FAM_UPDATE_SCRIPT));
		panel.add(updateFamHookField,"span,wrap");
		return panel;
	}
	
	private void updateFamilyScriptPanel() {
		addFamHookField.setText(handler.getToolkitProperty(ScriptConstants.FAM_ADD_SCRIPT));
		cloneFamHookField.setText(handler.getToolkitProperty(ScriptConstants.FAM_CLONE_SCRIPT));
		deleteFamHookField.setText(handler.getToolkitProperty(ScriptConstants.FAM_DELETE_SCRIPT));
		getFamAuxDataHookField.setText(handler.getToolkitProperty(ScriptConstants.FAM_GET_AUX_SCRIPT));
		setFamAuxDataHookField.setText(handler.getToolkitProperty(ScriptConstants.FAM_SET_AUX_SCRIPT));
		updateFamHookField.setText(handler.getToolkitProperty(ScriptConstants.FAM_UPDATE_SCRIPT));
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
				handler.setToolkitProperty(ScriptConstants.APP_ADD_SCRIPT,addAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_CLONE_SCRIPT,cloneAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_DELETE_SCRIPT,deleteAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_GET_AUX_SCRIPT,getAppAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_SET_AUX_SCRIPT,setAppAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.APP_UPDATE_SCRIPT,updateAppHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_ADD_SCRIPT,addFamHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_CLONE_SCRIPT,cloneFamHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_DELETE_SCRIPT,deleteFamHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_GET_AUX_SCRIPT,getFamAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_SET_AUX_SCRIPT,setFamAuxDataHookField.getText());
				handler.setToolkitProperty(ScriptConstants.FAM_UPDATE_SCRIPT,updateFamHookField.getText());
				dispose();
			}
		});
	}

	/**
	 * Create a button and event listener that pops up a file chooser, 
	 * then imports values into the text fields.
	 */
	private void addImportButton(final ToolkitConfigurationDialog dialog) {
		importButton = new JButton("Import");
		buttonPanel.add(importButton);
		importButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
			    FileNameExtensionFilter filter = new FileNameExtensionFilter("Property defaults", "csv");
			    fc.setFileFilter(filter);
			    String startDirectoryName = prefs.get(BLTProperties.PREF_CONFIG_DIRECTORY,System.getProperty(BLTProperties.EXIM_PATH));
			    if(startDirectoryName!=null ) {
			    	File startDirectory = new File(startDirectoryName);
				    fc.setCurrentDirectory(startDirectory);
			    }
			    fc.setDialogTitle(rb.getString("Import.Configuration.Title"));
			    fc.setApproveButtonText(rb.getString("Import.ApproveButton"));
			    fc.setEnabled(false);
			    fc.setMultiSelectionEnabled(false);
			    fc.setName(FILE_CHOOSER_NAME);
			    
			    int option = fc.showOpenDialog(dialog);
			    if( option==JFileChooser.APPROVE_OPTION) {
			    	handleSelection(fc.getSelectedFile());
			    }
			}
		});
	}

	/**
	 * Handle results from the file chooser.
	 */
	private void handleSelection(File filePath) {
		log.infof("%s.handleSelection %s ",TAG,filePath.getName());
		if( filePath!=null ) {
			String fileName = filePath.getName();
			if(fileName.indexOf(".")<0) {
				filePath = new File(filePath.getAbsolutePath()+".csv");
			}
			parseConfigFile(filePath);
			prefs.put(BLTProperties.PREF_CONFIG_DIRECTORY, filePath.getParent());
		}
	}

	/**
	 * We assume that the file holds two columns = key, value
	 * @param csv
	 */
	private void parseConfigFile(File csv) {
		@SuppressWarnings("resource")
		BufferedReader reader = null;
		try { 
			String line;
			reader = new BufferedReader(new FileReader(csv));
			while ((line = reader.readLine()) != null) {
				// use comma as separator
				String[] args = line.split(",");
				if( args.length>1 ) {
					log.debugf("%s.parseConfigFile. Read %s,%s ",TAG,args[0],args[1]);
					handler.setToolkitProperty(args[0].trim(),args[1].trim());
				}
				else if( args.length>0 ) {
					log.debugf("%s.parseConfigFile. Read %s, ",TAG,args[0]);
					handler.setToolkitProperty(args[0].trim(),"");
				}
			}
			updateApplicationScriptPanel();
			updateFamilyScriptPanel();
		} 
		catch (FileNotFoundException fnfe) {
			log.warnf("%s.parseConfigFile. File not found %s (%s) ",TAG,csv.getAbsolutePath(),fnfe.getMessage());
		} 
		catch (IOException ioe) {
			log.warnf("%s.parseConfigFile. Exception reading %s (%s) ",TAG,csv.getAbsolutePath(),ioe.getMessage());
		} 
		finally {
			if (reader != null) {
				try {
					reader.close();
				} 
				catch (IOException ignore) {}
			}
		}
	}
}
