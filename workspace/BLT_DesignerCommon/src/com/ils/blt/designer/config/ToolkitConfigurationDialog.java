/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.config;


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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.prefs.Preferences;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.PalettePrototype;
import com.ils.blt.common.script.ScriptConstants;
import com.ils.blt.common.script.ScriptExtensionManager;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * Display a dialog to configure attributes that are toolkit-wide.
 */
public class ToolkitConfigurationDialog extends ConfigurationDialog  { 
	private final static String TAG = "ToolkitConfigurationDialog";
	private static final long serialVersionUID = 2882399976824334427L;
	private final static String FILE_CHOOSER_NAME = "FileChoser";
	private final int DIALOG_HEIGHT = 360;
	private final int DIALOG_WIDTH = 400;
	private JPanel scriptPanel = null;
	private final ApplicationRequestHandler handler;
	private final ScriptExtensionManager sem;
	private final Preferences prefs;
	private JFileChooser fc = null;
	private JButton importButton = null;
	private final Map<String,AuxInterfacePanel> classPanels;

	public ToolkitConfigurationDialog(Frame frame,DesignerContext ctx) {
		super(ctx);
		this.handler = new ApplicationRequestHandler();
		this.sem = ScriptExtensionManager.getInstance();
		this.classPanels = new HashMap<>();
		this.setTitle(rb.getString("Toolkit.Title"));
		this.setPreferredSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
		this.prefs = Preferences.userRoot().node(BLTProperties.PREFERENCES_NAME);
        initialize();
	}
	/**
	 * The central panel holds a list of panes, one for each class that has auxiliary data.
	 * We have known entries for Applications and Families. Beyond that we search the
	 * block prototypes ...
	 */
	private void initialize() {
		scriptPanel = createScriptPanel();
		JScrollPane scrollPane = new JScrollPane(scriptPanel);
		contentPanel.add(scrollPane,BorderLayout.CENTER);
		addImportButton(this);
		setOKActions();
		updateScriptPanel();
	}

	/**
	 * Create the content pane as a grid 1 column wide.
	 */
	private JPanel createScriptPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new MigLayout("ins 10,wrap","[]",""));
		
		// Create a class panel for every class that needs scripts.
		AuxInterfacePanel interfacePanel = null;
		List<BlockDescriptor> descriptors = sem.getClassDescriptors();
		for( BlockDescriptor desc:descriptors ) {
			log.tracef("%s.createScriptPanel: block class = %s",TAG,desc.getBlockClass());
			addSeparator(panel,desc.getEmbeddedLabel());
			interfacePanel = new AuxInterfacePanel("","","");
			panel.add(interfacePanel, "skip,growx,push");
			classPanels.put(desc.getBlockClass(), interfacePanel);
		}
		return panel;
	}

	private void updateScriptPanel() {
		// Update each panel with values from the HSQLdb database
		String key = "";
		String value;
		for( String clss:classPanels.keySet()) {
			AuxInterfacePanel panel = classPanels.get(clss);
			// Query and update for each flavor
			key = ScriptExtensionManager.makeKey(clss,ScriptConstants.PROPERTY_GET_SCRIPT); 
			value = handler.getToolkitProperty(key);
			if (value!=null ) panel.updateGetField(value);
			key = ScriptExtensionManager.makeKey(clss,ScriptConstants.PROPERTY_SET_SCRIPT);
			value = handler.getToolkitProperty(key);
			if (value!=null ) panel.updateSetField(value);
			key = ScriptExtensionManager.makeKey(clss,ScriptConstants.PROPERTY_RENAME_SCRIPT);
			value = handler.getToolkitProperty(key);
			if (value!=null ) panel.updateRenameField(value);
		}
	}
	

	// The OK button copies data from the components and sets the persistent
	// properties. It also updates the script manager.
	// The super class already created the button and placed it in the panel. We
	// just need to add the action listener.
	private void setOKActions() {
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				// Set attributes from fields
				String key = "";
				for( String clss:classPanels.keySet()) {
					AuxInterfacePanel panel = classPanels.get(clss);
					// Query and update for both flavors. We update the database as well as the script manager
					key = ScriptExtensionManager.makeKey(clss,ScriptConstants.PROPERTY_GET_SCRIPT); 
					sem.addScript(clss, ScriptConstants.PROPERTY_GET_SCRIPT, panel.getGetFieldValue());
					handler.setToolkitProperty(key,panel.getGetFieldValue());
					key = ScriptExtensionManager.makeKey(clss,ScriptConstants.PROPERTY_SET_SCRIPT);
					sem.addScript(clss, ScriptConstants.PROPERTY_SET_SCRIPT, panel.getSetFieldValue());
					handler.setToolkitProperty(key,panel.getSetFieldValue());
					key = ScriptExtensionManager.makeKey(clss,ScriptConstants.PROPERTY_RENAME_SCRIPT);
					sem.addScript(clss, ScriptConstants.PROPERTY_RENAME_SCRIPT, panel.getRenameFieldValue());
					handler.setToolkitProperty(key,panel.getRenameFieldValue());
				}
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
		importButton.setPreferredSize(BUTTON_SIZE);
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
	 * We assume that the file holds three columns = className, flavor, value
	 * @param csv
	 */
	private void parseConfigFile(File csv) {
		BufferedReader reader = null;
		try { 
			String line;
			reader = new BufferedReader(new FileReader(csv));
			while ((line = reader.readLine()) != null) {
				// use comma as separator
				String[] args = line.split(",");
				if( args.length>2 ) {
					log.infof("%s.parseConfigFile. Read %s,%s,%s ",TAG,args[0],args[1],args[2]);
					String key = ScriptExtensionManager.makeKey(args[0].trim(),args[1].trim());  // Class, flavor
					handler.setToolkitProperty(key,args[2].trim());
				}
				else if( args.length>1 ) {
					log.infof("%s.parseConfigFile. Read %s,%s ",TAG,args[0],args[1]);
					String key = ScriptExtensionManager.makeKey(args[0].trim(),args[1].trim());  // Class, flavor
					handler.setToolkitProperty(key,"");
				}
				else if(!line.startsWith("#")){
					log.warnf("%s.parseConfigFile: Less than 2 fields, ignored (%s)",TAG,line);
				}
			}
			updateScriptPanel();
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
	
	/**
	 * This is a panel on the "main" panel page that edits script paths for a class.
	 * There are two "flavors" of paths - getter, setter, rename.
	 * On the main screen the panel is displayed with a separator that contains
	 * the class name. Here we have two lines, each with a single label and text box.
   	 * 
   	 * The parent class queries for values on "OK"
	 */
	private class AuxInterfacePanel extends JPanel  {
		private static final long serialVersionUID = 2264535784255009984L;
		//private final Dimension TEXTBOX_SIZE = new Dimension(120,20);
		private static final String columnConstraints = "";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		private final JTextField getScriptField;
		private final JTextField setScriptField;
		private final JTextField renameScriptField;
		
		
		public AuxInterfacePanel(String getPath,String setPath, String renamePath) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			
			JLabel getLabel = new JLabel("GetAuxData");
			add(getLabel,"");
			getScriptField = new JTextField(getPath);
			getScriptField.setPreferredSize(NAME_BOX_SIZE);
			add(getScriptField,"skip,growx,wrap");
			JLabel setLabel = new JLabel("SetAuxData");
			add(setLabel,"");
			setScriptField = new JTextField(getPath);
			setScriptField.setPreferredSize(NAME_BOX_SIZE);
			add(setScriptField,"skip,growx,wrap");
			JLabel renameLabel = new JLabel("Rename");
			add(renameLabel,"");
			renameScriptField = new JTextField( renamePath);
			renameScriptField.setPreferredSize(NAME_BOX_SIZE);
			add(renameScriptField,"skip,growx,wrap");
		}
		public String getGetFieldValue() { return getScriptField.getText(); }
		public String getSetFieldValue() { return setScriptField.getText(); }
		public String getRenameFieldValue() { return renameScriptField.getText(); }
		public void updateGetField(String val) { getScriptField.setText(val); }
		public void updateSetField(String val) { setScriptField.setText(val); }
		public void updateRenameField(String val) { renameScriptField.setText(val); }
	}
}
