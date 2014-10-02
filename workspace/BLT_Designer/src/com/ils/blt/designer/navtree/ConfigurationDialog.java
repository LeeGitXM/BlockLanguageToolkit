/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import javax.swing.filechooser.FileNameExtensionFilter;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableApplication;
import com.inductiveautomation.ignition.common.BundleUtil;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
/**
 * Parent dialog for Application and Family configurations.
 *    ConfigurationDialog cd = new ConfigurationDialog("Attribute Editor");
 *    cd.pack();
 *    cd.setVisible(true);   // Terminates when dialog closed.
 *    result = cd.getModel();
 */

public class ConfigurationDialog extends JDialog { 
	protected static final String PREFIX = BLTProperties.BUNDLE_PREFIX;  // Required for some defaults
	private static final long serialVersionUID = 2882399376824334427L;
	private final int DIALOG_HEIGHT = 300;
	private final int DIALOG_WIDTH = 400;
	protected static final Dimension ENTRY_BOX_SIZE  = new Dimension(160,24);
	protected final LoggerEx log;
	protected JTextField nameField;
	
	
	public ConfigurationDialog() {
		super();
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(new Dimension(DIALOG_WIDTH,DIALOG_HEIGHT));
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout that is common to both.
	 */
	private void initialize() {
		final String columnConstraints = "[para]0[]0[]";
		final String layoutConstraints = "filly,ins 10";
		final String rowConstraints = "";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		

	}
	
	/**
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String bundle) {
		JLabel label = new JLabel(BundleUtil.get().getString(bundle+".Name"));
		
		return label;
	}
	/**
	 * Create a text field for editing the name
	 */
	protected JTextField createTextField(String bundle,String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(ENTRY_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(BundleUtil.get().getString(bundle+".Desc"));
		return field;
	}
}
