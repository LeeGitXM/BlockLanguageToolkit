/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Dimension;

import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.ActiveState;
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

	protected static final Dimension DESCRIPTION_BOX_SIZE  = new Dimension(160,80);
	protected static final Dimension ENTRY_BOX_SIZE  = new Dimension(160,24);
	protected final LoggerEx log;
	protected JTextArea descriptionArea;
	protected JTextField nameField;
	
	
	public ConfigurationDialog() {
		super();
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	
	/**
	 * Create the content pane and initialize layout that is common to both.
	 * This is a single wide column
	 */
	private void initialize() {
		final String columnConstraints = "para[:240:]";
		final String layoutConstraints = "filly,ins 2";
		final String rowConstraints = "[24]0[]0[]0[]0";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));

	}
	/**
	 * Create a combo box for "active" state
	 */
	protected JComboBox<String> createActiveStateCombo(String bundle) {
		JComboBox<String> box = new JComboBox<String>();
		for(ActiveState state : ActiveState.values()) {
			box.addItem(state.name());
		}
		box.setToolTipText(BundleUtil.get().getString(bundle+".Desc"));
		return box;
	}
	/**
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String bundle) {
		JLabel label = new JLabel(BundleUtil.get().getString(bundle+".Name")+": ");
		return label;
	}
	/*
	 * Create a text area for editing the description
	 */
	protected JTextArea createTextArea(String bundle,String text) {	
		final JTextArea area = new JTextArea(text);
		area.setPreferredSize(DESCRIPTION_BOX_SIZE);
		area.setEditable(true);
		area.setToolTipText(BundleUtil.get().getString(bundle+".Desc"));
		return area;
	}
	 /*
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
