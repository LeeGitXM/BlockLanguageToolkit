/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer.navtree;


import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.RampMethod;
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

	protected static final Dimension COMBO_SIZE  = new Dimension(120,24);
	protected static final Dimension DESCRIPTION_BOX_SIZE  = new Dimension(280,80);
	protected static final Dimension NAME_BOX_SIZE  = new Dimension(280,24);
	protected static final Dimension NUMBER_BOX_SIZE  = new Dimension(50,24);
	protected final LoggerEx log;
	protected JTextArea descriptionArea;
	protected JTextField nameField;
	protected boolean cancelled = false;
	protected JComboBox<String> stateBox;
	
	public ConfigurationDialog(Frame frame) {
		super(frame);
		setModal(true);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        this.log = LogUtil.getLogger(getClass().getPackage().getName());
        initialize();
	}
	

	/**
	 * Create the content pane as a grid 4 columns wide:
	 *     label | value | label | value
	 *     label | value -- span 3
	 */
	private void initialize() {
		final String columnConstraints = "para[][][][]";
		final String layoutConstraints = "ins 10,gapy 3,gapx 5,fillx";
		final String rowConstraints = "para[][][][][][][][][]";
		setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
	}
	/**
	 * Create a combo box for "active" state
	 */
	protected JComboBox<String> createActiveStateCombo(String bundle,ActiveState state) {
		JComboBox<String> box = new JComboBox<String>();
		for(ActiveState as : ActiveState.values()) {
			box.addItem(as.name());
		}
		box.setToolTipText(BundleUtil.get().getString(bundle+".Desc"));
		box.setSelectedItem(state.name());
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	/**
	 * Create a new checkbox. The text is the bundle key for the tooltip.
	 */
	protected JCheckBox createCheckBox(String bundle,boolean initialValue) {
		JCheckBox box = new JCheckBox();
		box.setToolTipText(BundleUtil.get().getString(bundle+".Desc")+": ");
		box.setSelected(initialValue);
		box.setText("");     // Don't use the standard label, it's on the wrong side.
		return box;
	}
	/**
	 * Create a new label. The text is the bundle key.
	 */
	protected JLabel createLabel(String bundle) {
		JLabel label = new JLabel(BundleUtil.get().getString(bundle+".Name")+": ");
		return label;
	}
	/**
	 * Create a combo box for ramp method
	 */
	protected JComboBox<String> createRampMethodCombo(String bundle,RampMethod method) {
		JComboBox<String> box = new JComboBox<String>();
		for(RampMethod as : RampMethod.values()) {
			box.addItem(as.name());
		}
		box.setToolTipText(BundleUtil.get().getString(bundle+".Desc"));
		box.setSelectedItem(method.name());
		box.setPreferredSize(COMBO_SIZE);
		return box;
	}
	/*
	 * Create a text area for editing the description
	 */
	protected JTextArea createTextArea(String bundle,String text) {	
		final JTextArea area = new JTextArea(text);
		area.setBorder(BorderFactory.createLineBorder(Color.BLACK, 1));     // Thickness
		area.setPreferredSize(DESCRIPTION_BOX_SIZE);
		area.setEditable(true);
		area.setToolTipText(BundleUtil.get().getString(bundle+".Desc"));
		return area;
	}
	/**
	 * Create a text field for editing the name
	 */
	protected JTextField createTextField(String bundle,String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(NAME_BOX_SIZE);
		field.setEditable(true);
		field.setToolTipText(BundleUtil.get().getString(bundle+".Desc"));
		return field;
	}
	/**
	 * Add a separator to a panel using Mig layout
	 */
	protected void addSeparator(JDialog dialog,String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(BundleUtil.get().getString(text));
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		dialog.add(label, "split 2,span");
		dialog.add(separator, "growx,wrap");
	}
	/*
	 * @return true if the user has selected the "Cancel" button.
	 */
	public boolean isCancelled() { return cancelled; }
}
