/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;

import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * The basic edit panel is an abstract base class that contains a
 * collection of useful methods and constants for concrete implementations
 * of panels that participate in block editing.   
 */
public class BasicEditPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	protected final LoggerEx log;
	public static final Dimension BUTTON_SIZE = new Dimension(16,16);
	public static final Dimension COMBO_BOX_SIZE  = new Dimension(120,24);
	public static final Dimension ENTRY_BOX_SIZE  = new Dimension(160,24);
	public static final Dimension OFFSET_BOX_SIZE  = new Dimension(40,24);
	public static final Dimension TABLE_SIZE       = new Dimension(150,120);
	protected final BlockPropertyEditor parent;
	
	
	
	public BasicEditPanel(BlockPropertyEditor editor) {
		this.parent = editor;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	protected void updatePanelForBlock(int index,ProcessBlockView block) {
		parent.updatePanelForBlock(index,block);
	}
	protected void updatePanelForProperty(int index,BlockProperty prop) {
		parent.updatePanelForProperty(index,prop);
	}
	protected void setSelectedPane(int selection) {
		parent.setSelectedPane(selection);
	}
	
	// ========================== Component Creation Methods ========================
	/**
	 * Add a heading to an edit panel using Mig layout.
	 * @return the label so that it can be changed later
	 */
	protected JLabel addHeading(JPanel panel) {
		JSeparator separator = new JSeparator();
		panel.add(separator, "growx,wrap");
		JLabel label = new JLabel();
		label.setFont(new Font("Tahoma", Font.BOLD, 14));
		label.setForeground(Color.BLUE);
		panel.add(label, "center,split 2,span");
		panel.add(separator, "growx,wrap");
		return label;
	}
	/**
	 * Add a separator to a panel using Mig layout
	 */
	protected JLabel addSeparator(JPanel panel,String text) {
		JSeparator separator = new JSeparator();
		JLabel label = new JLabel(text);
		label.setFont(new Font("Tahoma", Font.PLAIN, 11));
		label.setForeground(Color.BLUE);
		panel.add(label, "split 2,span");
		panel.add(separator, "growx,wrap");
		return label;
	}
	

	/**
	 * Create a new label
	 */
	protected JLabel createLabel(String text) {
		return new JLabel(text);
	}
	/**
	 * Create a text field for display offsets.
	 */
	protected JTextField createOffsetTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(OFFSET_BOX_SIZE);
		field.setEditable(true);
		return field;
	}
	/**
	 * Create a text field for read-only values
	 */
	protected JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setEditable(false);
		return field;
	}

}
