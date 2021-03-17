/**
 *   (c) 2014-2020  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Color;
import java.awt.Font;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;

import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;

/**
 * The basic edit panel contains a collection of useful methods and constants 
 * for concrete implementations of panels that participate in block editing.   
 */
public class BasicEditPanel extends JPanel {
	private static final long serialVersionUID = 1L;
	protected final ILSLogger log;
	protected final AbstractPropertyEditor editor;
	
	
	public BasicEditPanel(AbstractPropertyEditor bpe) {
		this.editor = bpe;
		this.log = LogMaker.getLogger(this);
	}
	protected void setSelectedPane(int selection) {
		editor.setSelectedPane(selection);
	}
	public AbstractPropertyEditor getEditor() { return this.editor; }
	
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
		field.setPreferredSize(BlockEditConstants.OFFSET_BOX_SIZE);
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
