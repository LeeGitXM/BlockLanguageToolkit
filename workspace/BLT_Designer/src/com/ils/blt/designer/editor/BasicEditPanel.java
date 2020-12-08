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

import com.ils.blt.common.DiagramState;
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
	protected final BlockPropertyEditor editor;
	
	
	
	public BasicEditPanel(BlockPropertyEditor bpe) {
		this.editor = bpe;
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	protected void setSelectedPane(int selection) {
		editor.setSelectedPane(selection);
	}
	public BlockPropertyEditor getEditor() { return this.editor; }
	
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

	/**
	 * @return the provider for the current state of the diagram.
	 */
	protected String getProvider() {
		DiagramState state = editor.getDiagram().getState();
		String provider = "";
		if( state.equals(DiagramState.ISOLATED)) provider = editor.getRequestHandler().getIsolationTagProvider();
		else provider =  editor.getRequestHandler().getProductionTagProvider();
		return provider;		
	}
}
