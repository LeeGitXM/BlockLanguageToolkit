/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.WorkspaceRepainter;

/**
 * Display a panel to edit the name of a block and its 
 * attribute display.  This is one of the sliding panels
 * in the block editor.   
 */

public class NameEditPanel extends BasicEditPanel {
	private static final long serialVersionUID = 1L;
	private static final String columnConstraints = "[para]0[]0[]0[]0[]";
	private static final String layoutConstraints = "ins 2";
	private static final String rowConstraints = "0[]0[]0[]0[]40[]";
	private ProcessBlockView block = null;
	private final JLabel headingLabel;
	private final JTextField nameField;
	private final JCheckBox annotationCheckBox;
	private final JTextField xfield;
	private final JTextField yfield;

	public NameEditPanel(BlockPropertyEditor editor) {
		super(editor);
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		headingLabel = addHeading(this);
		//Create three panels - binding type, data type, display option.
		JPanel namePanel = new JPanel();
		namePanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(namePanel,"Block Name");
		namePanel.add(createLabel("Name"),"skip");
		nameField = createNameTextField("");
		namePanel.add(nameField,"span 2,growx,wrap");
		add(namePanel,"");
		
		JPanel displayPanel = new JPanel();
		displayPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(displayPanel,"Attribute Display");
		annotationCheckBox = new JCheckBox("Display attribute?");
		annotationCheckBox.setHorizontalTextPosition(SwingConstants.LEFT);
		displayPanel.add(annotationCheckBox,"wrap");
		displayPanel.add(createLabel("X offset"),"skip");
		xfield = createOffsetTextField("0");
		displayPanel.add(xfield,"span,growx,wrap");
		displayPanel.add(createLabel("Y offset"),"skip");
		yfield = createOffsetTextField("0");
		displayPanel.add(yfield,"span,growx,wrap");
		add(displayPanel,"");
		//add(new JSeparator(),"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel(new MigLayout("", "60[center]5[center]",""));
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if( !nameField.getText().isEmpty()) block.setName(nameField.getText());
				try {
					block.setNameDisplayed(annotationCheckBox.isSelected());
					block.setNameOffsetX(Integer.parseInt(xfield.getText()));
					block.setNameOffsetY(Integer.parseInt(yfield.getText()));
					setSelectedPane(BlockEditConstants.HOME_PANEL);
					block.setDirty(true);
					SwingUtilities.invokeLater(new WorkspaceRepainter());
				}
				catch(NumberFormatException nfe) {
					JOptionPane.showMessageDialog(NameEditPanel.this, String.format("Illegal value for offset--please re-enter (%s)",nfe.getLocalizedMessage()),
							"Display Parameter Entry Error",JOptionPane.ERROR_MESSAGE);
					block.setNameDisplayed(false);
				}
			}
		});
		JButton cancelButton = new JButton("Cancel");
		buttonPanel.add(cancelButton,"");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setSelectedPane(BlockEditConstants.HOME_PANEL);
			}			
		});
	}
	
	/**
	 * Create a text field for editing the name
	 */
	protected JTextField createNameTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(ENTRY_BOX_SIZE);
		field.setEditable(true);
		return field;
	}

	/**
	 * Change the values displayed given a new block.
	 * @param blk
	 */
	public void updateForBlock(ProcessBlockView blk) {
		this.block = blk;
		headingLabel.setText(block.getName());
		nameField.setText(block.getName());
		annotationCheckBox.setSelected(blk.isNameDisplayed());
		xfield.setText(String.valueOf(blk.getNameOffsetX()));
		yfield.setText(String.valueOf(blk.getNameOffsetY()));
		// Repaint to update the name display
		SwingUtilities.invokeLater(new WorkspaceRepainter());
	}
}
