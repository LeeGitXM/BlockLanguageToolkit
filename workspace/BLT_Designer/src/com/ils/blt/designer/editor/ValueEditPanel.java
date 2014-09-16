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
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.designer.workspace.WorkspaceRepainter;

/**
 * Display a panel to edit the name of a block and its 
 * attribute display.  This is one of the sliding panels
 * in the block editor.   
 */

public class ValueEditPanel extends BasicEditPanel {
	// A ValueEdit panel is designed to edit the value of a single property.
	private final static String TAG = "ValueEditPanel";
	private static final long serialVersionUID = 1L;
	private static final String columnConstraints = "[para]0[]0[]0[]0[]";
	private static final String layoutConstraints = "ins 2";
	private static final String rowConstraints = "";
	private final UtilityFunctions fncs;
	private BlockProperty property = null;
	private final JLabel headingLabel;
	private final JLabel valueLabel;
	private final JCheckBox annotationCheckBox;
	private final JTextField xfield;
	private final JTextField yfield;
	private final JTextField valueField;

	public ValueEditPanel(BlockPropertyEditor editor) {
		super(editor);
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		this.fncs = new UtilityFunctions();
		headingLabel = addHeading(this);
		//Create two panels - value edit display option.
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		valueLabel = addSeparator(editPanel,"Value");
		valueField = createTextField("");
		editPanel.add(valueField,"skip");
		add(editPanel,"");

		JPanel displayPanel = new JPanel();
		displayPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(displayPanel,"Attribute Display");
		annotationCheckBox = new JCheckBox("Display attribute?");
		displayPanel.add(annotationCheckBox,"wrap");
		displayPanel.add(createLabel("X offset"),"skip");
		xfield = createOffsetTextField("0");
		displayPanel.add(xfield,"span,growx,wrap");
		displayPanel.add(createLabel("Y offset"),"skip");
		yfield = createOffsetTextField("0");
		displayPanel.add(yfield,"span,growx,wrap");
		add(displayPanel,"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel(new MigLayout("", "60[center]5[center]",""));
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(property!=null) {
					// Coerce to the correct data type
					Object value = valueField.getText();
					if( property.getType().equals(PropertyType.BOOLEAN ))     value = fncs.coerceToBoolean(value);
					else if( property.getType().equals(PropertyType.DOUBLE )) value = fncs.coerceToDouble(value);
					else if( property.getType().equals(PropertyType.TIME ))   value = fncs.coerceToDouble(value);  // secs
					else if( property.getType().equals(PropertyType.INTEGER ))value = fncs.coerceToInteger(value);
					property.setValue(value);
					property.setDisplayed(annotationCheckBox.isSelected());
					try {
						property.setDisplayOffsetX(Integer.parseInt(xfield.getText()));
						property.setDisplayOffsetY(Integer.parseInt(yfield.getText()));
					}
					catch(NumberFormatException nfe) {
						JOptionPane.showMessageDialog(ValueEditPanel.this, String.format("ValueEditPanel: Bad entry for display offset (%s)",nfe.getLocalizedMessage()));
						property.setDisplayed(false);
					}
				}
				editor.getBlock().setDirty(true);
				SwingUtilities.invokeLater(new WorkspaceRepainter());
				updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
				setSelectedPane(BlockEditConstants.HOME_PANEL);
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

	public void updateForProperty(BlockProperty prop) {
		log.infof("%s.updateForProperty: %s",TAG,prop.getName());
		this.property = prop;
		headingLabel.setText(prop.getName());
		valueLabel.setText("Value ("+prop.getType().name().toLowerCase()+")");
		if( prop.getValue()!=null ) {
			valueField.setText(fncs.coerceToString(prop.getValue()));
		}   
		annotationCheckBox.setSelected(prop.isDisplayed());
		xfield.setText(String.valueOf(prop.getDisplayOffsetX()));
		yfield.setText(String.valueOf(prop.getDisplayOffsetX()));
	}

	/**
	 * Create a text field for data entry
	 */
	protected JTextField createTextField(String text) {	
		final JTextField field = new JTextField(text);
		field.setPreferredSize(ENTRY_BOX_SIZE);
		field.setEditable(true);
		return field;
	}
}
	