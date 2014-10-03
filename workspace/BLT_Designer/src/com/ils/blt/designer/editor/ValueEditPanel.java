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

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;

/**
 * Display a panel to edit the name of a block and its 
 * attribute display.  This is one of the sliding panels
 * in the block editor.   
 */

public class ValueEditPanel extends BasicEditPanel {
	// A ValueEdit panel is designed to edit the value of a single property.
	private final static String TAG = "ValueEditPanel";
	private static final long serialVersionUID = 1L;
	private final UtilityFunctions fncs;
	private BlockProperty property = null;
	private final JLabel headingLabel;
	private final JLabel valueLabel;
	private final JCheckBox annotationCheckBox;
	private final JTextField xfield;
	private final JTextField yfield;
	private final JTextField valueField;

	public ValueEditPanel(final BlockPropertyEditor editor) {
		super(editor);
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		this.fncs = new UtilityFunctions();
		headingLabel = addHeading(this);
		//Create two panels - value edit display option.
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout("ins 2","[para]0[]0[]0[]0[]",""));
		valueLabel = addSeparator(editPanel,"Value");
		valueField = createTextField("");
		editPanel.add(valueField,"skip");
		add(editPanel,"");

		JPanel displayPanel = new JPanel();
		displayPanel.setLayout(new MigLayout("ins 2","[para]0[]0[]0[]0[]","[]10[]20"));
		addSeparator(displayPanel,"Attribute Display");
		annotationCheckBox = new JCheckBox("Display ?");
		displayPanel.add(annotationCheckBox,"skip,gapafter 15");
		displayPanel.add(createLabel("X offset"),"");
		xfield = createOffsetTextField("0");
		displayPanel.add(xfield,"");
		displayPanel.add(createLabel("Y offset"),"gapbefore 15");
		yfield = createOffsetTextField("0");
		displayPanel.add(yfield,"wrap");
		add(displayPanel,"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, "dock south");
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(property!=null) {
					// Coerce to the correct data type
					Object value = valueField.getText();
					if( property.getType().equals(PropertyType.BOOLEAN ))     value = new Boolean(fncs.coerceToBoolean(value));
					else if( property.getType().equals(PropertyType.DOUBLE )) value = new Double(fncs.coerceToDouble(value));
					else if( property.getType().equals(PropertyType.TIME ))   value = new Double(fncs.coerceToDouble(value));  // secs
					else if( property.getType().equals(PropertyType.INTEGER ))value = new Integer(fncs.coerceToInteger(value));
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
				editor.notifyOfChange();    // Mark elements as "dirty", repaint
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
		yfield.setText(String.valueOf(prop.getDisplayOffsetY()));
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
	