/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.DistributionType;
import com.ils.blt.common.block.HysteresisType;
import com.ils.blt.common.block.LimitType;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.block.TruthValue;

/**
 * Display a panel to edit enumerated types using  combo box.
 * This is one of the sliding panels in the block editor.   
 */

public class EnumEditPanel extends BasicEditPanel {
	// A ValueEdit panel is designed to edit the value of a single property.
	private final static String TAG = "EnumEditPanel";
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
	private JComboBox<String> valueCombo;

	public EnumEditPanel(final BlockPropertyEditor editor) {
		super(editor);
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		this.fncs = new UtilityFunctions();
		headingLabel = addHeading(this);
		//Create two panels - value edit display option.
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		valueLabel = addSeparator(editPanel,"Value");
		valueCombo = createBooleanCombo();    // Placeholder
		editPanel.add(valueCombo,"skip");
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
					String value = valueCombo.getSelectedItem().toString();
					log.infof("%s.actionPerformed: OK = %s",TAG,value);
					property.setValue(value);
					property.setDisplayed(annotationCheckBox.isSelected());
					try {
						property.setDisplayOffsetX(Integer.parseInt(xfield.getText()));
						property.setDisplayOffsetY(Integer.parseInt(yfield.getText()));
					}
					catch(NumberFormatException nfe) {
						JOptionPane.showMessageDialog(EnumEditPanel.this, String.format("ValueEditPanel: Bad entry for display offset (%s)",nfe.getLocalizedMessage()));
						property.setDisplayed(false);
					}
				}
				editor.notifyOfChange();  // Handle various dirty flags and repaint
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
		if( prop.getType().equals(PropertyType.BOOLEAN))          valueCombo = createBooleanCombo();
		else if(prop.getType().equals(PropertyType.DISTRIBUTION)) valueCombo = createDistributionTypeCombo();
		else if(prop.getType().equals(PropertyType.HYSTERESIS))   valueCombo = createHysteresisTypeCombo();
		else if(prop.getType().equals(PropertyType.LIMIT))        valueCombo = createLimitTypeCombo();
		else if(prop.getType().equals(PropertyType.SCOPE))	      valueCombo = createTransmissionScopeCombo();
		else if(prop.getType().equals(PropertyType.TRUTHVALUE) )  valueCombo = createTruthValueCombo();
			
		if( prop.getValue()!=null ) {
			log.infof("%s.updateForProperty: %s=%s",TAG,prop.getName(),prop.getValue().toString().toUpperCase());
			valueCombo.setSelectedItem(prop.getValue().toString().toUpperCase());
		}   
		annotationCheckBox.setSelected(prop.isDisplayed());
		xfield.setText(String.valueOf(prop.getDisplayOffsetX()));
		yfield.setText(String.valueOf(prop.getDisplayOffsetY()));
	}

	// ================================ Combo boxes for different enumerations ===================================

	/**
	 * Create a combo box for true/false 
	 */
	private JComboBox<String> createBooleanCombo() {
		String[] entries = new String[2];
		entries[0]=Boolean.TRUE.toString().toUpperCase();
		entries[1]=Boolean.FALSE.toString().toUpperCase();

		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}
	/**
	 * Create a combo box for distribution type 
	 */
	private JComboBox<String> createDistributionTypeCombo() {
		String[] entries = new String[DistributionType.values().length];
		int index=0;
		for(DistributionType dt : DistributionType.values()) {
			entries[index]=dt.name();
			index++;
		}

		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}
	/**
	 * Create a combo box for hysteresis type
	 */
	private JComboBox<String> createHysteresisTypeCombo() {
		String[] entries = new String[HysteresisType.values().length];
		int index=0;
		for(HysteresisType type : HysteresisType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}
	/**
	 * Create a combo box for limit type
	 */
	private JComboBox<String> createLimitTypeCombo() {
		String[] entries = new String[LimitType.values().length];
		int index=0;
		for(LimitType scope : LimitType.values()) {
			entries[index]=scope.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}

	/**
	 * Create a combo box for transmission scope
	 */
	private JComboBox<String> createTransmissionScopeCombo() {
		String[] entries = new String[TransmissionScope.values().length];
		int index=0;
		for(TransmissionScope scope : TransmissionScope.values()) {
			entries[index]=scope.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}
	/**
	 * Create a combo box for limit type
	 */
	private JComboBox<String> createTruthValueCombo() {
		String[] entries = new String[TruthValue.values().length];
		int index=0;
		for(TruthValue tv : TruthValue.values()) {
			entries[index]=tv.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		return box;
	}
		
}
	