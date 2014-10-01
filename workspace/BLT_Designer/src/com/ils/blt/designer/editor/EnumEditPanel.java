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
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;

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
		headingLabel = addHeading(this);
		//Create two panels - value edit display option.
		JPanel editPanel = new JPanel();
		editPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		valueLabel = addSeparator(editPanel,"Value");
		valueCombo = new JComboBox<String>();    // Placeholder
		editPanel.add(valueCombo,"skip");
		add(editPanel,"");

		JPanel displayPanel = new JPanel();
		displayPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,"[]10[]20"));
		addSeparator(displayPanel,"Attribute Display");
		annotationCheckBox = new JCheckBox("Display ?");
		displayPanel.add(annotationCheckBox,"gapafter 15");
		displayPanel.add(createLabel("X offset"),"");
		xfield = createOffsetTextField("0");
		displayPanel.add(xfield,"");
		displayPanel.add(createLabel("Y offset"),"gapbefore 15");
		yfield = createOffsetTextField("0");
		displayPanel.add(yfield,"span,growx,wrap");
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
		if( prop.getType().equals(PropertyType.BOOLEAN))          setBooleanCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.DISTRIBUTION)) setDistributionTypeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.HYSTERESIS))   setHysteresisTypeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.LIMIT))        setLimitTypeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.SCOPE))	      setTransmissionScopeCombo(valueCombo);
		else if(prop.getType().equals(PropertyType.TRUTHVALUE) )  setTruthValueCombo(valueCombo); 
		valueCombo.setEditable(true);
		valueCombo.repaint();
		this.invalidate();
			
		if( prop.getValue()!=null ) {
			final String selection = prop.getValue().toString().toUpperCase();
			log.infof("%s.updateForProperty: %s=%s",TAG,prop.getName(),selection);
			SwingUtilities.invokeLater( new Runnable() {
				public void run() {
					valueCombo.setSelectedItem(selection);
				}
			});
			valueCombo.getModel().setSelectedItem(selection);
			log.infof("%s.updateForProperty: selection now=%s",TAG,valueCombo.getModel().getSelectedItem().toString());
		}   
		annotationCheckBox.setSelected(prop.isDisplayed());
		xfield.setText(String.valueOf(prop.getDisplayOffsetX()));
		yfield.setText(String.valueOf(prop.getDisplayOffsetY()));
	}
	// ================================ Combo boxes for different enumerations ===================================

	/**
	 * Create a combo box for true/false 
	 */
	private void setBooleanCombo(JComboBox<String> box) {
		box.removeAllItems();
		box.addItem(Boolean.TRUE.toString().toUpperCase());
		box.addItem(Boolean.FALSE.toString().toUpperCase());
	}
	
	/**
	 * Create a combo box for distribution type 
	 */
	private void setDistributionTypeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(DistributionType dt : DistributionType.values()) {
			box.addItem(dt.name());
		}
	}
	/**
	 * Create a combo box for hysteresis type
	 */
	private void setHysteresisTypeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(HysteresisType type : HysteresisType.values()) {
			box.addItem(type.name());
		}
	}
	/**
	 * Create a combo box for limit type
	 */
	private void setLimitTypeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(LimitType scope : LimitType.values()) {
			box.addItem(scope.name());
		}
	}

	/**
	 * Create a combo box for transmission scope
	 */
	private void setTransmissionScopeCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(TransmissionScope scope : TransmissionScope.values()) {
			box.addItem(scope.name());
		}
	}
	/**
	 * Create a combo box for limit type
	 */
	private void setTruthValueCombo(JComboBox<String> box) {
		box.removeAllItems();
		for(TruthValue tv : TruthValue.values()) {
			box.addItem(tv.name());
		}
	}
		
}
	