/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.BorderLayout;
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

import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;

/**
 * Display a panel to edit the binding of a property and its 
 * attribute display.  This is one of the sliding panels
 * in the block editor.   
 */
public class ConfigurationPanel extends BasicEditPanel {
	private final static String TAG = "ConfigurationPanel";
	// A Configuration panel is designed to configuration of a single property.
	// Allow modification of the binding (subject to certain restrictions),
	// show the data type (read-only) and allow for attribute display.

	private static final long serialVersionUID = 1L;
	private static final String columnConstraints = "[para]0[]0[]0[]0[]";
	private static final String layoutConstraints = "ins 2";
	private static final String rowConstraints = "";
	private BlockProperty property = null;
	private final JLabel headingLabel;
	private final JComboBox<String> bindingTypeCombo;
	private final JComboBox<String> propertyTypeCombo;
	private final JLabel connectedId;
	// delete the next 3 lines - obsolete
	private final JCheckBox annotationCheckBox;
	private final JTextField xfield;
	private final JTextField yfield;


	public ConfigurationPanel(final BlockPropertyEditor editor) {
		super(editor);
		setLayout(new BorderLayout());
		JPanel interiorPanel = new JPanel();
		interiorPanel.setLayout(new MigLayout("top,flowy,ins 2","",""));
		headingLabel = addHeading(interiorPanel);
		//Create three panels - binding type, data type, display option.
		JPanel bindingPanel = new JPanel();
		bindingPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(bindingPanel,"Binding");
		bindingTypeCombo = createBindingTypeCombo(null);
		bindingPanel.add(bindingTypeCombo,"skip");
		interiorPanel.add(bindingPanel,"");

		JPanel typePanel = new JPanel();
		typePanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(typePanel,"Property Type");
		propertyTypeCombo = createPropertyTypeCombo();   
		propertyTypeCombo.setEditable(false);
		propertyTypeCombo.setEnabled(false);
		typePanel.add(propertyTypeCombo,"skip");
		interiorPanel.add(typePanel,"");

		JPanel displayPanel = new JPanel();
		displayPanel.setLayout(new MigLayout("ins 2","[para]0[]0[]0[]0[]","[]10[]2"));

		// for now, only show if it already has one.  You can turn it off but not on.  obsolete feature 
		// remove this next section - obsolete
		annotationCheckBox = new JCheckBox("Display ?");
		xfield = createOffsetTextField("0");
		yfield = createOffsetTextField("0");
		addSeparator(displayPanel,"");
		annotationCheckBox.setEnabled(false);
		xfield.setEnabled(false);
		yfield.setEnabled(false);
		displayPanel.add(annotationCheckBox,"skip,gapafter 15");
		displayPanel.add(createLabel("X offset"),"");
		displayPanel.add(xfield,"");
		displayPanel.add(createLabel("Y offset"),"gapbefore 15");
		displayPanel.add(yfield,"wrap");

		addSeparator(displayPanel,"Connected Property Display Block Info");
		connectedId = createLabel("Not Set");
		displayPanel.add(connectedId, "");
		interiorPanel.add(displayPanel,"");

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab. 
		JPanel buttonPanel = new JPanel();
		JButton okButton = new JButton("OK");
		buttonPanel.add(okButton,"");
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(property!=null) {
					property.setBindingType(BindingType.valueOf(bindingTypeCombo.getSelectedItem().toString()));
					property.setDisplayed(annotationCheckBox.isSelected());
					try {
						property.setDisplayOffsetX(Integer.parseInt(xfield.getText()));
						property.setDisplayOffsetY(Integer.parseInt(yfield.getText()));
					}
					catch(NumberFormatException nfe) {
						JOptionPane.showMessageDialog(ConfigurationPanel.this, String.format("ConfigurationPanel: Bad entry for display offset (%s)",nfe.getLocalizedMessage()));
						property.setDisplayed(false);
					}
				}
				editor.saveDiagramClean();  
				editor.updatePanelForProperty(BlockEditConstants.HOME_PANEL,property);
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
		
		add(interiorPanel,BorderLayout.CENTER);
		add(buttonPanel, BorderLayout.SOUTH);
	}

	public void updateForProperty(BlockProperty prop) {
		this.property = prop;
		headingLabel.setText(prop.getName());
		populateBindingTypeCombo(bindingTypeCombo,prop);
		bindingTypeCombo.setSelectedItem(prop.getBindingType().toString());
		// The only changeable bindings are NONE and TAG_MONITOR
		bindingTypeCombo.setEnabled(prop.getBindingType().equals(BindingType.NONE)||
				                    prop.getBindingType().equals(BindingType.TAG_MONITOR));
		propertyTypeCombo.setSelectedItem(prop.getType().toString());
		if (prop.isShowProperty()) {
			connectedId.setText("display block UUID:" + prop.getDisplayedBlockUUID().toString());
		} else {
			connectedId.setText("No connected display block");
		}
		// delete this next section - obsolete
		annotationCheckBox.setSelected(prop.isDisplayed());
		xfield.setText(String.valueOf(prop.getDisplayOffsetX()));
		yfield.setText(String.valueOf(prop.getDisplayOffsetY()));
		if (prop.isDisplayed()) {  // if it is displayed then allow it to be turned off, but not back on (obsolete feature)
			annotationCheckBox.setEnabled(true);
		}

	}

	/**
	 * Create a combo box for binding type. Leave out bindings that are core
	 * to the workings of the block as options. An original placeholder is
	 * created with a null property. It is later replaced.
	 */
	private JComboBox<String> createBindingTypeCombo(BlockProperty prop) {
		final JComboBox<String> box = new JComboBox<String>();
		populateBindingTypeCombo(box,null);
		box.setPreferredSize(BlockEditConstants.COMBO_BOX_SIZE);
		return box;
	}
	/**
	 * Populate a combo box with binding types. If the property is null,
	 * skip property-related restrictions. 
	 */
	private void populateBindingTypeCombo(JComboBox<String> box,BlockProperty prop) {
		box.removeAllItems();
		if( prop==null ) return;   // How does this happen?
		for(BindingType type : BindingType.values()) {
			if(type.name().equals(BindingType.ENGINE.name()) ) {
				if( !BindingType.ENGINE.equals(prop.getBindingType()) ) continue;
			}
			else if(type.name().equals(BindingType.TAG_READ.name()) ) continue;
			else if(type.name().equals(BindingType.OPTION.name()) ) { 
				if( !BindingType.OPTION.equals(prop.getBindingType()) ) continue;
			}
			else if(type.name().equals(BindingType.TAG_WRITE.name()) ) continue;
			else if(type.name().equals(BindingType.TAG_READWRITE.name()) ) continue;
			// We also disallow tag bindings to complex datatypes
			else if(type.name().equals(BindingType.TAG_MONITOR.name()) ) {
				if( BindingType.ENGINE.equals(prop.getBindingType()) ) continue;
				PropertyType pt = prop.getType();
				if( !pt.equals(PropertyType.BOOLEAN) &&
					!pt.equals(PropertyType.DOUBLE) &&
					!pt.equals(PropertyType.INTEGER)  &&
					!pt.equals(PropertyType.STRING) &&
					!pt.equals(PropertyType.TIME_MINUTES) &&
					!pt.equals(PropertyType.TIME_SECONDS) &&
					!pt.equals(PropertyType.TIME))
						continue;
			}
			else {                   // NONE
				if( BindingType.ENGINE.equals(prop.getBindingType())) 
					continue;
			}
			log.tracef("%s.createBindingTypeCombo: %s",TAG,type.name());
			box.addItem(type.name());
		}
	}
	
	/**
	 * Create a combo box for property type
	 */
	private JComboBox<String> createPropertyTypeCombo() {
		String[] entries = new String[PropertyType.values().length];
		int index=0;
		for(PropertyType type : PropertyType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.setPreferredSize(BlockEditConstants.COMBO_BOX_SIZE);
		return box;
	}
}
	