/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicComboBoxRenderer;

import net.miginfocom.swing.MigLayout;

import com.ils.block.common.BindingType;
import com.ils.block.common.BlockProperty;
import com.ils.block.common.PropertyType;
import com.ils.blt.designer.workspace.WorkspaceRepainter;

/**
 * Display a panel to edit the name of a block and its 
 * attribute display.  This is one of the sliding panels
 * in the block editor.   
 */

public class ConfigurationPanel extends BasicEditPanel {
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
	private final JCheckBox annotationCheckBox;
	private final JTextField xfield;
	private final JTextField yfield;

	public ConfigurationPanel(BlockPropertyEditor editor) {
		super(editor);
		setLayout(new MigLayout("top,flowy,ins 2","",""));
		headingLabel = addHeading(this);
		//Create three panels - binding type, data type, display option.
		JPanel bindingPanel = new JPanel();
		bindingPanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(bindingPanel,"Binding");
		bindingTypeCombo = createBindingTypeCombo();
		bindingPanel.add(bindingTypeCombo,"skip");
		add(bindingPanel);

		JPanel typePanel = new JPanel();
		typePanel.setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
		addSeparator(typePanel,"Property Type");
		propertyTypeCombo = createPropertyTypeCombo();
		propertyTypeCombo.setEditable(false);
		typePanel.add(propertyTypeCombo,"skip");
		add(typePanel);

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
		add(displayPanel);

		// The OK button copies data from the components and sets the property properties.
		// It then returns to the main tab
		JPanel buttonPanel = new JPanel(new MigLayout("", "60[center]5[center]",""));
		add(buttonPanel, "dock south");
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
				SwingUtilities.invokeLater(new WorkspaceRepainter());
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
		this.property = prop;
		headingLabel.setText(prop.getName());
		bindingTypeCombo.setSelectedItem(prop.getBindingType().toString());
		bindingTypeCombo.setEnabled(!prop.getBindingType().equals(BindingType.ENGINE));
		propertyTypeCombo.setSelectedItem(prop.getType().toString());
		annotationCheckBox.setSelected(prop.isDisplayed());
		xfield.setText(String.valueOf(prop.getDisplayOffsetX()));
		yfield.setText(String.valueOf(prop.getDisplayOffsetY()));
	}

	/**
	 * Create a combo box for binding type
	 */
	private JComboBox<String> createBindingTypeCombo() {
		String[] entries = new String[BindingType.values().length];
		int index=0;
		for(BindingType type : BindingType.values()) {
			entries[index]=type.name();
			index++;
		}
		final JComboBox<String> box = new JComboBox<String>(entries);
		box.setPreferredSize(COMBO_BOX_SIZE);
		return box;
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
		final JComboBox<String> box = new BindingCombo<String>(entries);
		box.setPreferredSize(COMBO_BOX_SIZE);
		return box;
	}
	/** 
	 * Create a special combo box class that does not allow selection of "ENGINE".
	 * It includes a model that will not allow the selection as well as a list
	 * renderer that grays it out.
	 */
	private class BindingCombo<E> extends JComboBox<E> {
		private static final long serialVersionUID = 1L;
		@SuppressWarnings("unchecked")
		public BindingCombo(E[] strings) {
			super(strings);
			setRenderer(new BindingTypeRenderer<E>());
		}
		@Override
		public ComboBoxModel<E> getModel() { return new BindingComboModel<E>(); }
	
	}
	/** 
	 * Create a special combo box model that does not allow selection of "ENGINE"
	 * @see http://stackoverflow.com/questions/11895822/disabling-individual-jcombobox-items
	 *
	 */
	private class BindingComboModel<T>  extends DefaultComboBoxModel<T> {
		private static final long serialVersionUID = 1L;

		public BindingComboModel() {
	    }

	    @Override
	    public void setSelectedItem(Object obj) {

	        if (obj != null) {
	            if (!obj.toString().equals(BindingType.ENGINE.name())) {
	                super.setSelectedItem(obj);
	            }

	        } else {

	            super.setSelectedItem(obj);
	        }
	    }
	}
	
	/**
	 * Show the "ENGINE" selection as disabled.
	 * @see  http://stackoverflow.com/questions/14852539/disable-jcombobox-items-at-runtime
	 */
	public class BindingTypeRenderer<E> extends BasicComboBoxRenderer {
		private static final long serialVersionUID = 1L;
		private Color disabledColor = Color.lightGray;

		/**
		 * Constructs a new renderer for a JComboBox which enables/disables items
		 * based upon the parameter model.
		 * 
		 * @param enabled
		 */
		public BindingTypeRenderer(){
		    super();        
		}

		/**
		 * Sets the color to render disabled items.
		 *  
		 * @param disabledColor
		 */
		public void setDisabledColor(Color disabledColor){
		    this.disabledColor = disabledColor;
		}

		/**
		 * Custom implementation to color items as enabled or disabled.
		 */
		@Override
		@SuppressWarnings("rawtypes")
		public Component getListCellRendererComponent( JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
		    Component c = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

		    boolean enabled = (value!=null && !value.toString().equals(BindingType.ENGINE.name()));
		    if (!enabled) {
		        if (isSelected) {
		            c.setBackground(UIManager.getColor("ComboBox.background"));
		        } else {
		            c.setBackground(super.getBackground());
		        }
		        c.setForeground(disabledColor);
		    } else {
		        c.setBackground(super.getBackground());
		        c.setForeground(super.getForeground());
		    }

		    return c;
		}
	}
}
	