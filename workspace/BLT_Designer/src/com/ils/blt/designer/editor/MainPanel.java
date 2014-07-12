/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TimeUtility;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;

/**
 * This is the panel that first appears when editing a block. It contains
 * a static core attribute display, and then a list of properties.
 * 
 * Note: block properties have been initialized in ProcessDiagramView.initBlockProperties()
 */
@SuppressWarnings("serial")
public class MainPanel extends BasicEditPanel {
	private final static String TAG = "MainPanel";
	private final ProcessBlockView block;
	private final Map<String,PropertyPanel> panelMap;
	private final UtilityFunctions fncs;
	
	public MainPanel(BlockPropertyEditor editor,ProcessBlockView blk) {
		super(editor);
		this.block = blk;
		this.panelMap = new HashMap<String,PropertyPanel>();
		this.fncs = new UtilityFunctions();
		
		setLayout(new MigLayout("top,flowy,ins 2,gapy 0:10:15","","[top]0[]"));
		JPanel panel = new CorePropertyPanel(block);
		add(panel,"grow,push");

		log.infof("%s.mainPanel: - editing %s (%s)",TAG,block.getId().toString(),block.getClassName());
		PropertyPanel propertyPanel = null;
		// Now fill the editor. We use the same edit panel class for all properties
		for(BlockProperty property:block.getProperties()) {
			// We have gotten null from serialization problems ...
			if( property==null || property.getName()==null) continue;
			log.infof("%s.init: - creating editor for property %s",TAG,property.getName());
			propertyPanel = new PropertyPanel(property);
			add(propertyPanel,"skip,growx,push,gaptop 0,gapbottom 0");
			panelMap.put(property.getName(), propertyPanel);
		
		}
		// "Sacrificial" row - else we had trouble scrolling to the bottom
		JSeparator separator = new JSeparator();
		add(separator,"span,growy");
	}

	public void updatePanelForProperty(BlockProperty prop ) {
		PropertyPanel pp = panelMap.get(prop.getName());
		if( pp!=null ) pp.updateForProperty(prop);
	}
	/**
	 * These properties are present in every block.
	 * class, label, state, statusText
	 */
	private class CorePropertyPanel extends JPanel {
		private static final String columnConstraints = "[para]0[]0[]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "[para]0[]0[]";

		public CorePropertyPanel(ProcessBlockView blk) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Core");
			add(createLabel("Name"),"skip");
			add(createTextField(blk.getName()),"growx,pushx");
			add(createNameEditButton(blk),"w :25:");
			add(createLabel("Class"),"newline,skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
	}
	/**
	 * A property panel is control panel for a single property. By default it contains:
	 *    First line --
	 *    	  title label
	 *    Next line --
	 *        valueBox     - text box with the current value
	 *        edit button  - go to editor screen to change value
	 *        binding btn  - go to separate screen to determine binding    
	 */
	private class PropertyPanel extends JPanel {
		private static final String columnConstraints = "";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "";
		private final JTextField bindingDisplayField;
		private final JTextField valueDisplayField;
		
		public PropertyPanel(BlockProperty prop) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));     // 3 cells across
			if( prop.getType().equals(PropertyType.TIME) ) {
				TimeUnit tu = TimeUtility.unitForValue(fncs.coerceToDouble(prop.getValue()));
				addSeparator(this,prop.getName()+" ~ "+tu.name().toLowerCase());
			}
			else {
				addSeparator(this,prop.getName());
			}
			// Now the second line.
			valueDisplayField = createValueDisplayField(prop);
			add(valueDisplayField,"skip,growx,push");
			add(createEditButton(prop),"w :25:");
			add(createConfigurationButton(prop),"w :25:,wrap");
			// If the BindingType is TAG, display the binding
			bindingDisplayField = createBindingDisplayField(prop);
			if( prop.getBindingType().equals(BindingType.TAG_MONITOR) ||
				prop.getBindingType().equals(BindingType.TAG_READ) ||
				prop.getBindingType().equals(BindingType.TAG_READWRITE) ||
				prop.getBindingType().equals(BindingType.TAG_WRITE)	) {
				add(bindingDisplayField,"skip,growx,push");
			}
		}
		// Update the panel for new property data
		public void updateForProperty(BlockProperty property) {
			log.infof("%s.updateForProperty: property %s, raw value=",TAG,property.getName(),property.getValue().toString());
			valueDisplayField.setText(fncs.coerceToString(property.getValue()));
			if( property.getBindingType().equals(BindingType.TAG_MONITOR) ||
				property.getBindingType().equals(BindingType.TAG_READ) ||
				property.getBindingType().equals(BindingType.TAG_READWRITE) ||
				property.getBindingType().equals(BindingType.TAG_WRITE)	) {
				bindingDisplayField.setText(fncs.coerceToString(property.getBinding()));
			}
		}
	}
	
	// =============================== Component Creation Methods ================================
	/**
	 * Create a button that slides to a panel allowing choices of binding and
	 * other property options.
	 * The button has only an image.
	 * @param prop 
	 */
	private JButton createConfigurationButton(final BlockProperty prop) {
		JButton btn = null;
		final String ICON_PATH  = "Block/icons/editor/data_link.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn = new JButton(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e){
					updatePanelForProperty(BlockEditConstants.CONFIGURATION_PANEL,prop);
					setSelectedPane(BlockEditConstants.CONFIGURATION_PANEL);
				}
			});
		}
		return btn;
	}
	/**
	 * Create a button that navigates to the proper editor.
	 */
	private JButton createEditButton(final BlockProperty prop) {
		JButton btn = null;
		final String ICON_PATH  = "Block/icons/editor/pencil.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn = new JButton(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BUTTON_SIZE);
			btn.setEnabled(prop.isEditable());
			btn.addActionListener(new ActionListener() {
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
					if( prop.getBindingType().equals(BindingType.TAG_MONITOR)   ||
						prop.getBindingType().equals(BindingType.TAG_READ)   ||
						prop.getBindingType().equals(BindingType.TAG_READWRITE) ||
						prop.getBindingType().equals(BindingType.TAG_WRITE)	 )  {
						updatePanelForProperty(BlockEditConstants.TAG_BROWSER_PANEL,prop);
						setSelectedPane(BlockEditConstants.TAG_BROWSER_PANEL);
					}
					else if( prop.getBindingType().equals(BindingType.ENGINE)) {
						;// Do nothing
					}
					// Use special editor for the enumerated types
					else if( prop.getType().equals(PropertyType.BOOLEAN) ||
							 prop.getType().equals(PropertyType.DISTRIBUTION) ||
							 prop.getType().equals(PropertyType.SCOPE)	      ||
							 prop.getType().equals(PropertyType.TRUTHVALUE)      ){
						log.infof("%s.editButton actionPerformed for property %s (%s)",TAG,prop.getName(),prop.getType());
						updatePanelForProperty(BlockEditConstants.ENUM_EDIT_PANEL,prop);
						setSelectedPane(BlockEditConstants.ENUM_EDIT_PANEL);
					}
					else {
						log.infof("%s.editButton actionPerformed for property %s (%s)",TAG,prop.getName(),prop.getType());
						updatePanelForProperty(BlockEditConstants.VALUE_EDIT_PANEL,prop);
						setSelectedPane(BlockEditConstants.VALUE_EDIT_PANEL);
					}
				}
			});
		}
		return btn;
	}
	
	/**
	 * Create a button that navigates to the proper editor for
	 * a block's name.
	 */
	private JButton createNameEditButton(final ProcessBlockView blk) {
		JButton btn = null;
		final String ICON_PATH  = "Block/icons/editor/pencil.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn = new JButton(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
					updatePanelForBlock(BlockEditConstants.NAME_EDIT_PANEL,blk);
					setSelectedPane(BlockEditConstants.NAME_EDIT_PANEL);
				}
			});
		}
		return btn;
	}
	/**
	 * Create a text box for the binding field. This is read-only.
	 */
	private JTextField createBindingDisplayField(final BlockProperty prop) {	
		Object val = prop.getBinding();
		if(val==null) val = "";
		final JTextField field = new JTextField(val.toString());
		field.setEditable(false);
		field.setEnabled(false);
		return field;
	}
	/**
	 * Create a text box for the value field. This is read-only.
	 */
	private JTextField createValueDisplayField(final BlockProperty prop) {	
		Object val = fncs.coerceToString(prop.getValue());
		if(val==null) val = "";
		final JTextField field = new JTextField(val.toString());
		field.setEditable(false);
		field.setEnabled(false);
		return field;
	}
}