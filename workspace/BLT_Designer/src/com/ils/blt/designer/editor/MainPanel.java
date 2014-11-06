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

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JSeparator;

import net.miginfocom.swing.MigLayout;

import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This is the "home" panel that first appears when editing a block. It contains
 * a static core attribute display, and then a list of properties.
 * 
 * Note: block properties have been initialized in ProcessDiagramView.initBlockProperties()
 */
@SuppressWarnings("serial")
public class MainPanel extends BasicEditPanel {
	private final static String TAG = "MainPanel";
	private final ProcessBlockView block;
	private final Map<String,PropertyPanel> panelMap;

	public MainPanel(DesignerContext context,BlockPropertyEditor editor,ProcessBlockView blk) {
		super(editor);
		this.block = blk;
		this.panelMap = new HashMap<String,PropertyPanel>();

		setLayout(new MigLayout("top,flowy,ins 2,gapy 0:10:15","","[top]0[]"));
		JPanel panel = new CorePropertyPanel(block);
		add(panel,"grow,push");

		log.debugf("%s.mainPanel: - editing %s (%s)",TAG,block.getId().toString(),block.getClassName());
		PropertyPanel propertyPanel = null;
		// Now fill the editor. We use the same panel class for each property.
		for(BlockProperty property:block.getProperties()) {
			// We have gotten null from serialization problems ...
			if( property==null || property.getName()==null) continue;
			log.debugf("%s.init: - creating panel for property %s",TAG,property.getName());
			propertyPanel = new PropertyPanel(context,this,blk,property);
			add(propertyPanel,"skip,growx,push,gaptop 0,gapbottom 0");
			panelMap.put(property.getName(), propertyPanel);

		}
		// "Sacrificial" row - else we had trouble scrolling to the bottom
		JSeparator separator = new JSeparator();
		add(separator,"span,growy");
	}
	
	public void notifyOfMajorChange() {parent.notifyOfMajorChange();}    // Dirties block
	public void notifyOfPropertyChange(BlockProperty prop) {parent.notifyOfPropertyChange(prop);} 

	/**
	 * Iterate over panels and close any subscriptions
	 */
	public void shutdown() {
		for( PropertyPanel p:panelMap.values()) {
			p.unsubscribe();
		}
	}
	/**
	 * This is the property summary on the main panel.
	 * @param prop
	 */
	public void updatePanelForProperty(BlockProperty prop ) {
		log.tracef("%s.updatePanelForProperty: %s", TAG,prop.getName());
		PropertyPanel pp = panelMap.get(prop.getName());
		if( pp!=null ) pp.update();
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
	
	// =============================== Component Creation Methods ================================
	/**
	 * Create a button that slides to a panel allowing choices of binding and
	 * other property options.
	 * The button has only an image.
	 * @param prop 
	 */
	public JButton createConfigurationButton(final BlockProperty prop) {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/data_link.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BlockEditConstants.BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn.setIcon(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BlockEditConstants.BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e){
					updatePanelForProperty(BlockEditConstants.CONFIGURATION_PANEL,prop);
					setSelectedPane(BlockEditConstants.CONFIGURATION_PANEL);
				}
			});
			if( prop.getType().equals(PropertyType.LIST) ) btn.setEnabled(false);
		}
		return btn;
	}
	/**
	 * Create a button that navigates to the proper editor. This is required only
	 * for LIST datatype and for values that are bound to a tag.
	 */
	public JButton createEditButton(final BlockProperty prop) {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/pencil.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BlockEditConstants.BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn.setIcon(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BlockEditConstants.BUTTON_SIZE);
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
					// Use special editor for list types
					else if( prop.getType().equals(PropertyType.LIST) ) {
						log.debugf("%s.editButton actionPerformed for property %s (%s)",TAG,prop.getName(),prop.getType());
						updatePanelForProperty(BlockEditConstants.LIST_EDIT_PANEL,prop);
						setSelectedPane(BlockEditConstants.LIST_EDIT_PANEL);
					}
					else {
						// Do nothing - shouldn't happen
					}
				}
			});
		}
		else {
			log.warnf("%s.editButton Unable to load image for %s (%s)",TAG,prop.getName(),ICON_PATH);
		}
		return btn;
	}
	
	/**
	 * Create a button that navigates to the proper editor for
	 * a block's name.
	 */
	private JButton createNameEditButton(final ProcessBlockView blk) {
		JButton btn = new JButton();
		final String ICON_PATH  = "Block/icons/editor/pencil.png";
		Image img = ImageLoader.getInstance().loadImage(ICON_PATH ,BlockEditConstants.BUTTON_SIZE);
		if( img !=null) {
			Icon icon = new ImageIcon(img);
			btn.setIcon(icon);
			btn.setMargin(new Insets(0,0,0,0));
			btn.setOpaque(false);
			btn.setBorderPainted(false);
			btn.setBackground(getBackground());
			btn.setBorder(null);
			btn.setPreferredSize(BlockEditConstants.BUTTON_SIZE);
			btn.addActionListener(new ActionListener() {
				// Determine the correct panel, depending on the property type
				public void actionPerformed(ActionEvent e){
					updatePanelForBlock(BlockEditConstants.NAME_EDIT_PANEL,blk);
					setSelectedPane(BlockEditConstants.NAME_EDIT_PANEL);
				}
			});
		}
		else {
			log.warnf("%s.createNameEditButton icon not found(%s)",TAG,ICON_PATH);
		}
		return btn;
	}	
}