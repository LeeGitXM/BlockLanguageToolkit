/**
 *   (c) 2014-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

import net.miginfocom.swing.MigLayout;

/**
 * This is the "home" panel that first appears when editing a block. It contains
 * a static core attribute display, and then a list of properties.
 * 
 * Note: block properties have been initialized in ProcessDiagramView.initBlockProperties()
 */
@SuppressWarnings("serial")
public class MainPanel extends BasicEditPanel {
	private final static String CLSS = "MainPanel";
	private static final boolean DEBUG = false;
	protected final ProcessBlockView block;
	protected final Map<String,PropertyPanel> panelMap;
	protected final CorePropertyPanel corePanel;
	protected final DesignerContext context;
	protected final DiagramWorkspace workspace;
	protected final BlockPropertyEditor bpe;

	public MainPanel(DesignerContext context, BlockPropertyEditor editor, ProcessBlockView blk, DiagramWorkspace wrkspc) {
		super(editor);
		this.bpe = editor;
		this.block = blk;
		this.panelMap = new HashMap<String,PropertyPanel>();
		this.corePanel = new CorePropertyPanel(block);
		this.context = context;
		this.workspace = wrkspc;
		// Always display the core panel
		setLayout(new MigLayout("top,flowy,ins 2,gapy 0:10:15","","[top]0[]"));
		add(corePanel,"grow,push");
	}
	// This must be called after the constructor in order to lay out the components
	public void initialize() {
		if(DEBUG)log.infof("%s.mainPanel: - editing %s (%s)",CLSS,block.getId().toString(),block.getClassName());
		PropertyPanel propertyPanel = null;
		// Now fill the editor. We use the same panel class for each property.
		for(BlockProperty property:block.getProperties()) {
			// We have gotten null from serialization problems ...
			if( property==null || property.getName()==null) continue;
			if( property.getName().equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_NAME)) continue;  // Edit name in CoreProperty panel
			propertyPanel = new PropertyPanel(context,this,block,property,workspace);
			add(propertyPanel,"skip,growx,push,gaptop 0,gapbottom 0");
			panelMap.put(property.getName(), propertyPanel);
		}
		// "Sacrificial" row - else we had trouble scrolling to the bottom
		JSeparator separator = new JSeparator();
		add(separator,"span,growy");
	}
	public BlockPropertyEditor getBlockPropertyEditor() { return this.bpe; }

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
	 * @param pbv block view
	 */
	public void updateCorePanel(ProcessBlockView pbv) {
		 corePanel.updatePanelForBlock(pbv);
	}
	/**
	 * This is the property summary on the main panel.
	 * @param prop
	 */
	public void updatePanelForProperty(BlockProperty prop ) {
		log.infof("%s.updatePanelForProperty: %s = %s", CLSS,prop.getName(),prop.getValue().toString());
		PropertyPanel pp = panelMap.get(prop.getName());
		if( pp!=null ) pp.updatePanelUI();
	}
	
	public void updatePanelValue(String propertyName,Object val) {
		log.infof("%s.updatePanelValue: %s = %s", CLSS,propertyName,val.toString());
		PropertyPanel pp = panelMap.get(propertyName);
		if( pp!=null ) pp.valueChange(new BasicQualifiedValue(val));
	}
	/**
	 * These properties are present in every block.
	 * class, label, state, statusText.
	 */
	public class CorePropertyPanel extends JPanel implements ChangeListener,FocusListener  {
		private static final String columnConstraints = "[para]0[]0[]";
		private static final String layoutConstraints = "ins 2";
		private static final String rowConstraints = "[para]0[]0[]";
		private final JTextField nameField;

		public CorePropertyPanel(ProcessBlockView blk) {
			setLayout(new MigLayout(layoutConstraints,columnConstraints,rowConstraints));
			addSeparator(this,"Core");
			add(createLabel("Name"),"skip");
			nameField = createTextField(blk.getName());
			nameField.setEditable(true);
			nameField.addFocusListener(this);
			block.addChangeListener(this);
			add(nameField,"growx,pushx");
			add(createLabel("Class"),"newline,skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
		}
		
		public void saveName() {
			String oldName = block.getName();
			if(oldName.equals(nameField.getText())){
				log.infof("%s.saveName() the name was unchanged", CLSS);
			} 
			else{
				log.infof("%s.saveName(). changed name from %s to %s", CLSS, block.getName(), nameField.getText());
				block.setName(nameField.getText());
				// Make the diagram dirty (mustard) since we aren't saving automatically - PAH 07/15/2021
				bpe.setDiagramDirty();
				bpe.getDiagram().updateNotificationHandlerForSave();
			}
			
			// Sources and Sinks are correlated simply by their names. In order to keep the correlation
			// as synchronized as possible, when we change the name of a sink, we will change the block
			// names of the corresponding source(s). However, actual bindings and tags are not modified until the 
			// project is saved.
			
			
		}
		
		// return the name of the appropriate tag provider
		private String getProvider() {
			DiagramState state = bpe.getDiagram().getState();
			String provider = (state.equals(DiagramState.ISOLATED)?
					bpe.getRequestHandler().getProjectIsolationTagProvider(context.getProjectName()):
					bpe.getRequestHandler().getProjectProductionTagProvider(context.getProjectName()));
			return provider;
		}
		// Replace the last element of path with name
		private String renamePath(String name,String path) {
			int index = path.lastIndexOf("/");
			if( index>0 ) {
				path = path.substring(0, index+1);
				path = path + name;
			}
			return path;
		}

		public void updatePanelForBlock(ProcessBlockView pbv) {
			nameField.setText(pbv.getName());
		}
		
		@Override
		public void finalize()  {
			block.removeChangeListener(this);
		}
		// --------------------------------- Change Listener ----------------------------
		// When we get an event, read the name from the block

		@Override
		public void stateChanged(ChangeEvent e) {
			log.infof("CorePropertyPanel.stateChanged: name now %s (was? %s)",e.getSource().getClass().getCanonicalName(),block.getName());
			nameField.setText(block.getName());
		}
		// ============================================== Focus listener ==========================================
		@Override
		public void focusGained(FocusEvent event) {
		}
		@Override
		public void focusLost(FocusEvent event) {
			saveName();
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
		final String ICON_PATH  = "Block/icons/editor/data.png";
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
					bpe.updatePanelForProperty(BlockEditConstants.CONFIGURATION_PANEL,prop);
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
		final String ICON_PATH  = "Block/icons/editor/data_edit.png";
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
						bpe.updatePanelForProperty(BlockEditConstants.TAG_BROWSER_PANEL,prop);
						setSelectedPane(BlockEditConstants.TAG_BROWSER_PANEL);
					}
					// Use special editor for list types
					else if( prop.getType().equals(PropertyType.LIST) ) {
						log.debugf("%s.editButton actionPerformed for property %s (%s)",CLSS,prop.getName(),prop.getType());
						bpe.updatePanelForProperty(BlockEditConstants.LIST_EDIT_PANEL,prop);
						setSelectedPane(BlockEditConstants.LIST_EDIT_PANEL);
					}
					else {
						// Do nothing - shouldn't happen
					}
				}
			});
		}
		else {
			log.warnf("%s.editButton Unable to load image for %s (%s)",CLSS,prop.getName(),ICON_PATH);
		}
		return btn;
	}	
}