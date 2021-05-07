/**
 *   (c) 2014-2021  ILS Automation. All rights reserved.
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

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BusinessRules;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.sqltags.model.types.DataType;
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
	private final static String TAG = "MainPanel";
	protected final ProcessBlockView block;
	protected final Map<String,PropertyPanel> panelMap;
	protected final CorePropertyPanel corePanel;
	protected final DesignerContext context;
	protected final DiagramWorkspace workspace;
	protected final BlockPropertyEditor bpe;

	public MainPanel(DesignerContext context,BlockPropertyEditor editor,ProcessBlockView blk, DiagramWorkspace wrkspc) {
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
		log.debugf("%s.mainPanel: - editing %s (%s)",TAG,block.getId().toString(),block.getClassName());
		PropertyPanel propertyPanel = null;
		// Now fill the editor. We use the same panel class for each property.
		for(BlockProperty property:block.getProperties()) {
			// We have gotten null from serialization problems ...
			if( property==null || property.getName()==null) continue;
			propertyPanel = new PropertyPanel(context,this,block,property,workspace);
			add(propertyPanel,"skip,growx,push,gaptop 0,gapbottom 0");
			panelMap.put(property.getName(), propertyPanel);
		}
		// "Sacrificial" row - else we had trouble scrolling to the bottom
		JSeparator separator = new JSeparator();
		add(separator,"span,growy");
	}
	public BlockPropertyEditor getBlockPropertyEditor() { return this.bpe; }
	public void saveDiagramClean() {bpe.saveDiagramClean();} 

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
		log.infof("%s.updatePanelForProperty: %s = %s", TAG,prop.getName(),prop.getValue().toString());
		PropertyPanel pp = panelMap.get(prop.getName());
		if( pp!=null ) pp.updatePanelUI();
	}
	
	public void updatePanelValue(String propertyName,Object val) {
		log.infof("%s.updatePanelValue: %s = %s", TAG,propertyName,val.toString());
		PropertyPanel pp = panelMap.get(propertyName);
		if( pp!=null ) pp.valueChange(new BasicQualifiedValue(val));
	}
	/**
	 * These properties are present in every block.
	 * class, label, state, statusText.
	 */
	private class CorePropertyPanel extends JPanel implements ChangeListener,FocusListener  {
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
			add(nameField,"growx,pushx");
			add(createLabel("Class"),"newline,skip");
			add(createTextField(blk.getClassName()),"span,growx");
			add(createLabel("UUID"),"skip");
			add(createTextField(blk.getId().toString()),"span,growx");
			
			// Listen on block name changes
			block.addChangeListener(this);
		}
		
		public void saveName() {
			block.setName(nameField.getText());
			
			// The block has a name and a name property - this keeps them in sync
			BlockProperty nameProp = block.getProperty(BlockConstants.BLOCK_PROPERTY_NAME);
			nameProp.setValue(nameField.getText());
			bpe.saveDiagramClean();    // Update property directly, immediately
			// bpe.saveDiagram(workspace.getActiveDiagram().getResourceId());
			
			// For Sinks we update the associated tag path
			if( block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK) ) {
				BlockProperty prop = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
				String path = prop.getBinding();
				ApplicationRequestHandler handler = bpe.getRequestHandler();
				// If the tag is is in the standard location, rename it
				// otherwise, create a new one. Name must be a legal tag path element.
				if( !BusinessRules.isStandardConnectionsFolder(path) ) {
					String provider = getProvider();
					path = String.format("[%s]%s/%s",provider,BlockConstants.SOURCE_SINK_TAG_FOLDER,nameField.getText());
					handler.createTag(DataType.String,path);
				}
				else {
					handler.renameTag(nameField.getText(), path);
					path = renamePath(nameField.getText(), path);
				}
				prop.setBinding(path);
				
				// Perform similar modification on connected sources
				// Use the scripting interface to handle diagrams besides the current
				// The block name has already changed.
				ProcessDiagramView diagram = bpe.getDiagram();
				for(SerializableBlockStateDescriptor desc:handler.listSourcesForSink(diagram.getId().toString(),block.getId().toString())) {
					SerializableResourceDescriptor rd = handler.getDiagramForBlock(desc.getIdString());
					if( rd==null ) continue;
					log.infof("NameEditPanel.actionPerformed: sink connected to %s",desc.getName());
					handler.setBlockPropertyBinding(rd.getId(), desc.getIdString(),BlockConstants.BLOCK_PROPERTY_TAG_PATH,path);
					handler.renameBlock(rd.getId(), desc.getIdString(), nameField.getText());
					bpe.saveDiagram(rd.getResourceId());
				}
			}
		}
		// return the name of the appropriate tag provider
		private String getProvider() {
			DiagramState state = bpe.getDiagram().getState();
			String provider = (state.equals(DiagramState.ISOLATED)?
					bpe.getRequestHandler().getIsolationTagProvider():
					bpe.getRequestHandler().getIsolationTagProvider());
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
			try {
				super.finalize();
			}
			catch(Throwable t) {}
		}
		// --------------------------------- Change Listener ----------------------------
		// When we get an event, read the name from the block

		@Override
		public void stateChanged(ChangeEvent e) {
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
						log.debugf("%s.editButton actionPerformed for property %s (%s)",TAG,prop.getName(),prop.getType());
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
			log.warnf("%s.editButton Unable to load image for %s (%s)",TAG,prop.getName(),ICON_PATH);
		}
		return btn;
	}	
}