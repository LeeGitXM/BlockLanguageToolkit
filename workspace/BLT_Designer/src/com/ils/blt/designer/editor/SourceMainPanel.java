/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;

import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.client.images.ImageLoader;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This is the "home" panel for a SourceConnection block. It includes a
 * "pseudo" property to list SourceConnections.
 */
@SuppressWarnings("serial")
public class SourceMainPanel extends MainPanel {
	private final static String TAG = "SourceMainPanel";
	public final static String PROP_NAME = "Associated Sink";
	private SerializableBlockStateDescriptor sink;
	private BlockProperty property = null;  // THe "pseudo" property
	
	public SourceMainPanel(DesignerContext context,BlockPropertyEditor editor,ProcessBlockView blk, DiagramWorkspace wrkspc) {
		super(context,editor,blk,wrkspc);
		sink = null;
	}
	
	/**
	 * Sneak a "pseudo" property in before the main list
	 */
	public void initialize() {
		// Determine the sink block by the tag, if set
		String sinkName = "";
		BlockProperty prop = block.getProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH);
		String tagPath = prop.getBinding();
		if( tagPath!=null ) {
			List<SerializableBlockStateDescriptor> blocks = bpe.getRequestHandler().listBlocksForTag(tagPath);
			for(SerializableBlockStateDescriptor blk:blocks) {
				if(blk.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK)) {
					sinkName = blk.getName();
					sink = blk;
					break;
				}
			}
		}
		property = new BlockProperty(PROP_NAME,sinkName,PropertyType.STRING,true);
		PropertyPanel propertyPanel = new PropertyPanel(context,this,block,property,workspace);
		add(propertyPanel,"skip,growx,push,gaptop 0,gapbottom 0");
		panelMap.put(property.getName(), propertyPanel);
		super.initialize();
	}
	
	
	
	/**
	 * Create a special button for selecting from a list of sink blocks.
	 * @param prop 
	 */
	public JButton createConfigurationButton(final BlockProperty prop) {
		if( !prop.getName().endsWith(PROP_NAME)) return super.createConfigurationButton(prop);
		
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
					bpe.updatePanelForProperty(BlockEditConstants.SOURCE_EDIT_PANEL,prop);
					setSelectedPane(BlockEditConstants.SOURCE_EDIT_PANEL);
				}
			});
		}
		return btn;
	}
	
	public void updatePanelValue(String propertyName,Object val) {
		if( propertyName.equalsIgnoreCase(PROP_NAME)) {
			property.setValue(val);
		}
		super.updatePanelValue(propertyName, val);
	}
}