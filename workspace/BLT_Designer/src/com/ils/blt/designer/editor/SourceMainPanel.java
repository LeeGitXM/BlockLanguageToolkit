/**
 *   (c) 2014  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer.editor;

import java.util.List;

import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.blt.designer.workspace.DiagramWorkspace;
import com.ils.blt.designer.workspace.ProcessBlockView;
import com.inductiveautomation.ignition.designer.model.DesignerContext;

/**
 * This is the "home" panel for a SourceConnection block. It includes a
 * "pseudo" property to list SourceConnections.
 */
@SuppressWarnings("serial")
public class SourceMainPanel extends MainPanel {
	private final static String TAG = "SourceMainPanel";

	public SourceMainPanel(DesignerContext context,BlockPropertyEditor editor,ProcessBlockView blk, DiagramWorkspace wrkspc) {
		super(context,editor,blk,wrkspc);
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
			List<SerializableBlockStateDescriptor> blocks = editor.getRequestHandler().listBlocksForTag(tagPath);
			for(SerializableBlockStateDescriptor block:blocks) {
				if(block.getClassName().equals(BlockConstants.BLOCK_CLASS_SINK)) {
					sinkName = block.getName();
					break;
				}
			}
		}
		BlockProperty property = new BlockProperty("Associated Sink",sinkName,PropertyType.STRING,true);
		PropertyPanel propertyPanel = new PropertyPanel(context,this,block,property,workspace);
		add(propertyPanel,"skip,growx,push,gaptop 0,gapbottom 0");
		panelMap.put(property.getName(), propertyPanel);
		super.initialize();
	}
	
}