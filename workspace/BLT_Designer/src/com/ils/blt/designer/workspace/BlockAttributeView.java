package com.ils.blt.designer.workspace;

import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.serializable.SerializableBlock;

/**
 * This is a special class that extends a ProcessBlockView to create a version that
 * displays a property value of a reference block.
 */
public class BlockAttributeView extends ProcessBlockView {
	private ProcessDiagramView diagram = null;
	private ProcessBlockView referenceBlock = null;
	private final UtilityFunctions fncs;
	/**
	 * Constructor: Used when a new block is created from the palette. 
	 *              Create a pseudo-random name.
	 */
	public BlockAttributeView(BlockDescriptor descriptor) {
		super(descriptor);
		this.fncs = new UtilityFunctions();
	}
	public BlockAttributeView(SerializableBlock sb) {
		super(sb);
		this.fncs = new UtilityFunctions();
	}
	
	public ProcessDiagramView getDiagram() { return this.diagram; }
	public void setDiagram(ProcessDiagramView dia) { this.diagram = dia; }
	public ProcessBlockView getReferenceBlock() { return this.referenceBlock; }
	public String getPropertyName() {
		return getProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY).getValue().toString();
	}
	public void setPropertyName(String name) {
		BlockProperty prop = new BlockProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY,name,PropertyType.STRING,false);
		this.setProperty(prop);
	}
	public void setReferenceBlock(ProcessBlockView blk) { this.referenceBlock=blk; }
	public int getOffsetX () { return fncs.parseInteger(this.getProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_X).getValue().toString()); }
	public int getOffsetY () { return fncs.parseInteger(this.getProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_Y).getValue().toString()); }
}

