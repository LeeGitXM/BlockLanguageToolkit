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
	private static final String CLSS = "BlockAttributeView";
	private ProcessDiagramView diagram = null;
	private ProcessBlockView referenceBlock = null;
	private final UtilityFunctions fncs;
	/**
	 * Constructor: Used when a new block is created from the selection dialog. 
	 */
	public BlockAttributeView(BlockDescriptor descriptor) {
		super(descriptor);
		this.fncs = new UtilityFunctions();;
	}
	/**
	 * Constructor used when diagram is de-serialized. 
	 * @param sb
	 */
	public BlockAttributeView(SerializableBlock sb) {
		super(sb);
		this.fncs = new UtilityFunctions();	
	}
	
	public ProcessDiagramView getDiagram() { return this.diagram; }
	public void setDiagram(ProcessDiagramView dia ) { this.diagram = dia; }

	// For these properties, do not worry about case insensitivity.
	public BlockProperty getProperty(String nam) {
		return this.getProperty(nam);
	}
	
	public String getPropertyName()  {return getProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY).getValue().toString(); }
	public void setPropertyName(String name) {
		BlockProperty prop = new BlockProperty(BlockConstants.ATTRIBUTE_PROPERTY_PROPERTY,name,PropertyType.STRING,false);
		this.setProperty(prop);
	}
	
	public int getOffsetX () { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_X).getValue().toString()); }
	public int getOffsetY () { return fncs.parseInteger(getProperty(BlockConstants.ATTRIBUTE_PROPERTY_OFFSET_Y).getValue().toString()); }

	/**.
	 * @return the block referenced by the display
	 */
	public ProcessBlockView getReferenceBlock() {  return this.referenceBlock; }
	public void setReferenceBlock(ProcessBlockView ref) { 
		this.referenceBlock=ref;
	}
}

