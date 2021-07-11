package com.ils.blt.designer.workspace;

import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.serializable.SerializableBlock;

/**
 * This is a special class that extends a ProcessBlockView to create a version that
 * displays a property value of a reference block.
 */
public class BlockAttributeView extends ProcessBlockView {

	/**
	 * Constructor: Used when a new block is created from the palette. 
	 *              Create a pseudo-random name.
	 */
	public BlockAttributeView(BlockDescriptor descriptor) {
		super(descriptor);
	}
	public BlockAttributeView(SerializableBlock sb) {
		super(sb);
	}
	
}

