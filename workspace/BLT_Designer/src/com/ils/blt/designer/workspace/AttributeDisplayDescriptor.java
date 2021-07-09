package com.ils.blt.designer.workspace;

import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;


/**
 * This is a view prototype for an AttributeDisplay block
 */
public class AttributeDisplayDescriptor extends BlockDescriptor {
	private static final String CLSS = "AttributeDisplayDescriptor";
	
	public AttributeDisplayDescriptor() {
		this.blockClass = BlockConstants.BLOCK_CLASS_ATTRIBUTE;
		setStyle(BlockStyle.ATTRIBUTE);
	}
	
	
}
