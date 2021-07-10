/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;

/**
 * This is a pseudo-block that displays the current value of another, associated
 * block. The "value" property is what is displayed.
 */
@ExecutableBlock
public class AttributeDisplay extends AbstractProcessBlock implements ProcessBlock {
	private static final String CLSS = "AttributeDisplay";
	public static final int DEFAULT_WIDTH = 100;
	public static final int DEFAULT_HEIGHT = 25;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public AttributeDisplay() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "Text".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public AttributeDisplay(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}

	/**
	 * Handle a change to the property value
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
	}
	@Override
	public void notifyOfStatus() {}
	@Override
	public void propagate() {}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName(CLSS);
		BlockProperty blockId = new BlockProperty(BlockConstants.BLOCK_PROPERTY_BLOCK_ID,"The id of the block to be displayed", PropertyType.STRING, true);
		setProperty(BlockConstants.BLOCK_PROPERTY_BLOCK_ID, blockId);	
		BlockProperty property = new BlockProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY,"The property to be displayed", PropertyType.STRING, true);
		setProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY, property);
		BlockProperty value = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"The current value of the block property", PropertyType.STRING, true);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, value);	
		BlockProperty width = new BlockProperty(BlockConstants.BLOCK_PROPERTY_WIDTH, Integer.valueOf(DEFAULT_WIDTH), PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_WIDTH, width);		
		BlockProperty height = new BlockProperty(BlockConstants.BLOCK_PROPERTY_HEIGHT, Integer.valueOf(DEFAULT_HEIGHT), PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_HEIGHT, height);		
		BlockProperty backgroundColor = new BlockProperty(BlockConstants.BLOCK_PROPERTY_BACKGROUND_COLOR, "TRANSPARENT", PropertyType.COLOR,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_BACKGROUND_COLOR, backgroundColor);		
	}
	
	/**
	 * Augment the palette prototype for this block class. The block does not appear on the paletter.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("");
		prototype.setPaletteLabel("");
		prototype.setTooltipText("");
		prototype.setTabName(BlockConstants.PALETTE_TAB_NONE);

		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ATTRIBUTE);
		desc.setPreferredHeight(DEFAULT_HEIGHT);
		desc.setPreferredWidth(DEFAULT_WIDTH);
	}
	
}