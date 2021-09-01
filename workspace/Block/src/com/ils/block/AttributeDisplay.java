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
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * This is a pseudo-block that displays the current value of another, associated
 * block. The "value" property is what is displayed.
 */
@ExecutableBlock
public class AttributeDisplay extends AbstractProcessBlock implements ProcessBlock {
	private static final String CLSS = "AttributeDisplay";
	public static final int ATTRIBUTE_DISPLAY_SEPARATION  = 30; // Default y separation
	public static final String DEFAULT_FONT = "SansSerif";      // Font family - Serif, Dialog,Monospaced
	public static final int DEFAULT_FONT_SIZE = 12;
	public static final int DEFAULT_HEIGHT = 25;
	public static final int DEFAULT_OFFSET_X = 50;
	public static final int DEFAULT_OFFSET_Y = 25;
	public static final int DEFAULT_WIDTH = 100;
	

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
	 * @param parent resource Id identifying the parent of this block (a diagram)
	 * @param block universally unique Id for the block
	 */
	public AttributeDisplay(ExecutionController ec,ProjectResourceId parent,UUID block) {
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
		BlockProperty blockId = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_BLOCK_ID,"The id of the block to be displayed", PropertyType.STRING, false);
		setProperty(BlockConstants.ATTRIBUTE_DISPLAY_BLOCK_ID, blockId);	
		BlockProperty property = new BlockProperty(BlockConstants.ATTRIBUTTE_DISPLAY_PROPERTY,"Name", PropertyType.STRING, false);
		setProperty(BlockConstants.ATTRIBUTTE_DISPLAY_PROPERTY, property);	
		BlockProperty width = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_WIDTH, Integer.valueOf(DEFAULT_WIDTH), PropertyType.INTEGER,true);
		setProperty(BlockConstants.ATTRIBUTE_DISPLAY_WIDTH, width);		
		BlockProperty height = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_HEIGHT, Integer.valueOf(DEFAULT_HEIGHT), PropertyType.INTEGER,true);
		setProperty(BlockConstants.ATTRIBUTE_DISPLAY_HEIGHT, height);		
		BlockProperty backgroundColor = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_BACKGROUND_COLOR, "TRANSPARENT", PropertyType.COLOR,true);
		setProperty(BlockConstants.ATTRIBUTE_DISPLAY_BACKGROUND_COLOR, backgroundColor);
		BlockProperty offsetx = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_X, Integer.valueOf(DEFAULT_WIDTH), PropertyType.INTEGER,true);
		setProperty(BlockConstants.ATTRIBUTE_DISPLAY_WIDTH, offsetx);		
		BlockProperty offsety = new BlockProperty(BlockConstants.ATTRIBUTE_DISPLAY_OFFSET_Y, Integer.valueOf(DEFAULT_HEIGHT), PropertyType.INTEGER,true);
		setProperty(BlockConstants.ATTRIBUTE_DISPLAY_HEIGHT, offsety);
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