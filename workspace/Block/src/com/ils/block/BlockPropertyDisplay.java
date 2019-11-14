/**
 *   (c) 2019  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * Display a single property of another block.
 */
@ExecutableBlock
public class BlockPropertyDisplay extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "PropertyDisplay";
	public static final int DEFAULT_WIDTH = 120;
	public static final int DEFAULT_HEIGHT = 15;
	protected BlockProperty text = null;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public BlockPropertyDisplay() {
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
	public BlockPropertyDisplay(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		
		// Overwrite blocks stored before 10/01/2015
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEditorClass("com.ils.blt.designer.config.BlockPropertyEditor");
	}


	@Override
	public void propagate() {}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName(TAG);
		// Property property is to designate what property of the connected block this shows
//		BlockProperty property = new BlockProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY,"", PropertyType.STRING, true);
//		setProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY, property);		

		text = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TEXT,"",PropertyType.STRING,false);
		text.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_TEXT, text);

		BlockProperty width = new BlockProperty(BlockConstants.BLOCK_PROPERTY_WIDTH, Integer.valueOf(DEFAULT_WIDTH), PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_WIDTH, width);		
		BlockProperty height = new BlockProperty(BlockConstants.BLOCK_PROPERTY_HEIGHT, Integer.valueOf(DEFAULT_HEIGHT), PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_HEIGHT, height);		
		BlockProperty prefix = new BlockProperty(BlockConstants.BLOCK_PROPERTY_PREFIX,"", PropertyType.STRING, true);
		setProperty(BlockConstants.BLOCK_PROPERTY_PREFIX, prefix);		
		BlockProperty suffix = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SUFFIX,"", PropertyType.STRING, true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SUFFIX, suffix);		
		BlockProperty backgroundColor = new BlockProperty(BlockConstants.BLOCK_PROPERTY_BACKGROUND_COLOR, "TRANSPARENT", PropertyType.COLOR,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_BACKGROUND_COLOR, backgroundColor);		
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/note.png");
		prototype.setPaletteLabel("PropertyDisplay");
		prototype.setTooltipText("Single property of a block");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		
		
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.PROPERTY_DISPLAY);
		desc.setEditorClass("com.ils.blt.designer.config.PropertyDisplayEditor");
		desc.setPreferredHeight(DEFAULT_HEIGHT);
		desc.setPreferredWidth(DEFAULT_WIDTH);
	}
	
}