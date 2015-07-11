/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.blt.common.annotation.ExecutableBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;

/**
 * Holds any free-form text notes the user wants to enter. Can be html, since
 * we use a JLabel to display.
 */
@ExecutableBlock
public class Note extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "Note";
	public static final int DEFAULT_WIDTH = 100;
	public static final int DEFAULT_HEIGHT = 25;
	public static String  initialString = "<html><h3>header</h3> body of message <br/second line </html>";

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Note() {
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
	public Note(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}

	/**
	 * Handle a change to the text
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
	}
	@Override
	public void notifyOfStatus() {}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName(TAG);
		BlockProperty text = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TEXT,"Your note here", PropertyType.STRING, true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TEXT, text);		
		BlockProperty width = new BlockProperty(BlockConstants.BLOCK_PROPERTY_WIDTH, Integer.valueOf(DEFAULT_WIDTH), PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_WIDTH, width);		
		BlockProperty height = new BlockProperty(BlockConstants.BLOCK_PROPERTY_HEIGHT, Integer.valueOf(DEFAULT_HEIGHT), PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_HEIGHT, height);		
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/note.png");
		prototype.setPaletteLabel("Note");
		prototype.setTooltipText("Any notes you would care to enter.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.NOTE);
		desc.setEditorClass("com.ils.blt.designer.config.NoteTextEditor");
		desc.setPreferredHeight(DEFAULT_HEIGHT);
		desc.setPreferredWidth(DEFAULT_WIDTH);
	}
	
}