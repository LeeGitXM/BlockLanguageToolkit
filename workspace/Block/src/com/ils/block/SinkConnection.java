/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.List;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;

/**
 * A Sink Connection is a class that propagates values directly
 * to a memory tag for transfer to a SinkConnection. The tag and
 * block names should be shared among all sources and sinks that
 * are logically connected.
 */
@ExecutableBlock
public class SinkConnection extends Output implements ProcessBlock {
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SinkConnection() {
		super();
	}
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public SinkConnection(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	@Override
	public String getClassName() {return BLTProperties.CLASS_NAME_SINK;}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	protected void initialize() {
		super.initialize();
		setName("SinkConnection");
	}
	
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/out_connection.png");
		prototype.setPaletteLabel("Sink");
		prototype.setTooltipText("Send incoming values off-diagram to source objects of same name");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setPreferredHeight(40);
		desc.setPreferredWidth(50);    // Leave 6-pixel inset on top and bottom
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setBackground(new Color(127,127,127).getRGB()); // Dark gray
		desc.setStyle(BlockStyle.ARROW);
		desc.setCtypeEditable(true);
		desc.setNameDisplayed(true);
		desc.setNameOffsetX(25);
		desc.setNameOffsetY(45);
	}
	
	/**
	 * In addition to the standard validation, make sure that there
	 * is something that will receive the output.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		String summary = super.validate();
		if( summary==null ) {
			List<SerializableBlockStateDescriptor> links = controller.listSourcesForSink(getParentId().toString(),getName());
			if( links.isEmpty() ) {
				summary = String.format("There are no sources linked to this sink block\t");
			}
		}
		return summary;
	}
}