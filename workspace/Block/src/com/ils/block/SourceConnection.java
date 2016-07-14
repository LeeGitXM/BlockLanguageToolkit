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
 * A Source Connection is a special class that receives values directly
 * from a tag that is meant to be logically connected to a SinkConnection. 
 * Connected sources and sinks should share common names.
 */
@ExecutableBlock
public class SourceConnection extends Input implements ProcessBlock {
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SourceConnection() {
		super();
	}

	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public SourceConnection(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
	}
	@Override
	public String getClassName() {return BLTProperties.CLASS_NAME_SOURCE;}
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	protected void initialize() {
		super.initialize();
		setName("SourceConnection");
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/in_connection.png");
		prototype.setPaletteLabel("Source");
		prototype.setTooltipText("Receive data from a sink of the same name");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setPreferredHeight(40);
		desc.setPreferredWidth(50);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setBackground(new Color(127,127,127).getRGB()); // Dark gray
		desc.setStyle(BlockStyle.ARROW);
		desc.setCtypeEditable(true);
		desc.setNameDisplayed(true);
		desc.setNameOffsetX(25);
		desc.setNameOffsetY(45);
	}
	
	/**
	 * In addition to the standard validation, make sure that there is
	 * something from which we can receive an input.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		String summary = super.validate();
		if( summary==null ) {
			List<SerializableBlockStateDescriptor> links = controller.listSinksForSource(getParentId().toString(),getName());
			if( links.isEmpty() ) {
				summary = String.format("There are no sinks linked to this source block\t");
			}
		}
		
		return summary;
	}
}