/**
 *   (c) 2014-2021  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.List;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;

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
	
	/**
	 * Make the tag path property non-editable
	 */
	protected void initialize() {
		super.initialize();
		setName("SinkConnection");
		pathProperty.setEditable(false);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/sink.png");
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
	}
	
	/**
	 * Guarantee that the class name matches the constant used throughout
	 * the application to identify a sink.
	 */
	@Override
	public String getClassName() { return BlockConstants.BLOCK_CLASS_SINK; }
	/**
	 * On reset, set the value of the backing tag to "UNSET". This prevents
	 * a refresh of the block to re-propagate the last value.
	 */
	@Override
	public void reset() {
		super.reset();
		if( pathProperty.getBindingType().equals(BindingType.TAG_WRITE)) {
			controller.updateTag(getParentId(),pathProperty.getBinding().toString(), new BasicQualifiedValue("UNSET"));
		}
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
			List<SerializableBlockStateDescriptor> links = controller.listSourcesForSink(
									getParentId().toString(),this.getBlockId().toString());
			if( links.isEmpty() ) {
				summary = String.format("There are no sources linked to this sink block\t");
			}
		}
		return summary;
	}
}