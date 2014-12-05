/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.ConnectionPostNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * A Sink Connection is a special class that propagates values directly
 * to the "engine" for later delivery. This is the only block class that
 * uses the 
 */
@ExecutableBlock
public class SinkConnection extends AbstractProcessBlock implements ProcessBlock {
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SinkConnection() {
		initialize();
		initializePrototype();
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
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("SinkConnection");
		
		// Define a single input - we get an input from the connection and broadcast it.
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		anchors.add(input);
	}
	
	/**
	 * We've got a new value on our input. Turn around and send it to the "engine" along with our name.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		if( qv!=null ) {
			ConnectionPostNotification notification = new ConnectionPostNotification(getParentId(),getName(),qv);
			controller.acceptConnectionPostNotification(notification);
		}
	}

	@Override
	public void notifyOfStatus() {}

	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
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
}