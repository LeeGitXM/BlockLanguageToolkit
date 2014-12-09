/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.UUID;

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
 * An Entry Connection is for use only on an EncapsulatedDiagram. Its function is to be
 * a surrogate for the associated anchor point in the parent Encapsulation block.
 * This block is automatically generated. It does NOT appear on the block palette.
 */
public class EntryConnection extends AbstractProcessBlock implements ProcessBlock {
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public EntryConnection() {
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
	public EntryConnection(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("SourceConnection");
		
		// Define a single input - we get an input from the connection and broadcast it.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
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
			notifyOfStatus(qv);
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
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