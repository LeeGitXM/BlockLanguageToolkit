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
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.control.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * A Source Connection is a special class that receives values directly
 * from the "engine". These values have originated from a Sink Connection
 * of the name configured in this block.
 */
@ExecutableBlock
public class SourceConnection extends AbstractProcessBlock implements ProcessBlock {
	protected static String BLOCK_PROPERTY_SINK_NAME = "SinkName";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SourceConnection() {
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
	public SourceConnection(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("SourceConnection");
		BlockProperty sink = new BlockProperty(BLOCK_PROPERTY_SINK_NAME,"",PropertyType.STRING,true);
		properties.put(BLOCK_PROPERTY_SINK_NAME, sink);
		
		// Define a single output. We receive a value from the "ether" and send it on our output connection
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
	}
	
	/**
	 * A new value has appeared on the input (routed by the engine). Simply pass it on.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		this.state = BlockState.ACTIVE;
		QualifiedValue value = incoming.getValue();
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(value));
		controller.acceptCompletionNotification(nvn);

	}
	
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
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
}