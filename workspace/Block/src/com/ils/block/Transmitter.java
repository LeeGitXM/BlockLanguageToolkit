/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.Signal;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * A transmitter is a special class that propagates broadcast messages directly
 * to the "engine" for later delivery. The value propagated is taken from its 
 * input signal connection. It is possible to define a scopes to limit
 * the number of messages propagated.
 */
@ExecutableBlock
public class Transmitter extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Transmitter";

	
	protected String sql = "";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Transmitter() {
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
	public Transmitter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Transmitter");
		BlockProperty scope = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCOPE,TransmissionScope.LOCAL.toString(),PropertyType.STRING,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SCOPE, scope);
		
		// Define a single input - we get an input from the connection and broadcast it.
		AnchorPrototype input = new AnchorPrototype(BlockConstants.BROADCAST_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.SIGNAL);
		anchors.add(input);
	}
	
	/**
	 * We've got a new value on our input. Turn around and transmit it.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		Signal sig = incoming.getValueAsSignal();
		if( sig!=null ) {
			TransmissionScope scope = TransmissionScope.LOCAL;
			BlockProperty scopeProperty = properties.get(BlockConstants.BLOCK_PROPERTY_SCOPE);
			if(scopeProperty!=null && scopeProperty.getValue()!=null  )
			try {
				scope = TransmissionScope.valueOf(scopeProperty.getValue().toString());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.setValue: %s property has illegal value (%s)",TAG,BlockConstants.BLOCK_PROPERTY_SCOPE,iae.getLocalizedMessage());
			}
			BroadcastNotification broadcast = new BroadcastNotification(getParentId(),scope,sig);
			controller.acceptBroadcastNotification(broadcast);
		}
	}

	@Override
	public void notifyOfStatus() {}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/transmitter.png");
		prototype.setPaletteLabel("Transmitter");
		prototype.setTooltipText("Broadcast signals");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setPreferredHeight(36);
		view.setPreferredWidth(24);    // Leave 6-pixel inset on top and bottom
		view.setIconPath("Block/icons/palette/transmitter.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.ICON);
	}
}