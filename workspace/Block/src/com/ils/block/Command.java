/**
 *   (c) 2014-2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.Signal;

/**
 * This class emits a specified signal. It is a generalization of the Reset
 * which emits a fixed command, "Reset". The default command for this block is
 * "Evaluate".
 */
@ExecutableBlock
public class Command extends Reset implements ProcessBlock {
	private static String TAG = "Command";

	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Command() {
		command = new Signal(BlockConstants.COMMAND_EVALUATE,"","");
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
	public Command(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		command = new Signal(BlockConstants.COMMAND_EVALUATE,"","");
		initialize();
	}

	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Command");
		
		BlockProperty commandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_COMMAND,command.getCommand(),PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_COMMAND, commandProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		anchors.add(output);
	}
	


	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.debugf("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_COMMAND)) {
			command.setCommand(event.getNewValue().toString());
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TRIGGER)) {
			try {
				trigger = TruthValue.valueOf(event.getNewValue().toString().toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.propertyChange: Trigger must be a TruthValue (%s)",TAG,iae.getMessage());
			}
		}
		// Buffer size handled in superior method
		else if( !propertyName.equals(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE) ){
			log.warnf("%s.propertyChange:Unrecognized property (%s)",getName(),propertyName);
		}
	}
	


	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/flash.png");
		prototype.setPaletteLabel("Command");
		prototype.setTooltipText("Transmit a fixed command on a signal connection");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/flash.png");
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}

}