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
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class emits a specified signal.
 */
@ExecutableBlock
public class Command extends AbstractProcessBlock implements ProcessBlock {
	private static String TAG = "Command";
	private Signal command = new Signal();
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Command() {
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
		initialize();
	}

	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Signal");
		
		BlockProperty commandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_COMMAND,command.getCommand(),PropertyType.STRING,false);
		properties.put(BlockConstants.BLOCK_PROPERTY_COMMAND, commandProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		anchors.add(output);
	}
	

	/**
	 * As soon as a fresh value arrives, trigger the output signal.
	 * Retain the timestamp.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		
		if( qv.getQuality().isGood() && !isLocked())  {
			QualifiedValue result = new BasicQualifiedValue(command,qv.getQuality(),qv.getTimestamp());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
		}
	}
	
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_COMMAND)) {
			command.setCommand(event.getNewValue().toString());
		}
		
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
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