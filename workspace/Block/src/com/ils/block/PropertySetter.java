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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class emits a signal that sets a property of a downstream block.
 */
@ExecutableBlock
public class PropertySetter extends AbstractProcessBlock implements ProcessBlock {
	private static String TAG = "PropertySetter";
	private Signal command = new Signal();
	private TruthValue trigger = TruthValue.TRUE;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public PropertySetter() {
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
	public PropertySetter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}

	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("PropertySetter");
		
		BlockProperty commandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_COMMAND,command.getCommand(),PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_COMMAND, commandProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		anchors.add(output);
	}
	

	/**
	 * When a fresh value arrives that matches the trigger, send the output signal.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		
		if( qv.getQuality().isGood() && !isLocked() && qv.getValue().toString().equalsIgnoreCase(trigger.name()))  {
			QualifiedValue result = new BasicQualifiedValue(command,qv.getQuality(),qv.getTimestamp());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
		}
	}
	/**
	 * When a fresh value arrives that matches the trigger, send the output signal.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {
		Signal sig = sn.getSignal();

		if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_START))  {
			QualifiedValue result = new BasicQualifiedValue(command);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
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
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
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
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
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