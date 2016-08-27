/**
 *   (c) 2014-2016  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Emit a "reset" signal when the input matches the trigger value.
 */
@ExecutableBlock
public class Reset extends AbstractProcessBlock implements ProcessBlock {
	protected Signal command = new Signal();
	protected TruthValue trigger = TruthValue.TRUE;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Reset() {
		initialize();
		initializePrototype();
		command = new Signal(BlockConstants.COMMAND_RESET,"","");
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Reset(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		command = new Signal(BlockConstants.COMMAND_RESET,"","");
	}
	
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Reset");

		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		output.setHint(PlacementHint.B);  // Got wierd behavior if Top
		anchors.add(output);
	}
	
	/**
	 * When a fresh value arrives that matches the trigger, send the output signal.
	 * It works just fine sending the signal down a connection.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		
		if( qv.getQuality().isGood() && !isLocked() && qv.getValue().toString().equalsIgnoreCase(trigger.name()))  {
			lastValue = new BasicQualifiedValue(command,qv.getQuality(),qv.getTimestamp());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
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
			lastValue = new BasicQualifiedValue(command);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}
	/**
	 * This is not called during normal operation. If explicitly called,
	 * we simply propagate the command.
	 */
	@Override
	public void evaluate() {
		lastValue = new BasicQualifiedValue(command);
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus(lastValue);
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
		prototype.setPaletteIconPath("Block/icons/palette/reset.png");
		prototype.setPaletteLabel("Reset");
		prototype.setTooltipText("Send a \"reset\" signal to connected blocks");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/reset.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setStyle(BlockStyle.SQUARE);
		desc.setReceiveEnabled(true);
	}
}