/**
 *   (c) 2014-2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Date;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 *  Explicitly thwart a reset. over a reset, retain previous state.
 */
@ExecutableBlock
public class LogicLatch extends AbstractProcessBlock implements ProcessBlock {
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public LogicLatch() {
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
	public LogicLatch(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		state = TruthValue.UNKNOWN;
		initialize();
	}
	
	/** Start any active monitoring or processing within the block.
	 * Here we propagate an UNKNOWN.
	 */
	@Override
	public void start() { 
		super.start();
		// Initially propagate an UNKNOWN which will have to suffice until we get a TRUE or FALSE
		lastValue = new TestAwareQualifiedValue(timer,state.name());
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus(lastValue);
	}
	/**
	 * Do not call the base-class reset() as this sets outgoing
	 * connection states to UNSET. Here we simply propagate the current value.
	 */
	@Override
	public void reset() {
		recordActivity(Activity.ACTIVITY_RESET,"(does not change value)");
		lastValue = new TestAwareQualifiedValue(timer,state.name());
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus(lastValue);
	}
	
	/**
	 * A new value has appeared on an input anchor. Send it on its way
	 * if TRUE or FALSE. Otherwise do nothing.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		lastValue = vcn.getValue();
		if(lastValue.getQuality().isGood()) {
			TruthValue incoming = qualifiedValueAsTruthValue(lastValue);
			if( incoming.equals(TruthValue.TRUE) || incoming.equals(TruthValue.FALSE)) {
				state = incoming;
				if( !isLocked() ) {
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(lastValue);
				}
			}	
		}
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(new TestAwareQualifiedValue(timer,state.name()));	
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * There are no properties for this class.
	 */
	private void initialize() {
		setName("LogicLatch");
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/latch.png");
		prototype.setPaletteLabel("LogicLatch");
		prototype.setTooltipText("Retain first value received until reset");
		prototype.setTabName(BlockConstants.PALETTE_TAB_LOGIC);
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setEmbeddedIcon("Block/icons/embedded/latch.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.SQUARE);
		view.setPreferredHeight(60);
		view.setPreferredWidth(60);
	}
	
}