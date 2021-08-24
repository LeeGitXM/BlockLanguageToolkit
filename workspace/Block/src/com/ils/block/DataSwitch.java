/**
 *   (c) 2017  ILS Automation. All rights reserved. 
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
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Route the input to one of three outputs depending on the value of the control
 * line (TRUE, UNKNOWN, FALSE). In a two-value mode, the UNKNOWN line is simply
 * not connected.
 */
@ExecutableBlock
public class DataSwitch extends AbstractProcessBlock implements ProcessBlock {

	private final static String FALSE_PORT_NAME = "false";
	private final static String TRUE_PORT_NAME = "true";
	private final static String UNKNOWN_PORT_NAME = "unknown";
	private final Watchdog dog;
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DataSwitch() {
		initialize();
		initializePrototype();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public DataSwitch(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		timer.removeWatchdog(dog);	
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("DataSwitch");
		state = TruthValue.UNSET;
		// Define the time for "coalescing" inputs ~ msec
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,synchInterval,PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		// Define a data input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		anchors.add(input);
		
		// Define a control port
		AnchorPrototype control = new AnchorPrototype(BlockConstants.RECEIVER_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		control.setHint(PlacementHint.T);
		anchors.add(control);

		// Define a three outputs
		AnchorPrototype outputTrue = new AnchorPrototype(TRUE_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		outputTrue.setAnnotation("T");
		outputTrue.setHint(PlacementHint.RT);
		anchors.add(outputTrue);
		
		AnchorPrototype outputUnknown = new AnchorPrototype(UNKNOWN_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		outputUnknown.setAnnotation("U");
		outputUnknown.setHint(PlacementHint.R);
		anchors.add(outputUnknown);
		
		AnchorPrototype outputFalse = new AnchorPrototype(FALSE_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		outputFalse.setAnnotation("F");
		outputFalse.setHint(PlacementHint.RB);
		anchors.add(outputFalse);
	}
	

	/**
	 * A new value has appeared on our input.  Pass it on.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		dog.setSecondsDelay(synchInterval);
		timer.updateWatchdog(dog);  // pet dog
		
		if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(BlockConstants.RECEIVER_PORT_NAME)) {
			setState(qualifiedValueAsTruthValue(vcn.getValue()));
		}
		else if(!isLocked() ) {
			if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(BlockConstants.IN_PORT_NAME)) {
				lastValue = vcn.getValue();
				dog.setSecondsDelay(synchInterval);
				timer.updateWatchdog(dog);  // pet dog
			}
		}
	}

	/**
	 * The coalescing time has expired. Place the current state on the output,
	 * if it has changed.
	 */
	@Override
	public void evaluate() {
		log.debugf("%s.evaluate",getName());
		if( !isLocked() ) {
			if( state.equals(TruthValue.TRUE)) {
				OutgoingNotification nvn = new OutgoingNotification(this,TRUE_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
			}
			else if( state.equals(TruthValue.UNKNOWN)) {
				OutgoingNotification nvn = new OutgoingNotification(this,UNKNOWN_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
			}
			else if( state.equals(TruthValue.FALSE)) {
				OutgoingNotification nvn = new OutgoingNotification(this,FALSE_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
			}
		}
	}
	/**
	 * Propogate does the same as evaluate
	 */
	@Override
	public void propagate() {
		evaluate();
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(lastValue);
	}

	private void notifyOfStatus(QualifiedValue qv) {
		updateStateForNewValue(qv);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/data_switch.png");
		prototype.setPaletteLabel("DataSwitch");
		prototype.setTooltipText("Divert incoming values to an output determined by the control value");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/3_way_switch.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setCtypeEditable(true);
	}
}