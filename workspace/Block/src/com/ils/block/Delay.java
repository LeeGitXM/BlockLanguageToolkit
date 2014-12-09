/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.LinkedList;
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
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Delay before passing the input onto the output.
 */
@ExecutableBlock
public class Delay extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "Delay";
	protected static String BLOCK_PROPERTY_DELAY = "SampleDelay";

	private double delayInterval = 1;    // ~ secs
	private final LinkedList<TimestampedData> buffer;
	private final Watchdog dog;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Delay() {
		dog = new Watchdog(TAG,this);
		initialize();
		buffer = new LinkedList<TimestampedData>();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "DelayInterval".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Delay(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		buffer = new LinkedList<TimestampedData>();
		initialize();
	}
	
	@Override
	public void reset() {
		controller.removeWatchdog(dog);
		buffer.clear();
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		controller.removeWatchdog(dog);
	}
	/**
	 * Handle a change to the delay interval or buffer size
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if( propertyName.equals(BLOCK_PROPERTY_DELAY) ) {
			try {
				delayInterval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert interval value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * A new value has appeared on an input anchor. Add it to the list and trigger the delay timer.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME) ) {
			long expirationTime = System.currentTimeMillis()+(int)(delayInterval*1000);
			TimestampedData data = new TimestampedData(vcn.getValue(),expirationTime);
			synchronized(this) {
				if( buffer.isEmpty() ) {
					dog.setSecondsDelay(delayInterval);
					controller.pet(dog);
				}
				else {
					// Possible if we've changed the expiration time.
					if( dog.isActive() && dog.getExpiration()>expirationTime ) {
						dog.setSecondsDelay(delayInterval);
						controller.pet(dog);
					}
				}
				buffer.addLast(data);
			}
		}
	}
	
	/**
	 * The delay interval has expired. Pop and propagate the top object.
	 * Set the timer for the next object, if any. 
	 */
	@Override
	public void evaluate() {
		TimestampedData data = buffer.removeFirst();
		if( !isLocked() ) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,data.qv);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(data.qv);
		}
		// Even if we're locked, we process things as normal
		if( !buffer.isEmpty() ) {
			// New delay is the difference between the value we just popped off
			// and the current head.
			TimestampedData head = buffer.getFirst();
			long delay = head.timestamp - data.timestamp;
			if( delay <= 0 ) delay = 1;  // Should never happen
			dog.setDelay(delay);
			controller.pet(dog);
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
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Delay");
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_DELAY,new Double(delayInterval),PropertyType.TIME,true);
		setProperty(BLOCK_PROPERTY_DELAY, constant);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/delay.png");
		prototype.setPaletteLabel("Delay");
		prototype.setTooltipText("Delay incoming values by a specified interval (~secs)");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setEmbeddedIcon("Block/icons/embedded/clock.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.SQUARE);
		view.setPreferredHeight(60);
		view.setPreferredWidth(60);
	}
	
	/**
	 * This what we store in the linked list. The timestamp is the 
	 * expiration time of the delay.
	 */
	private class TimestampedData {
		public final long timestamp;
		public final QualifiedValue qv;
		
		public TimestampedData(QualifiedValue data,long time) {
			this.qv = data;
			this.timestamp = time;
			
		}
	}
}