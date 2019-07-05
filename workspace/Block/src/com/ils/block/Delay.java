/**
 *   (c) 2014-2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Delay before passing the input onto the output.
 */
@ExecutableBlock
public class Delay extends AbstractProcessBlock implements ProcessBlock {
	protected static String BLOCK_PROPERTY_DELAY = "SampleDelay";

	private double delayInterval = 1;    // ~ secs
	private final ConcurrentLinkedQueue<TimestampedData> buffer;
	private final Watchdog dog;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Delay() {
		dog = new Watchdog(getName(),this);
		initialize();
		buffer = new ConcurrentLinkedQueue<TimestampedData>();
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
		dog = new Watchdog(getName(),this);
		buffer = new ConcurrentLinkedQueue<TimestampedData>();
		initialize();
	}

	@Override
	public void reset() {
		super.reset();
		timer.removeWatchdog(dog);
		buffer.clear();
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
				log.warnf("%s: propertyChange Unable to convert interval value to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * A new value has appeared on an input anchor. Add it to the list and trigger the delay timer.
	 * The superior method set "lastValue", 
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME) && vcn.getValue()!=null ) {
			String key = vcn.getConnection().getSource().toString();
			recordActivity(Activity.ACTIVITY_RECEIVE,port,vcn.getValue().toString(),key);
			
			long expirationTime = System.currentTimeMillis()+(int)(delayInterval*1000);
			TimestampedData data = new TimestampedData(vcn.getValue(),expirationTime);
			log.debugf("%s.acceptValue: %s",getName(),vcn.getValue().toString());
			synchronized(this) {
				if( buffer.isEmpty() ) {
					dog.setSecondsDelay(delayInterval);
					timer.updateWatchdog(dog);  // pet dog
				}
				else {
					// Possible if we've changed the expiration time.
					if( dog.isActive() && dog.getExpiration()>expirationTime ) {
						dog.setSecondsDelay(delayInterval);
						timer.updateWatchdog(dog);  // pet dog
					}
				}
				buffer.add(data);
			}
		}
	}
	
	/**
	 * The delay interval has expired. Pop and propagate the top object.
	 * Set the timer for the next object, if any. 
	 */
	@Override
	public void evaluate() {
		// This next line doesn't really prevent the problem, so I changed the buffer LinkedList to ConcurrentLinkedQueue - CJL 12/14/18
		if(buffer.isEmpty()) return;     // Could happen on race between clearing buffer and removing watch-dog on a reset.
								
		TimestampedData data = buffer.peek();
		if( !isLocked() ) {
			log.debugf("%s.evaluate: %s",getName(),data.qualValue.getValue().toString());
			lastValue = new BasicQualifiedValue(coerceToMatchOutput(BlockConstants.OUT_PORT_NAME,data.qualValue.getValue()),data.qualValue.getQuality(),data.qualValue.getTimestamp()); 
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
		// Even if we're locked, we process things as normal
		if( !buffer.isEmpty() ) {
			// New delay is the difference between the value we just popped off
			// and the current head.
			TimestampedData head = buffer.remove();
			long delay = head.timestamp - data.timestamp;
			if( delay <= 0 ) delay = 1;  // Should never happen
			dog.setDelay(delay);
			timer.updateWatchdog(dog);  // pet dog
		}
	}
	/**
	 * Send status update notification for our last transmitted value. If we've 
	 * never transmitted one, lastValue will be null.
	 */
	@Override
	public void notifyOfStatus() {
		if(lastValue!=null) {
			controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lastValue);
		}
		else {
			QualifiedValue lv = new BasicQualifiedValue(coerceToMatchOutput(BlockConstants.OUT_PORT_NAME,null));
			controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lv);
		}
	}

	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Delay");
		
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_DELAY,new Double(delayInterval),PropertyType.TIME_MINUTES,true);
		setProperty(BLOCK_PROPERTY_DELAY, constant);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(false);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		if( dog.isActive() ) {
			Map<String,String> attributes = descriptor.getAttributes();
			long now = System.nanoTime()/1000000;   // Work in milliseconds
			long waitTime = (long)(dog.getExpiration()-now);
			attributes.put("MSecsToNextOutput",String.valueOf(waitTime));
		}
		List<Map<String,String>> outbuffer = descriptor.getBuffer();
		for( TimestampedData td:buffer) {
			Map<String,String> qvMap = new HashMap<>();
			qvMap.put("Value", td.qualValue.getValue().toString());
			qvMap.put("Expiration", dateFormatter.format(new Date(td.timestamp)));
			outbuffer.add(qvMap);
		}
		return descriptor;
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
		view.setCtypeEditable(true);
	}
	
	/**
	 * This what we store in the linked list. The timestamp is the 
	 * expiration time of the delay.
	 */
	private class TimestampedData {
		public final long timestamp;
		public final QualifiedValue qualValue;
		
		public TimestampedData(QualifiedValue data,long time) {
			this.qualValue = data;
			this.timestamp = time;
			
		}
	}
}