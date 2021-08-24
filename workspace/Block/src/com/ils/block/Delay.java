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
import java.util.concurrent.TimeUnit;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.TimeUtility;
import com.ils.blt.common.block.Activity;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
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
	private UiUpdateTimer updateTimer;
	private BlockProperty valueProperty = null;

	private Thread updater = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Delay() {
		dog = new Watchdog(getName(),this);
		initialize();
		updateTimer = null;
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

//		startUpdateTimer();  no, don't do this until a value comes in

		
		buffer = new ConcurrentLinkedQueue<TimestampedData>();
		initialize();
//		updateTimer.start();
	}

	@Override
	public void reset() {
		super.reset();
		timer.removeWatchdog(dog);
		log.infof("%s: Delay block reset: ",getName());

		if(updateTimer != null) updateTimer.stop();
		startUpdateTimer();
		
		buffer.clear();
		valueProperty.setValue("");
	}
	/**
	 * Disconnect from the timer thread.
	 */
	@Override
	public void stop() {
		super.stop();
		timer.removeWatchdog(dog);
		log.infof("%s: Delay block stop: ",getName());
		if(updateTimer!=null) updateTimer.stop();
	}
	/**
	 * reconnect the timer thread.
	 */
	@Override
	public void start() {
		super.start();
		timer.updateWatchdog(dog);
		log.infof("%s: Delay block start: ",getName());

		if (updateTimer != null) { // should already be stopped
			updateTimer.stop();
		}
		startUpdateTimer();
		
		
	}

	
	/**
	 * Starts the ui Update thread.  Ideally this would only be used if the chart this is on was open in the client.
	 * As of now, it just goes all the time - CJL  
	 */
	private void startUpdateTimer() {
		updateTimer = new UiUpdateTimer();
		updater = new Thread(updateTimer, "UI Timer");
		updater.setDaemon(true);
		updater.start();
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
	 * The superior method sets "lastValue", 
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME) && vcn.getValue()!=null ) {
			valueProperty.setValue("");
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
				if (buffer.size() > 0 & dog.isActive() == false) {  // dog stuck, reset it.
					log.errorf("%s.acceptValue.  Dog is stuck, resetting!",getName());
					if (updateTimer != null) {
						updateTimer.stop();
					}
					startUpdateTimer();
					buffer.clear();
				}
				buffer.add(data);
				if (buffer.size() == 1) {  // only update if it was empty.
					if (updateTimer.stopped) {
						startUpdateTimer();
					}
					notifyOfStatus();
				}
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
		if(data==null) return;
		if (dog.getExpiration() <= System.nanoTime()/1000000) {
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
				TimestampedData dataNext = buffer.peek();
				long next = 0;
				if (dataNext != null) {
					next = dataNext.timestamp;
					updateTimer.stop();
				}
				long delay = next - head.timestamp;
				if( delay <= 0 ) delay = 1;  // happens if no buffer empty
				dog.setDelay(delay);
				timer.updateWatchdog(dog);  // pet dog
			}
			controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lastValue);
		}
		updateDisplay();
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
		updateDisplay();
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lastValue);
	}


	private void updateDisplay() {
		if (valueProperty != null) {
			long now = System.nanoTime()/1000000;   // Work in milliseconds
			double timerr = (dog.getExpiration()-now) / 1000;
			if (timerr < 0) {
				timerr = 0;
				updateTimer.stop(); // No need to keep updating the UI until we get a new value
			}
			String formattedTime = String.format("%02d:%02d:%02d", TimeUtility.remainderValue(timerr, TimeUnit.HOURS),
					TimeUtility.remainderValue(timerr, TimeUnit.MINUTES),TimeUtility.remainderValue(timerr, TimeUnit.SECONDS));
			valueProperty.setValue(formattedTime);
			
			Object val = valueProperty.getValue();
			if( val!=null ) {
				QualifiedValue displayQV = new TestAwareQualifiedValue(timer,val.toString());
				log.tracef("%s.notifyOfStatus display = %s",getName(),val.toString());
				controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,displayQV);
			}
		}
	}
	

	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Delay");
		
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_DELAY,delayInterval,PropertyType.TIME_MINUTES,true);
		setProperty(BLOCK_PROPERTY_DELAY, constant);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);

		// The value is the count-down shown in the UI
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
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
		prototype.setPaletteIconPath("Block/icons/palette/PMIDigitalDisplay32.png");
//		prototype.setPaletteIconPath("Block/icons/palette/delay.png");
		prototype.setPaletteLabel("Delay");
		prototype.setTooltipText("Delay incoming values by a specified interval (~secs)");
		prototype.setTabName(BlockConstants.PALETTE_TAB_TIMERS_COUNTERS);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setEmbeddedIcon("Block/icons/embedded/clock.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.READOUT);
		
//		need to adjust everything else and add countdown like presistGate
		view.setPreferredHeight(46);
		view.setPreferredWidth(90);
		view.setBadgeCharacter("d");
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
	
	public class UiUpdateTimer implements Runnable   {
		protected final static int IDLE_DELAY = 5000;    // 5 seconds
		protected boolean stopped = true;

		/**
		 * Constructor: Creates a timeout timer. The timer thread is started and
		 *              runs continuously until a stop is issued.
		 */
		public UiUpdateTimer()  {
			stopped = false;
//			start();
			log.debug("START UiUpdateTimer thread ");
		}


//		/**
//		 * This is for a restart. Use a new thread.
//		 */
//		public synchronized void start() {
//			if( stopped ) {
//				stopped = false;
//				log.info("RESTART UI Update Thread");
//			}
//		}
//
		/**
		 * On stop, set all the dogs to inactive.
		 */
		public synchronized void stop() {
			if( !stopped ) {
				log.debug(getName()+":STOPPED");
				stopped = true;
			}
		}
		
		/**
		 * A timeout causes the head to be notified, then pops up the next dog. 
		 */
		public synchronized void run() {
			while( !stopped  ) {
				try {
					log.trace(getName()+" Delay UI Update BEEP stopped = " + (stopped?"stopped":"running") + " in " + getName()); 
					wait(IDLE_DELAY);
					if (!stopped) { 
						updateDisplay();
					}
				} 
				catch( Exception ex ) {
					log.errorf(getName()+".Exception during ui update processing ("+ex.getLocalizedMessage()+")",ex);  // Prints stack trace
				} 
			}
		}
	}

}