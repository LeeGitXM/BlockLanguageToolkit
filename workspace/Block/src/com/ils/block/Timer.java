/**
 *   (c) 2014-2016  ILS Automation. All rights reserved.  
 */
package com.ils.block;

import java.util.Date;
import java.util.Map;
import java.util.UUID;
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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * The Timer block writes the number of seconds that it has been in a specified state. The
 * output is written at a configured interval.
 */
@ExecutableBlock
public class Timer extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Timer";
	private final static String BLOCK_PROPERTY_ACCUMULATE_VALUES = "AccumulateValues";
	private final static String BLOCK_PROPERTY_STOP_ON = "StopOn";
	private double interval = 10;  // ~secs
	private boolean accumulateValues = false;
	private double duration = 0.;  //Elapsed time ~ secs at the triggering state.
//	private BlockProperty tagProperty = null;
	private TruthValue stopOn = TruthValue.FALSE;
	private TruthValue trigger = TruthValue.TRUE;
	private Date triggerReceiptTime = null;
	private BlockProperty valueProperty = null;
	private final Watchdog dog;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Timer() {
		dog = new Watchdog(TAG,this);
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
	public Timer(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Timer");
//		this.setReceiver(true);
		BlockProperty accumulateProperty = new BlockProperty(BLOCK_PROPERTY_ACCUMULATE_VALUES,new Boolean(accumulateValues),PropertyType.BOOLEAN,true);
		setProperty(BLOCK_PROPERTY_ACCUMULATE_VALUES, accumulateProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);
//		tagProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.STRING,true);
//		tagProperty.setBinding("");
//		tagProperty.setBindingType(BindingType.TAG_WRITE);
//		setProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH, tagProperty);
		BlockProperty commandProperty = new BlockProperty(BLOCK_PROPERTY_STOP_ON,stopOn,PropertyType.TRUTHVALUE,true);
		setProperty(BLOCK_PROPERTY_STOP_ON, commandProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.TRUTHVALUE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		
		// The value is the count-down shown in the UI
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * Do not call the super method. We emit a zero instead of a NaN.
	 */
	@Override
	public void reset() {
		this.state = TruthValue.UNSET;
		this.duration = 0.;
		valueProperty.setValue("");
		recordActivity(Activity.ACTIVITY_RESET,"");
		if( controller!=null ) {
			// Send notifications on all outputs to indicate empty connections.
			lastValue = new TestAwareQualifiedValue(timer,new Integer((int)duration));
			triggerReceiptTime = lastValue.getTimestamp();
			evaluate();
		}
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
	 * A new value has appeared on the input. If it represents a change and
	 * matches the trigger, then we start counting.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		if( vcn.getConnection()!=null ) {
			valueProperty.setValue("");
			String port = vcn.getConnection().getDownstreamPortName();
			if( port.equals(BlockConstants.IN_PORT_NAME) ) {
				TruthValue tv = vcn.getValueAsTruthValue();
				if( !tv.equals(state) )  {
					setState(tv);
					// This represents a state change
					if( trigger.equals(tv)  ) {
						if( !accumulateValues) duration = 0.0;
						lastValue = new TestAwareQualifiedValue(timer,new Integer((int)duration));
						triggerReceiptTime = lastValue.getTimestamp();
						evaluate();
					}
					else if(stopOn.equals(tv)) {
						evaluate();  // One last time
						timer.removeWatchdog(dog);
					}
				}
			}
		}
	}
	/**
	 * This method implements special handling of a reset() signal that
	 * allows it to retain its current state.
	 * 
	 * @param sn notification of a signal.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {
		Signal sig = sn.getSignal();
		if( sig.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_RESET) ) {
			recordActivity(Activity.ACTIVITY_RECEIVE,sig.getCommand());
			duration = 0.0;
			lastValue = new TestAwareQualifiedValue(timer,new Integer((int)duration));
			triggerReceiptTime = lastValue.getTimestamp();
			evaluate();
		}
		else  {
			super.acceptValue(sn);
		}
		
	}
	/**
	 * The interval has expired. Update the duration, then emit a value. 
	 * Write to the tag, if configured. If we're in the triggering state,
	 * set the timer for another cycle, else terminate it.
	 */
	@Override
	public synchronized void evaluate() {
		//log.infof("%s.evaluate ... %f secs",TAG,interval);
		//if we're in the triggering state, then update duration, re-set the timer
		if( lastValue!=null ) {
			long testtime = timer.getTestTime();
			long qvtime = lastValue.getTimestamp().getTime();
			duration += ((testtime - qvtime) * timer.getFactor())/1000.;
			lastValue = new TestAwareQualifiedValue(timer,new Integer((int)duration));
			if( !isLocked() ) {	
				OutgoingNotification sig = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(sig);
	
				double elapsed = Double.parseDouble(lastValue.getValue().toString());
				String formattedTime = String.format("%02d:%02d:%02d", TimeUtility.remainderValue(elapsed, TimeUnit.HOURS),
						TimeUtility.remainderValue(elapsed, TimeUnit.MINUTES),TimeUtility.remainderValue(elapsed, TimeUnit.SECONDS));
				valueProperty.setValue(formattedTime);
//				String path = tagProperty.getBinding().toString();
//				if( !path.isEmpty() ) controller.updateTag(getParentId(),path, lastValue);
				notifyOfStatus(lastValue);
			}
		}
		if(state.equals(trigger) ) {
			// Stroke the dog
			dog.setSecondsDelay(interval);
			timer.updateWatchdog(dog);  // pet dog
		}
		else {
			timer.removeWatchdog(dog);
		}
		
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(lastValue);
	}

	// NOTE: The "value" property is what we want to display (the countdown)
	private void notifyOfStatus(QualifiedValue qv) {
		Object val = valueProperty.getValue();
		if( val!=null ) {
			QualifiedValue displayQV = new TestAwareQualifiedValue(timer,val.toString());
			log.tracef("%s.notifyOfStatus display = %s",getName(),val.toString());
			controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,displayQV);
		}
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Note, an interval change resets the timeout period.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		String val = event.getNewValue().toString();
		log.debugf("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equalsIgnoreCase(BLOCK_PROPERTY_ACCUMULATE_VALUES) ) {
			try {
				accumulateValues = Boolean.parseBoolean(val);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert accumulate values flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_INTERVAL)) {
			try {
				interval = Double.parseDouble(val);
				if( dog.isActive() && interval>0.0 && state.equals(trigger) ) {
					// Update duration for "time served". 
					double elapsed = (timer.getTestTime() - lastValue.getTimestamp().getTime())/1000;
					duration+=elapsed;
					dog.setSecondsDelay(interval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TRIGGER)) {
			try {
				trigger = TruthValue.valueOf(val.toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s: propertyChange Unable to convert %s to a truth value (%s)",TAG,val,iae.getLocalizedMessage());
			}
		}
		
		else if( propertyName.equalsIgnoreCase(BLOCK_PROPERTY_STOP_ON) ) {
			try {
				stopOn = TruthValue.valueOf(val.toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s: propertyChange Unable to convert %s to a truth value (%s)",TAG,val,iae.getLocalizedMessage());
			}
		}
		// Buffer size handled in superior method
		else if( !propertyName.equals(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE) ){
			log.warnf("%s.propertyChange:Unrecognized property (%s)",getName(),propertyName);
		}
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("SecondsInState", String.valueOf(duration));
		if( triggerReceiptTime!=null ) {
			attributes.put("TriggerReceiptTime", dateFormatter.format(triggerReceiptTime));
		}
		return descriptor;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/PMIDigitalDisplay32.png");
//		prototype.setPaletteIconPath("Block/icons/palette/clock.png");
		prototype.setPaletteLabel("Timer");
		prototype.setTooltipText("Record time in the configured state ~ secs");
		prototype.setTabName(BlockConstants.PALETTE_TAB_TIMERS_COUNTERS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/alarm_clock.png");
		desc.setBlockClass(getClass().getCanonicalName());

		desc.setPreferredHeight(46);
		desc.setPreferredWidth(90);
		desc.setBadgeCharacter("t");
		desc.setStyle(BlockStyle.READOUT);
	}
}