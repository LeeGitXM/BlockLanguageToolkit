/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 *   Code based on sample code at: 
 *        http://www.codeproject.com/Articles/36459/PID-process-control-a-Cruise-Control-example
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
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
	private double interval = 60;  // ~secs
	private boolean accumulateValues = false;
	private double duration = 0.;  //Elapsed time ~ secs at the triggering state.
	private QualifiedValue qv = null;    // Most recent output value
	private BlockProperty tagProperty = null;
	private TruthValue stopOn = TruthValue.FALSE;
	private TruthValue trigger = TruthValue.TRUE;
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
		this.isReceiver = true;
		BlockProperty accumulateProperty = new BlockProperty(BLOCK_PROPERTY_ACCUMULATE_VALUES,new Boolean(accumulateValues),PropertyType.BOOLEAN,true);
		setProperty(BLOCK_PROPERTY_ACCUMULATE_VALUES, accumulateProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL,new Double(interval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INTERVAL, intervalProperty);
		tagProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.STRING,true);
		tagProperty.setBinding("");
		tagProperty.setBindingType(BindingType.TAG_WRITE);
		setProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH, tagProperty);
		BlockProperty commandProperty = new BlockProperty(BLOCK_PROPERTY_STOP_ON,stopOn,PropertyType.TRUTHVALUE,true);
		setProperty(BLOCK_PROPERTY_STOP_ON, commandProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.TRUTHVALUE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * On reset clear the counter.
	 */
	@Override
	public void reset() {
		super.reset();
		duration = 0;
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
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME) ) {
			TruthValue tv = vcn.getValueAsTruthValue();
			if( !tv.equals(state) )  {
				// This represents a state change
				if( trigger.equals(tv)  ) {
					if( !accumulateValues) duration = 0.0;
					qv = new BasicQualifiedValue(new Integer((int)duration));
					evaluate();
				}
				else if(stopOn.equals(tv)) {
					evaluate();  // One last time
					timer.removeWatchdog(dog);
				}
				setState(tv);
			}
		}
	}
	/**
	 * The interval has expired. If we are still in the triggering state,
	 * then emit a value. Write to the tag, if configured.
	 */
	@Override
	public synchronized void evaluate() {
		log.infof("%s.evaluate ... %f secs",TAG,interval);
		//if we're in the triggering state, then update duration, re-set the timer
		if(state.equals(trigger) && qv!=null ) {
			duration += (timer.getTestTime() - qv.getTimestamp().getTime()) * timer.getFactor();
			if( !isLocked() ) {
				qv = new TestAwareQualifiedValue(timer,new Double(duration));
				OutgoingNotification sig = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
				controller.acceptCompletionNotification(sig);
	
				String path = tagProperty.getBinding().toString();
				if( !path.isEmpty() ) controller.updateTag(getParentId(),path, qv);
				notifyOfStatus(qv);
			}
			// Stoke the dog
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
		notifyOfStatus(qv);
	}
	private void notifyOfStatus(QualifiedValue qualValue) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qualValue);
	}
	/**
	 * Note, an interval change resets the timeout period.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		String val = event.getNewValue().toString();
		log.infof("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
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
					double elapsed = (timer.getTestTime() - qv.getTimestamp().getTime())/1000;
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
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/clock.png");
		prototype.setPaletteLabel("Timer");
		prototype.setTooltipText("Record time in the configured state ~ secs");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/alarm_clock.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setStyle(BlockStyle.SQUARE);
	}
}