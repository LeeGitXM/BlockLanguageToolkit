/**
 *   (c) 2014-2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.TimeUtility;
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
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Monitor a truth-value. Do not emit it from the output until it has been unchanged for
 * a configured interval. An input of the opposing state is processed immediately and
 * resets the counter.
 * 
 * Note that an "active" watchdog is one that is waiting for its time to expire.
 */
@ExecutableBlock
public class PersistenceGate extends AbstractProcessBlock implements ProcessBlock {
	private int count = 0;     // Countdown - number of intervals to go ...
	private double scanInterval = 10.;  // ~secs
	private double timeWindow = 0.;  // ~secs
	private TruthValue trigger = TruthValue.TRUE;     // Nothing will trigger until this is set * 08/22/19 Pete wants this to default to TRUE
	private BlockProperty valueProperty = null;
	private final Watchdog dog;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public PersistenceGate() {
		initialize();
		initializePrototype();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public PersistenceGate(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("PersistenceGate");
		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME_MINUTES,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty scanIntervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, scanIntervalProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.TRUTHVALUE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		// The value is the count-down shown in the UI
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		// Define a single output and a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setIsMultiple(false);
		anchors.add(input);

		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		if( dog.isActive() ) timer.removeWatchdog(dog);
		count = 0;
		valueProperty.setValue("");
		notifyOfStatus();
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
	 * A new value has appeared on an input anchor. If it matches the trigger, start the count-down.
	 * Ignore any values that come in during the countdown.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		
		lastValue = vcn.getValue();
		//log.infof("%s.acceptValue: Received %s, trigger is %s",TAG,qv.getValue().toString(),trigger);
		// A different value than the trigger, or a bad value aborts the countdown
		if( !lastValue.getQuality().isGood() || !lastValue.getValue().toString().equalsIgnoreCase(trigger.toString()) ) {
			count = 0;
			valueProperty.setValue("");
			if( dog.isActive() ) timer.removeWatchdog(dog);
			if( !isLocked() ) {
				// Propagate value immediately and reset the block
				log.tracef("%s.acceptValue: No match, sent immediate %s",getName(),lastValue.getValue().toString());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				state = qualifiedValueAsTruthValue(lastValue);
				notifyOfStatus(lastValue);
			}
		}
		// Only execute if the block state is not at the trigger
		else if( !trigger.toString().equalsIgnoreCase(state.name()) ) {
			//log.infof("%s.acceptValue: Matched trigger %s",TAG,qv.getValue().toString());
			// Good quality and equal to the trigger.
			if( !dog.isActive() ) {
				// Start the countdown
				count = (int)(timeWindow/scanInterval+0.5);
				log.debugf("%s.acceptValue: Start countdown %d cycles (%f in %f)",getName(),count,scanInterval,timeWindow);
				if( count> 0 ) {
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
		}
	}
	
	/**
	 * The interval has expired. Reset interval, then compute output.
	 * Do not compute anything until all parameters have been set.
	 */
	@Override
	public synchronized void evaluate() {
		log.tracef("%s.evaluate: cycle %d (%s).",getName(),count,timer.getName());
		if( count> 0 ) {
			dog.setSecondsDelay(scanInterval);
			timer.updateWatchdog(dog);  // pet dog
			
			double timeRemaining = count*scanInterval;
//			TimeUnit tu = TimeUtility.unitForValue(timeRemaining);
			String formattedTime = String.format("%02d:%02d:%02d", TimeUtility.remainderValue(timeRemaining, TimeUnit.HOURS),
					TimeUtility.remainderValue(timeRemaining, TimeUnit.MINUTES),TimeUtility.remainderValue(timeRemaining, TimeUnit.SECONDS));
			log.debugf("%s.evaluate: cycle %d property value =  %s.",getName(),count,formattedTime);
			valueProperty.setValue(formattedTime);
			notifyOfStatus();
		}
		else if( !isLocked()) {
			// Finally propagate the held value
			lastValue = new TestAwareQualifiedValue(timer,trigger);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			valueProperty.setValue("---");
			state = qualifiedValueAsTruthValue(lastValue);
			notifyOfStatus(lastValue);
		}
		count--;
	}
	
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.debugf("%s.propertyChange: Received %s = %s",getName(),propertyName,event.getNewValue().toString());
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TRIGGER)) {
			trigger = TruthValue.valueOf(event.getNewValue().toString().toUpperCase());
			
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW)) {
			try {
				timeWindow = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				scanInterval = Double.parseDouble(event.getNewValue().toString());
				if( scanInterval < 0.1 ) scanInterval = 0.1;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else {
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
		attributes.put("Count", String.valueOf(count));
		attributes.put("Value", state.name());
		return descriptor;
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		if( getBlockId()!=null ) {
			QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
			notifyOfStatus(qv);
		}
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
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/PMIDigitalDisplay32.png");
		prototype.setPaletteLabel("PersistGate");
		prototype.setTooltipText("Monitor the incoming value for change over a specified period");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(40);
		desc.setPreferredWidth(90);
		desc.setStyle(BlockStyle.READOUT);
//		desc.setReceiveEnabled(true);
	}
}