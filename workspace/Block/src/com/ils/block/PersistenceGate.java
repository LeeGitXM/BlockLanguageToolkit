/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 *   Code based on sample code at: 
 *        http://www.codeproject.com/Articles/36459/PID-process-control-a-Cruise-Control-example
 */
package com.ils.block;

import java.util.UUID;
import java.util.concurrent.TimeUnit;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.TimeUtility;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Emit a configured signal when the monitored truth-value changes 
 * before the monitoring time expires.
 */
@ExecutableBlock
public class PersistenceGate extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "PersistenceGate";
	private final String BLOCK_PROPERTY_TRIGGER = "Trigger";
	private int cycle = 0;
	private double scanInterval = 10.;  // ~secs
	private double timeWindow = 0.;  // ~secs
	private String trigger = "";     // Nothing will trigger until this is set
	private BlockProperty valueProperty = null;;
	private final Watchdog dog;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public PersistenceGate() {
		dog = new Watchdog(TAG,this);
		initialize();
		initializePrototype();
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
		dog = new Watchdog(TAG,this);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("PersistenceGate");

		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty scanIntervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, scanIntervalProperty);
		BlockProperty triggerProperty = new BlockProperty(BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.STRING,true);
		properties.put(BLOCK_PROPERTY_TRIGGER, triggerProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		properties.put(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		// Define a single output and a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(input);
		anchors.add(output);
		reset();
	}
	
	@Override
	public void reset() {
		super.reset();
		if( dog.isActive() ) controller.removeWatchdog(dog);
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
	 * A new value has appeared on an input anchor. If it matches the trigger, start the count-down.
	 * Ignore any values that come in during the countdown.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);

		QualifiedValue qv = vcn.getValue();
		log.debugf("%s.acceptValue: Received %s",TAG,qv.getValue().toString());
		// A different value than the trigger, or a bad value aborts the countdown
		if( !qv.getQuality().isGood() || !qv.getValue().toString().equals(trigger) ) {
			cycle = 0;
			if( dog.isActive() ) {
				controller.removeWatchdog(dog);
			}
			if( !isLocked() ) {
				// Propagate value immediately
				log.infof("%s.acceptValue: Sent immediate %s",TAG,qv.getValue().toString());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
				controller.acceptCompletionNotification(nvn);
			}
		}
		else {
			log.debugf("%s.acceptValue: Matched trigger %s",TAG,qv.getValue().toString());
			// Good quality and equal to the trigger.
			if( !dog.isActive() ) {
				// Start the countdown
				cycle = (int)(timeWindow/scanInterval);
				log.infof("%s.acceptValue: Start countdown %d cycles (%f in %f)",TAG,cycle,scanInterval,timeWindow);
				if( cycle> 0 ) {
					dog.setSecondsDelay(scanInterval);
					controller.pet(dog);
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
		log.debugf("%s.evaluate ... cycle = %d",TAG,cycle);
		if( cycle> 0 ) {
			cycle--;
			dog.setSecondsDelay(scanInterval);
			controller.pet(dog);
			
			double timeRemaining = cycle*scanInterval;
			TimeUnit tu = TimeUtility.unitForValue(timeRemaining);
			String formattedTime = String.format("%.2f %s", TimeUtility.valueForCanonicalValue(timeRemaining, tu),TimeUtility.abbreviationForUnit(tu));
			QualifiedValue qv = new BasicQualifiedValue(formattedTime); 
			log.infof("%s.evaluate: cycle %d property value =  %s.",TAG,cycle,formattedTime);
			valueProperty.setValue(formattedTime);
			controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE, qv);
		}
		else if( !isLocked()) {
			// Finally propagate the held value
			QualifiedValue outval = new BasicQualifiedValue(trigger);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
			controller.acceptCompletionNotification(nvn);
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
		if( propertyName.equals(BLOCK_PROPERTY_TRIGGER)) {
			trigger = event.getNewValue().toString();
			
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW)) {
			try {
				timeWindow = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				scanInterval = Double.parseDouble(event.getNewValue().toString());
				if( scanInterval < 0.1 ) scanInterval = 0.1;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
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
		prototype.setPaletteIconPath("Block/icons/palette/PMIDigitalDisplay32.png");
		prototype.setPaletteLabel("PersistGate");
		prototype.setTooltipText("Monitor the incoming value for change over a specified period");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedLabel("- N");
		desc.setPreferredHeight(40);
		desc.setPreferredWidth(80);
		desc.setStyle(BlockStyle.READOUT);
		desc.setReceiveEnabled(true);
	}
}