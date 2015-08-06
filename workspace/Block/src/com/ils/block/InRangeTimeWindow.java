/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.HysteresisType;
import com.ils.blt.common.block.ProcessBlock;
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
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class computes an average of the input over a time interval.
 */
@ExecutableBlock
public class InRangeTimeWindow extends AbstractProcessBlock implements ProcessBlock {
	private final static String BLOCK_PROPERTY_LOWER_LIMIT  = "LowerLimit";
	private final static String BLOCK_PROPERTY_UPPER_LIMIT  = "UpperLimit";
	private final LinkedList<Double> buffer;
	private double currentValue = Double.NaN;
	private double scanInterval = 1.0;    // ~secs
	private double timeWindow = 60.;     // ~ secs
	private final Watchdog dog;
	private double lowerLimit = Double.MIN_VALUE;
	private double upperLimit = Double.MAX_VALUE;
	private double deadband = 0;
	private HysteresisType hysteresis = HysteresisType.NEVER;
	private boolean fillRequired = true;
	private int triggerCount = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public InRangeTimeWindow() {
		initialize();
		dog = new Watchdog(getName(),this);
		buffer = new LinkedList<Double>();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom properties are limit, standardDeviation
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public InRangeTimeWindow(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		dog = new Watchdog(getName(),this);
		buffer = new LinkedList<Double>();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("InRangeTime");
		BlockProperty minprop = new BlockProperty(BLOCK_PROPERTY_LOWER_LIMIT, lowerLimit,PropertyType.DOUBLE, true);
		setProperty(BLOCK_PROPERTY_LOWER_LIMIT, minprop);
		BlockProperty maxprop = new BlockProperty(BLOCK_PROPERTY_UPPER_LIMIT, upperLimit,PropertyType.DOUBLE, true);
		setProperty(BLOCK_PROPERTY_UPPER_LIMIT, maxprop);
		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, intervalProperty);;
		BlockProperty fillProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED,new Boolean(fillRequired),PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED, fillProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT,new Integer(triggerCount),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT, triggerProperty);
		BlockProperty deadbandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND,new Double(deadband),PropertyType.DOUBLE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND, deadbandProperty);
		BlockProperty hProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_HYSTERESIS,hysteresis,PropertyType.HYSTERESIS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_HYSTERESIS, hProperty);
		
		// Define a single input.
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define the main output.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		buffer.clear();
		timer.removeWatchdog(dog);    // Stop evaluation
		state = TruthValue.UNSET;
	}

	@Override
	public void start() {
		super.stop();
	}
	@Override
	public void stop() {
		timer.removeWatchdog(dog);
	}
	
	/**
	 * A new value has arrived. Simply set the current value.
	 * If the timer is not running, start it now.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		if( qual.isGood() && qv.getValue()!=null ) {
			currentValue = Double.NaN;
			try {
				currentValue = Double.parseDouble(qv.getValue().toString());
				log.infof("%s.acceptValue: %s",getName(),qv.getValue().toString());
				if( !dog.isActive() && scanInterval>0.0 ) {
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
					log.infof("InRange.acceptValue TRIGGERED TIMER");
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Exception converting incoming %s to double (%s)",getName(),qv.getValue().toString(),nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.acceptValue received a GOOD value, but null",getName());
		}
	}
	
	/**
	 * The interval timer has expired. Evaluate the buffer.
	 */
	@Override
	public void evaluate() {
		if( Double.isNaN(currentValue) ) return;

		// Evaluate the buffer and report
		// Add the currentValue to the queue
		Double val = new Double(currentValue);
		buffer.addLast(val);
		int maxPoints = (int)((timeWindow+0.99*scanInterval)/scanInterval);
		while(buffer.size() > maxPoints ) {
			buffer.removeFirst();
		}
		log.tracef("%s.evaluate %d of %d points",getName(),buffer.size(),maxPoints);
		TruthValue result = checkPassConditions(state);
		if( buffer.size()<maxPoints && fillRequired && result.equals(TruthValue.FALSE) ) result = TruthValue.UNKNOWN;
		if( !result.equals(state) && !isLocked() ) {
			// Give it a new timestamp
			QualifiedValue outval = new TestAwareQualifiedValue(timer,result);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(outval);
		}
		state = result;
		dog.setSecondsDelay(scanInterval);
		timer.updateWatchdog(dog);  // pet dog
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		for( Double dbl:buffer) {
			Map<String,String> qvMap = new HashMap<>();
			qvMap.put("Value", String.valueOf(dbl));
			descBuffer.add(qvMap);
		}

		return descriptor;
	}
	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.infof("%s.propertyChange: %s = %s",getName(),propertyName,event.getNewValue().toString());
		if(propertyName.equals(BLOCK_PROPERTY_LOWER_LIMIT)) {
			try {
				lowerLimit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert lower limit to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BLOCK_PROPERTY_UPPER_LIMIT)) {
			try {
				upperLimit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert upper limit to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_DEADBAND)) {
			try {
				deadband = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert deadband to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED)) {
			fillRequired = fcns.coerceToBoolean(event.getNewValue().toString());
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_HYSTERESIS)) {
			try {
				hysteresis = HysteresisType.valueOf(event.getNewValue().toString().toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.propertyChange: Unable to convert hysteresis (%s)",getName(),iae.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT) ) {
			// Trigger an evaluation
			try {
				triggerCount = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert trigger count to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				double oldInterval = scanInterval;
				scanInterval = Double.parseDouble(event.getNewValue().toString());
				if( scanInterval < 0.1 ) scanInterval = 0.1;   // Don't allow to go too fast
				if( dog.isActive() && scanInterval < oldInterval ) {
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW)) {
			try {
				timeWindow = Double.parseDouble(event.getNewValue().toString());
				if( timeWindow<=0.0) timeWindow = scanInterval;
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
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/in_range_window.png");
		prototype.setPaletteLabel("InRange(t)");
		prototype.setTooltipText("Return true if a specified count of points in a sample is within a range.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/xbart_range.png");
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Check pass-fail criteria, presumably because of a new input.
	 * The queue never contains bad values. 
	 */
	private TruthValue checkPassConditions(TruthValue current) {
		TruthValue result = TruthValue.UNSET;
		double upperThreshold = upperLimit;
		double lowerThreshold = lowerLimit;
		switch(hysteresis) {
		case TRUE:
			if( current.equals(TruthValue.TRUE)) {
				upperThreshold = upperLimit - deadband;
				lowerThreshold = lowerLimit + deadband;
			}
			break;
		case FALSE:
			if( current.equals(TruthValue.TRUE)) {
				upperThreshold = upperLimit;
				lowerThreshold = lowerLimit; 
			}
			else {
				upperThreshold = upperLimit + deadband;
				lowerThreshold = lowerLimit - deadband;
			}
			break;
		case ALWAYS:
			if( current.equals(TruthValue.TRUE)) {
				upperThreshold = upperLimit - deadband;
				lowerThreshold = lowerLimit + deadband;
			}
			else {
				upperThreshold = upperLimit + deadband;
				lowerThreshold = lowerLimit - deadband;
			}
			break;
		case NEVER:
		default:
			upperThreshold = upperLimit;
			lowerThreshold = lowerLimit;  
		}
		int count = 0;
		double val = Double.NaN;
		Iterator<Double> walker = buffer.iterator();
		while( walker.hasNext() ) {
			Double dbl = walker.next();
			val = dbl.doubleValue();
			if( val<lowerThreshold || val>upperThreshold ) count++;
			log.infof("%s.checkPassConditions: %f>%f>%f",getName(),upperThreshold,val,lowerThreshold);
		}
		if( count>=triggerCount ) result = TruthValue.TRUE;
		else result = TruthValue.FALSE;
		int size = (int)((timeWindow+0.99*scanInterval)/scanInterval);
		log.tracef("%s:checkPassConditions count %d of %d (%f<%f<%f) %s (was %s)",getName(),count,size,lowerThreshold,val,upperThreshold,result.name(),current.name());
		return result;
	}
}