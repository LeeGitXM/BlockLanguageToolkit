/**
 *   (c) 2014  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.block.BlockState;
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
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class computes an average of the input over a time interval.
 */
@ExecutableBlock
public class InRangeTimeWindow extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "InRangeTimeWindow";
	private final static String BLOCK_PROPERTY_LOWER_LIMIT  = "LowerLimit";
	private final static String BLOCK_PROPERTY_UPPER_LIMIT  = "UpperLimit";
	private final LinkedList<Double> buffer;
	private double currentValue = Double.NaN;
	private double scanInterval = 1.0;    // ~secs
	private double timeWindow = 60;     // ~ secs
	private final Watchdog dog;
	private TruthValue truthValue = TruthValue.UNSET;
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
		dog = new Watchdog(TAG,this);
		buffer = new LinkedList<Double>();
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
	public InRangeTimeWindow(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		buffer = new LinkedList<Double>();
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("InRangeTime");
		BlockProperty minprop = new BlockProperty(BLOCK_PROPERTY_LOWER_LIMIT, lowerLimit,PropertyType.DOUBLE, true);
		properties.put(BLOCK_PROPERTY_LOWER_LIMIT, minprop);
		BlockProperty maxprop = new BlockProperty(BLOCK_PROPERTY_UPPER_LIMIT, upperLimit,PropertyType.DOUBLE, true);
		properties.put(BLOCK_PROPERTY_UPPER_LIMIT, maxprop);
		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, intervalProperty);;
		BlockProperty fillProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED,new Boolean(fillRequired),PropertyType.BOOLEAN,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED, fillProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT,new Integer(triggerCount),PropertyType.INTEGER,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT, triggerProperty);
		BlockProperty deadbandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND,new Double(deadband),PropertyType.DOUBLE,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_DEADBAND, deadbandProperty);
		BlockProperty hProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_HYSTERESIS,hysteresis,PropertyType.HYSTERESIS,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_HYSTERESIS, hProperty);
		
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
		if( scanInterval>0.0) {
			dog.setSecondsDelay(scanInterval);
			controller.pet(dog);
		}
		truthValue = TruthValue.UNSET;
	}

	@Override
	public void start() {
		reset();
	}
	@Override
	public void stop() {
		controller.removeWatchdog(dog);
	}
	
	/**
	 * A new value has arrived. Simply set the current value.
	 * (We poll the current value on an interval).
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		this.state = BlockState.ACTIVE;
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		if( qual.isGood() && qv.getValue()!=null ) {
			currentValue = Double.NaN;
			try {
				currentValue = Double.parseDouble(qv.getValue().toString());
				log.infof("%s.acceptValue current value is %s",TAG,qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue exception converting incoming %s to double (%s)",TAG,qv.getValue().toString(),nfe.getLocalizedMessage());
			}
		}
		else if(!qual.isGood()) {
			// Bad quality, emit the result immediately
			currentValue = Double.NaN;
			if( !isLocked() ) {
				QualifiedValue outval = new BasicQualifiedValue(TruthValue.UNKNOWN,qual,qv.getTimestamp());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
				controller.acceptCompletionNotification(nvn);
			}
			reset();     // Reset the evaluation interval
		}
		else {
			log.warnf("%s.acceptValue received a GOOD value, but null",TAG);
		}
	}
	
	/**
	 * The interval timer has expired. Evaluate the buffer.
	 */
	@Override
	public void evaluate() {
		log.infof("%s.evaluate",TAG);
		if( Double.isNaN(currentValue) ) return;

		// Evaluate the buffer and report
		// Add the currentValue to the queue
		Double val = new Double(currentValue);
		buffer.addLast(val);
		int maxPoints = (int)((timeWindow+0.99*scanInterval)/scanInterval);
		while(buffer.size() > maxPoints ) {
			buffer.removeFirst();
		}
		log.infof("%s.evaluate %d of %d points",TAG,buffer.size(),maxPoints);
		if( buffer.size() >= maxPoints || !fillRequired) {
			TruthValue result = checkPassConditions(truthValue);
			if( !result.equals(truthValue) && !isLocked() ) {
				// Give it a new timestamp
				truthValue = result;
				QualifiedValue outval = new BasicQualifiedValue(result);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
				controller.acceptCompletionNotification(nvn);
			}
		}
		else {
			if( !truthValue.equals(TruthValue.UNKNOWN) && !isLocked() ) {
				truthValue = TruthValue.UNKNOWN;
				QualifiedValue outval = new BasicQualifiedValue(TruthValue.UNKNOWN);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
				controller.acceptCompletionNotification(nvn);
			}
		}

		dog.setSecondsDelay(scanInterval);
		controller.pet(dog);
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
		log.infof("%s.propertyChange: %s = %s",TAG,propertyName,event.getNewValue().toString());
		if(propertyName.equals(BLOCK_PROPERTY_LOWER_LIMIT)) {
			try {
				lowerLimit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert lower limit to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equals(BLOCK_PROPERTY_UPPER_LIMIT)) {
			try {
				upperLimit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert upper limit to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_DEADBAND)) {
			try {
				deadband = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert deadband to a double (%s)",TAG,nfe.getLocalizedMessage());
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
				log.warnf("%s.propertyChange: Unable to convert hysteresis (%s)",TAG,iae.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT) ) {
			// Trigger an evaluation
			try {
				triggerCount = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert trigger count to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				double oldInterval = scanInterval;
				scanInterval = Double.parseDouble(event.getNewValue().toString());
				if( scanInterval < 0.1 ) scanInterval = 0.1;   // Don't allow to go too fast
				if( scanInterval < oldInterval ) {
					dog.setSecondsDelay(scanInterval);
					controller.pet(dog);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW)) {
			try {
				timeWindow = Double.parseDouble(event.getNewValue().toString());
				if( timeWindow<=0.0) timeWindow = scanInterval;
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
		Iterator<Double> walker = buffer.iterator();
		while( walker.hasNext() ) {
			Double dbl = walker.next();
			double val = dbl.doubleValue();
			if( val>=lowerThreshold && val<=upperThreshold ) count++;
			log.tracef("%s.checkPassConditions: %f>%f>%f",TAG,upperThreshold,val,lowerThreshold);
		}
		log.tracef("%s.checkPassConditions: count %d, need %d",TAG,count,triggerCount);
		if( count>=triggerCount ) result = TruthValue.TRUE;
		else result = TruthValue.FALSE;
		return result;
	}
}