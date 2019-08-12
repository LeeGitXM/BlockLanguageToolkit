/**
 *   (c) 2014,2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.HysteresisType;
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
 * Monitor the incoming truth-value for percent-of-time true. The output
 * reflects whether the ratio exceeds the limit.
 */
@ExecutableBlock
public class LogicFilter extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "LogicFilter";
	private final static String BLOCK_PROPERTY_MINIMUM_TRUE_FRACTION = "MinimumTrueFraction";
	private final static String BLOCK_PROPERTY_RATIO = "Ratio";
	private TruthValue currentValue = TruthValue.UNSET;
	private double deadband = 0.0;
	private double ratio = Double.NaN;
	private final ConcurrentLinkedQueue<TruthValue> buffer;
	private double limit = 0.0;
	private double scanInterval = 1.0;    // ~secs
	private double timeWindow = 60; // ~ secs
	private int bufferSize = 1;
	private HysteresisType hysteresis = HysteresisType.NEVER;
	private final Watchdog dog;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public LogicFilter() {
		initialize();
		buffer = new ConcurrentLinkedQueue<TruthValue>();
		bufferSize = (int)(0.5+timeWindow/scanInterval);
		dog = new Watchdog(getName(),this);
		initializePrototype();	
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public LogicFilter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
		buffer = new ConcurrentLinkedQueue<TruthValue>();
		dog = new Watchdog(getName(),this);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("LogicFilter");
		BlockProperty deadbandProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND,new Double(deadband),PropertyType.DOUBLE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_DEADBAND, deadbandProperty);
		BlockProperty hProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_HYSTERESIS,hysteresis,PropertyType.HYSTERESIS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_HYSTERESIS, hProperty);
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, intervalProperty);
		BlockProperty limitProperty = new BlockProperty(BLOCK_PROPERTY_MINIMUM_TRUE_FRACTION,new Double(limit),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_MINIMUM_TRUE_FRACTION,limitProperty);
		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME_MINUTES,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty ratioProperty = new BlockProperty(BLOCK_PROPERTY_RATIO,new Double(0.0),PropertyType.DOUBLE,false);
		ratioProperty.setBindingType(BindingType.ENGINE);
		ratioProperty.setDisplayed(true);
		ratioProperty.setDisplayOffsetX(25);
		ratioProperty.setDisplayOffsetY(45);
		setProperty(BLOCK_PROPERTY_RATIO, ratioProperty);

		// Define a single input and output
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setIsMultiple(false);
		anchors.add(input);
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	// Retain the current value and restart sampling
	@Override
	public void reset() {
		super.reset();
		dog.setSecondsDelay(scanInterval);
		timer.updateWatchdog(dog);  // pet dog
		buffer.clear();
		ratio = Double.NaN;
		currentValue = TruthValue.UNSET;
	}

	@Override
	public void start() {
		super.start();
		reset();
	}
	@Override
	public void stop() {
		timer.removeWatchdog(dog);
	}
	
	/**
	 * A new value has arrived. Simply set the current value.
	 * (We poll the current value on an interval).
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		if( qual.isGood() && qv!=null && qv.getValue()!=null ) {
			currentValue = incoming.getValueAsTruthValue();
			if(!dog.isActive()) evaluate();
		}
	}
	/**
	 * The interval has expired. Reset interval, then compute output.
	 * Do not compute anything if scan interval is illegal or
	 * there has been no input since start or reset().
	 */
	@Override
	public synchronized void evaluate() {
		//log.tracef("%s.evaluate: currentValue %s",getName(),currentValue.name());
		if( scanInterval<= 0.0) {
			reset();
			return;
		}
		if( currentValue.equals(TruthValue.UNSET) ) return;   // Nothing on input yet
		
		// Add the currentValue to the queue
		buffer.add(currentValue);
		
		while(buffer.size() > bufferSize ) {
			buffer.remove();
		}
		
		//log.tracef("%s.evaluate buffer %d of %d, current value=%s, state=%s (%s)",
		//		getName(),buffer.size(),bufferSize,currentValue.name(),state.name(),timer.getName());
		TruthValue newState = TruthValue.UNKNOWN;
		if( buffer.size() >= 1 ) {
			ratio = computeTrueRatio(bufferSize);
			// Even if locked, we update the property state
			controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_RATIO,
						new TestAwareQualifiedValue(timer,new Double(ratio)));
			newState = computeState(state,ratio,computeFalseRatio(bufferSize));
			//log.infof("%s.evaluate ... ratio %f (%s was %s)",getName(),ratio,newState.name(),state.name());
		}
		
		if( !isLocked() ) {
			if(!newState.equals(state)) {
				setState(newState);  // Sets last value as side effect
				//log.infof("%s.evaluate ... new state is %s",getName(),state.name());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
			}
		}
		// Set up the next poll, if we're not homogeneous.
		if( ratio>0.0 || ratio<1.0) {
			dog.setSecondsDelay(scanInterval);
			timer.updateWatchdog(dog);  // pet dog
		}
	}
	/**
	 * The explanation for this block just reports the comparison results
	 * 
	 * @return an explanation for the current state of the block.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram parent,List<UUID> members) {
		String explanation = "";
		if( state.equals(TruthValue.TRUE) ) {
			explanation = String.format("At %s, True ratio %s > %3.2f",getName(),String.valueOf(ratio),limit);
		}
		else if( state.equals(TruthValue.FALSE)) {
			explanation = String.format("At %s, False ratio %s >= %3.2f",getName(),String.valueOf(1.0-ratio),limit);
		}
		return explanation;
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Ratio", String.valueOf(ratio));
		attributes.put("LatestValue", currentValue.name());
		attributes.put("CurrentState", state.name());
		attributes.put("ActivelySampling",(dog.isActive()?"TRUE":"FALSE") );
		
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		int index = 0;
		for( TruthValue tv:buffer) {
			Map<String,String> qvMap = new HashMap<>();
			qvMap.put("RawValue"+index, tv.name());
			descBuffer.add(qvMap);
			index++;
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
		log.debugf("%s.propertyChange: Received %s = %s",TAG,propertyName,event.getNewValue().toString());
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_DEADBAND)) {
			try {
				deadband = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert deadband to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_HYSTERESIS)) {
			try {
				hysteresis = HysteresisType.valueOf(event.getNewValue().toString().toUpperCase());
			}
			catch(IllegalArgumentException iae) {
				log.warnf("%s.propertyChange: Unable to convert hysteresis (%s)",TAG,iae.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL)) {
			try {
				scanInterval = Double.parseDouble(event.getNewValue().toString());
				if( scanInterval>0.0) {
					if( scanInterval < 0.1 ) scanInterval = 0.1;
					if( scanInterval > timeWindow ) scanInterval = timeWindow;
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
					bufferSize = (int)(0.5+timeWindow/scanInterval);
					log.debugf("%s.propertyChange: buffer size now %d (%f / %f )",TAG,bufferSize,timeWindow,scanInterval);
				}
				else {
					reset();
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BLOCK_PROPERTY_MINIMUM_TRUE_FRACTION)) {
			try {
				limit = Double.parseDouble(event.getNewValue().toString());
				if( limit<0. ) limit = 0.0;
				if( limit>1.0) limit = 1.0; 
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert limit to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW)) {
			try {
				timeWindow = Double.parseDouble(event.getNewValue().toString()) * 60.0;
				if( timeWindow>0.0) {
					if( scanInterval > timeWindow ) scanInterval = timeWindow;
					dog.setSecondsDelay(scanInterval);
					timer.updateWatchdog(dog);  // pet dog
					if( scanInterval>0.0) bufferSize = (int)(0.5+timeWindow/scanInterval);
					log.debugf("%s.propertyChange: buffer size now %d (%f / %f )",TAG,bufferSize,timeWindow,scanInterval);
				}
				else {
					timer.removeWatchdog(dog);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_ACTIVITY_BUFFER_SIZE)) {
			;   // Handled by superclass
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
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
		controller.sendPropertyNotification(getBlockId().toString(), BLOCK_PROPERTY_RATIO,
				new TestAwareQualifiedValue(timer,new Double(ratio)));
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	

	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/logic_filter.png");
		prototype.setPaletteLabel("LogicFilter");
		prototype.setTooltipText("Monitor the incoming value for change over a specified period");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/logic_percent.png");
		desc.setPreferredHeight(40);
		desc.setPreferredWidth(80);
		desc.setStyle(BlockStyle.SQUARE);
	}
	
	/**
	 * Compute the fraction of true (known over total window).
	 */
	private double computeTrueRatio(int pointsInWindow) {
		int trueCount = 0;
		for(TruthValue tv:buffer) {
			if( tv.equals(TruthValue.TRUE)) trueCount++;
		}
		return 1.0*trueCount/pointsInWindow;	
	}
	/**
	 * Compute the fraction of true (known over total window).
	 */
	private double computeFalseRatio(int pointsInWindow) {
		int falseCount = 0;
		for(TruthValue tv:buffer) {
			if( tv.equals(TruthValue.FALSE)) falseCount++;
		}
		return 1.0*falseCount/pointsInWindow;	
	}
	
	/**
	 * Compute the overall state, presumably because of a new input.
	 * We take into account the hysteresis. If the buffer is not full,
	 * true and false ratios will not add to one.
	 */
	private TruthValue computeState(TruthValue current,double trueRatio,double falseRatio) {
		
		TruthValue result = TruthValue.UNSET;
		double threshold = limit;
		switch(hysteresis) {
			case TRUE:
				if( current.equals(TruthValue.TRUE)) threshold = limit - deadband;
				else threshold = limit;
				break;
			case FALSE:
				if( current.equals(TruthValue.TRUE)) threshold = limit;
				else threshold = limit + deadband;
				break;
			case ALWAYS:
				if( current.equals(TruthValue.TRUE)) threshold = limit - deadband;
				else threshold = limit + deadband;
				break;
			case NEVER:
			default:
				threshold = limit;  
		}
		if( trueRatio>=threshold ) result = TruthValue.TRUE;
		else if(falseRatio>1.0-threshold) result = TruthValue.FALSE;
		else result = TruthValue.UNKNOWN;
		return result;	
	}
}