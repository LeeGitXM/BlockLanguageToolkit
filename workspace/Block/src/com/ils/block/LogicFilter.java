/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 *   Code based on sample code at: 
 *        http://www.codeproject.com/Articles/36459/PID-process-control-a-Cruise-Control-example
 */
package com.ils.block;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
 * Monitor the incoming truth-value for percent-of-time true. The output
 * reflects whether the ratio exceeds the limit.
 */
@ExecutableBlock
public class LogicFilter extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "LogicFilter";
	private final static String BLOCK_PROPERTY_RATIO = "Ratio";
	private TruthValue currentState = TruthValue.UNSET;
	private TruthValue currentValue = TruthValue.UNSET;
	private double deadband = 0.0;
	private double ratio = Double.NaN;
	private final LinkedList<TruthValue> buffer;
	private double limit = 0.0;
	private double scanInterval = 1.0;    // ~secs
	private double timeWindow = 60; // ~ secs
	private HysteresisType hysteresis = HysteresisType.NEVER;
	private final Watchdog dog;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public LogicFilter() {
		dog = new Watchdog(TAG,this);
		buffer = new LinkedList<TruthValue>();
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
	public LogicFilter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		buffer = new LinkedList<TruthValue>();
		initialize();
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
		BlockProperty intervalProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL,new Double(scanInterval),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SCAN_INTERVAL, intervalProperty);
		BlockProperty limitProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_LIMIT,new Double(limit),PropertyType.DOUBLE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_LIMIT, limitProperty);
		BlockProperty windowProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW,new Double(timeWindow),PropertyType.TIME,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TIME_WINDOW, windowProperty);
		BlockProperty ratioProperty = new BlockProperty(BLOCK_PROPERTY_RATIO,new Double(0.0),PropertyType.DOUBLE,false);
		ratioProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, ratioProperty);

		// Define a single input and output
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(input);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		if( scanInterval>0.0) {
			dog.setSecondsDelay(scanInterval);
			controller.pet(dog);
		}
		buffer.clear();
		currentState = TruthValue.UNSET;
		currentValue = TruthValue.UNSET;
		ratio = Double.NaN;
	}

	@Override
	public void start() {
		if(!running) reset();
		super.start();
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
		if( qual.isGood() && qv!=null && qv.getValue()!=null ) {
			currentValue = incoming.getValueAsTruthValue();
		}
		else {
			qv = new BasicQualifiedValue(Double.NaN,qual,qv.getTimestamp());
			reset();     // Reset the evaluation interval
		}

	}
	/**
	 * The interval has expired. Reset interval, then compute output.
	 * Do not compute anything until all parameters have been set.
	 */
	@Override
	public synchronized void evaluate() {
		if( currentValue.equals(TruthValue.UNSET) ) return;   // Nothing on input yet
		
		// Add the currentValue to the queue
		buffer.addLast(currentValue);
		int maxPoints = (int)(timeWindow/scanInterval);
		while(buffer.size() > maxPoints ) {
			buffer.removeFirst();
		}
		//log.tracef("%s.evaluate buffer %d of %d",TAG,buffer.size(),maxPoints);
		
		dog.setSecondsDelay(scanInterval);
		controller.pet(dog);
		
		TruthValue newState = TruthValue.UNKNOWN;
		ratio = computeTrueRatio(maxPoints);
		// Even if locked, we update the current state
		controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_RATIO,new BasicQualifiedValue(new Double(ratio)));
		newState = computeState(currentState,ratio,computeFalseRatio(maxPoints));
		log.tracef("%s.evaluate ... ratio %f (%s)",TAG,ratio,newState.name());
		
		if( !isLocked() ) {
			if(newState!=currentState) {
				currentState = newState;
				QualifiedValue result = new BasicQualifiedValue(currentState.name());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(result);
			}
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
					controller.pet(dog);
				}
				else {
					controller.removeWatchdog(dog);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.propertyChange: Unable to convert scan interval to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_LIMIT)) {
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
				timeWindow = Double.parseDouble(event.getNewValue().toString());
				if( timeWindow>0.0) {
					if( scanInterval > timeWindow ) {
						scanInterval = timeWindow;
						dog.setSecondsDelay(scanInterval);
						controller.pet(dog);
					}
				}
				else {
					controller.removeWatchdog(dog);
				}
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
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new BasicQualifiedValue(new Double(ratio));
		notifyOfStatus(qv);
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BLOCK_PROPERTY_RATIO,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		for( TruthValue tv:buffer) {
			Map<String,String> qvMap = new HashMap<>();
			qvMap.put("Value", tv.name());
			descBuffer.add(qvMap);
		}
		return descriptor;
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
	 * We take into account the hysteresis.
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