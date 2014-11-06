/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.block.common.FixedSizeQueue;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.LimitType;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TransmissionScope;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.BroadcastNotification;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class applies one of the Westinghouse (Western Electric) SPC rules to its input.
 * If the scan interval is zero, then the application runs in an event-driven mode.
 * Otherwise, a timing loop is created and the input (the last value to have arrived) is
 * evaluated on loop timeout.
 */
@ExecutableBlock
public class SQC extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "SQC";
	protected static final String BLOCK_PROPERTY_MAXIMUM_OUT_OF_RANGE = "MaximumOutOfRange";
	protected static final String BLOCK_PROPERTY_TEST_LABEL = "TestLabel";
	protected static final String PORT_STANDARD_DEVIATION = "standardDeviation";
	protected static final String PORT_TARGET = "target";
	protected static final String PORT_VALUE = "value";
	private final static int DEFAULT_BUFFER_SIZE = 10;
	
	
	private boolean clearOnReset = false;
	private double limit = 3.0;
	private LimitType limitType = LimitType.HIGH;
	private int maxOut = 0;
	private FixedSizeQueue<Double> queue;
	private int sampleSize = DEFAULT_BUFFER_SIZE;
	private double standardDeviation = Double.NaN;
	private double mean = Double.NaN;
	TruthValue truthState = TruthValue.UNSET;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public SQC() {
		queue = new FixedSizeQueue<Double>(DEFAULT_BUFFER_SIZE);
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
	public SQC(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		queue = new FixedSizeQueue<Double>(DEFAULT_BUFFER_SIZE);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("SQC");
		this.isReceiver = true;
		this.isTransmitter = true;
		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,new Boolean(clearOnReset),PropertyType.BOOLEAN,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		BlockProperty limitProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_LIMIT,new Double(limit),PropertyType.DOUBLE,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_LIMIT, limitProperty);
		BlockProperty limitTypeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_LIMIT_TYPE,new String(limitType.name()),PropertyType.STRING,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_LIMIT_TYPE, limitTypeProperty);
		BlockProperty maxOutProperty = new BlockProperty(BLOCK_PROPERTY_MAXIMUM_OUT_OF_RANGE,new Integer(maxOut),PropertyType.INTEGER,true);
		properties.put(BLOCK_PROPERTY_MAXIMUM_OUT_OF_RANGE, maxOutProperty);
		BlockProperty sizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,new Integer(DEFAULT_BUFFER_SIZE),PropertyType.INTEGER,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sizeProperty);
		BlockProperty labelProperty = new BlockProperty(BLOCK_PROPERTY_TEST_LABEL,"",PropertyType.STRING,true);
		properties.put(BLOCK_PROPERTY_TEST_LABEL, labelProperty);
		
		// Define a 3 inputs.
		AnchorPrototype input = new AnchorPrototype(PORT_STANDARD_DEVIATION,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("S");
		anchors.add(input);
		input = new AnchorPrototype(PORT_TARGET,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("T");
		anchors.add(input);
		input = new AnchorPrototype(PORT_VALUE,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("V");
		anchors.add(input);

		// Define the main output, a truth value.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset ) {
			clear();
		}
	}

	@Override
	public void start() {
		clear();
	}
	
	private void clear() {
		queue.clear();
		truthState = TruthValue.UNSET;
	}
	
	/**
	 * A new value has arrived. Add it to the queue. Reset the timeout timer.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		this.state = BlockState.ACTIVE;
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		String port = incoming.getConnection().getDownstreamPortName();
		if( port.equals(PORT_VALUE)  ) {
			if( qual.isGood() && qv!=null && qv.getValue()!=null ) {
				try {
					Double dbl  = Double.parseDouble(qv.getValue().toString());
					if( dbl!=null ) {
						queue.add(dbl);
						evaluate();
					}
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s.acceptValue exception converting incoming %s to double (%s)",TAG,qv.getValue().toString(),nfe.getLocalizedMessage());
				}
			}
			else {
				// Bad quality, emit the result immediately
				if( !truthState.equals(TruthValue.UNKNOWN) ) {
					truthState = TruthValue.UNKNOWN;
					if( !isLocked() ) {
						QualifiedValue outval = new BasicQualifiedValue(truthState,qual,qv.getTimestamp());
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
						controller.acceptCompletionNotification(nvn);
					}
				}
				clear();   // Reset the current buffer
			}
		}
		else if( port.equals(PORT_TARGET)  ) {
			qv = incoming.getValue();
			if( qv==null || qv.getValue()==null) return;
			try {
				mean = Double.parseDouble(qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert target value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
			evaluate();
		}
		else if( port.equals(PORT_STANDARD_DEVIATION)  ) {
			qv = incoming.getValue();
			if( qv==null || qv.getValue()==null) return;
			try {
				standardDeviation = Double.parseDouble(qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert standard deviation value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
			evaluate();
		}
	}
	/**
	 * We're received a transmitted signal. Deal with it, if appropriate.
	 * At a later time, we may implement pattern filtering or some other
	 * method to filter out unwanted messages. For now, if we recognize the command,
	 * then execute it.
	 * 
	 * @param sn signal notification.
	 */
	@Override
	public void acceptValue(SignalNotification sn) {
		Signal signal = sn.getSignal();
		log.infof("%s.acceptValue: signal = %s ",TAG,signal.getCommand());
		if( signal.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_CLEAR_HIGH) && limitType.equals(LimitType.HIGH)) {
			reset();
		}
		else if( signal.getCommand().equalsIgnoreCase(BlockConstants.COMMAND_CLEAR_LOW) && limitType.equals(LimitType.LOW)) {
			reset();
		}
	}
	
	
	/**
	 * Unlike most blocks, this method is not associated with a timer expiration.
	 * We simply use this to do the calculation.
	 */
	@Override
	public void evaluate() {
		log.infof("%s.evaluate",TAG);
		if( Double.isNaN(mean) )              return;
		if( Double.isNaN(standardDeviation) ) return;

		// Evaluate the buffer and report
		log.infof("%s.evaluate %d of %d",TAG,queue.size(),sampleSize);
		if( queue.size() >= sampleSize) {
			TruthValue newState = getRuleState();
			if( !isLocked() && !newState.equals(truthState) ) {
				// Give it a new timestamp
				truthState = newState;
				QualifiedValue outval = new BasicQualifiedValue(truthState);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
				controller.acceptCompletionNotification(nvn);
				
				// Notify other blocks to suppress alternate results
				if( truthState.equals(TruthValue.TRUE)) {
					if( limitType.equals(LimitType.HIGH )) {
						Signal sig = new Signal(BlockConstants.COMMAND_CLEAR_LOW,"","");
						BroadcastNotification broadcast = new BroadcastNotification(getParentId(),TransmissionScope.LOCAL,sig);
						controller.acceptBroadcastNotification(broadcast);
					}
					else if( limitType.equals(LimitType.LOW )) {
						Signal sig = new Signal(BlockConstants.COMMAND_CLEAR_HIGH,"","");
						BroadcastNotification broadcast = new BroadcastNotification(getParentId(),TransmissionScope.LOCAL,sig);
						controller.acceptBroadcastNotification(broadcast);
					}
				}
			}
		}
		else {
			// Too few points (can still be TRUE)
			TruthValue newState = getRuleState();
			if( newState.equals(TruthValue.FALSE)) newState = TruthValue.UNKNOWN;
			if( !truthState.equals(newState)) {
				truthState = newState;
				if( !isLocked()  ) {
					QualifiedValue outval = new BasicQualifiedValue(truthState);
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
					controller.acceptCompletionNotification(nvn);
				}
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
		log.infof("%s.propertyChange: %s = %s",TAG,propertyName,event.getNewValue().toString());
		if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_MAXIMUM_OUT_OF_RANGE)) {
			try {
				maxOut = Integer.parseInt(event.getNewValue().toString());
				if( maxOut<1 ) maxOut = 1;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert max out-of-range to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE)) {
			try {
				sampleSize = Integer.parseInt(event.getNewValue().toString());
				if( sampleSize < 1 ) sampleSize = 1; 
				queue.setBufferSize(sampleSize);
				reset();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert sample size to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_LIMIT)) {
			try {
				limit = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert target value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_LIMIT_TYPE)) {
			String type = event.getNewValue().toString().toUpperCase();
			limitType = LimitType.valueOf(type);
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		for( Double dbl:queue) {
			Map<String,String> qvMap = new HashMap<>();
			qvMap.put("Value", String.valueOf(dbl));
			descBuffer.add(qvMap);
		}
		return descriptor;
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/SQC.png");
		prototype.setPaletteLabel("SQC");
		prototype.setTooltipText("Perform an SPC analysis on the input and place results on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("SQC");
		desc.setEmbeddedFontSize(36);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the state, presumably because of a new input.
	 */
	private TruthValue getRuleState() {
		TruthValue result = TruthValue.UNKNOWN;
		int total= 0;
		int high = 0;
		int low  = 0;
		int highside  = 0;   // Consecutive
		int lowside   = 0;
		int maxhighside  = 0;   // Consecutive
		int maxlowside   = 0;
		int outside = 0;
		double highLimit = mean+standardDeviation*limit;
		double lowLimit  = mean-standardDeviation*limit;
		for( Double dbl:queue) {
			double val = dbl.doubleValue();
			
			//log.infof("%s.getRuleState: val = %f (%f - %f)",TAG,val,lowLimit,highLimit);
			if( val < lowLimit) {
				low++;
			}
			else if( val > highLimit) {
				high++;
			}
			
			if( val>mean ) {
				highside++;
				if( highside>maxhighside ) maxhighside = highside;
				lowside= 0;
			}
			else if( val<mean ) {
				lowside++;
				if( highside>maxlowside ) maxlowside = lowside;
				highside= 0;
			}
			else {
				lowside= 0;
				highside = 0;
			}
			
			if(limitType.equals(LimitType.LOW) ) {
				if( val <lowLimit) {
					outside++;
				}
			}
			else if(limitType.equals(LimitType.HIGH) ) {
				if( val > highLimit) {
					outside++;
				}
			}
			else if(limitType.equals(LimitType.BOTH) ) {
				if(      val > mean+standardDeviation*limit) outside++;
				else if( val < mean-standardDeviation*limit) outside++;
			}
			total++;
		}
		
		if( limitType.equals(LimitType.CONSECUTIVE) ) {
			if( maxhighside >= maxOut || maxlowside >= maxOut ) result = TruthValue.TRUE; 
			else if( total>=sampleSize ) result = TruthValue.FALSE;
		}
		else if( outside>=maxOut) result = TruthValue.TRUE;
		else if( total>=sampleSize ) result = TruthValue.FALSE;
		
		log.infof("%s.getRuleState: Of %d results,  %d high, %d low => %s (%s)",TAG,total,high,low,result.toString(),limitType.toString());
		return result;	
	}
}