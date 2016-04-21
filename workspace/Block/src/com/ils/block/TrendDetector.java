/**
 *   (c) 2016  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.SlopeCalculationOption;
import com.ils.blt.common.block.TrendDirection;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.FixedSizeQueue;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class applies SQC-like rules to its input with the objective of detecting trends.
 * If the scan interval is zero, then the application runs in an event-driven mode.
 * Otherwise, a timing loop is created and the input (the last value to have arrived) is
 * evaluated on loop timeout.
 */
@ExecutableBlock
public class TrendDetector extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "TrendDetector";
	protected static final String BLOCK_PROPERTY_CALCULATION_OPTION = "SlopeCalculationOption";
	protected static final String BLOCK_PROPERTY_PROJECTION = "ProjectedValue";
	protected static final String BLOCK_PROPERTY_RELATIVE_TO_TARGET = "RelativeToTarget";
	protected static final String BLOCK_PROPERTY_SLOPE = "CalculatedSlope";
	protected static final String BLOCK_PROPERTY_STDDEV = "CalculatedStandardDeviation";
	protected static final String BLOCK_PROPERTY_STANDARD_DEVIATION_MULTIPLIER = "StandardDeviationMultiplicativeFactor";
	protected static final String BLOCK_PROPERTY_TEST_LABEL = "TestLabel";
	protected static final String BLOCK_PROPERTY_TREND_COUNT_THRESHOLD = "TrendCountThreshold";
	protected static final String BLOCK_PROPERTY_TREND_DIRECTION = "TrendDirection";
	protected static final String BLOCK_PROPERTY_TREND_POINTS_REQUIRED = "NumberOfTrendPointsRequired";
	
	
	// Input ports
	protected static final String PORT_STANDARD_DEVIATION = "standardDeviation";
	protected static final String PORT_TARGET = "target";
	protected static final String PORT_VALUE = "value";
	// Output ports
	protected static final String PORT_SLOPE = "slope";
	protected static final String PORT_VARIANCE = "var";
	protected static final String PORT_PROJECTION = "projection";
	private final static int DEFAULT_BUFFER_SIZE = 10;
	
	
	private boolean clearOnReset = false;
	private SlopeCalculationOption calculationOption = SlopeCalculationOption.AVERAGE;
	private int countThreshold = 2;
	private double multiplier = 1.0;
	private int pointsRequired = DEFAULT_BUFFER_SIZE;
	private boolean relativeToTarget = false;
	private TrendDirection trendDirection = TrendDirection.BOTH;
	
	
	private FixedSizeQueue<QualifiedValue> buffer;

	private double standardDeviation = Double.NaN;
	private double mean = Double.NaN;
	// Calculated parameters
	private double lastSignificantValue = Double.NaN;
	private double slope = Double.NaN;
	private double variance = Double.NaN;
	private double intercept = Double.NaN;
	private double interceptVariance = Double.NaN;
	private double projection = Double.NaN;
	// Counts
	int downwardCount = 0;
	int upwardCount   = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TrendDetector() {
		buffer = new FixedSizeQueue<QualifiedValue>(DEFAULT_BUFFER_SIZE);
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
	public TrendDetector(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		buffer = new FixedSizeQueue<QualifiedValue>(DEFAULT_BUFFER_SIZE);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Trend");
		this.isReceiver = false;
		this.isTransmitter = false;
		BlockProperty calculationOptionProperty = new BlockProperty(BLOCK_PROPERTY_CALCULATION_OPTION,calculationOption,PropertyType.SLOPEOPTION,true);
		setProperty(BLOCK_PROPERTY_CALCULATION_OPTION, calculationOptionProperty);
		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,new Boolean(clearOnReset),PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		BlockProperty labelProperty = new BlockProperty(BLOCK_PROPERTY_TEST_LABEL,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_TEST_LABEL, labelProperty);
		BlockProperty thresholdProperty = new BlockProperty(BLOCK_PROPERTY_TREND_COUNT_THRESHOLD,new Integer(countThreshold),PropertyType.INTEGER,true);
		setProperty(BLOCK_PROPERTY_TREND_COUNT_THRESHOLD, thresholdProperty);
		BlockProperty pointsRequiredProperty = new BlockProperty(BLOCK_PROPERTY_TREND_POINTS_REQUIRED,new Integer(pointsRequired),PropertyType.INTEGER,true);
		setProperty(BLOCK_PROPERTY_TREND_POINTS_REQUIRED, pointsRequiredProperty);
		BlockProperty relativeToTargetProperty = new BlockProperty(BLOCK_PROPERTY_RELATIVE_TO_TARGET,new Boolean(relativeToTarget),PropertyType.BOOLEAN,true);
		setProperty(BLOCK_PROPERTY_RELATIVE_TO_TARGET, relativeToTargetProperty);
		BlockProperty multiplierProperty = new BlockProperty(BLOCK_PROPERTY_STANDARD_DEVIATION_MULTIPLIER,new Double(multiplier),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_STANDARD_DEVIATION_MULTIPLIER, multiplierProperty);
		BlockProperty directionProperty = new BlockProperty(BLOCK_PROPERTY_TREND_DIRECTION,trendDirection,PropertyType.TRENDDIRECTION,true);
		setProperty(BLOCK_PROPERTY_TREND_DIRECTION, directionProperty);
		
		// Define ancillary properties that are "bound to the engine"
		BlockProperty slopeProperty = new BlockProperty(BLOCK_PROPERTY_SLOPE,new Double(0.0),PropertyType.DOUBLE,false);
		slopeProperty.setBindingType(BindingType.ENGINE);
		setProperty(BLOCK_PROPERTY_SLOPE, slopeProperty);
		BlockProperty stddevProperty = new BlockProperty(BLOCK_PROPERTY_STDDEV,new Double(0.0),PropertyType.DOUBLE,false);
		slopeProperty.setBindingType(BindingType.ENGINE);
		setProperty(BLOCK_PROPERTY_STDDEV, stddevProperty);
		BlockProperty projectionProperty = new BlockProperty(BLOCK_PROPERTY_PROJECTION,new Double(0.0),PropertyType.DOUBLE,false);
		projectionProperty.setBindingType(BindingType.ENGINE);
		setProperty(BLOCK_PROPERTY_PROJECTION, projectionProperty);
		
		
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
		// Auxiliary outputs contain trend calculations
		output = new AnchorPrototype(PORT_SLOPE,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("S");
		output.setHint(PlacementHint.B);
		anchors.add(output);
		output = new AnchorPrototype(PORT_VARIANCE,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("V");
		output.setHint(PlacementHint.B);
		anchors.add(output);
		output = new AnchorPrototype(PORT_PROJECTION,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("P");
		output.setHint(PlacementHint.B);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset ) {
			clear();

		}
	}
	
	private void clear() {
		buffer.clear();
		state = TruthValue.UNKNOWN;
		downwardCount = 0;
		upwardCount   = 0;
		lastSignificantValue = Double.NaN;
	}
	
	/**
	 * A new value has arrived. Add it to the queue. Reset the timeout timer.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		String port = incoming.getConnection().getDownstreamPortName();
		if( port.equals(PORT_VALUE)  ) {
			if( qual.isGood() && qv!=null && qv.getValue()!=null ) {
				if( !buffer.isEmpty() ) {
					QualifiedValue lastPoint = buffer.getLast();
					// Preserve the last value if we're within std deviation .... 
					try {
						double previous = ((Double)(lastPoint.getValue())).doubleValue();
						double current = Double.parseDouble(qv.getValue().toString());
						if( isSignificantlyDifferent(current) ) {
							if(current>previous) {
								upwardCount++;
								downwardCount = 0;
							}
							else {
								downwardCount++;
								upwardCount = 0;
							}
							evaluate();
						}
						buffer.add(new TestAwareQualifiedValue(timer,new Double(current)));
					}
					catch(NumberFormatException nfe) {
						log.warnf("%s.acceptValue Unable to convert number of points required to an integer (%s)",TAG,nfe.getLocalizedMessage());
					}
				}
				else {
					buffer.add(qv);
					try {
						double current = Double.parseDouble(qv.getValue().toString());
						lastSignificantValue = current;  // First value in buffer is significant
					}
					catch(NumberFormatException nfe) {
						log.warnf("%s.acceptValue Unable to convert current value to a double (%s)",TAG,nfe.getLocalizedMessage());
					}
				}
			}
			else {
				// Bad quality, emit the result immediately
				if( !state.equals(TruthValue.UNKNOWN) ) {
					state = TruthValue.UNKNOWN;
					if( !isLocked() ) {
						QualifiedValue outval = new BasicQualifiedValue(state,qual,qv.getTimestamp());
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
				// Need to test for last buffer value significant
				QualifiedValue lastPoint = buffer.getLast();
				double last = (Double)(lastPoint.getValue());
				if( isSignificantlyDifferent(last) ) {
					evaluate();
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert target value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
			
		}
		else if( port.equals(PORT_STANDARD_DEVIATION)  ) {
			qv = incoming.getValue();
			if( qv==null || qv.getValue()==null) return;
			if( buffer.isEmpty() ) return;
			try {
				standardDeviation = Double.parseDouble(qv.getValue().toString());
				// Need to test for last buffer value significant
				QualifiedValue lastPoint = buffer.getLast();
				double last = (Double)(lastPoint.getValue());
				if( isSignificantlyDifferent(last) ) {
					evaluate();
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert standard deviation value to a float (%s)",TAG,nfe.getLocalizedMessage());
			}
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
		log.debugf("%s.evaluate %d of %d",TAG,buffer.size(),pointsRequired);
		if( buffer.size() >= pointsRequired) {
			//Calculate the slope of the data
			TruthValue newState = getTrendState();
			if( !isLocked() && !newState.equals(state) ) {
				// Give it a new timestamp
				state = newState;
				QualifiedValue outval = new TestAwareQualifiedValue(timer,state);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
				controller.acceptCompletionNotification(nvn);
				
				if( state.equals(TruthValue.TRUE) || state.equals(TruthValue.FALSE)) {
					// Propagate the other values
					outval = new TestAwareQualifiedValue(timer,slope);
					nvn = new OutgoingNotification(this,PORT_SLOPE,outval);
					controller.acceptCompletionNotification(nvn);
					// output the standardDeviation, not variance
					double stddev = Math.sqrt(variance);
					outval = new TestAwareQualifiedValue(timer,stddev);
					nvn = new OutgoingNotification(this,PORT_VARIANCE,outval);
					controller.acceptCompletionNotification(nvn);
					outval = new TestAwareQualifiedValue(timer,projection);
					nvn = new OutgoingNotification(this,PORT_PROJECTION,outval);
					controller.acceptCompletionNotification(nvn);
					// Write the auxilliary values to block parameters
					controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_SLOPE,
							new TestAwareQualifiedValue(timer,new Double(slope)));
					controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_STDDEV,
							new TestAwareQualifiedValue(timer,new Double(stddev)));
					controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_PROJECTION,
							new TestAwareQualifiedValue(timer,new Double(projection)));
				}	
				notifyOfStatus();
			}
		}
	}
	
	/**
	 * Test to see if the most recent point is significantly different from the last
	 * @return
	 */
	private boolean isSignificantlyDifferent(double newval) {
		boolean result = false;
		if( !Double.isNaN(lastSignificantValue) ) {
			double lowLimit = lastSignificantValue - standardDeviation*multiplier;
			double highLimit = lastSignificantValue + standardDeviation*multiplier;
			if( newval<lowLimit || newval>=highLimit) {
				result = true;
			}
		}
		return result;
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
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.debugf("%s.propertyChange: %s = %s",TAG,propertyName,event.getNewValue().toString());
		if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_CALCULATION_OPTION)) {
			String type = event.getNewValue().toString().toUpperCase();
			calculationOption = SlopeCalculationOption.valueOf(type);
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET)) {
			try {
				clearOnReset = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert clear flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_RELATIVE_TO_TARGET)) {
			try {
				relativeToTarget = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert relative to target flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_STANDARD_DEVIATION_MULTIPLIER)) {
			try {
				multiplier = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert multiplier value to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TEST_LABEL)) {
			;   // Default handling is sufficient
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TREND_DIRECTION)) {
			String type = event.getNewValue().toString().toUpperCase();
			trendDirection = TrendDirection.valueOf(type);
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TREND_COUNT_THRESHOLD)) {
			try {
				countThreshold = Integer.parseInt(event.getNewValue().toString());
				if( countThreshold<2 ) countThreshold = 2;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert number of count thresholdto an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TREND_POINTS_REQUIRED)) {
			try {
				pointsRequired = Integer.parseInt(event.getNewValue().toString());
				if( pointsRequired<2 ) pointsRequired = 2;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert number of points required to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
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
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Mean (target)", String.valueOf(mean));
		attributes.put("StandardDeviation", String.valueOf(standardDeviation));
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		for( QualifiedValue qv:buffer) {
			Map<String,String> qvMap = new HashMap<>();
			qvMap.put("Value", qv.getValue().toString());
			descBuffer.add(qvMap);
		}
		return descriptor;
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/trend_observation.png");
		prototype.setPaletteLabel("Trend");
		prototype.setTooltipText("Perform an analysis on the input to detect trends");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("Trend");
		desc.setEmbeddedFontSize(18);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredWidth(100);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the trend detection state, presumably because of a new input.
	 * We are guaranteed that the buffer is full. 
	 */
	private TruthValue getTrendState() {
		TruthValue result = TruthValue.UNKNOWN;
		// Calculate the slope of the data
		if( estimateSlope() ) {
			double current = ((Double)buffer.getLast().getValue()).doubleValue();
			if( upwardCount>countThreshold && !trendDirection.equals(TrendDirection.DOWNWARD)) {
				if( relativeToTarget && current<mean ) result=TruthValue.TRUE;
				else if( relativeToTarget && current>=mean ) result=TruthValue.UNKNOWN;
				else result=TruthValue.TRUE;
			}
			else if( downwardCount>countThreshold && !trendDirection.equals(TrendDirection.UPWARD)) {
				if( relativeToTarget && current<mean ) result=TruthValue.TRUE;
				else if( relativeToTarget && current>=mean ) result=TruthValue.UNKNOWN;
				else result=TruthValue.TRUE;
			}
			else {
				result=TruthValue.FALSE;
			}
		}
		log.infof("%s.getTrendState: %d upward, %d downward => %s (%s)",TAG,upwardCount,downwardCount,result.toString(),trendDirection.toString());
		return result;	
	}
	/**
	 * Compute the slope and projected value. This function is only called if the required
	 * point threshold is met.
	 */
	private boolean estimateSlope() {
		boolean success = true;
		QualifiedValue begin = buffer.getFirst();
		QualifiedValue end = buffer.getLast();
		double averageInterval = ((double)(end.getTimestamp().getTime() - begin.getTimestamp().getTime()))/(pointsRequired-1);
		if( calculationOption.equals(SlopeCalculationOption.AVERAGE)) {
			
			slope = (((Double)(end.getValue())).doubleValue() - ((Double)(begin.getValue())).doubleValue() ) /
					(end.getTimestamp().getTime() - begin.getTimestamp().getTime() );
			// To calculate the projected value, we need the average time interval represented by the points
			// and then take the last point to project forward in time.
			projection = slope*averageInterval + ((Double)(end.getValue())).doubleValue();
			variance = 0.0;
		}
		else {
			// Linear fit
			if( leastSquareRegression() ) {
				projection = slope * (averageInterval + end.getTimestamp().getTime()) + intercept;
			}
			else {
				success =  false;
			}
		}
		return success;
	}
	/**
	 * Calculate the coefficients for the equation of the line (y = Mx + B) that best fits 
	 * the supplied points using a least squares regression.  Note that this method is only
	 * called if there are sufficient points in the buffer
	 * @return true if a slope could be calculated
	 */
	private boolean leastSquareRegression() {
		if( standardDeviation<=0.0 ) return false;
		
		slope = 0.0;
		variance = 0.0;
		projection = 0.0;

		double S = 0.0; 
		double Sx = 0.0; 
		double Sy = 0.0; 
		double Sxx = 0.0; 
		double Sxy = 0.0;
		for(QualifiedValue qv:buffer) {
			double x = qv.getTimestamp().getTime();     // msecs
			double y = ((Double)(qv.getValue())).doubleValue();
			S = S + 1/(standardDeviation*standardDeviation);
			Sx = Sx + x/(standardDeviation*standardDeviation);
			Sy = Sy + y/(standardDeviation*standardDeviation);
			Sxx = Sxx + x*x/(standardDeviation*standardDeviation);
			Sxy = Sxy + x*y/(standardDeviation*standardDeviation);
		}
		// Denominator
		double DEN = S * Sxx - Sx*Sx;
		// Calculate slope and intercept.  Make sure DEN is not 0! 
		if( DEN == 0.0 ) return false;

		slope = (S * Sxy - Sx * Sy)/DEN;
		variance = S / DEN;
		intercept = (Sxx * Sy - Sx * Sxy)/DEN;
		interceptVariance = Sxx / DEN;

		return true;
	}
}