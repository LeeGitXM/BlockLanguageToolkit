/**
 *   (c) 2016-2019  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.math3.fitting.PolynomialCurveFitter;
import org.apache.commons.math3.fitting.WeightedObservedPoints;

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
	protected static final String PORT_SLOPE_VARIANCE = "var";
	protected static final String PORT_PROJECTION = "projection";
	private final static int DEFAULT_BUFFER_SIZE = 10;
	
	
	private SlopeCalculationOption calculationOption = SlopeCalculationOption.AVERAGE;
	private int countThreshold = 2;
	private double multiplier = 1.0;
	private int pointsRequired = DEFAULT_BUFFER_SIZE;
	private boolean relativeToTarget = false;   // When false compare to actual mean
	private TrendDirection trendDirection = TrendDirection.BOTH;
	
	
	private FixedSizeQueue<QualifiedValue> buffer;
	private double previous = Double.NaN;
	private double current  = Double.NaN;
	private double standardDeviation = Double.NaN;
	private double target = Double.NaN;
	// Calculated parameters
	private double slope = Double.NaN;
	private double slopeVariance = Double.NaN;
	private double intercept = Double.NaN;
	private double interceptVariance = Double.NaN;
	private double projection = Double.NaN;
	// Counts
	private int downwardCount = 0;
	private int upwardCount   = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TrendDetector() {
		buffer = new FixedSizeQueue<QualifiedValue>(DEFAULT_BUFFER_SIZE);
		initialize();
		initializePrototype();
		clear();
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
//		this.setReceiver(false);
//		this.setTransmitter(false);
		BlockProperty calculationOptionProperty = new BlockProperty(BLOCK_PROPERTY_CALCULATION_OPTION,calculationOption,PropertyType.SLOPEOPTION,true);
		setProperty(BLOCK_PROPERTY_CALCULATION_OPTION, calculationOptionProperty);
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
		AnchorPrototype input = new AnchorPrototype(PORT_TARGET,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("T");
		input.setHint(PlacementHint.LT);
		input.setIsMultiple(false);
		anchors.add(input);
		input = new AnchorPrototype(PORT_VALUE,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("V");
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		anchors.add(input);
		input = new AnchorPrototype(PORT_STANDARD_DEVIATION,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setAnnotation("S");
		input.setHint(PlacementHint.LB);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define the main output, a truth value.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
		// Auxiliary outputs contain trend calculations
		output = new AnchorPrototype(PORT_SLOPE,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("S");
		output.setHint(PlacementHint.BL);
		anchors.add(output);
		output = new AnchorPrototype(PORT_SLOPE_VARIANCE,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("V");
		output.setHint(PlacementHint.B);
		anchors.add(output);
		output = new AnchorPrototype(PORT_PROJECTION,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setAnnotation("P");
		output.setHint(PlacementHint.BR);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		clear();
	}
	
	private void clear() {
		buffer.clear();
		downwardCount = 0;
		upwardCount   = 0;
	}
	
	/**
	 * A new value has arrived. Add it to the queue. Reset the timeout timer.
	 * @param incoming new value.
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
					// Preserve the last value if we're within a std deviation .... 
					try {
						previous = Double.parseDouble(lastPoint.getValue().toString());
						current = Double.parseDouble(qv.getValue().toString());
						if( !isOutlier(previous,current) ) {
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
						log.warnf("%s.acceptValue Unable to convert input value to an double (%s)",getName(),nfe.getLocalizedMessage());
					}
				}
				// Buffer was empty, add the point
				else {
					buffer.add(qv);
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
					notifyOfStatus();
				}
				clear();   // Reset the current buffer
			}
		}
		else if( port.equals(PORT_TARGET)  ) {
			qv = incoming.getValue();
			if( qv==null || qv.getValue()==null) return;
			try {
				target = Double.parseDouble(qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert target value to a float (%s)",getName(),nfe.getLocalizedMessage());
			}
			
		}
		else if( port.equals(PORT_STANDARD_DEVIATION)  ) {
			qv = incoming.getValue();
			if( qv==null || qv.getValue()==null) return;
			try {
				standardDeviation = Double.parseDouble(qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert standard deviation value to a float (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
	}

	/**
	 * Unlike most blocks, this method is not associated with a timer expiration.
	 * We simply use this to do the calculation. Make sure that all inputs are defined.
	 * If the buffer is empty or inputs are not set, return UNKNOWN.
	 */
	@Override
	public void evaluate() {
		if( buffer.size() >= pointsRequired             && 
			!(relativeToTarget && Double.isNaN(target)) &&
			!Double.isNaN(current)                     &&	
			!Double.isNaN(previous)                     &&
		    !Double.isNaN(standardDeviation)            ) {
			// Evaluate the buffer and report
			log.debugf("%s.evaluate %d of %d",getName(),buffer.size(),pointsRequired);
			//Calculate the slope of the data
			TruthValue newState = getTrendState();
			if( !isLocked() && !newState.equals(state) ) {
				setState(newState);    // Sets lastValue, updates activity buffer
				
				if( state.equals(TruthValue.TRUE) || state.equals(TruthValue.FALSE)) {
					// Propagate the other values
					QualifiedValue qv = new TestAwareQualifiedValue(timer,slope);
					OutgoingNotification notification = new OutgoingNotification(this,PORT_SLOPE,qv);
					controller.acceptCompletionNotification(notification);
					// output the standardDeviation, not variance
					double stddev = Math.sqrt(slopeVariance);
					qv = new TestAwareQualifiedValue(timer,stddev);
					notification = new OutgoingNotification(this,PORT_SLOPE_VARIANCE,qv);
					controller.acceptCompletionNotification(notification);
					qv = new TestAwareQualifiedValue(timer,projection);
					notification = new OutgoingNotification(this,PORT_PROJECTION,qv);
					controller.acceptCompletionNotification(notification);
					// Write the auxiliary values to block parameters
					controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_SLOPE,
							new TestAwareQualifiedValue(timer,new Double(slope)));
					controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_STDDEV,
							new TestAwareQualifiedValue(timer,new Double(stddev)));
					controller.sendPropertyNotification(getBlockId().toString(),BLOCK_PROPERTY_PROJECTION,
							new TestAwareQualifiedValue(timer,new Double(projection)));
				}	
				
			}
			
		}
		// Not enough points
		else {
			setState(TruthValue.UNKNOWN);
		}
		notifyOfStatus();
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
		controller.acceptCompletionNotification(nvn);
		
		//log.infof("%s.evaluate lastValue = %s",getName(),lastValue.getValue().toString());
	}
	
	/**
	 * Test to see if the most recent point is too far different from the last
	 * @return true if the point is OK to use
	 */
	private boolean isOutlier(double priorval,double newval) {
		boolean result = false;
		if( !Double.isNaN(priorval) ) {
			double lowLimit =   priorval - standardDeviation*multiplier;
			double highLimit = priorval + standardDeviation*multiplier;
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
		//log.infof("%s.notifyOfStatus %s",getName(),qv.getValue().toString());
		lastValue = qv;
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Special implementation, since we feed to 4 ports.
	 */
	@Override
	public void propagate() {
		super.propagate();     // Handles port OUT
		if( Double.isNaN(slope)) {
			QualifiedValue qv = new TestAwareQualifiedValue(timer,slope);
			OutgoingNotification nvn = new OutgoingNotification(this,PORT_SLOPE,qv);
			controller.acceptCompletionNotification(nvn);
		}
		if( Double.isNaN(slopeVariance)) {
			// output the standardDeviation, not variance
			double stddev = Math.sqrt(slopeVariance);
			QualifiedValue qv = new TestAwareQualifiedValue(timer,stddev);
			OutgoingNotification nvn = new OutgoingNotification(this,PORT_SLOPE_VARIANCE,qv);
			controller.acceptCompletionNotification(nvn);
		}
		if( Double.isNaN(projection)) {
			QualifiedValue qv = new TestAwareQualifiedValue(timer,projection);
			OutgoingNotification nvn = new OutgoingNotification(this,PORT_PROJECTION,qv);
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
		log.debugf("%s.propertyChange: %s = %s",getName(),propertyName,event.getNewValue().toString());
		if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_CALCULATION_OPTION)) {
			String type = event.getNewValue().toString().toUpperCase();
			calculationOption = SlopeCalculationOption.valueOf(type);
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_RELATIVE_TO_TARGET)) {
			try {
				relativeToTarget = Boolean.parseBoolean(event.getNewValue().toString());
				evaluate();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert relative to target flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_STANDARD_DEVIATION_MULTIPLIER)) {
			try {
				multiplier = Double.parseDouble(event.getNewValue().toString());
				evaluate();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert multiplier value to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TEST_LABEL)) {
			;   // Default handling is sufficient
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TREND_DIRECTION)) {
			String type = event.getNewValue().toString().toUpperCase();
			trendDirection = TrendDirection.valueOf(type);
			evaluate();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TREND_COUNT_THRESHOLD)) {
			try {
				countThreshold = Integer.parseInt(event.getNewValue().toString());
				if( countThreshold<2 ) countThreshold = 2;
				evaluate();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert number of count thresholdto an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_TREND_POINTS_REQUIRED)) {
			try {
				pointsRequired = Integer.parseInt(event.getNewValue().toString());
				if( pointsRequired<2 ) pointsRequired = 2;
				evaluate();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert number of points required to an integer (%s)",getName(),nfe.getLocalizedMessage());
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
		attributes.put("Mean (target)", String.valueOf(target));
		attributes.put("StandardDeviation", String.valueOf(standardDeviation));
		attributes.put("Downward Count", String.valueOf(downwardCount));
		attributes.put("UpwardCount", String.valueOf(upwardCount));
		attributes.put("CountThreshold", String.valueOf(countThreshold));
		attributes.put("TrendDirection", String.valueOf(trendDirection.name()));
		attributes.put("RelativeToTarget", (relativeToTarget?"TRUE":"FALSE"));
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
		prototype.setPaletteLabel("TrendDetection");
		prototype.setTooltipText("Perform an analysis on the input to detect trends");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
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
	 * We are guaranteed that the buffer is full and values are good. Only those 
	 * points that are within a std deviation of the previous are in the history.
	 * 
	 * This code is a direct translation of the G2:
	 * 			 EM-GDA-SIMPLE-TREND-OBSERVATION.EM-GDA-EVALUATOR
	 */
	private TruthValue getTrendState() {
		TruthValue result = TruthValue.FALSE;     // No trend detected
		// Calculate the slope of the data
		if( estimateSlope() ) {
			if( upwardCount>countThreshold && !trendDirection.equals(TrendDirection.DOWNWARD)) {
				if( relativeToTarget && current>target ) result=TruthValue.TRUE;
				else if( relativeToTarget && current<=target ) result=TruthValue.UNKNOWN;
				else result=TruthValue.TRUE;
			}
			else if( downwardCount>countThreshold && !trendDirection.equals(TrendDirection.UPWARD)) {
				if( relativeToTarget && current<target ) result=TruthValue.TRUE;
				else if( relativeToTarget && current>=target ) result=TruthValue.UNKNOWN;
				else result=TruthValue.TRUE;
			}
			else {
				result=TruthValue.FALSE;
			}
		}
		//log.infof("%s.getTrendState: %d upward, %d downward => %s (%s)",getName(),upwardCount,downwardCount,result.toString(),trendDirection.toString());
		return result;	
	}
	/**
	 * Compute the slope and projected value. This function is only called if the required
	 * point threshold is met. It is a direct translation of em-gda-calc-slope-estimate
	 */
	private boolean estimateSlope() {
		boolean success = true;
		QualifiedValue begin = buffer.getFirst();
		QualifiedValue end = buffer.getLast();
		double averageInterval = ((double)(end.getTimestamp().getTime() - begin.getTimestamp().getTime()))/(pointsRequired-1);
		if( calculationOption.equals(SlopeCalculationOption.AVERAGE)) {
			
			slope = (fcns.coerceToDouble(end.getValue()) - fcns.coerceToDouble(begin.getValue()) ) /
					(end.getTimestamp().getTime() - begin.getTimestamp().getTime() );
			// To calculate the projected value, we need the average time interval represented by the points
			// and then take the last point to project forward in time.
			projection = slope*averageInterval + fcns.coerceToDouble(end.getValue());
			slopeVariance = 0.0;
		}
		// Least squares fit
		else {
			final WeightedObservedPoints obs = new WeightedObservedPoints();
			for(QualifiedValue qv:buffer) {
				obs.add(qv.getTimestamp().getTime(),fcns.coerceToDouble(qv.getValue()));
			}
			// Instantiate a linear fitter
			final PolynomialCurveFitter fitter = PolynomialCurveFitter.create(1);
			// Retrieve fitted parameters (coefficients of the polynomial function).
			double[] coefficients = fitter.fit(obs.toList());
			//log.infof("%s.computeFit: Coefficients are: %s %s",getName(),String.valueOf(coefficients[0]),String.valueOf(coefficients[1]));
			// Value = mx + b
			slope = coefficients[1];
			projection = coefficients[1]*end.getTimestamp().getTime()+coefficients[0];
		}
		return success;
	}

}