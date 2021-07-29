/**
 *   (c) 2020  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.ils.common.FixedSizeQueue;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualityCode;

/**
 * Compute the best fit line over a recent history of data points. The incoming
 * points must arrive together within the space of a configured interval.
 */
@ExecutableBlock
public class XYFit extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "xyFit";
	private final static String BLOCK_PROPERTY_SCALE_FACTOR = "ScaleFactor";
	private final static int MIN_SAMPLE_SIZE = 2;
	private final static String Y_INTERCEPT_PORT_NAME = "yIntercept";
	private final static String SLOPE_PORT_NAME = "slope";
	private final static String X_PORT_NAME = "x";
	private final static String Y_PORT_NAME = "y";
	private double synchInterval = 0.5; // 1/2 sec synchronization by default
	
	private boolean clearOnReset = true;
	private boolean fillRequired = true;
	double[] coefficients = null;
	private int n = 0;
	protected QualifiedValue x = null;
	protected QualifiedValue y = null;
	private final FixedSizeQueue<QualifiedValue> xQueue;
	private final FixedSizeQueue<QualifiedValue> yQueue;
	private int sampleSize = 2;
	private double scaleFactor = 1.0;
	private BlockProperty valueProperty = null;
	protected final Watchdog dog;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public XYFit() {
		dog = new Watchdog(TAG,this);
		log.infof("Creating a value queue with %d elements", sampleSize);
		xQueue = new FixedSizeQueue<QualifiedValue>(sampleSize);
		yQueue = new FixedSizeQueue<QualifiedValue>(sampleSize);
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
	public XYFit(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		dog = new Watchdog(TAG,this);
		log.infof("Creating (2) a value queue with %d elements", sampleSize);
		xQueue = new FixedSizeQueue<QualifiedValue>(sampleSize);
		yQueue = new FixedSizeQueue<QualifiedValue>(sampleSize);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset ) {
			xQueue.clear();
			yQueue.clear();
			valueProperty.setValue(new Double(Double.NaN));
		}
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("XYFit");

		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		
		BlockProperty fillProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED, fillProperty);
		
		BlockProperty synch = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL,new Double(synchInterval),PropertyType.TIME_SECONDS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL, synch);
		
		BlockProperty sampleSizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,new Integer(sampleSize),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sampleSizeProperty);
		
		BlockProperty sfProperty = new BlockProperty(BLOCK_PROPERTY_SCALE_FACTOR,new Double(scaleFactor),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_SCALE_FACTOR, sfProperty);
		
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,new Double(Double.NaN),PropertyType.DOUBLE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		// Define the x input
		AnchorPrototype input = new AnchorPrototype(X_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		input.setAnnotation("X");
		anchors.add(input);
		
		// Define the y input
		input = new AnchorPrototype(Y_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		input.setAnnotation("Y");
		anchors.add(input);

		// There are three outputs.  The main one is the predicted y value.
		// Then there is M and b, from the equation of a line, y = Mx+b
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.RT);
		anchors.add(output);
		
		AnchorPrototype slopePort = new AnchorPrototype(SLOPE_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		slopePort.setHint(PlacementHint.R);
		slopePort.setAnnotation("M");
		anchors.add(slopePort);
		
		AnchorPrototype yInerceptPort = new AnchorPrototype(Y_INTERCEPT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		yInerceptPort.setHint(PlacementHint.RB);
		yInerceptPort.setAnnotation("B");
		anchors.add(yInerceptPort);
	}
	

	/**
	 * We are notified that a new value has appeared on one of our input anchors
	 * For now we simply record the change in the map and start the watchdog.
	 * 
	 * A new value has arrived. Add it to the queue and compute statistics, if appropriate.
	 * @param vcn incoming new value.
	 */
	@Override
	public synchronized void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		
		log.infof("Accepting a value, the sample size is %d", sampleSize);
		QualifiedValue qv = vcn.getValue();
		if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(X_PORT_NAME)) {
			if( qv!=null && qv.getValue()!=null ) {
				x = qv;
			}
			else {
				x = null;
			}
		}
		else if (vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(Y_PORT_NAME)) {
			if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
				y = qv;
			}
			else {
				y = null;
			}
		}
		log.infof("Synch Interval: %f", synchInterval);
		dog.setSecondsDelay(synchInterval);
		timer.updateWatchdog(dog);  // pet dog
	}
	
	/**
	 * The coalescing time has expired. Place the current state on the output,
	 */
	@Override
	public void evaluate() {
		if( !isLocked() ) {
			log.infof("In evaluate()");
			state = TruthValue.UNKNOWN;
			QualifiedValue currentValue = null;
			if( x==null ) {
				currentValue = new TestAwareQualifiedValue(timer,state,QualityCode.Bad);
			}
			else if( y==null ) {
				currentValue = new TestAwareQualifiedValue(timer,state,QualityCode.Bad);
			}
			else if( !x.getQuality().isGood()) {
				currentValue = new TestAwareQualifiedValue(timer,state,x.getQuality());
			}
			else if( !y.getQuality().isGood()) {
				currentValue = new TestAwareQualifiedValue(timer,state,y.getQuality());
			}
			double xx = Double.NaN;
			double yy = Double.NaN;
			if( currentValue == null ) {
				try {
					xx = Double.parseDouble(x.getValue().toString());
					try {
						yy = Double.parseDouble(y.getValue().toString());
					}
					catch(NumberFormatException nfe) {
						currentValue = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN,QualityCode.Bad);	
					}
				}
				catch(NumberFormatException nfe) {
					currentValue = new TestAwareQualifiedValue(timer,TruthValue.UNKNOWN,QualityCode.Bad);
				}
			}
			
			if( currentValue==null ) {     // Success!
				if( x.getQuality().isGood() && y.getQuality().isGood() ) {
					xQueue.add(x);
					yQueue.add(y);
					computeFit();
				}
				else {
					QualityCode q = x.getQuality();
					if( q.isGood()) 
						q = y.getQuality();
					lastValue = new TestAwareQualifiedValue(timer,state,q);
					log.debugf("%s.evaluate: UNKNOWN x=%s, y=%s",getName(),x.toString(),y.toString());
				}
			}
			else {
				lastValue = currentValue;
			}
			/*  TODO - this should be done by compute fit
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
			*/
		}
	}
	
	/**
	 * @return a block-specific description of internal statue
	 * This runs in the gateway in response to a request from Designer to View Internal State
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		log.tracef("In getInternalStatus()");
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("BestFit", String.valueOf(valueProperty.getValue()));
		attributes.put("n", String.valueOf(n));
		if( coefficients!=null) {
			attributes.put("Intercept", String.valueOf(coefficients[0]));
			attributes.put("Slope", String.valueOf(coefficients[1]));
		}
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		
		log.tracef("Formatting the history buffer...");
		n = 0;
		while (n < xQueue.size()){
			log.tracef("Adding a point...");
			Map<String,String> qvMap = new HashMap<>();
			qvMap.put("X Value", xQueue.get(n).getValue().toString());
			qvMap.put("X Timestamp", xQueue.get(n).getTimestamp().toString());
			qvMap.put("Y Value", yQueue.get(n).getValue().toString());
			qvMap.put("Y Timestamp", yQueue.get(n).getTimestamp().toString());
			descBuffer.add(qvMap);
			n++;
		}
		log.tracef("...made the history buffer!");
		
		return descriptor;
	}
	/**
	 * We have a custom version as there are two ports, the normal output and slope.
	 */
	@Override
	public void propagate() {
		super.propagate();
		if( coefficients!=null && lastValue!=null ) {
			// Propagate the slope scaled
			QualifiedValue qv = new BasicQualifiedValue(new Double(coefficients[1]*scaleFactor),lastValue.getQuality(),lastValue.getTimestamp());
			OutgoingNotification nvn = new OutgoingNotification(this,SLOPE_PORT_NAME,qv);
			controller.acceptCompletionNotification(nvn);
		}
	}
	/**
	 * Handle a change to one of our custom properties.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET)) {
			try {
				clearOnReset = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert clear flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED)) {
			try {
				fillRequired = Boolean.parseBoolean(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert fill flag to a boolean (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SYNC_INTERVAL)) {
			try {
				synchInterval = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert synch interval to a double (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE) ) {
			// Trigger an evaluation
			try {
				int val = Integer.parseInt(event.getNewValue().toString());
				if( val>0 ) {
					sampleSize = val;
					if( sampleSize< MIN_SAMPLE_SIZE ) sampleSize = MIN_SAMPLE_SIZE;
					xQueue.setBufferSize(sampleSize);
					yQueue.setBufferSize(sampleSize);
					// Even if locked, we update the current state
					valueProperty.setValue(Double.NaN);
					controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,
							new TestAwareQualifiedValue(timer,0.0));
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert sample size to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_SCALE_FACTOR)) {
			try {
				scaleFactor = Double.parseDouble(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert scale factor to an number (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		updateStateForNewValue(qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/xy_fit.png");
		prototype.setPaletteLabel("XYFit");
		prototype.setTooltipText("Calculate linear fit on recent history of x,y values");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/xy_fit_noframe.png");
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	
	/**
	 * Compute a linear or cubic fit. Updates valueProperty with the best fit for the
	 * last data point. Also sets the slope.  The x value is simply the current point
	 * count.
	 */
	private void computeFit() {
		final WeightedObservedPoints obs = new WeightedObservedPoints();
		QualifiedValue x_qv = null;
		QualifiedValue y_qv = null;
		QualifiedValue qv = null;
		log.infof("%s.computeFit(), samples in queue = %d, computing...", getName(), xQueue.size());
		
		if (xQueue.size() != yQueue.size()){
			// TODO Do something drastic here, probs reinitialize everything!
			log.errorf("%s.computeFit() detected different sized x and y arrays",getName());
			return;
		}
		
		if (fillRequired && xQueue.size() < sampleSize && xQueue.size() < MIN_SAMPLE_SIZE) {
			log.warnf("%s.computeFit(), skipping evaluation because the buffer must be full.  The buffer size is %d and there are %d samples..", getName(), sampleSize, xQueue.size());
			return;
		}
		
		n = 0;
		double xVal = Double.NaN;
		double yVal = Double.NaN;
		
		while (n < xQueue.size()){

			try {
				xVal = Double.parseDouble(xQueue.get(n).getValue().toString());
				yVal = Double.parseDouble(yQueue.get(n).getValue().toString());
				
				x_qv = xQueue.get(n);
				y_qv = yQueue.get(n);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.computeRateOfChange detected not-a-number in queue (%s), ignored",getName(),nfe.getLocalizedMessage());
				continue;
			}
			obs.add(xVal, yVal);
			n++;
		}
		// Instantiate a linear fitter
		final PolynomialCurveFitter fitter = PolynomialCurveFitter.create(1);
		
		// Retrieve fitted parameters (coefficients of the polynomial function).
		coefficients = fitter.fit(obs.toList());
		log.infof("%s.computeFit: Coefficients are: %s %s",getName(),String.valueOf(coefficients[0]),String.valueOf(coefficients[1]));
		
		// Value = mx + b
		yVal = coefficients[1] * xVal + coefficients[0];
		valueProperty.setValue(yVal);
		
		// Propagate the calculated y value
		qv = new BasicQualifiedValue(yVal, x_qv.getQuality(), x_qv.getTimestamp());
		OutgoingNotification nvn = new OutgoingNotification(this, BlockConstants.OUT_PORT_NAME, qv);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus(lastValue);
		
		// Propagate the slope scaled
		qv = new BasicQualifiedValue(new Double(coefficients[1]*scaleFactor), x_qv.getQuality(), x_qv.getTimestamp());
		nvn = new OutgoingNotification(this, SLOPE_PORT_NAME, qv);
		controller.acceptCompletionNotification(nvn);
		
		// Propagate the y-intercept
		qv = new BasicQualifiedValue(new Double(coefficients[0]), x_qv.getQuality(), x_qv.getTimestamp());
		nvn = new OutgoingNotification(this, Y_INTERCEPT_PORT_NAME, qv);
		controller.acceptCompletionNotification(nvn);
	}
}