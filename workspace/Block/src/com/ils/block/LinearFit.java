/**
 *   (c) 2017-2020  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
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
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * Compute the best fit line over a recent history of data points. The x-axis
 * is the point count. 
 */
@ExecutableBlock
public class LinearFit extends AbstractProcessBlock implements ProcessBlock {
	private final static String BLOCK_PROPERTY_SCALE_FACTOR = "ScaleFactor";
	private final static int MIN_SAMPLE_SIZE = 2;
	private final static String Y_INTERCEPT_PORT_NAME = "yIntercept";
	private final static String SLOPE_PORT_NAME = "slope";

	
	private boolean clearOnReset = true;
	private boolean fillRequired = true;
	double[] coefficients = null;
	private int n = 0;
	private final FixedSizeQueue<QualifiedValue> queue;
	private int sampleSize = 2;
	private double scaleFactor = 1.0;
	private BlockProperty valueProperty = null;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public LinearFit() {
		queue = new FixedSizeQueue<QualifiedValue>(sampleSize);
		initialize();
		initializePrototype();
	}
	
	
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent resource Id identifying the parent of this block (a diagram)
	 * @param block universally unique Id for the block
	 */
	public LinearFit(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		queue = new FixedSizeQueue<QualifiedValue>(sampleSize);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset ) {
			queue.clear();
			valueProperty.setValue(Double.NaN);
		}
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("LinearFit");

		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		BlockProperty fillProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_FILL_REQUIRED, fillProperty);
		
		BlockProperty sampleSizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,sampleSize,PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sampleSizeProperty);
		BlockProperty sfProperty = new BlockProperty(BLOCK_PROPERTY_SCALE_FACTOR,scaleFactor,PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_SCALE_FACTOR, sfProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,Double.NaN,PropertyType.DOUBLE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
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
	 * A new value has arrived. Add it to the queue and compute statistics, if appropriate.
	 * @param vcn incoming new value.
	 */
	@Override
	public synchronized void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		log.infof("Accepting a new value...");
		
		QualifiedValue qv = vcn.getValue();
		log.debugf("%s.acceptValue: Received %s",getName(),qv.getValue().toString());
		if( qv.getQuality().isGood() ) {
			queue.add(qv);
			if( queue.size() >= sampleSize || (!fillRequired && queue.size()>=MIN_SAMPLE_SIZE)  ) {
				computeFit();     // Updates valueProperty
			}
		}
		else {
			// Post bad value on output, clear queue
			lastValue = new BasicQualifiedValue(Double.NaN,qv.getQuality(),qv.getTimestamp());
			if( !isLocked() ) {
				//TODO take a closer look at this!
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
				
				// The slope is also bad. Re-use lastValue.
				coefficients = null;
				nvn = new OutgoingNotification(this,SLOPE_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
			}
			queue.clear();
		}
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("BestFit", String.valueOf(valueProperty.getValue()));
		attributes.put("n", String.valueOf(n));
		if( coefficients!=null) {
			attributes.put("Intercept", String.valueOf(coefficients[0]));
			attributes.put("Slope", String.valueOf(coefficients[1]));
		}
		List<Map<String,String>> descBuffer = descriptor.getBuffer();
		Iterator<QualifiedValue> walker = queue.iterator();
		while( walker.hasNext() ) {
			Map<String,String> qvMap = new HashMap<>();
			QualifiedValue qv = walker.next();
			qvMap.put("Value", qv.getValue().toString());
			qvMap.put("Quality", qv.getQuality().toString());
			qvMap.put("Timestamp", qv.getTimestamp().toString());
			descBuffer.add(qvMap);
		}
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
			QualifiedValue qv = new BasicQualifiedValue(coefficients[1]*scaleFactor,lastValue.getQuality(),lastValue.getTimestamp());
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
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE) ) {
			// Trigger an evaluation
			try {
				int val = Integer.parseInt(event.getNewValue().toString());
				if( val>0 ) {
					sampleSize = val;
					if( sampleSize< MIN_SAMPLE_SIZE ) sampleSize = MIN_SAMPLE_SIZE;
					queue.setBufferSize(sampleSize);
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
		prototype.setPaletteIconPath("Block/icons/palette/linear_fit.png");
		prototype.setPaletteLabel("LinearFit");
		prototype.setTooltipText("Calculate linear fit on recent history");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ARITHMETIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/linear_fit_noframe.png");
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	
	/**
	 * Compute a linear or cubic fit. Updates valueProperty with the best fit for the
	 * last data point. Also sets the slope and y-intercept.  The x value is the number 
	 * of seconds from the first point (the first point is 0).
	 */
	private void computeFit() {
		final WeightedObservedPoints obs = new WeightedObservedPoints();
		QualifiedValue qv = null;
		QualifiedValue qvNew = null;
		Double xVal = null;
		Double yVal = null;
		Date baseDate = null;
		Date theDate = null;
		log.infof("Computing fit...");
		n = 0;
		while (n < queue.size()){
			qv = queue.get(n);
			try {
				yVal = Double.parseDouble(qv.getValue().toString());
				if (n == 0){
					xVal = 0.0;
					baseDate = qv.getTimestamp();
				}
				else{
					theDate = qv.getTimestamp();
					xVal = (double) (theDate.getTime()/1000 - baseDate.getTime()/1000);
				}
					
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.computeRateOfChange detected not-a-number in queue (%s), ignored",getName(),nfe.getLocalizedMessage());
				continue;
			}
			log.infof("Adding (%f, %f)", xVal, yVal);
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
		qvNew = new BasicQualifiedValue(yVal, qv.getQuality(), qv.getTimestamp());
		OutgoingNotification nvn = new OutgoingNotification(this, BlockConstants.OUT_PORT_NAME, qvNew);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus(lastValue);
		
		// Propagate the slope scaled
		qvNew = new BasicQualifiedValue(coefficients[1]*scaleFactor, qv.getQuality(), qv.getTimestamp());
		nvn = new OutgoingNotification(this, SLOPE_PORT_NAME, qvNew);
		controller.acceptCompletionNotification(nvn);
		
		// Propagate the y-intercept
		qvNew = new BasicQualifiedValue(coefficients[0], qv.getQuality(), qv.getTimestamp());
		nvn = new OutgoingNotification(this, Y_INTERCEPT_PORT_NAME, qvNew);
		controller.acceptCompletionNotification(nvn);
	}
}