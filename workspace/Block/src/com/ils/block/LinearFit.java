/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.math3.analysis.function.Pow;
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

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class LinearFit extends AbstractProcessBlock implements ProcessBlock {
	private final static String BLOCK_PROPERTY_SCALE_FACTOR = "ScaleFactor";
	private final static int MIN_SAMPLE_SIZE = 2;
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
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public LinearFit(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		queue = new FixedSizeQueue<QualifiedValue>(sampleSize);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset ) {
			queue.clear();
			valueProperty.setValue(new Double(Double.NaN));
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
		
		BlockProperty sampleSizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,new Integer(sampleSize),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sampleSizeProperty);
		BlockProperty sfProperty = new BlockProperty(BLOCK_PROPERTY_SCALE_FACTOR,new Double(scaleFactor),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_SCALE_FACTOR, sfProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,new Double(Double.NaN),PropertyType.DOUBLE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		anchors.add(input);

		// Define a two outputs
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.R);
		anchors.add(output);
		
		AnchorPrototype slopePort = new AnchorPrototype(SLOPE_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		slopePort = new AnchorPrototype(SLOPE_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		slopePort.setHint(PlacementHint.B);
		slopePort.setAnnotation("S");
		anchors.add(slopePort);
	}
	

	/**
	 * A new value has arrived. Add it to the queue and compute statistics, if appropriate.
	 * @param vcn incoming new value.
	 */
	@Override
	public synchronized void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		
			QualifiedValue qv = vcn.getValue();
			log.debugf("%s.acceptValue: Received %s",getName(),qv.getValue().toString());
			if( qv.getQuality().isGood() ) {
				queue.add(qv);
				if( queue.size() >= sampleSize || (!fillRequired && queue.size()>=MIN_SAMPLE_SIZE)  ) {
					computeFit();     // Updates alueProperty
					if( !isLocked() ) {
						// Give it a new timestamp
						lastValue = new BasicQualifiedValue(valueProperty.getValue(),qv.getQuality(),qv.getTimestamp());
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
						controller.acceptCompletionNotification(nvn);
						notifyOfStatus(lastValue);
						
						// Propagate the slope scaled
						qv = new BasicQualifiedValue(new Double(coefficients[1]*scaleFactor),qv.getQuality(),qv.getTimestamp());
						nvn = new OutgoingNotification(this,SLOPE_PORT_NAME,qv);
						controller.acceptCompletionNotification(nvn);
					}	
				}
			}
			else {
				// Post bad value on output, clear queue
				if( !isLocked() ) {
					lastValue = new BasicQualifiedValue(new Double(Double.NaN),qv.getQuality(),qv.getTimestamp());
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
		prototype.setTooltipText("Calculate linear (or cubic) fit on recent history");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
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
	 * last data point. Also sets the slope.  The x value is simply the current point
	 * count.
	 */
	private void computeFit() {
		final WeightedObservedPoints obs = new WeightedObservedPoints();
		
		n = 0;
		for(QualifiedValue qv:queue) {
			n++;
			double val = Double.NaN;
			try {
				val = Double.parseDouble(qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%computeRateOfChange detected not-a-number in queue (%s), ignored",getName(),nfe.getLocalizedMessage());
				continue;
			}
			obs.add(n,val);
			
			// Instantiate a linear fitter
			final PolynomialCurveFitter fitter = PolynomialCurveFitter.create(1);
			// Retrieve fitted parameters (coefficients of the polynomial function).
			coefficients = fitter.fit(obs.toList());
			log.infof("%s.computeFit: Coefficients are: %s %s",getName(),String.valueOf(coefficients[0]),String.valueOf(coefficients[1]));
			// Value = mx + b
			valueProperty.setValue(coefficients[1]*n+coefficients[0]);
		}
	}
}