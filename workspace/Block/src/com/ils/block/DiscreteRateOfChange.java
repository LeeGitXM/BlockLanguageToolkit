/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

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
import com.ils.common.FixedSizeQueue;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class DiscreteRateOfChange extends AbstractProcessBlock implements ProcessBlock {
	private final static String BLOCK_PROPERTY_NUMBER_OF_POINTS = "NumberOfPoints";
	private final static String BLOCK_PROPERTY_POLYNOMIAL_ORDER = "PolynomialOrder";
	private final static String BLOCK_PROPERTY_SCALE_FACTOR = "ScaleFactor";

	
	private boolean clearOnReset = true;
	private final FixedSizeQueue<QualifiedValue> queue;
	private int polynomialOrder = 2;
	private int sampleSize = 5;
	private double scaleFactor = 1.0;
	private BlockProperty valueProperty = null;

	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DiscreteRateOfChange() {
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
	public DiscreteRateOfChange(ExecutionController ec,UUID parent,UUID block) {
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
		setName("DiscreteRateOfChange");
		
		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		BlockProperty poProperty = new BlockProperty(BLOCK_PROPERTY_POLYNOMIAL_ORDER,new Integer(polynomialOrder),PropertyType.INTEGER,true);
		setProperty(BLOCK_PROPERTY_POLYNOMIAL_ORDER, poProperty);
		BlockProperty sampleSizeProperty = new BlockProperty(BLOCK_PROPERTY_NUMBER_OF_POINTS,new Integer(sampleSize),PropertyType.INTEGER,true);
		setProperty(BLOCK_PROPERTY_NUMBER_OF_POINTS, sampleSizeProperty);
		BlockProperty sfProperty = new BlockProperty(BLOCK_PROPERTY_SCALE_FACTOR,new Double(scaleFactor),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_SCALE_FACTOR, sfProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,new Double(Double.NaN),PropertyType.DOUBLE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.R);
		anchors.add(output);
	}
	

	/**
	 * A new value has arrived. Add it to the queue and compute statistics, if appropriate.
	 * The queue must be full to trigger a computation.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		log.debugf("%s.acceptValue: Received %s",getName(),qv.getValue().toString());
			if( qv.getQuality().isGood() ) {
				queue.add(qv);
				if( queue.size() >= sampleSize) {
					double result = computeRateOfChange();
					if( !isLocked() ) {
						// Give it a new timestamp
						lastValue = new BasicQualifiedValue(result,qv.getQuality(),qv.getTimestamp());
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
						controller.acceptCompletionNotification(nvn);
						notifyOfStatus(lastValue);
					}
					else {
						// Even if locked, we update the current state
						valueProperty.setValue(result);
						controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
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
				}
				queue.clear();
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
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_POLYNOMIAL_ORDER)) {
			try {
				polynomialOrder = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert order number to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
			if( polynomialOrder!=2 && polynomialOrder!=3 ) {
				log.warnf("%s: propertyChange Polynomial order must be 2 or 3. Defaulting to 2",getName());
				polynomialOrder = 2;
			}
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_NUMBER_OF_POINTS) ) {
			// Trigger an evaluation
			try {
				int val = Integer.parseInt(event.getNewValue().toString());
				if( val>0 ) {
					sampleSize = val;
					if( sampleSize!=5 && sampleSize!=7) sampleSize = 5;
					queue.setBufferSize(sampleSize);
					// Even if locked, we update the current state
					valueProperty.setValue(Double.NaN);
					controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,
							new TestAwareQualifiedValue(timer,0.0));
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert number of points to an integer (%s)",getName(),nfe.getLocalizedMessage());
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
		prototype.setPaletteIconPath("Block/icons/palette/rate_of_change.png");
		prototype.setPaletteLabel("DiscreteChange");
		prototype.setTooltipText("Compute the instantaneous rate of change based on a quadratic or cubic fit over recent history");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/discrete_rate_of_change.png");
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(32);
		desc.setPreferredWidth(32);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	
	/**
	 * Compute a quadratic or cubic fit. \Use the commons math library curve fitter.
	 * @return the best fit slope evaluated at the last data point.
	 */
	private double computeRateOfChange() {
		final WeightedObservedPoints obs = new WeightedObservedPoints();
		
		double roc = Double.NaN;
		Pow pow = new Pow();
		int n = 0;
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
			
			// Instantiate a polynomial fitter of the proper degree.
			final PolynomialCurveFitter fitter = PolynomialCurveFitter.create(polynomialOrder);
			// Retrieve fitted parameters (coefficients of the polynomial function).
			final double[] coefficients = fitter.fit(obs.toList());
			// Our answer is the derivative at "n". We assume the coefficients are in decreasing order.
			int index = polynomialOrder;
			double derivative = 0.0;
			for( double coeff:coefficients) {
				if( index>0 ) {
					derivative = derivative + index * coeff * pow.value(n,index-1);
					index--;
				}
			}
			roc = derivative;
		}
		return roc;
	}
}