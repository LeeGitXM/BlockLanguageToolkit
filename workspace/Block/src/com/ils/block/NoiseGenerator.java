/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import org.apache.commons.math3.distribution.ExponentialDistribution;
import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.distribution.RealDistribution;
import org.apache.commons.math3.distribution.UniformRealDistribution;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.DistributionType;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class adds noise to its input according to a selected distribution. Not all parameters are 
 * applicable to any given distribution.
 * 
 * Uniform - upper, lower
 * Exponential - mean
 * Normal - mean, standardDeviation
 */
@ExecutableBlock
public class NoiseGenerator extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "NoiseGenerator";
	protected static String BLOCK_PROPERTY_LOWER = "Lower";
	protected static String BLOCK_PROPERTY_MEAN = "Mean";
	protected static String BLOCK_PROPERTY_STANDARD_DEVIATION = "StandardDeviation";
	protected static String BLOCK_PROPERTY_UPPER = "Upper";
	private RealDistribution distribution = null;
	private DistributionType distributionType = DistributionType.UNIFORM;
	private double lower = 0.;               // Default for uniform distribution
	private double mean = 0.0;               // Default for normal distribution
	private double standardDeviation = 1.0;  // Default for normal distribution
	private double upper = 1.0;              // Default for uniform distribution
	private double value = Double.NaN;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public NoiseGenerator() {
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
	public NoiseGenerator(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Handle a change to the distribution properties. On change, we create a new distribution.
	 * Do not allow erroneous values.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		double val = Double.NaN;
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_DISTRIBUTION)) {
			if( distributionType==DistributionType.EXPONENTIAL )  distribution = new ExponentialDistribution(mean);
			else if( distributionType==DistributionType.NORMAL )  distribution = new NormalDistribution(mean,standardDeviation);
			else if( distributionType==DistributionType.UNIFORM )  distribution = new UniformRealDistribution(lower,upper);
		}
		else if( propertyName.equals(BLOCK_PROPERTY_LOWER) ) {
			try {
				val = Double.parseDouble(event.getNewValue().toString());
				if( distributionType==DistributionType.UNIFORM && val < upper) {
					lower = val;
					distribution = new UniformRealDistribution(lower,upper);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert lower to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BLOCK_PROPERTY_MEAN) ) {
			try {
				mean = Double.parseDouble(event.getNewValue().toString());
				if( distributionType==DistributionType.EXPONENTIAL )  distribution = new ExponentialDistribution(mean);
				else if( distributionType==DistributionType.NORMAL )  distribution = new NormalDistribution(mean,standardDeviation);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert upper to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BLOCK_PROPERTY_STANDARD_DEVIATION) ) {
			try {
				standardDeviation = Double.parseDouble(event.getNewValue().toString());
				if( distributionType==DistributionType.NORMAL )  distribution = new NormalDistribution(mean,standardDeviation);
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert standard deviation to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equals(BLOCK_PROPERTY_UPPER) ) {
			try {
				val = Double.parseDouble(event.getNewValue().toString());
				if( distributionType==DistributionType.UNIFORM && val>lower) {
					upper = val;
					distribution = new UniformRealDistribution(lower,upper);
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert upper to a double (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * A new value has appeared on an input anchor. Smooth it exponentially and place it on the
	 * output.
	 * 
	 * Randomly spread values. The relevant block parameters depend on the chosen distribution. 
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		if( !isLocked() ) {
			String port = vcn.getConnection().getDownstreamPortName();
			QualifiedValue qv = vcn.getValue();
			if( port.equals(BlockConstants.IN_PORT_NAME) && qv.getQuality().isGood() ) {
				try {
					value = Double.parseDouble(qv.getValue().toString());
					if( distribution!=null) value += distribution.sample();
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(value));
					controller.acceptCompletionNotification(nvn);
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
				}
			}
			else {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new BasicQualifiedValue(new Double(Double.NaN),
						qv.getQuality(),qv.getTimestamp()));
				controller.acceptCompletionNotification(nvn);
			}
		}
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Noise generator");
		distribution = new UniformRealDistribution();
		BlockProperty type = new BlockProperty(BlockConstants.BLOCK_PROPERTY_DISTRIBUTION,distributionType.name(),PropertyType.STRING,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_DISTRIBUTION, type);
		// Uniform Distribution
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_LOWER,new Double(lower),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_LOWER, constant);
		constant = new BlockProperty(BLOCK_PROPERTY_UPPER,new Double(upper),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_UPPER, constant);
		constant = new BlockProperty(BLOCK_PROPERTY_MEAN,new Double(upper),PropertyType.DOUBLE,true);
		// Normal Distribution
		properties.put(BLOCK_PROPERTY_MEAN, constant);
		constant = new BlockProperty(BLOCK_PROPERTY_STANDARD_DEVIATION,new Double(upper),PropertyType.DOUBLE,true);
		properties.put(BLOCK_PROPERTY_STANDARD_DEVIATION, constant);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/noise_generator.png");
		prototype.setPaletteLabel("Random");
		prototype.setTooltipText("Add random noise to the incoming data.A noise generator.");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/noise.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}