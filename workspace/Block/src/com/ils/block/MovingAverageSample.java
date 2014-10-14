/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.block.common.FixedSizeQueue;
import com.ils.blt.common.UtilityFunctions;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.BlockPropertyChangeEvent;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.control.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class computes the average of the last "n" readings.
 */
@ExecutableBlock
public class MovingAverageSample extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "MovingAverageSample";
	private final static int DEFAULT_BUFFER_SIZE = 1;
	
	private final FixedSizeQueue<QualifiedValue> queue;
	private int sampleSize = DEFAULT_BUFFER_SIZE;
	private boolean clearOnReset = false;
	private BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public MovingAverageSample() {
		queue = new FixedSizeQueue<QualifiedValue>(DEFAULT_BUFFER_SIZE);
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
	public MovingAverageSample(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		queue = new FixedSizeQueue<QualifiedValue>(DEFAULT_BUFFER_SIZE);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("MovingAverageSample");

		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,new Boolean(clearOnReset),PropertyType.BOOLEAN,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		BlockProperty sizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,new Integer(sampleSize),PropertyType.INTEGER,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sizeProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,TruthValue.UNKNOWN,PropertyType.TRUTHVALUE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		properties.put(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		
		// Define a single input.
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define the main output, a truth value.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset) {
			queue.clear();
		}
	}
	
	/**
	 * A new value has arrived. Add it to the queue.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(BlockConstants.IN_PORT_NAME) ) {
			QualifiedValue qv = vcn.getValue();
			log.infof("%s.acceptValue: Received %s",TAG,qv.getValue().toString());
			if( qv.getQuality().isGood() ) {
				queue.add(qv);
				if( queue.size() >= sampleSize) {
					double result = computeAverage();
					if( !isLocked() ) {
						// Give it a new timestamp
						QualifiedValue outval = new BasicQualifiedValue(result);
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
						controller.acceptCompletionNotification(nvn);
					}
					// Even if locked, we update the current state
					valueProperty.setValue(result);
					controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,new BasicQualifiedValue(result));
				}
			}
			else {
				// Post bad value on output, clear queue
				if( !isLocked() ) {
					QualifiedValue outval = new BasicQualifiedValue(new Double(Double.NaN),qv.getQuality(),qv.getTimestamp());
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
					controller.acceptCompletionNotification(nvn);
				}
				queue.clear();
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
		if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET)) {
			clearOnReset = (new UtilityFunctions()).coerceToBoolean(event.getNewValue().toString());
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE) ) {
			// Trigger an evaluation
			try {
				int val = Integer.parseInt(event.getNewValue().toString());
				if( val>0 ) {
					sampleSize = val;
					queue.clear();
					queue.setBufferSize(sampleSize);
					// Even if locked, we update the current state
					valueProperty.setValue(0.0);
					controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,new BasicQualifiedValue(0.0));
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert sample size to an integer (%s)",TAG,nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}

	
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/moving_average.png");
		prototype.setPaletteLabel("SampleAve");
		prototype.setTooltipText("Compute the moving average of the input and place results on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/xbarn.png");
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
	/**
	 * Compute the average, presumably because of a new input.
	 */
	private double computeAverage() {
		double result = 0.0;
		double sum = 0.0;
		int count = 0;
		
		for( QualifiedValue qv:queue) {
			if( qv.getValue()==null ) continue;  // Shouldn't happen
			double val = 0.0;
			try {
				val = Double.parseDouble(qv.getValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s:computeAverage detected not-a-number in queue (%s), ignored",TAG,nfe.getLocalizedMessage());
				continue;
			}
			sum = sum + val;
			count++;
		}
		
		if( count>0 ) result = sum/count;
		return result;	
	}
	
}