/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQuality;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.model.values.Quality;

/**
 * This class accumulates an average value of all points that have arrived at the block's 
 * input during its lifetime. All points are given the same weight.
 */
@ExecutableBlock
public class MovingAverage extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "MovingAverage";
	private boolean clearOnReset = false;
	private double sum = 0.0;
	private int count = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public MovingAverage() {
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
	public MovingAverage(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}


	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("MovingAverage");
		BlockProperty resetProperty =  new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.FALSE,PropertyType.BOOLEAN,true);
		properties.put(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, resetProperty);
		
		// Define a single input.
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define the main output, a data value.
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset ) {
			count = 0;
			sum = 0.0;
		}
	}
	

	/**
	 * A new value has arrived. Add it to the total (if good).
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		this.state = BlockState.ACTIVE;
		double result = Double.NaN;
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		if( qual.isGood() && qv!=null && qv.getValue()!=null ) {
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				this.sum += dbl;
				this.count++;
				result = sum/count;
				qv = new BasicQualifiedValue(result,qv.getQuality(),qv.getTimestamp());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",TAG,nfe.getLocalizedMessage());
				qv = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
		}
		else {
			qv = new BasicQualifiedValue(Double.NaN,qual,qv.getTimestamp());
		}
		if( !isLocked() ) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(qv);
		}
	}

	/**
	 * Handle a changes to the various attributes.
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
				log.warnf("%s: propertyChange Unable to convert clear flag to a boolean (%s)",TAG,nfe.getLocalizedMessage());
			}
		}	
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("CountToPresent", String.valueOf(count));
		attributes.put("SumToPresent", String.valueOf(count));
		return descriptor;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/moving_average.png");
		prototype.setPaletteLabel("Average");
		prototype.setTooltipText("Compute the moving average of the input and place results on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/xbar.png");
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}