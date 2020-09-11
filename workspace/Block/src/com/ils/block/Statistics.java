/**
 *   (c) 2020  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.StatFunction;
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
 * This class performs a statistical calculation from the most recent values on 
 * each of its potential multiple inputs.
 */
@ExecutableBlock
public class Statistics extends AbstractProcessBlock implements ProcessBlock {
	private final String CLSS = "Statistics";
	private boolean clearOnReset = false;
	private StatFunction function = StatFunction.RANGE;
	private double sum = 0.0;
	private int count = 0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Statistics() {
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
	public Statistics(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}


	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("Statistics");
		BlockProperty resetProperty =  new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,new Boolean(clearOnReset),PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, resetProperty);
		BlockProperty statProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION,function,PropertyType.STATISTICS,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION, statProperty);
		
		// Define a single input, but it can support multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setIsMultiple(true);
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
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		double result = Double.NaN;
		QualifiedValue qv = incoming.getValue();
		Quality qual = qv.getQuality();
		if( qual.isGood() && qv!=null && qv.getValue()!=null ) {
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				this.sum += dbl;
				this.count++;
				result = sum/count;
				lastValue = new BasicQualifiedValue(result,qv.getQuality(),qv.getTimestamp());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s.acceptValue: Unable to convert incoming value to a double (%s)",CLSS,nfe.getLocalizedMessage());
				lastValue = new BasicQualifiedValue(Double.NaN,new BasicQuality(nfe.getLocalizedMessage(),Quality.Level.Bad),qv.getTimestamp());
			}
		}
		else {
			lastValue = new BasicQualifiedValue(Double.NaN,qual,qv.getTimestamp());
		}
		if( !isLocked() ) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
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
				log.warnf("%s: propertyChange Unable to convert clear flag to a boolean (%s)",CLSS,nfe.getLocalizedMessage());
			}
		}
		else if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_STATISTICS_FUNCTION)) {
			try {
				function = StatFunction.valueOf(event.getNewValue().toString());
				reset();
			}
			catch(IllegalArgumentException nfe) {
				log.warnf("%s: propertyChange Unable to convert %s to a function (%s)",CLSS,event.getNewValue().toString(),nfe.getLocalizedMessage());
			}
		}	
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qval) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qval);
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("CountToPresent", String.valueOf(count));
		attributes.put("SumToPresent", String.valueOf(sum));
		return descriptor;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/statistics.png");
		prototype.setPaletteLabel("Statistics");
		prototype.setTooltipText("Compute a selected statistic across all inputs and place results on output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_STATISTICS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/statistics.png");
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}