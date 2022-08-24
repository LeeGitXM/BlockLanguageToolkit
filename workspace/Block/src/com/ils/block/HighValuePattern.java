/**
 *   (c) 2017-2018  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
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
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * Detect if the last m of n samples are above a specified threshold value.
 */
@ExecutableBlock
public class HighValuePattern extends AbstractProcessBlock implements ProcessBlock {
	private final static int MIN_SAMPLE_SIZE = 1;
	
	private boolean clearOnReset = true;
	private int sampleSize = 1;
	private double threshold = 0.0;
	private int triggerCount = 1;
	private final FixedSizeQueue<QualifiedValue> queue;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public HighValuePattern() {
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
	public HighValuePattern(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		queue = new FixedSizeQueue<QualifiedValue>(sampleSize);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		if( clearOnReset ) {
			queue.clear();
			setState(TruthValue.UNSET);  // Updates activity and lastValue
		}
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		BlockProperty clearProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET,Boolean.TRUE,PropertyType.BOOLEAN,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_CLEAR_ON_RESET, clearProperty);
		BlockProperty sampleSizeProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,sampleSize,PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, sampleSizeProperty);
		BlockProperty thresholdProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_THRESHOLD,threshold,PropertyType.DOUBLE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_THRESHOLD, thresholdProperty);
		BlockProperty tcProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT,triggerCount,PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT, tcProperty);

		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		output.setHint(PlacementHint.R);
		anchors.add(output);
	}
	

	/**
	 * A new value has arrived. Add it to the queue and compute statistics, if appropriate.
	 * @param vcn incoming new value.
	 */
	@Override
	public synchronized void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		
			QualifiedValue qv = vcn.getValue();
			//log.infof("%s.acceptValue: Received %s",getName(),qv.getValue().toString());
			if( qv.getQuality().isGood() ) {
				queue.add(qv);
				setState(computeState());   // Sets lastValue
				if( !isLocked() ) {
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(lastValue);
				}
			}
			else {
				// Post bad value on output, clear queue
				if( !isLocked() ) {
					lastValue = new BasicQualifiedValue(Double.NaN,qv.getQuality(),qv.getTimestamp());
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(lastValue);
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
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE)) {
			try {
				sampleSize = Integer.parseInt(event.getNewValue().toString());
				if( sampleSize< MIN_SAMPLE_SIZE ) sampleSize = MIN_SAMPLE_SIZE;
				queue.setBufferSize(sampleSize);
				queue.clear();
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert sample size to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_THRESHOLD)) {
			try {
				threshold = Double.parseDouble(event.getNewValue().toString());
				if(threshold<0) threshold = 0;
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert scale factor to an number (%s)",getName(),nfe.getLocalizedMessage());
			}
		}	
		else if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_TRIGGER_COUNT)) {
			try {
				triggerCount = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert trigger count to an integer (%s)",getName(),nfe.getLocalizedMessage());
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
		prototype.setPaletteIconPath("Block/icons/palette/high_pattern.png");
		prototype.setPaletteLabel("HighValPattern");
		prototype.setTooltipText("Return TRUE if TriggerCount values are above the threshold");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/n_greater_equal.png");
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
		desc.setCtypeEditable(true);
	}
	
	/**
	 * Compute the overall state, presumably because of a new input.
	 * This is an "nTrue"
	 */
	private TruthValue computeState() {
		TruthValue result = TruthValue.UNKNOWN;
		int patternCount = 0;
		int otherCount = 0;
		
		for(QualifiedValue qv:queue) {
			if(qv.getQuality().isGood()) {
				double val = Double.NaN;
				try {
					val = Double.parseDouble(qv.getValue().toString());
					if( val>=threshold) patternCount++;
					else otherCount++;
				}
				catch(NumberFormatException nfe) {
					log.warnf("%s.computeState detected not-a-number in queue (%s), ignored",getName(),nfe.getLocalizedMessage());
					continue;
				}
			}
		}
		if (patternCount >= triggerCount)                 result = TruthValue.TRUE;
		else if( sampleSize - otherCount < triggerCount ) result = TruthValue.FALSE;
		else 								    		  result = TruthValue.UNKNOWN;
		
		//log.infof("%s.computeState Pattern=%d,Other=%d of %d, need %d => %s",getName(),patternCount,otherCount,queue.size(),triggerCount,result.name());
		return result;	
	}
}