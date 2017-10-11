/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.LinkedList;
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
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Delay before passing the input onto the output.
 */
@ExecutableBlock
public class DataShift extends AbstractProcessBlock implements ProcessBlock {

	private int sampleSize = 0;  
	private final LinkedList<QualifiedValue> buffer;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DataShift() {
		initialize();
		buffer = new LinkedList<QualifiedValue>();
	}
	
	/**
	 * Constructor. Custom property is "SampleSize".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public DataShift(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		buffer = new LinkedList<QualifiedValue>();
		initialize();
	}
	
	@Override
	public void reset() {
		buffer.clear();
		lastValue = null;
	}
	

	/**
	 * A new value has appeared on an input anchor. Add it to the list and trigger the delay timer.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);

		QualifiedValue qv = vcn.getValue();
		log.debugf("%s.acceptValue: Received %s (%d of %d)",getName(),qv.getValue().toString(),buffer.size(),sampleSize);
		if( qv.getQuality().isGood() ) {
			buffer.add(qv);
			if( buffer.size() > sampleSize) {
				lastValue = buffer.removeFirst();
				log.debugf("%s.acceptValue: Popped %s",getName(),lastValue.getValue().toString());
				if( !isLocked() ) {
					// Give it a new timestamp
					lastValue = new TestAwareQualifiedValue(timer,lastValue.getValue(),qv.getQuality());
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(lastValue);
				}
			}
		}
		else {
			if( qv!=null && !isLocked()) {
				// Propagate a bad value result
				lastValue = new BasicQualifiedValue(lastValue.getValue(),qv.getQuality(),qv.getTimestamp());
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(lastValue);
			}
		}
	}
	/**
	 * Handle a change to the delay interval or buffer size
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		log.debugf("%s.propertyChange: Received %s = %s",getName(),propertyName,event.getNewValue().toString());
		if( propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE) ) {
			try {
				sampleSize = Integer.parseInt(event.getNewValue().toString());
				// If we've made the buffer smaller, report the excess
				while( buffer.size()>sampleSize ) {
					QualifiedValue qv = buffer.removeFirst();
					if( !isLocked() ) {
						// Give it a new timestamp
						QualifiedValue outval = new TestAwareQualifiedValue(timer,qv.getValue());
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,outval);
						controller.acceptCompletionNotification(nvn);
						notifyOfStatus(outval);
					}
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert sample size value to a integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",getName(),propertyName);
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		if( lastValue!=null ) notifyOfStatus(lastValue);
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("DataShift");
		BlockProperty samples = new BlockProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE,new Long(sampleSize),PropertyType.INTEGER,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_SAMPLE_SIZE, samples);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		anchors.add(output);
		initializePrototype();
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/data_shift.png");
		prototype.setPaletteLabel("DataShift");
		prototype.setTooltipText("Delay incoming values by a specified count");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("- N");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setCtypeEditable(true);
	}
}