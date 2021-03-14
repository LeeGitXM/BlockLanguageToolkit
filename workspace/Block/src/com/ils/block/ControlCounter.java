/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

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
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */

public class ControlCounter extends AbstractProcessBlock implements ProcessBlock {
	private BlockProperty valueProperty = null;
	private int counter = 0;
	private String format = "%s";
	protected PropertyType type = PropertyType.STRING;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public ControlCounter() {
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
	public ControlCounter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		counter = 0;
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("ControlCounter");
		counter = 0;
		
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"0",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.R);
		anchors.add(output);
	}
	
	/**
	 * On a reset, zero the display.
	 */
	@Override
	public void reset() {
		super.reset();
		counter = 0;
		valueProperty.setValue("0");
		notifyOfStatus();
	}
	/**
	 * Zero the display on start of the block.
	 */
	@Override
	public void start() { 
		super.start();
		counter = 0;
		valueProperty.setValue("0");
		notifyOfStatus();
	}

	/**
	 * A new value has appeared on the input. Post a notification, then pass it on.
	 * Ignore types that are not explicitly handled by this class.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		lastValue = incoming.getValue();
		counter++;
		log.errorf("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),""+counter);
		if( lastValue!=null && lastValue.getValue()!=null ) {
			if( !isLocked()  ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
				// Convert the value according to the data type specified by the format.
				String value = "" + counter;

				updateStateForNewValue(lastValue);
				QualifiedValue qv = new BasicQualifiedValue(value,lastValue.getQuality(),lastValue.getTimestamp()); 
				valueProperty.setValue(value);
				log.tracef("%s.acceptValue: port %s formatted value =  %s.",getName(),incoming.getConnection().getUpstreamPortName(),value);
				notifyOfStatus(qv);
			}
		}
	}
	

	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,valueProperty.getValue());
		notifyOfStatus(qv);
	}

	private void notifyOfStatus(QualifiedValue qv) {
		updateStateForNewValue(qv);
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/counter.png");
		prototype.setPaletteLabel("ControlCounter");
		prototype.setTooltipText("Show count of data passing through");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.READOUT);
		view.setPreferredHeight(40);
		view.setPreferredWidth(100);
		view.setCtypeEditable(true);

	}
}