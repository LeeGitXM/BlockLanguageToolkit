/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Present a value as it passes through.
 */
@ExecutableBlock
public class TestPoint extends AbstractProcessBlock implements ProcessBlock {
	private BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TestPoint() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. There should be a custom property called format.
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public TestPoint(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {	
		setName("TestPoint");
		
		// Define a label for this block
		BlockProperty label = new BlockProperty(BlockConstants.BLOCK_PROPERTY_LABEL,"",PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_LABEL, label);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.STRING,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
				
		// Define a single input -- but allow multiple connections
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	

	/**
	 * A new value has appeared on the input. Record the value, then pass it on.
	 * @param incoming incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		super.acceptValue(incoming);
		QualifiedValue qv = incoming.getValue();
		if( qv.getValue()!=null ) {
			valueProperty.setValue(qv.getValue());
			
			if( !isLocked() ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(qv);
			}
			else {
				// Propagate value even if locked
				controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
			}
		}	
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new BasicQualifiedValue(valueProperty.getValue());
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Define the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/test_point.png");
		prototype.setPaletteLabel("TestPoint");
		prototype.setTooltipText("Preserve current value as it passes through");
		prototype.setTabName(BlockConstants.PALETTE_TAB_MISC);
		
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.ROUND);
		view.setPreferredHeight(40);
		view.setPreferredWidth(40);
		view.setCtypeEditable(true);
	}
}