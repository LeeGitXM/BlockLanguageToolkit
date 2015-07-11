/**
 *   (c) 2015  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.UUID;

import com.ils.blt.common.annotation.ExecutableBlock;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class subscribes to a tag, but does not propagate the value until
 * it is externally evaluated.
 * 
 * Annotate the constructor.
 */
@ExecutableBlock
public class TriggerableInput extends Input implements ProcessBlock {
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TriggerableInput() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor: Custom property is "entry"
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public TriggerableInput(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add the tag property and link it to the value property.
	 */
	protected void initialize() {
		super.initialize();
		setName("TriggerInput");
	}

	
	/**
	 * The block is notified that a new value has appeared on a pseudo port named as
	 * the tag path property. The value contains all the tag quality information.
	 * @param vcn notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		qv = vcn.getValue();
		
		// Even if locked, we update the current state
		if( qv.getValue()!=null) {
			valueProperty.setValue(qv.getValue());
		}
	}

	/**
	 * This method is not called during normal operation of the block.
	 * It is called externally to propagate a tag value.
	 */
	@Override
	public void evaluate() {
		String path = tagPathProperty.getBinding().toString();
		QualifiedValue val = controller.getTagValue(getParentId(),path);
		if( val!=null ) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,val);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
	}
	

	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/input.png");
		prototype.setPaletteLabel("Trigger");
		prototype.setTooltipText("Place a value on the output when the block is evaluated");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ARROW);
		desc.setPreferredHeight(45);
		desc.setPreferredWidth(60);
		desc.setBackground(Color.blue.getRGB());
		desc.setCtypeEditable(true);
	}

}