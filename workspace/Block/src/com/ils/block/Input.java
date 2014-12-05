/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class subscribes to value changes for a specified tag.
 * Value changes are propagated to the output connection.
 * 
 * Annotate the constructor.
 */
@ExecutableBlock
public class Input extends AbstractProcessBlock implements ProcessBlock {
	private BlockProperty tagPathProperty = null;
	private BlockProperty valueProperty = null;
	private QualifiedValue qv = null;    // Most recent output value
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Input() {
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
	public Input(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add the tag property and link it to the value property.
	 */
	private void initialize() {
		setName("Input");
		// This property causes the engine to start a subscription.
		tagPathProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.OBJECT,true);
		tagPathProperty.setBinding("");
		tagPathProperty.setBindingType(BindingType.TAG_READ);
		properties.put(BlockConstants.BLOCK_PROPERTY_TAG_PATH, tagPathProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.OBJECT,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		properties.put(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * The block is notified that a new value has appeared on a pseudo port named as
	 * the tag path property. The value contains all the tag quality information.
	 * @param vcn notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		qv = vcn.getValue();
		if( !isLocked() ) {
			log.infof("%s.acceptValue: received %s",getName(),qv.toString());
			if( qv.getValue() != null ) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
				controller.acceptCompletionNotification(nvn);
			}
			else {
				log.warnf("%s.acceptValue: received a null value, ignoring",getName());
			}
		}
		// Even if locked, we update the current state
		if( qv.getValue()!=null) {
			valueProperty.setValue(qv.getValue());
			notifyOfStatus(qv);
		}
	}

	/**
	 * On a change to the tag path, we need to inform the controller 
	 * so as to change our subscription.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH)) {
			log.infof("%s.propertyChange path now %s",getName(),event.getNewValue().toString());
		}
	}
	
	/**
	 * Send status update notification for our last output value.
	 */
	@Override
	public void notifyOfStatus() {
		if( qv.getValue()!=null) {
			notifyOfStatus(qv);
		}	
	}
	private void notifyOfStatus(QualifiedValue qval) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qval);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qval);
	}

	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/input.png");
		prototype.setPaletteLabel("Input");
		prototype.setTooltipText("Place values on the output when a configured tag is updated");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ARROW);
		desc.setPreferredHeight(45);
		desc.setPreferredWidth(60);
		desc.setBackground(Color.cyan.getRGB());
		desc.setCtypeEditable(true);
	}
}