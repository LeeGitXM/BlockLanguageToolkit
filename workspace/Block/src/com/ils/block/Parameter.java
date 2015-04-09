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
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * A parameter block emulates a G2 parameter. It functions as both a 
 * tag reader and writer.
 */
@ExecutableBlock
public class Parameter extends AbstractProcessBlock implements ProcessBlock {
	protected static String BLOCK_PROPERTY_TAG_PATH = "TagPath";
	private BlockProperty tag = null;
	private BlockProperty valueProperty = null;
	private QualifiedValue qv = null;    // Most recent output value
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Parameter() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "tag path".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Parameter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("Parameter");
		tag = new BlockProperty(BLOCK_PROPERTY_TAG_PATH,"",PropertyType.STRING,true);
		tag.setBinding("");
		tag.setBindingType(BindingType.TAG_READWRITE);
		setProperty(BLOCK_PROPERTY_TAG_PATH, tag);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.OBJECT,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setHint(PlacementHint.L);
		anchors.add(input);
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.ANY);
		output.setHint(PlacementHint.R);
		anchors.add(output);
	}
	
	/**
	 * The block is notified that a new value has appeared on one of its input anchors.
	 * Write the value to the configured tag. Handle any of the possible input types.
	 * 
	 * The input notification can be either:
	 *    1) From the input connection. Write to the tag.
	 *    2) From the tag, do nothing additional.
	 *    
	 * In either case, update the value in the tag property, then propagate to the output.
	 * 
	 * @param vcn notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		qv = vcn.getValue();
		if( !isLocked() ) {
			if( vcn.getConnection()!=null && vcn.getConnection().getDownstreamPortName().equals(BlockConstants.IN_PORT_NAME) ) {
				// Arrival through the input connection
				String path = tag.getBinding().toString();
				controller.updateTag(getParentId(),path, qv);
				log.infof("%s.acceptValue: Updated tag %s = %s",getName(),path,qv.getValue().toString());
			}
			// In either mode of update, we propagate the value on the output.
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
			controller.acceptCompletionNotification(nvn);
		}
		// Even if locked, we update the current state
		if( qv.getValue()!=null) {
			valueProperty.setValue(qv.getValue());
			notifyOfStatus(qv);
		}
	}
	
	/**
	 * The super method handles setting the new property. A save of the block
	 * as a project resource will inform the controller so that it can change the
	 * tag subscription, if necessary. 
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
		prototype.setPaletteIconPath("Block/icons/palette/parameter.png");
		prototype.setPaletteLabel("Parameter");
		prototype.setTooltipText("Write the incoming value to a pre-configured tag. Subscribe to that same tag for output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/information.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBackground(new Color(125,125,125).getRGB());   // Dark gray
		desc.setCtypeEditable(true);
	}
}