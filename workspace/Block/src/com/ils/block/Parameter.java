/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
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
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * A parameter block emulates a G2 parameter. It functions as both a 
 * tag reader and writer.
 */
@ExecutableBlock
public class Parameter extends AbstractProcessBlock implements ProcessBlock {
	private BlockProperty tagProperty = null;
	private BlockProperty valueProperty = null;
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
		delayStart = true;
		tagProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.STRING,true);
		tagProperty.setBinding("");
		tagProperty.setBindingType(BindingType.TAG_READWRITE);
		setProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH, tagProperty);
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
	 * On reset, set the value of the backing tag to "UNSET". This prevents
	 * a refresh of the block to re-propagate the last value.
	 */
	@Override
	public void reset() {
		super.reset();
		if( tagProperty.getBindingType().equals(BindingType.TAG_READWRITE)) {
			controller.updateTag(getParentId(),tagProperty.getBinding().toString(), new BasicQualifiedValue("UNSET"));
		}
	}
	/**
	 * We may have received a premature value due to creation of a subscription 
	 * before we're actually started. Pass that value on now.
	 */
	@Override
	public void start() {
		super.start();
		if( lastValue!=null &&  lastValue.getValue() != null && !isLocked()  ) {
			log.debugf("%s.start: %s (%s)",getName(),lastValue.getValue().toString(),lastValue.getQuality().getName());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
		}
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
		lastValue = vcn.getValue();
		if( !isLocked() && running ) {
			if( vcn.getConnection()!=null && vcn.getConnection().getDownstreamPortName().equals(BlockConstants.IN_PORT_NAME) ) {
				// Arrival through the input connection
				String path = tagProperty.getBinding().toString();
				controller.updateTag(getParentId(),path, lastValue);
				log.debugf("%s.acceptValue: Updated tag %s = %s",getName(),path,lastValue.getValue().toString());
			}
			// In either mode of update, we propagate the value on the output.
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
		}
		// Even if locked, we update the current state
		if( lastValue.getValue()!=null) {
			valueProperty.setValue(lastValue.getValue());
			notifyOfStatus(lastValue);
		}
	}
	
	/**
	 * @return a block-specific description of internal status. Add quality to the default list
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("Value", valueProperty.getValue().toString());
		return descriptor;
	}
	
	/**
	 * This method is not called during normal operation of the block.
	 * It is called externally to propagate a tag value.
	 */
	@Override
	public void evaluate() {
		String path = tagProperty.getBinding().toString();
		QualifiedValue val = controller.getTagValue(getParentId(),path);
		if( val!=null ) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,val);
			controller.acceptCompletionNotification(nvn);
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
			log.debugf("%s.propertyChange path now %s",getName(),event.getNewValue().toString());
		}
	}

	/**
	 * Send status update notification for our last output value.
	 */
	@Override
	public void notifyOfStatus() {
		if( lastValue!=null && lastValue.getValue()!=null) {
			notifyOfStatus(lastValue);
		}	
	}
	private void notifyOfStatus(QualifiedValue qval) {
		updateStateForNewValue(qval);
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qval);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qval);
	}
	
	/**
	 * In addition to the standard validation, do not allow the tag path property
	 * to have an empty binding.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		String summary = super.validate();
		String tagPath = tagProperty.getBinding();
		if( summary==null && (tagPath==null || tagPath.length()==0 || tagPath.endsWith("]") )) {
			summary = String.format("%s: binding is not configured\t",tagProperty.getName());
		}
		return summary;
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
		desc.setEmbeddedIcon("Block/icons/embedded/parameter.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(50);
		desc.setPreferredWidth(50);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);  
		desc.setCtypeEditable(true);
	}
}