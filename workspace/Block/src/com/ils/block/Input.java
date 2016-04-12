/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.Date;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.util.LogUtil;

/**
 * This class subscribes to value changes for a specified tag.
 * Value changes are propagated to the output connection.
 * 
 * Annotate the constructor.
 */
@ExecutableBlock
public class Input extends AbstractProcessBlock implements ProcessBlock {
	private BlockProperty tagPathProperty = null;
	protected BlockProperty valueProperty = null;
	protected QualifiedValue qv = null;    // Most recent output value
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Input() {
		initialize();
		initializePrototype();
		log = LogUtil.getLogger(getClass().getPackage().getName()+".input");
	}
	
	/**
	 * Constructor: Custom property is "tag path"
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
	protected void initialize() {
		setName("Input");
		delayStart = true;
		// This property causes the engine to start a subscription.
		tagPathProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.OBJECT,true);
		tagPathProperty.setBinding("");
		tagPathProperty.setBindingType(BindingType.TAG_READ);
		setProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH, tagPathProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.OBJECT,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * We may have received a premature value due to creation of a subscription 
	 * before we're actually started. Pass that value on now.
	 */
	@Override
	public void start() {
		super.start();
		if( qv!=null &&  qv.getValue() != null && !isLocked()  ) {
			log.debugf("%s.start: %s (%s)",getName(),qv.getValue().toString(),qv.getQuality().getName());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
			controller.acceptCompletionNotification(nvn);
		}
	}

	@Override
	public void stop() {
		super.stop();
	}
	
	/**
	 * The block is notified that a new value has appeared on a pseudo port named as
	 * the tag path property. The value contains all the tag quality information.
	 * @param vcn notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		baseAcceptValue(vcn);
		qv = vcn.getValue();
		if( !isLocked() && running ) {
			if( qv.getValue() != null ) {
				log.debugf("%s.acceptValue: propagating %s (%s at %s)",getName(),qv.getValue().toString(),qv.getQuality().getName(),
						qv.getTimestamp().toString());
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
	
	/*
	 * Provide a way for sub-classes to override acceptValue with
	 * different semantics than we have here.
	 */
	protected void baseAcceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
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
			log.debugf("%s.evaluate: Read %s = %s",getName(),path,val.getValue().toString());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,val);
			controller.acceptCompletionNotification(nvn);
		}
	}
	@Override
	public String getExplanation(DiagnosticDiagram parent) { 
		String explanation = "";
		String tagPath = tagPathProperty.getBinding().toString();
		if( tagPath!=null && !tagPath.isEmpty()) {
			int pos = tagPath.lastIndexOf("/");
			if(pos>0 ) tagPath = tagPath.substring(pos+1);
			if( state.equals(TruthValue.FALSE)) {
				explanation = String.format("At %s, %s is false",getName(),tagPath);
			}
			else if( state.equals(TruthValue.TRUE)) {
				explanation = String.format("At %s, %s is true",getName(),tagPath);
			}
		}
		return explanation; 
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		if( qv!=null && qv.getValue()!=null ) attributes.put("Value", qv.getValue().toString());
		if( qv!=null && qv.getQuality()!=null )attributes.put("Quality", qv.getQuality().toString());
		if(qv!=null && qv.getTimestamp()!=null) attributes.put("Timestamp",dateFormatter.format(new Date(qv.getTimestamp().getTime())));
		String path = controller.getSubscribedPath(this, tagPathProperty);
		attributes.put("CurrentSubscription",path);
		return descriptor;
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
		if( qv!=null && qv.getValue()!=null) {
			notifyOfStatus(qv);
		}	
	}
	protected void notifyOfStatus(QualifiedValue qval) {
		updateStateForNewValue(qval);
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qval);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qval);
	}

	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
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