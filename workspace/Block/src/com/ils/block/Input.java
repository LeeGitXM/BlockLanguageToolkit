/**
 *   (c) 2014-2018  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.DiagnosticDiagram;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
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
	 * before we're actually started. Pass that value on now. (May be unnecessary)
	 */
	@Override
	public void start() {
		super.start();
		if( tagPathProperty!=null && tagPathProperty.getBinding() != null && !tagPathProperty.getBinding().isEmpty() ) {
			lastValue = controller.getTagValue(getParentId(), tagPathProperty.getBinding());
		}
		if( lastValue!=null &&  lastValue.getValue() != null && !isLocked()  ) {
			log.debugf("%s.start: %s (%s)",getName(),lastValue.getValue().toString(),lastValue.getQuality().getName());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
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
	public synchronized void acceptValue(IncomingNotification vcn) {
		baseAcceptValue(vcn);
		lastValue = vcn.getValue();
		if( !isLocked() && running ) {
			if( lastValue!=null &&  lastValue.getValue() != null ) {
				String valueString = lastValue.getValue().toString();
				String qualityString = (lastValue.getQuality()==null?"NO QUALITY":lastValue.getQuality().getName());
				String timestampString = (lastValue.getTimestamp()==null?"NO TIME":dateFormatter.format(lastValue.getTimestamp()));
				log.debugf("%s.acceptValue: propagating %s (%s at %s)",getName(),valueString,qualityString,timestampString);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
				controller.acceptCompletionNotification(nvn);
			}
			else {
				log.warnf("%s.acceptValue: received a null value, ignoring",getName());
			}
		}
		// Even if locked, we update the current state
		if( lastValue!=null && lastValue.getValue()!=null) {
			valueProperty.setValue(lastValue.getValue());
			notifyOfStatus(lastValue);
		}
		else {
			valueProperty.setValue("null");
		}
	}
	
	/*
	 * Provide a way for sub-classes to override acceptValue with
	 * different semantics than we have here.
	 */
	protected void baseAcceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
	}

	@Override
	public synchronized void reset() {
		this.state = TruthValue.UNSET;
		this.lastValue = null;
		recordActivity(Activity.ACTIVITY_RESET,"");
		if( controller!=null ) {
			// Send notifications on all outputs to indicate empty connections.
			// For truth-values, actually propagate UNSET.
			
			//  trigger evaluation so it propagates last value
			evaluate();
			
		}
		
	}
	

	/**
	 * This method is not called during normal operation of the block.
	 * Except during diagram state change.
	 * It may be called externally to propagate a tag value.
	 */
	@Override
	public synchronized void evaluate() {
		String path = tagPathProperty.getBinding().toString();
		QualifiedValue val = controller.getTagValue(getParentId(),path);
		if( val!=null && val.getValue()!=null && path!=null ) {
			log.debugf("%s.evaluate: Read %s = %s",getName(),path,val.getValue().toString());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,val);
			controller.acceptCompletionNotification(nvn);
		}
		else {
			log.infof("%s.evaluate: Null value on %s (diagram state change?)",getName(),path);
		}
	}
	@Override
	public String getExplanation(DiagnosticDiagram parent,List<UUID> members) { 
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
		if( lastValue!=null && lastValue.getValue()!=null ) attributes.put("Value", lastValue.getValue().toString());
		if( lastValue!=null && lastValue.getQuality()!=null )attributes.put("Quality", lastValue.getQuality().toString());
		if(lastValue!=null && lastValue.getTimestamp()!=null) attributes.put("Timestamp",dateFormatter.format(new Date(lastValue.getTimestamp().getTime())));
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
	public synchronized void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		//if(propertyName.equals(BlockConstants.BLOCK_PROPERTY_TAG_PATH)) {
		//	log.debugf("%s.propertyChange tag path now %s",getName(),event.getNewValue().toString());
		//}
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
	protected void notifyOfStatus(QualifiedValue qval) {
		updateStateForNewValue(qval);
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qval);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qval);
	}
	
	@Override
	public void propagate() {
		recordActivity(Activity.ACTIVITY_PROPAGATE,"");
		evaluate(); 
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
	
	/**
	 * In addition to the standard validation, do not allow the tag path property
	 * to have an empty binding.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		String summary = super.validate();
		if( tagPathProperty!=null ) {
			String tagPath = tagPathProperty.getBinding();
			if( summary==null && (tagPath==null || tagPath.length()==0 || tagPath.endsWith("]") )) {
				summary = String.format("%s: binding is not configured\t",tagPathProperty.getName());
			}
		}
		return summary;
	}
}