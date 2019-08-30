/**
 *   (c) 2014-2107  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * On receipt of a trigger, this class inhibits further input from propagating.
 * Values that arrive during the inhibit period are discarded. Otherwise
 * values are propagated without change.
 */
public class InferenceInhibitor extends AbstractProcessBlock implements ProcessBlock {
	private boolean inhibiting = false;
	private TruthValue controlValue = TruthValue.UNSET;
	private TruthValue initialValue = TruthValue.UNSET;
	private TruthValue trigger = TruthValue.UNSET; 
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public InferenceInhibitor() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom property is "interval".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public InferenceInhibitor(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	// On a reset, set the time to the start of the Unix epoch
	@Override
	public void reset() {
		super.reset();
		inhibiting = controlValue.equals(trigger);
		setState(initialValue);
		if(!locked && !inhibiting && !initialValue.equals(TruthValue.UNSET)) {
			lastValue = new TestAwareQualifiedValue(timer,initialValue);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}

	}
	/**
	 * A new value has appeared on an input anchor. If we are in an "inhibit" state, then 
	 * retain this value until we are un-inhibited.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);  // Simply log the arrival
		if( !isLocked() ) {
			String port = vcn.getConnection().getDownstreamPortName();
			if( port.equals(BlockConstants.IN_PORT_NAME)  ) {
				QualifiedValue qv = vcn.getValue();
				if( qv != null && qv.getValue()!=null ) {
					if( qv.getQuality().isGood() ) {
						lastValue = qv;
						if( !inhibiting ) {
							OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
							controller.acceptCompletionNotification(nvn);
							notifyOfStatus();
							state = qualifiedValueAsTruthValue(lastValue);
						}
					}
					else {
						recordActivity(Activity.ACTIVITY_BLOCKED,lastValue.getValue().toString());
						log.infof("%s.acceptValue: Ignoring inhibited or BAD input ... (%s)",getName(),lastValue.getValue().toString());
					}
				}
				else {
					log.infof("%s.acceptValue: Received null %s (IGNORED)",getName(),(lastValue==null?"":"value"));
				}
			}
			
			else if( port.equals(BlockConstants.CONTROL_PORT_NAME)  ) {
				QualifiedValue qv = vcn.getValue();
				if( qv != null && qv.getValue()!=null ) {
					if( qv.getQuality().isGood() ) {
						TruthValue cv = qualifiedValueAsTruthValue(qv);
						// If this leads to a new mismatch, then we propagate the last value
						if( inhibiting && !cv.equals(trigger)) {
							OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
							controller.acceptCompletionNotification(nvn);
							notifyOfStatus();
							state = qualifiedValueAsTruthValue(lastValue);
						}
						controlValue = cv;
						inhibiting = controlValue.equals(trigger);
						recordActivity((inhibiting?Activity.ACTIVITY_BLOCKING:Activity.ACTIVITY_UNBLOCKED),controlValue.toString());
					}
				}
				else {
					log.infof("%s.acceptValue: Received null %s (IGNORED)",getName(),(lastValue==null?"":"value"));
				}
			}
		}
	}

	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		
		attributes.put("Inhibiting", (inhibiting?"true":"false"));
		
		return descriptor;
	}

	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 * Under certain conditions we propage an initial value on start.
	 */
	private void initialize() {
		setName("InferenceInhibitor");
		delayStart = propagateOnStart();
		state = TruthValue.UNSET;
		BlockProperty initialValueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_INITIAL_VALUE,initialValue,PropertyType.TRUTHVALUE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_INITIAL_VALUE, initialValueProperty);
		BlockProperty triggerProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER,trigger,PropertyType.TRUTHVALUE,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_TRIGGER, triggerProperty);
		
		// Define a data input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		anchors.add(input);
		
		// Define the control input
		AnchorPrototype triggerIn = new AnchorPrototype(BlockConstants.CONTROL_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		triggerIn.setHint(PlacementHint.T);
//		triggerIn.setAnnotation("T");
		anchors.add(triggerIn);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}
	
	/**
	 * Handle a change to the interval value. Note that this does not currently
	 * change any inhibit in-progress.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
//		this.setReceiver(true);
		String propertyName = event.getPropertyName();
		if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_TRIGGER)) {
			trigger = TruthValue.valueOf(event.getNewValue().toString().toUpperCase());	
		}
		else if( propertyName.equals(BlockConstants.BLOCK_PROPERTY_INITIAL_VALUE)) {
			
			initialValue = TruthValue.valueOf(event.getNewValue().toString());
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",getName(),propertyName);
		}
	}
	/**
	 * Send status update notification for our last transmitted value. If we've 
	 * never transmitted one, lastValue will be null.
	 */
	@Override
	public void notifyOfStatus() {
		if(lastValue!=null) {
			controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lastValue);
		}
		else {
			QualifiedValue lv = new BasicQualifiedValue(coerceToMatchOutput(BlockConstants.OUT_PORT_NAME,null));
			controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lv);
		}
	}
	/**
	 * If the trigger condition and initial value match, then
	 * we propagate a value on startup and on reset.
	 * @return
	 */
	private boolean propagateOnStart() {
		boolean result = false;
		if( controlValue.equals(TruthValue.UNSET) && !initialValue.equals(TruthValue.UNSET)) {
			result = true;
		}
		else if( !controlValue.equals(TruthValue.UNSET) && !initialValue.equals(TruthValue.UNSET) &&
				!trigger.equals(TruthValue.UNSET) && !controlValue.equals(trigger) ) {
			result = true;
		}
		return result;
	}

	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/inference_inhibit.png");
		prototype.setPaletteLabel("InferenceInhibitor");
		prototype.setTooltipText("Discard incoming values that arrive while the inhibit control does not match the trigger value");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(80);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.CLAMP);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_BLUE_GRAY);

	}
}