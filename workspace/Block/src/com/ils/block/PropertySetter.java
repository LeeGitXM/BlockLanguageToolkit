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
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 *  
 *   *** DEPRECATED BLOCK.  Replaced by SetProperty in BLT 2.0
 *   
 *    This is only left here to ensure BLT 1.0 systems still operate properly
 * 
 */
@ExecutableBlock
public class PropertySetter extends AbstractProcessBlock implements ProcessBlock {
	private static String TAG = "PropertySetter";
	private Signal signal = null;   // Last signal sent
	private String propertyName = "";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public PropertySetter() {
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
	public PropertySetter(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}

	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("PropertySetter");
		
		BlockProperty propertyProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY,propertyName,PropertyType.STRING,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_PROPERTY, propertyProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.ANY);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		anchors.add(output);
	}
	
	@Override
	public void reset() {
		super.reset();
		signal = null;
	}

	/**
	 * As soon as a fresh value arrives, trigger the output signal.
	 * Retain the timestamp.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();

		if( qv.getQuality().isGood() && !isLocked() && qv.getValue() != null )  {
			signal = new Signal(BlockConstants.COMMAND_CONFIGURE,propertyName,qv.getValue().toString());
			lastValue = new BasicQualifiedValue(signal,qv.getQuality(),qv.getTimestamp());
			// This ends up as a signal notification on the output.
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}

	/**
	 * Handle a changes to the various attributes.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propName = event.getPropertyName();
		log.debugf("%s.propertyChange: Received %s = %s",TAG,propName,event.getNewValue().toString());
		if( propName.equals(BlockConstants.BLOCK_PROPERTY_PROPERTY)) {
			propertyName = event.getNewValue().toString();
		}
		else {
			log.warnf("%s.propertyChange:Unrecognized property (%s)",TAG,propertyName);
		}
	}
	
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		
		if( signal!=null ) {
			attributes.put("Property Name",signal.getArgument() );
			attributes.put("Property Value",signal.getPayload());
		}
		return descriptor;
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {

//		prototype.setPaletteIconPath("Block/icons/palette/property_edit.png");
//		prototype.setPaletteLabel("SetProperty");
//		prototype.setTooltipText("Transmit a signal to set properties of the downstream block");
//		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/edit.png");
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}

}