/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class identifies the maximum input.
 */
@ExecutableBlock
public class HighSelector extends AbstractProcessBlock implements ProcessBlock {
	private final static String TAG = "HighSelector";
	private double max = Double.MIN_VALUE;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public HighSelector() {
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
	public HighSelector(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	@Override
	public void reset() {
		super.reset();
		max = Double.MIN_VALUE;
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		setName("HighSelector");

		// We allow multiple connections on the input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	
	/**
	 * Accept notification that a value has arrived on an input
	 * @param vcn incoming notification
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		String port = vcn.getConnection().getDownstreamPortName();
		
		if( port.equalsIgnoreCase(BlockConstants.IN_PORT_NAME) ) {
			QualifiedValue qv = vcn.getValue();
			try {
				Double dbl = Double.parseDouble(qv.getValue().toString());
				double val = dbl.doubleValue();
				if(!isLocked() ) {
					if( qv.getQuality().isGood() ) {
						if( val>max ) {
							max = val;
							OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
							controller.acceptCompletionNotification(nvn);
							notifyOfStatus(qv);
						}
					}
					else {
						qv = new BasicQualifiedValue(new Double(Double.NaN),qv.getQuality(),qv.getTimestamp());
						OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
						controller.acceptCompletionNotification(nvn);
						notifyOfStatus(qv);
					}
				}
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: setValue Unable to convert incoming value (%s) to a double (%s)",TAG,qv.getValue().toString(),nfe.getLocalizedMessage());
			}
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * @return a block-specific description of internal statue
	 */
	@Override
	public SerializableBlockStateDescriptor getInternalStatus() {
		SerializableBlockStateDescriptor descriptor = super.getInternalStatus();
		Map<String,String> attributes = descriptor.getAttributes();
		attributes.put("CurrentMaximum", String.valueOf(max));
		return descriptor;
	}
	
	/**
	 *  When unlocking, set the remembered state as "UNSET". This will allow
	 *  the next value to generate output, no matter what.
	 */
	@Override
	public void setLocked(boolean flag) {
		this.locked = flag;
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/high_selector.png");
		prototype.setPaletteLabel("HighSelector");
		prototype.setTooltipText("Determine the maximum input value");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/max.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}