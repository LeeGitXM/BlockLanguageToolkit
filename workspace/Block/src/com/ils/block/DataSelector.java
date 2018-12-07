/**
 *   (c) 2017  ILS Automation. All rights reserved. 
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
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.serializable.SerializableBlockStateDescriptor;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class DataSelector extends AbstractProcessBlock implements ProcessBlock {
	private static final String IN_PORT1_NAME = "in1";
	private static final String IN_PORT2_NAME = "in2";
	private QualifiedValue in1 = null;  // save most recent inputs in case switch is thrown
	private QualifiedValue in2 = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public DataSelector() {
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
	public DataSelector(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("DataSelector");
		this.state = TruthValue.UNSET;
		
		// Define two data inputs
		AnchorPrototype input1 = new AnchorPrototype(IN_PORT1_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input1.setHint(PlacementHint.LT);
		input1.setAnnotation("1");
		anchors.add(input1);
		AnchorPrototype input2 = new AnchorPrototype(IN_PORT2_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input2.setHint(PlacementHint.LB);
		input2.setAnnotation("2");
		anchors.add(input2);
		AnchorPrototype control = new AnchorPrototype(BlockConstants.RECEIVER_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		control.setHint(PlacementHint.T);
		anchors.add(control);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.R);
		anchors.add(output);
		
		for(AnchorPrototype desc:getAnchors()) {
			log.tracef("EREIAM JH - initAnchorPoints counts(tblr)" + desc.getAnnotation() + " " + desc.getConnectionType().name());
		}
			

		
	}
	

	/**
	 * A new value has appeared on our input.  Pass it on.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		if(!isLocked() ) {
			if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(BlockConstants.RECEIVER_PORT_NAME)) {
				this.state = qualifiedValueAsTruthValue(vcn.getValue());

				QualifiedValue out = null;
				if( state.equals(TruthValue.TRUE)) { out = in1;	}
				if( state.equals(TruthValue.FALSE)) { out = in2; }
				if (out != null) {
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,out);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(lastValue);
				}
				
			}
			else if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(IN_PORT1_NAME)) {
				in1 = vcn.getValue();
				if( state.equals(TruthValue.TRUE)) {
					lastValue = vcn.getValue();
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(lastValue);
				}
			}
			else if( vcn.getConnection().getDownstreamPortName().equalsIgnoreCase(IN_PORT2_NAME)) {
				in2 = vcn.getValue();
				if( state.equals(TruthValue.FALSE)) {
					lastValue = vcn.getValue();
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(lastValue);
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
		attributes.put("State", getState().toString());
		if( state.equals(TruthValue.TRUE)) attributes.put("Propagating Input","1");
		if( state.equals(TruthValue.FALSE)) attributes.put("Propagating Input","2");
		
		return descriptor;
	}

	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(lastValue);
	}
	
	
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/dataselector.png");
		prototype.setPaletteLabel("DataSelector");
		prototype.setTooltipText("Select which of 2 inputs to pass based on a control line");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/data_selector.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
		desc.setCtypeEditable(true);
	}
}