/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockState;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.block.TruthValue;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.Watchdog;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Hold the first truth-value that arrives.
 */
@ExecutableBlock
public class LogicLatch extends AbstractProcessBlock implements ProcessBlock {
	private static final String TAG = "LogicLatch";

	protected TruthValue tv = TruthValue.UNKNOWN;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public LogicLatch() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. Custom properties are "HoldInterval" and "Trigger".
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public LogicLatch(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	@Override
	public void reset() {
		tv = TruthValue.UNKNOWN;
	}
	
	/**
	 * A new value has appeared on an input anchor. Send it on its way. Start timer to 
	 * propagate an inverse after the expiration time.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		if( tv.equals(TruthValue.UNKNOWN)) {
			if( !isLocked() ) {
				String port = vcn.getConnection().getDownstreamPortName();
				if( port.equals(BlockConstants.IN_PORT_NAME) ) {
					QualifiedValue qv = vcn.getValue();
					if(qv.getQuality().isGood()) {
						tv = qualifiedValueAsTruthValue(qv);
					}
					OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
					controller.acceptCompletionNotification(nvn);
					notifyOfStatus(qv);
				}
			}
		}
	}
	

	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		notifyOfStatus(new BasicQualifiedValue(tv.name()));	
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * There are no properties for this class.
	 */
	private void initialize() {
		setName("LogicLatch");
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		anchors.add(output);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/latch.png");
		prototype.setPaletteLabel("LogicLatch");
		prototype.setTooltipText("Retain first value received until reset");
		prototype.setTabName(BlockConstants.PALETTE_TAB_LOGIC);
		BlockDescriptor view = prototype.getBlockDescriptor();
		view.setEmbeddedIcon("Block/icons/embedded/latch.png");
		view.setBlockClass(getClass().getCanonicalName());
		view.setStyle(BlockStyle.SQUARE);
		view.setPreferredHeight(60);
		view.setPreferredWidth(60);
	}
	
}