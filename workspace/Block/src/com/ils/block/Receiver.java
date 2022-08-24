/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

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
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.blt.common.notification.Signal;
import com.ils.blt.common.notification.SignalNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * A receiver is a special class that receives broadcast signals directly
 * from the "engine". It is possible to define filters to limit the number
 * of messages propagated.
 */
@ExecutableBlock
public class Receiver extends AbstractProcessBlock implements ProcessBlock {
	private final String TAG = "Receiver";
	protected static String BLOCK_PROPERTY_PATTERN = "AcceptancePattern";
	private BlockProperty patternProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Receiver() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor. 
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent resource Id identifying the parent of this block (a diagram)
	 * @param block universally unique Id for the block
	 */
	public Receiver(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	private void initialize() {
		patternProperty = new BlockProperty(BLOCK_PROPERTY_PATTERN,"*",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_PATTERN, patternProperty);
		
		// Define a single output. We receive a value from the "ether" and send it on our output connection
		AnchorPrototype output = new AnchorPrototype(BlockConstants.RECEIVER_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		anchors.add(output);
	}
	
	/**
	 * We've received a transmitted signal. If it is appropriate 
	 * based on our configured filters, forward the signal on to our output.
	 * @param sn incoming signal
	 */
	public void acceptValue(SignalNotification sn) {
		Signal signal = sn.getSignal();
		log.tracef("%s.setValue: signal = %s",TAG,signal.getCommand());
		if( patternProperty==null || signal.getPattern()==null || patternProperty.getValue()==null ||
					patternProperty.getValue().toString().equalsIgnoreCase("*")                        ||
					patternProperty.getValue().toString().equalsIgnoreCase(signal.getPattern())) {
			// Passed the filtering. Send to the output.
			lastValue = new TestAwareQualifiedValue(timer,signal);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.RECEIVER_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.RECEIVER_PORT_NAME, qv);
	}
	/**
	 * We overwrite the standard implementation because we are using
	 * an output port that is different from the usual
	 */
	@Override
	public void propagate() {
		if( lastValue!=null ) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.RECEIVER_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/receiver.png");
		prototype.setPaletteLabel("Receiver");
		prototype.setTooltipText("Receive broadcast signals");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setIconPath("Block/icons/palette/receiver.png");
		desc.setPreferredHeight(36);
		desc.setPreferredWidth(24);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ICON);
	}
}