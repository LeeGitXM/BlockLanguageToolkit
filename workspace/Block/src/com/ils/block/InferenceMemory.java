/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.Activity;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class InferenceMemory extends AbstractProcessBlock implements ProcessBlock {
	protected static String RESET_PORT_NAME = "reset";
	protected static String SET_PORT_NAME   = "set";

	private TruthValue priorValue = TruthValue.UNSET;
	private TruthValue resetValue = TruthValue.UNSET;
	private TruthValue setValue = TruthValue.UNSET; 
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public InferenceMemory() {
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
	public InferenceMemory(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	// On a reset, propagate an unknown
	@Override
	public void reset() {
		super.reset();
		state = TruthValue.UNKNOWN;
		if(!locked) {
			lastValue = new TestAwareQualifiedValue(timer,state);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
	}
	// On a start, propagate an unknown
	@Override
	public void start() {
		super.start();
		state = TruthValue.UNKNOWN;
		lastValue = new TestAwareQualifiedValue(timer,state);
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus();
	}
		
	
	/**
	 * Define the ports. There are no user-settable properties
	 */
	private void initialize() {	
		setName("InferenceMemory");
		delayStart = true;
		// Define set and reset inputs
		AnchorPrototype setInput = new AnchorPrototype(SET_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		setInput.setHint(PlacementHint.L);
		setInput.setAnnotation("S");
		anchors.add(setInput);
		AnchorPrototype resetInput = new AnchorPrototype(RESET_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		resetInput.setHint(PlacementHint.L);
		resetInput.setAnnotation("R");
		anchors.add(resetInput);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		output.setHint(PlacementHint.R);
		anchors.add(output);
	}
	

	/**
	 * A new value has appeared on either the set or reset input. Determine the output value.
	 * @param vcn change notification.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);  // Simply log the arrival
		String port = vcn.getConnection().getDownstreamPortName();
		if( port.equals(SET_PORT_NAME)  ) {
			QualifiedValue qv = vcn.getValue();
			if( qv != null && qv.getValue()!=null && qv.getQuality().isGood() ) {
				lastValue = qv;
				setValue = qualifiedValueAsTruthValue(lastValue);
			}
		}
		else if( port.equals(RESET_PORT_NAME)  ) {
			QualifiedValue qv = vcn.getValue();
			if( qv != null && qv.getValue()!=null && qv.getQuality().isGood() ) {
				resetValue = qualifiedValueAsTruthValue(qv);
			}
		}
		if( !isLocked() ) {
			TruthValue output = determineOutput();
			if( !output.equals(state) ) {
				state = output;
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,new TestAwareQualifiedValue(timer,state));
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus();
			}
		}
	}
	private TruthValue determineOutput() {
		TruthValue output = TruthValue.UNSET;
		
		if( setValue.equals(TruthValue.TRUE) && resetValue.equals(TruthValue.FALSE) ) { output = TruthValue.TRUE; }
		else if( resetValue.equals(TruthValue.TRUE)  )                                { output = TruthValue.FALSE; }
		else if( resetValue.equals(TruthValue.UNKNOWN)  )                             { output = TruthValue.UNKNOWN; }
		else if( setValue.equals(TruthValue.TRUE)  )                                  { output = TruthValue.TRUE; }
		else if( state.equals(TruthValue.TRUE) )                                      { output = TruthValue.TRUE; }
		else if( setValue.equals(TruthValue.UNKNOWN)  )                               { output = TruthValue.UNKNOWN; }
		return output;
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {}
	private void notifyOfStatus(QualifiedValue qv) {
		updateStateForNewValue(qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/memory.png");
		prototype.setPaletteLabel("InferenceMemory");
		prototype.setTooltipText("Return TRUE if the input has ever been set");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/inference_memory.png");
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}
}