/**
 *   (c) 2017-2019  ILS Automation. All rights reserved. 
 */
package com.ils.block;

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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * The Inference Memory block remembers whether the top input (marked ) has ever been TRUE since the block was started or reset
 */
@ExecutableBlock
public class InferenceMemory extends AbstractProcessBlock implements ProcessBlock {
	protected static String RESET_PORT_NAME = "reset";
	protected static String SET_PORT_NAME   = "set";

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
		resetValue = TruthValue.UNSET;
		setValue = TruthValue.UNSET; 
		setState(TruthValue.UNKNOWN);
		if(!locked) {
			//log.infof("%s.reset lastValue = %s",getName(),lastValue.getValue().toString());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus();
		}
	}
	// On a start, propagate an unknown
	@Override
	public void start() {
		super.start();
		setState(TruthValue.UNKNOWN);  // Sets lastValue
		//log.infof("%s.start lastValue = %s",getName(),lastValue.getValue().toString());
		OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
		controller.acceptCompletionNotification(nvn);
		notifyOfStatus();
	}
		
	
	/**
	 * Define the ports. There are no user-settable properties
	 */
	private void initialize() {	
		setName("InferenceMemory");
		delayStart = false;
		// Define set and reset inputs
		AnchorPrototype setInput = new AnchorPrototype(SET_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		setInput.setHint(PlacementHint.LT);
		setInput.setAnnotation("S");
		setInput.setIsMultiple(false);
		anchors.add(setInput);
		AnchorPrototype resetInput = new AnchorPrototype(RESET_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		resetInput.setHint(PlacementHint.LB);
		resetInput.setAnnotation("R");
		resetInput.setIsMultiple(false);
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
				setValue = qualifiedValueAsTruthValue(qv);
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
				setState(output);
				QualifiedValue qv = new TestAwareQualifiedValue(timer,output);
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
				controller.acceptCompletionNotification(nvn);
				notifyOfStatus(qv);   // 
			}
		}
	}
	private TruthValue determineOutput() {
		TruthValue output = state;
		// NOTE: For inputs UNSET is equivalent to UNKNOWN. Junk on inputs can cause them to be UNSET as well.
		if( resetValue.equals(TruthValue.UNSET)) resetValue = TruthValue.UNKNOWN;
		if( setValue.equals(TruthValue.UNSET))   setValue = TruthValue.UNKNOWN;
		
		if( setValue.equals(TruthValue.TRUE) && resetValue.equals(TruthValue.FALSE) ) { output = TruthValue.TRUE; }
		else if( resetValue.equals(TruthValue.TRUE)  )                                { output = TruthValue.FALSE; }
		else if( resetValue.equals(TruthValue.UNKNOWN)  )                             { output = TruthValue.UNKNOWN; }
		else if( setValue.equals(TruthValue.TRUE)  )                                  { output = TruthValue.TRUE; }
		else if( state.equals(TruthValue.TRUE) )                                      { output = TruthValue.TRUE; }
		else if( setValue.equals(TruthValue.UNKNOWN)  )                               { output = TruthValue.UNKNOWN; }
		else {
			log.warnf("%s.determineOutput UNSET S=%s, R=%s",getName(),setValue.name(),resetValue.name());
		}
		//log.infof("%s.determineOutput State=%s R=%s, S=%s: %s",getName(),state.name(),resetValue.name(),setValue.name(),output.name());
		return output;
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,state);
		notifyOfStatus(qv);
	}
	private void notifyOfStatus(QualifiedValue qv) {
		//log.infof("%s.notifyOfStatus lastValue = %s",getName(),lastValue.getValue().toString());
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, lastValue);
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