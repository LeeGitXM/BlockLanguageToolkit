/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.List;
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
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class emits the logical inverse of its input. 
 * Input and output are truth-values.
 */
@ExecutableBlock
public class Not extends AbstractProcessBlock implements ProcessBlock {
	BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Not() {
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
	public Not(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}

	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Not");
		state = TruthValue.UNSET;
		
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,TruthValue.UNKNOWN,PropertyType.TRUTHVALUE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TRUTHVALUE);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
		anchors.add(output);
	}

	/**
	 * Notify the block that a new value has appeared on one of its input anchors.
	 * As soon as an input is received, invert it and send it off. Retain the timestamp.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		TruthValue tv = vcn.getValueAsTruthValue();
		if( tv.equals(TruthValue.FALSE)) state = TruthValue.TRUE;
		else if( tv.equals(TruthValue.TRUE)) state = TruthValue.FALSE;
		else if(tv.equals(TruthValue.UNKNOWN)) state = TruthValue.UNKNOWN;
		else return;  // Ignore an UNSET
		lastValue = new BasicQualifiedValue(state.name(),qv.getQuality(),qv.getTimestamp());
		if( !isLocked()) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
		// Set the internal property locked or not
		valueProperty.setValue(lastValue.getValue());
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,lastValue);
	}
	/**
	 * Describe the reason for either a TRUE or FALSE state. .
	 * 
	 * @return an explanation for the current state of this block.
	 *         Look at upstream blocks with the opposite state.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram parent,List<UUID> members ) {
		String explanation = "";
		members.add(getBlockId());
		List<ProcessBlock>predecessors = parent.getUpstreamBlocks(this);
		if( state.equals(TruthValue.TRUE) ) {
			for( ProcessBlock predecessor:predecessors ) {
				if( members.contains(predecessor.getBlockId())) {
					explanation = explanation + "-- truncated (circular reasoning)";
				}
				else if( TruthValue.FALSE.equals(predecessor.getState())) {
					explanation = predecessor.getExplanation(parent,members);
				}
			}
		}
		else if( state.equals(TruthValue.FALSE) ) {
			for( ProcessBlock predecessor:predecessors ) {
				if( members.contains(predecessor.getBlockId())) {
					explanation = explanation + "-- truncated (circular reasoning)";
				}
				else if( TruthValue.TRUE.equals(predecessor.getState())) {
					explanation = predecessor.getExplanation(parent,members);
				}
			}
		}
		return explanation;
	}
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new TestAwareQualifiedValue(timer,valueProperty.getValue());
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/not.png");
		prototype.setPaletteLabel("Not");
		prototype.setTooltipText("Negate the input and place in on the output");
		prototype.setTabName(BlockConstants.PALETTE_TAB_LOGIC);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedLabel("NOT");
		desc.setEmbeddedFontSize(10);
		desc.setPreferredHeight(60);
		desc.setPreferredWidth(60);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.LOGIC_NOT);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}

}