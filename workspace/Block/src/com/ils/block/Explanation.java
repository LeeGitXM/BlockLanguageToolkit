/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

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
 * This class is a pass-thru. It's only purpose is for custom explanations. 
 * Input and output are truth-values.
 */
@ExecutableBlock
public class Explanation extends AbstractProcessBlock implements ProcessBlock {
	private final static String EXPLANATION_WHEN_FALSE = "ExplanationWhenFalse";
	private final static String EXPLANATION_WHEN_TRUE = "ExplanationWhenTrue";
	BlockProperty explanationFalseProperty = null;
	BlockProperty explanationTrueProperty = null;
	BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Explanation() {
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
	public Explanation(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}

	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Explanation");
		state = TruthValue.UNSET;
		
		explanationFalseProperty = new BlockProperty(EXPLANATION_WHEN_FALSE,"",PropertyType.STRING,true);
		setProperty(EXPLANATION_WHEN_FALSE, explanationFalseProperty);
		
		explanationTrueProperty = new BlockProperty(EXPLANATION_WHEN_TRUE,"",PropertyType.STRING,true);
		setProperty(EXPLANATION_WHEN_TRUE, explanationTrueProperty);
		
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
	
	@Override
	public void reset() {
		super.reset();
		state = TruthValue.UNKNOWN;
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
		if( tv.equals(TruthValue.FALSE)) state = TruthValue.FALSE;
		else if( tv.equals(TruthValue.TRUE)) state = TruthValue.TRUE;
		else if(tv.equals(TruthValue.UNKNOWN)) state = TruthValue.UNKNOWN;
		else return;  // Ignore an UNSET
		QualifiedValue result = new BasicQualifiedValue(state.name(),qv.getQuality(),qv.getTimestamp());
		if( !isLocked()) {
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,result);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(result);
		}
		// Set the internal property locked or not
		valueProperty.setValue(result.getValue());
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,result);
	}
	/**
	 * Describe the reason for either a TRUE or FALSE state. If our explanation
	 * is configured, use it. Otherwise search upstream.
	 * 
	 * @return an explanation for the current state of this block.
	 *         Look at upstream blocks with the opposite state.
	 */
	@Override
	public String getExplanation(DiagnosticDiagram parent) {
		String explanation = "";
		if( state.equals(TruthValue.TRUE) && !explanationTrueProperty.getValue().toString().isEmpty() ) {
			explanation = explanationTrueProperty.getValue().toString();
		}
		else if( state.equals(TruthValue.FALSE) && !explanationFalseProperty.getValue().toString().isEmpty() ) {
			explanation = explanationFalseProperty.getValue().toString();
		}
		else {
			explanation = super.getExplanation(parent);
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
		prototype.setPaletteIconPath("Block/icons/palette/explanation.png");
		prototype.setPaletteLabel("Explanation");
		prototype.setTooltipText("Provide custom explanations for downstream blocks");
		prototype.setTabName(BlockConstants.PALETTE_TAB_ANALYSIS);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/information.png");
		desc.setEmbeddedFontSize(10);
		desc.setPreferredHeight(50);
		desc.setPreferredWidth(50);
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.SQUARE);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
	}

}