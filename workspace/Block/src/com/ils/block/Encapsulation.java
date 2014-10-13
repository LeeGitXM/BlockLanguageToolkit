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
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.control.IncomingNotification;
import com.ils.blt.common.control.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class propagates data on its inputs to configured connection posts.
 * It then listens on the connection posts and propagates results to its
 * outputs
 */
@ExecutableBlock
public class Encapsulation extends AbstractProcessBlock implements ProcessBlock {
	public static final String BLOCK_PROPERTY_SUB_DIAGRAM_ID = "EncapsulatedDiagram";
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Encapsulation() {
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
	public Encapsulation(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("Encapsulation");
		BlockProperty constant = new BlockProperty(BLOCK_PROPERTY_SUB_DIAGRAM_ID,"",PropertyType.STRING,false);
		properties.put(BLOCK_PROPERTY_SUB_DIAGRAM_ID, constant);
	}
	

	/**
	 * A new value has appeared on our input.  Retain the timestamp.
	 * 
	 * Note: there can be several connections attached to a given port.
	 * @param vcn incoming new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		this.state = BlockState.ACTIVE;
		QualifiedValue qv = vcn.getValue();
		if( qv!=null && qv.getValue()!=null && qv.getQuality().isGood()) {
				OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,qv);
				controller.acceptCompletionNotification(nvn);
		}
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/encapsulation.png");
		prototype.setPaletteLabel("Encapsulation");
		prototype.setTooltipText("Encapsulate a diagram as a sub-function");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setEmbeddedIcon("Block/icons/embedded/green_arrow_down.png");
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEditorClass("com.ils.blt.designer.workspace.EncapsulationAnchorPointEditor");
		desc.setStyle(BlockStyle.SQUARE);
		desc.setPreferredHeight(100);
		desc.setPreferredWidth(100);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
		desc.setEncapsulation(true);
	}
}