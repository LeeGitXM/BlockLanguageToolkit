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
import com.ils.blt.common.block.PlacementHint;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * This class is a no-op. It simply passes its input onto the output.
 */
@ExecutableBlock
public class NotGate extends AbstractProcessBlock implements ProcessBlock {
	private final static String BLOCK_PROPERTY_LOGIC = "Logic";
	private final static String BLOCK_PROPERTY_UNCERTAINTY = "Uncertainty";
	
	private String logic = "";
	private double uncertainty = 1.0;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public NotGate() {
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
	public NotGate(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("NotGate");

		BlockProperty logicProp = new BlockProperty(BLOCK_PROPERTY_LOGIC,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_LOGIC, logicProp);
		BlockProperty unProperty = new BlockProperty(BLOCK_PROPERTY_UNCERTAINTY,new Double(uncertainty),PropertyType.DOUBLE,true);
		setProperty(BLOCK_PROPERTY_UNCERTAINTY, unProperty);
		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.DATA);
		output.setHint(PlacementHint.R);
		anchors.add(output);
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
			lastValue = vcn.getValue();
			//log.infof("%s.acceptValue: %s", getName(),qv.getValue().toString());
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			notifyOfStatus(lastValue);
		}
	}
	/**
	 * Handle a change to one of our custom properties.
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_LOGIC)) {
			logic = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_UNCERTAINTY)) {
			try {
				uncertainty = Integer.parseInt(event.getNewValue().toString());
			}
			catch(NumberFormatException nfe) {
				log.warnf("%s: propertyChange Unable to convert number of states to an integer (%s)",getName(),nfe.getLocalizedMessage());
			}
		}
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
		prototype.setPaletteIconPath("Block/icons/palette/todo.png");
		prototype.setPaletteLabel("NotGate");
		prototype.setTooltipText("Pass through");
		prototype.setTabName(BlockConstants.PALETTE_TAB_INFERENCE);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.JUNCTION);
		desc.setPreferredHeight(32);
		desc.setPreferredWidth(32);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_MUSTARD);
		desc.setCtypeEditable(true);
	}
}