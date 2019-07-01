/**
 *   (c) 2017  ILS Automation. All rights reserved. 
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
public class TripleOptionControl extends AbstractProcessBlock implements ProcessBlock {
	private final static String BLOCK_PROPERTY_CURRENT_OPTION = "CurrentOption";
	private final static String BLOCK_PROPERTY_DEFAULT_OPTION = "DefaultOption";
	private final static String BLOCK_PROPERTY_DISPLAY_ROUTING = "DisplayRouting";
	private final static String BLOCK_PROPERTY_DESCRIPTION1 = "Description1";
	private final static String BLOCK_PROPERTY_DESCRIPTION2 = "Description2";
	private final static String BLOCK_PROPERTY_MESSAGE_TEXT = "MessageText";

	private String currentOption = "";
	private String defaultOption = "";
	private String description1 = "";
	private String description2 = "";
	private String displayRouting = "";
	private String messageText = "";
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public TripleOptionControl() {
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
	public TripleOptionControl(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("TripleOptionControl");

		BlockProperty currentProp = new BlockProperty(BLOCK_PROPERTY_CURRENT_OPTION,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_CURRENT_OPTION, currentProp);
		BlockProperty defaultProp = new BlockProperty(BLOCK_PROPERTY_DEFAULT_OPTION,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_DEFAULT_OPTION, defaultProp);
		BlockProperty dispProp = new BlockProperty(BLOCK_PROPERTY_DISPLAY_ROUTING,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_DISPLAY_ROUTING, dispProp);
		BlockProperty desc1Prop = new BlockProperty(BLOCK_PROPERTY_DESCRIPTION1,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_DESCRIPTION1, desc1Prop);
		BlockProperty desc2Prop = new BlockProperty(BLOCK_PROPERTY_DESCRIPTION2,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_DESCRIPTION2, desc2Prop);
		BlockProperty msgProp = new BlockProperty(BLOCK_PROPERTY_MESSAGE_TEXT,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_MESSAGE_TEXT, msgProp);
		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a two outputs
		AnchorPrototype outputa = new AnchorPrototype("a",AnchorDirection.OUTGOING,ConnectionType.DATA);
		outputa.setHint(PlacementHint.R);
		anchors.add(outputa);
		AnchorPrototype outputb = new AnchorPrototype("b",AnchorDirection.OUTGOING,ConnectionType.DATA);
		outputb.setHint(PlacementHint.R);
		anchors.add(outputb);
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
		if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_CURRENT_OPTION)) {
			currentOption = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_DEFAULT_OPTION)) {
			defaultOption = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_DISPLAY_ROUTING)) {
			displayRouting = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_DESCRIPTION1)) {
			description1 = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_DESCRIPTION2)) {
			description2 = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_MESSAGE_TEXT)) {
			messageText = event.getNewValue().toString();
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
		prototype.setPaletteLabel("TripleOption");
		prototype.setTooltipText("Pass through");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.JUNCTION);
		desc.setPreferredHeight(32);
		desc.setPreferredWidth(32);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_MUSTARD);
		desc.setCtypeEditable(true);
	}
}