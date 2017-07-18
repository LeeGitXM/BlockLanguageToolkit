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
import com.ils.blt.common.block.TruthValue;
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
public class MultiStateObservation extends AbstractProcessBlock implements ProcessBlock {
	private final static String BLOCK_PROPERTY_CATEGORY0 = "Category0";
	private final static String BLOCK_PROPERTY_CATEGORY1 = "Category1";
	private final static String BLOCK_PROPERTY_EXPLANATION0 = "Explanation0";
	private final static String BLOCK_PROPERTY_EXPLANATION1 = "Explanation1";
	private final static String BLOCK_PROPERTY_INPUT_TYPE = "InputType";
	private final static String BLOCK_PROPERTY_LOGIC 	  = "Logic";
	private final static String BLOCK_PROPERTY_NUMBER_OF_STATES = "NumberOfStates";
	
	private String category0 = "";
	private String category1 = "";
	private String explanation0 = "";
	private String explanation1 = "";
	private String inputType = "STRING";  // Subset of PropertyType
	private String logic = "";
	private int numberOfStates = 1;
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public MultiStateObservation() {
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
	public MultiStateObservation(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports.
	 */
	private void initialize() {	
		setName("MultiStateObservation");
		state = TruthValue.UNSET;
		
		BlockProperty cat0 = new BlockProperty(BLOCK_PROPERTY_CATEGORY0,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_CATEGORY0, cat0);
		BlockProperty cat1 = new BlockProperty(BLOCK_PROPERTY_CATEGORY1,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_CATEGORY1, cat1);
		BlockProperty exp0 = new BlockProperty(BLOCK_PROPERTY_EXPLANATION0,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_EXPLANATION0, exp0);
		BlockProperty exp1 = new BlockProperty(BLOCK_PROPERTY_EXPLANATION1,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_EXPLANATION1, exp1);
		BlockProperty typeProperty = new BlockProperty(BLOCK_PROPERTY_INPUT_TYPE,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_INPUT_TYPE, typeProperty);
		BlockProperty logicProp = new BlockProperty(BLOCK_PROPERTY_LOGIC,"",PropertyType.STRING,true);
		setProperty(BLOCK_PROPERTY_LOGIC, logicProp);
		BlockProperty nStates = new BlockProperty(BLOCK_PROPERTY_NUMBER_OF_STATES,new Integer(numberOfStates),PropertyType.INTEGER,true);
		setProperty(BLOCK_PROPERTY_NUMBER_OF_STATES, nStates);
		
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
		if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_CATEGORY0)) {
			category0 = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_CATEGORY1)) {
			category1 = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_EXPLANATION0)) {
			explanation0 = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_EXPLANATION1)) {
			explanation1 = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_INPUT_TYPE)) {
			inputType = event.getNewValue().toString().toUpperCase();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_LOGIC)) {
			logic = event.getNewValue().toString();
		}
		else if(propertyName.equalsIgnoreCase(BLOCK_PROPERTY_NUMBER_OF_STATES)) {
			try {
				numberOfStates = Integer.parseInt(event.getNewValue().toString());
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
		prototype.setPaletteLabel("MultiStateObs");
		prototype.setTooltipText("Pass through");
		prototype.setTabName(BlockConstants.PALETTE_TAB_OBSERVATION);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.JUNCTION);
		desc.setPreferredHeight(32);
		desc.setPreferredWidth(32);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_MUSTARD);
		desc.setCtypeEditable(true);
	}
}