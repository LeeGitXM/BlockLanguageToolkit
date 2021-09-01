/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.ils.block.annotation.ExecutableBlock;
import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
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
import com.ils.common.watchdog.TestAwareQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;

/**
 * Compare the input to a list of states, each associated with a truth-value.
 * Recognize "other" as a catch-all.
 */
@ExecutableBlock
public class StateLookup extends AbstractProcessBlock implements ProcessBlock {
	public static final String STATE_OTHER = "OTHER";
	private BlockProperty valueProperty = null;
	private Map<String,TruthValue> lookupMap;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public StateLookup() {
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
	public StateLookup(ExecutionController ec,ProjectResourceId parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Define the synchronization property and ports. 
	 */
	private void initialize() {	
		setName("StateLookup");
		state = TruthValue.UNSET;
		lookupMap = new HashMap<>();
		String nameValues = "OTHER:UNKNOWN";
		
		BlockProperty nameValuesProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_NAME_VALUES,nameValues,PropertyType.LIST,true);
		setProperty(BlockConstants.BLOCK_PROPERTY_NAME_VALUES, nameValuesProperty);

		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,TruthValue.UNSET,PropertyType.TRUTHVALUE,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);

		
		// Define an input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.TEXT);
		input.setHint(PlacementHint.L);
		input.setIsMultiple(false);
		anchors.add(input);

		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.TRUTHVALUE);
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
		if(!isLocked() &&  vcn.getValue()!=null && vcn.getValue().getValue()!=null ) {
			String lookupString = vcn.getValue().getValue().toString().trim().toUpperCase();
			TruthValue tv = lookupMap.get(lookupString);
			if( tv==null ) tv = lookupMap.get(STATE_OTHER);
			log.debugf("%s.acceptValue: %s => %s", getName(),lookupString,tv.name());
			
			lastValue = new TestAwareQualifiedValue(timer,tv);
			OutgoingNotification nvn = new OutgoingNotification(this,BlockConstants.OUT_PORT_NAME,lastValue);
			controller.acceptCompletionNotification(nvn);
			valueProperty.setValue(tv);
			notifyOfStatus(lastValue);
		}
	}
	
	/**
	 * The statevalues property is a comma-delimited list of name : values.
	 * The list MUST include an "other".
	 */
	@Override
	public void propertyChange(BlockPropertyChangeEvent event) {
		super.propertyChange(event);
		String propertyName = event.getPropertyName();
		if(propertyName.equalsIgnoreCase(BlockConstants.BLOCK_PROPERTY_NAME_VALUES)) {
			String nameValues = event.getNewValue().toString();
			log.debugf("%s.propertyChange: %s => %s", getName(),BlockConstants.BLOCK_PROPERTY_NAME_VALUES,nameValues);
			lookupMap.clear();
			String[] keyvalues = nameValues.split(",");
			for(String key:keyvalues ) {
				String[] nv = key.split(":");
				String nam = nv[0].trim().toUpperCase();
				TruthValue tv = TruthValue.UNKNOWN;
				if( nv.length>1) {
					String text = nv[1].trim().toUpperCase();
					try {
						tv = TruthValue.valueOf(text);
					}
					catch(IllegalStateException ise) {}
				}
				lookupMap.put(nam,tv);
			}
			TruthValue other = lookupMap.get(STATE_OTHER);
			if( other==null ) {
				lookupMap.put(STATE_OTHER,TruthValue.UNKNOWN);
				nameValues = String.format("%s,%s:%s", nameValues,STATE_OTHER,TruthValue.UNKNOWN.name());
				BlockProperty bp = getProperty(BlockConstants.BLOCK_PROPERTY_NAME_VALUES);
				bp.setValue(nameValues);
				QualifiedValue qv = new TestAwareQualifiedValue(timer,bp.getValue());
				controller.sendPropertyNotification(getBlockId().toString(),bp.getName(), qv);
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
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
		controller.sendConnectionNotification(getBlockId().toString(), BlockConstants.OUT_PORT_NAME, qv);
	}
	/**
	 * Augment the palette prototype for this block class.
	 */
	private void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/statelookup.png");
		prototype.setPaletteLabel("StateLookup");
		prototype.setTooltipText("Select output truthvalue based on a lookup of string values");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONTROL);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setEmbeddedIcon("Block/icons/embedded/state_lookup.png");
		desc.setStyle(BlockStyle.DIAMOND);
		desc.setPreferredHeight(70);
		desc.setPreferredWidth(70);
		desc.setBackground(BlockConstants.BLOCK_BACKGROUND_LIGHT_GRAY);
		desc.setEditorClass("com.ils.blt.designer.config.StateLookupEditor");
	}
}