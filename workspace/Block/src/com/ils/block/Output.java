/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.block;

import java.awt.Color;
import java.util.UUID;

import com.ils.blt.common.annotation.ExecutableBlock;
import com.ils.blt.common.block.AnchorDirection;
import com.ils.blt.common.block.AnchorPrototype;
import com.ils.blt.common.block.BindingType;
import com.ils.blt.common.block.BlockConstants;
import com.ils.blt.common.block.BlockDescriptor;
import com.ils.blt.common.block.BlockProperty;
import com.ils.blt.common.block.BlockStyle;
import com.ils.blt.common.block.PropertyType;
import com.ils.blt.common.connection.ConnectionType;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.inductiveautomation.ignition.common.model.values.BasicQualifiedValue;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * An output block propagates values on its input to a configured tag path.
 */
@ExecutableBlock
public class Output extends AbstractProcessBlock implements ProcessBlock {
	private BlockProperty pathProperty = null;
	private BlockProperty valueProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Output() {
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
	public Output(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * The block is notified that a new value has appeared on one of its input anchors.
	 * Write the value to the configured tag. Handle any of the possible input types.
	 * @param vcn notification of the new value.
	 */
	@Override
	public void acceptValue(IncomingNotification vcn) {
		super.acceptValue(vcn);
		QualifiedValue qv = vcn.getValue();
		if( !isLocked() ) {
			log.debugf("%s.acceptValue: received %s",getName(),qv.toString());
			if( pathProperty.getBindingType().equals(BindingType.TAG_WRITE)) {
				log.tracef("%s.acceptValue: writing to path %s",getName(),pathProperty.getBinding().toString());
				controller.updateTag(getParentId(),pathProperty.getBinding().toString(), qv);
			}
			
		}
		valueProperty.setValue(qv.getValue());
		notifyOfStatus(qv);
	}
	
	/**
	 * Send status update notification for our last latest state.
	 */
	@Override
	public void notifyOfStatus() {
		QualifiedValue qv = new BasicQualifiedValue(valueProperty.getValue());
		notifyOfStatus(qv);
		
	}
	private void notifyOfStatus(QualifiedValue qv) {
		controller.sendPropertyNotification(getBlockId().toString(), BlockConstants.BLOCK_PROPERTY_VALUE,qv);
	}
	
	/**
	 * Add properties that are new for this class.
	 * Populate them with default values.
	 */
	protected void initialize() {
		setName("Output");
	    // TAG_WRITE means that we won't create a subscription to it
		pathProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.STRING,true);
		pathProperty.setBindingType(BindingType.TAG_WRITE);
		pathProperty.setBinding("");
		setProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH, pathProperty);
		valueProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_VALUE,"",PropertyType.OBJECT,false);
		valueProperty.setBindingType(BindingType.ENGINE);
		setProperty(BlockConstants.BLOCK_PROPERTY_VALUE, valueProperty);
		
		// Define a single input
		AnchorPrototype input = new AnchorPrototype(BlockConstants.IN_PORT_NAME,AnchorDirection.INCOMING,ConnectionType.DATA);
		anchors.add(input);
	}
	
	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/output.png");
		prototype.setPaletteLabel("Output");
		prototype.setTooltipText("Write the incoming value to a pre-configured tag");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ARROW);
		desc.setPreferredHeight(45);
		desc.setPreferredWidth(60);
		desc.setBackground(new Color(125,110,230).getRGB());   // Purple
		desc.setCtypeEditable(true);
	}
	
	/**
	 * Check the block configuration for missing input tag.
	 * @return a validation summary. Null if everything checks out.
	 */
	@Override
	public String validate() {
		String generic = super.validate();
		StringBuffer summary = new StringBuffer();
		if( generic!=null ) summary.append(generic);
		
		String binding = pathProperty.getBinding();
		if( binding==null || binding.length()==0 ) {
			summary.append(String.format("%s: binding is not configured\t",pathProperty.getName()));
		}
		
		if( summary.length()==0 ) return null;
		else return summary.toString();
	}
}