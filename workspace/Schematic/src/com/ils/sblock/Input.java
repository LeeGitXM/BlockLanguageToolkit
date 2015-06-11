/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.sblock;

import java.awt.Color;
import java.util.UUID;

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
import com.ils.blt.common.notification.BlockPropertyChangeEvent;
import com.ils.blt.common.notification.IncomingNotification;
import com.ils.blt.common.notification.OutgoingNotification;
import com.ils.common.annotation.CompilableBlock;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * When evaluated, append to the python procedure code.
 * 
 * Annotate the constructor.
 */
@CompilableBlock
public class Input extends AbstractSchematicBlock implements SchematicBlock {
	private BlockProperty tagPathProperty = null;
	
	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 */
	public Input() {
		initialize();
		initializePrototype();
	}
	
	/**
	 * Constructor: Custom property is "entry"
	 * 
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block
	 * @param block universally unique Id for the block
	 */
	public Input(ExecutionController ec,UUID parent,UUID block) {
		super(ec,parent,block);
		initialize();
	}
	
	/**
	 * Add the tag property and link it to the value property.
	 */
	protected void initialize() {
		setName("Input");
		// This property causes the engine to start a subscription.
		tagPathProperty = new BlockProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH,"",PropertyType.STRING,true);
		tagPathProperty.setBinding("");
		tagPathProperty.setBindingType(BindingType.TAG_READ);
		setProperty(BlockConstants.BLOCK_PROPERTY_TAG_PATH, tagPathProperty);
		inputCount = 0;
		
		// Define a single output
		AnchorPrototype output = new AnchorPrototype(BlockConstants.OUT_PORT_NAME,AnchorDirection.OUTGOING,ConnectionType.SIGNAL);
		anchors.add(output);
	}
	
	/**
	 * This method is not called during normal operation of the block.
	 * It is called externally to propagate a tag value.
	 */
	@Override
	public void evaluate() {
		String path = tagPathProperty.getBinding().toString();
		QualifiedValue val = controller.getTagValue(getParentId(),path);
	}

	/**
	 * Augment the palette prototype for this block class.
	 */
	protected void initializePrototype() {
		prototype.setPaletteIconPath("Block/icons/palette/input.png");
		prototype.setPaletteLabel("Input");
		prototype.setTooltipText("Place values on the output when a configured tag is updated");
		prototype.setTabName(BlockConstants.PALETTE_TAB_CONNECTIVITY);
		
		BlockDescriptor desc = prototype.getBlockDescriptor();
		desc.setBlockClass(getClass().getCanonicalName());
		desc.setStyle(BlockStyle.ARROW);
		desc.setPreferredHeight(45);
		desc.setPreferredWidth(60);
		desc.setBackground(Color.cyan.getRGB());
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
		
		String binding = tagPathProperty.getBinding();
		if( binding==null || binding.length()==0 ) {
			summary.append(String.format("%s: binding is not configured\t",tagPathProperty.getName()));
		}
		
		if( summary.length()==0 ) return null;
		else return summary.toString();
	}
}