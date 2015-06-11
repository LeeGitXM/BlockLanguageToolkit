/**
 *   (c) 2013-2015  ILS Automation. All rights reserved. 
 */
package com.ils.sblock;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.block.AbstractBlock;
import com.ils.blt.common.block.SchematicBlock;
import com.ils.blt.common.control.ExecutionController;
import com.ils.blt.common.notification.IncomingNotification;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;


/**
 * This abstract class is the base of all blocks. It cannot in itself
 * be instantiated. 
 *  
 * The subclasses depend on the "ExecutableBlock" class annotation
 * as the signal to group a particular subclass into the list of 
 * available executable block types.
 */
public abstract class AbstractSchematicBlock extends AbstractBlock implements SchematicBlock {
	protected int inputCount = 0;   // Number of incoming ports
	protected final Map<String,String> inputMap;

	/**
	 * Constructor: The no-arg constructor is used when creating a prototype for use in the palette.
	 *              It does not correspond to a functioning block.
	 */
	public AbstractSchematicBlock() {
		inputMap = new HashMap<>();
	}
	
	/**
	 * Constructor: Use this version to create a block that correlates to a block in the diagram.
	 * @param ec execution controller for handling block output
	 * @param parent universally unique Id identifying the parent of this block. The id may be null for blocks that are "unattached"
	 * @param block universally unique Id for the block
	 */
	public AbstractSchematicBlock(ExecutionController ec, UUID parent, UUID block) {
		super(ec,parent,block);
		inputMap = new HashMap<>();
	}


	/**
	 * When this block has received a value on each of its inputs, then
	 *    1) Append the block's code contribution to the parent diagram's
	 *       procedure.
	 *    2) Propagate the name of the variable used in the procedure code to each
	 *       of the outputs.
	 */
	@Override
	public void acceptValue(IncomingNotification incoming) {
		String port = incoming.getConnection().getSource().toString();
        String value= incoming.getValue().toString();
        inputMap.put(port, value);
        if(inputMap.size()==inputCount) evaluate();
	}

	/**
	 * Resetting a schematic block simply involves setting the visit count to zero.
	 */
	@Override
	public void reset() {
		inputMap.clear();
	}
	
	/**
	 * Write this block's contribution to the diagram's procedure. The default
	 * implementation does nothing.
	 */
	@Override
	public void evaluate() {}
}