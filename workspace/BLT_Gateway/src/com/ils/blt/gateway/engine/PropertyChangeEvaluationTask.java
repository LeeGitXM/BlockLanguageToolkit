/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.Hashtable;

import com.ils.block.BasicBlock;
import com.ils.block.BlockProperties;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;

/**
 * Evaluate the specified block. The thread should end with the block placing a
 * value on its output. In this case, the block responds to a property change, 
 * usually a tag that is an attribute of the block.
 */
public class PropertyChangeEvaluationTask implements Runnable{
	private final BasicBlock block;
	/**
	 * Constructor.
	 * 
	 * @param bblock the block instance to evaluate
	 */
	public PropertyChangeEvaluationTask(BasicBlock bblock,String propertyName,QualifiedValue value)  {	
		this.block = bblock;
		Hashtable<String,String> attribute = block.getProperty(propertyName);
		attribute.put(BlockProperties.BLOCK_ATTRIBUTE_VALUE, value.getValue().toString());
		attribute.put(BlockProperties.BLOCK_ATTRIBUTE_QUALITY, value.getQuality().getName());
		attribute.put(BlockProperties.BLOCK_ATTRIBUTE_TIMESTAMP, String.valueOf(value.getTimestamp().getTime()));	
	}
	
	public void run()   { 
		block.evaluate();
	}
}
