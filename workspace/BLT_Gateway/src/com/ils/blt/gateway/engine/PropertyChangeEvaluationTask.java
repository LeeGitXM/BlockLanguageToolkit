/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.Hashtable;

import com.ils.block.BlockProperties;
import com.ils.block.NewValueNotification;
import com.ils.block.ProcessBlock;

/**
 * Evaluate the specified block. The thread should end with the block placing a
 * value on its output. In this case, the block responds to a property change, 
 * usually for a tag that is an attribute of the block.
 */
public class PropertyChangeEvaluationTask implements Runnable{
	private final ProcessBlock block;
	/**
	 * Constructor.
	 * 
	 * @param bblock the block instance to evaluate
	 */
	public PropertyChangeEvaluationTask(NewValueNotification nvn)  {	
		this.block = nvn.getBlock();
		Hashtable<String,String> attribute = block.getProperty(nvn.getPropertyName());
		attribute.put(BlockProperties.BLOCK_ATTRIBUTE_VALUE, nvn.getValue().getValue().toString());
		attribute.put(BlockProperties.BLOCK_ATTRIBUTE_QUALITY, nvn.getValue().getQuality().getName());
		attribute.put(BlockProperties.BLOCK_ATTRIBUTE_TIMESTAMP, String.valueOf(nvn.getValue().getTimestamp().getTime()));	
	}
	
	public void run()   { 
		block.evaluate();
	}
}
