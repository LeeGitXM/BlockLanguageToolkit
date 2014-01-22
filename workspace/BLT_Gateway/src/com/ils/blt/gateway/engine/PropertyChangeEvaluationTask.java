/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.beans.PropertyChangeEvent;

import com.ils.block.ProcessBlock;

/**
 * Evaluate the specified block. The thread should end with the block placing a
 * value on its output. In this case, the block responds to a property change, 
 * usually for a tag that is an attribute of the block.
 */
public class PropertyChangeEvaluationTask implements Runnable{
	private final ProcessBlock block;
	private final PropertyChangeEvent event;
	/**
	 * Constructor.
	 * 
	 * @param block the block instance to evaluate
	 */
	public PropertyChangeEvaluationTask(ProcessBlock block,PropertyChangeEvent event)  {	
		this.block = block;
		this.event = event;
	}
	
	public void run()   { 
		block.propertyChange(event);
	}
}
