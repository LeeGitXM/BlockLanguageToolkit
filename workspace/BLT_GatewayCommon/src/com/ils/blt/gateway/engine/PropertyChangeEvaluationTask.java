/**
 *   (c) 2013-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.ils.blt.common.block.CoreBlock;
import com.ils.blt.common.notification.BlockPropertyChangeEvent;

/**
 * Evaluate the specified block. The thread should end with the block placing a
 * value on its output. In this case, the block responds to a property change, 
 * usually for a tag that is an attribute of the block.
 */
public class PropertyChangeEvaluationTask implements Runnable{
	private final CoreBlock block;
	private final BlockPropertyChangeEvent event;
	/**
	 * Constructor.
	 * 
	 * @param block the block instance to evaluate
	 */
	public PropertyChangeEvaluationTask(CoreBlock block,BlockPropertyChangeEvent event)  {	
		this.block = block;
		this.event = event;
	}
	
	public void run()   { 
		block.propertyChange(event);
	}
}
