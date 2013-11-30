/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.ils.block.NewValueNotification;
import com.ils.block.ProcessBlock;

/**
 * A value has been received as an output from a block
 * connected to the target block. Record the new input then
 * evaluate the specified block. The thread should end with the block placing a
 * value on its output. In this case, the block responds to a property change, 
 * usually for a tag that is an attribute of the block.
 */
public class PortReceivesValueTask implements Runnable{
	private final ProcessBlock block;
	/**
	 * Constructor.
	 * 
	 * @param nvn notification describing the new value
	 */
	public PortReceivesValueTask(NewValueNotification nvn)  {
		this.block = nvn.getBlock();
		block.setValue(nvn.getPort(), nvn.getValue());
	}
	
	public void run()   { 
		block.evaluate();
	}
}
