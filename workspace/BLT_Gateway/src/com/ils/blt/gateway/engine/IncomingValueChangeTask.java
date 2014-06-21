/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.ils.blt.common.block.ProcessBlock;
import com.ils.blt.common.control.IncomingNotification;

/**
 * A value has been received as an output from a block
 * connected to the target block. The target should record the new input.
 * The thread should end with either the target doing nothing or with it placing a
 * value on its output.
 */
public class IncomingValueChangeTask implements Runnable{
	private final ProcessBlock target;
	private final IncomingNotification notification;
	/**
	 * Constructor.
	 * 
	 * @param blk the block to be notified of the new value on its input
	 * @param vcn notification describing the new value
	 */
	public IncomingValueChangeTask(ProcessBlock blk,IncomingNotification vcn)  {
		this.target = blk;
		this.notification = vcn;
	}
	
	public void run()   { 
		target.acceptValue(notification);
	}
}
