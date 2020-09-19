/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import com.ils.blt.common.ProcessBlock;
import com.ils.blt.common.notification.SignalNotification;

/**
 * A broadcast signal has been received that is appropriate to the target
 * block. The block should record the new input then do what it needs to do. 
 * The thread should end with the block either doing nothing or placing a
 * value on its output. 
 */
public class IncomingBroadcastTask implements Runnable{
	private final ProcessBlock target;
	private final SignalNotification notification;
	/**
	 * Constructor.
	 * 
	 * @param blk the block to be notified of the new value on its input
	 * @param sn notification describing the new value
	 */
	public IncomingBroadcastTask(ProcessBlock blk,SignalNotification sn)  {
		this.target = blk;
		this.notification = sn;
	}
	
	public void run()   { 
		if(target!=null) target.acceptValue(notification);
	}
}
