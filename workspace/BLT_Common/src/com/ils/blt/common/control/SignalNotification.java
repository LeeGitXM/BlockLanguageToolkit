/**
 *   (c) 2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.common.control;

import com.ils.blt.common.block.ProcessBlock;


/**
 * A SignalNotification is an in-bound message to a block that contains
 * a signal object as its data. The block must be typed as a signal receptor.
 * There is no relationship to connections.
 * 
 * This is a property container with no behavior.
 */
public class SignalNotification {
	private final ProcessBlock block;
	private final Signal signal;
	
	/**
	 * Constructor. The signal is the only property.
	 * 
	  * @param sig the signal to be delivered to the block.
	 */
	public SignalNotification(ProcessBlock blk,Signal sig)  {
		this.block = blk;
		this.signal = sig;
	}
	
	public ProcessBlock getBlock() {return block;}
	public Signal getSignal() {return signal;}
}
