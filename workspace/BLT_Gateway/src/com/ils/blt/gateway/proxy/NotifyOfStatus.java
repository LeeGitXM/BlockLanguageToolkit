/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Trigger a status notification from the block 
 */
public class NotifyOfStatus extends Callback {

	public NotifyOfStatus() {
		module = "notifyOfStatus";
		setLocalVariableList("block");
	}
}

