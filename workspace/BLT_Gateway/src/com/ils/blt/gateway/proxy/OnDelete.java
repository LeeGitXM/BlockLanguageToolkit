/**
 *   (c) 2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Execute a block reset() method. 
 */
public class OnDelete extends Callback {

	public OnDelete() {
		module = "onDelete";
		setLocalVariableList("block");
	}

}

