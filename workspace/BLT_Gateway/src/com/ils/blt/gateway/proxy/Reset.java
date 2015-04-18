/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Execute a block reset() method. 
 */
public class Reset extends Callback {

	public Reset() {
		module = "reset";
		setLocalVariableList("block");
	}

}

