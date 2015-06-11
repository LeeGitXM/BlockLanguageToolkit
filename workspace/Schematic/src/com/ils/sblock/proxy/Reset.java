/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.sblock.proxy;

/**
 * Execute a block reset() method. 
 */
public class Reset extends Callback {

	public Reset() {
		module = "reset";
		setLocalVariableList("block");
	}

}

