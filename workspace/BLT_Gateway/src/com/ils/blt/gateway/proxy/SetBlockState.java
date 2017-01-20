/**
 *   (c) 2017  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that sets a property in a specified block. 
 */
public class SetBlockState extends Callback {

	public SetBlockState() {
		module = "setBlockState";
		setLocalVariableList("block,state");
	}
}

