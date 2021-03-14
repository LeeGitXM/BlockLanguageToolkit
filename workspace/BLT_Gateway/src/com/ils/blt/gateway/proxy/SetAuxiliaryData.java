/**
 *   (c) 2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that sets a property in a specified block. 
 */
public class SetAuxiliaryData extends Callback {

	public SetAuxiliaryData() {
		module = "setAuxiliaryData";
		setLocalVariableList("block,aux");
	}
}

