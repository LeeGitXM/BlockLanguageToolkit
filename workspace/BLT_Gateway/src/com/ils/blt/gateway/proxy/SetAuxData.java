/**
 *   (c) 2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that sets external data in a block and writes it
 * to an external destination. 
 */
public class SetAuxData extends Callback {

	public SetAuxData() {
		module = "setAuxData";
		setLocalVariableList("block,aux");
	}
}

