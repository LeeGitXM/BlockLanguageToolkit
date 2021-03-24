/**
 *   (c) 2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that returns the auxiliary data from an external source and
 * saves in a block. 
 */
public class GetAuxData extends Callback {

	public GetAuxData() {
		module = "getAuxData";
		setLocalVariableList("block,aux");
	}
}