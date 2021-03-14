/**
 *   (c) 2021  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that returns the property list for a block. 
 */
public class GetAuxiliaryData extends Callback {

	public GetAuxiliaryData() {
		module = "getAuxiliaryData";
		setLocalVariableList("block,aux");
	}

}

