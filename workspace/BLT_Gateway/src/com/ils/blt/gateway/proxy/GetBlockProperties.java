/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that returns the property list for a block. 
 */
public class GetBlockProperties extends Callback {

	public GetBlockProperties() {
		module = "getBlockProperties";
		localVariableList="block,properties";
	}

}

