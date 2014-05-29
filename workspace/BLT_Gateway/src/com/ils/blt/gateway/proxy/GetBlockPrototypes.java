/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that returns a list of prototypes
 * corresponding to all blocks that are implemented in python. 
 */
public class GetBlockPrototypes extends Callback {

	public GetBlockPrototypes() {
		module = "getBlockPrototypes";
		localVariableList="prototypes";
	}

}

