/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.block.proxy;

/**
 * Define the module that returns the state of a block. 
 */
public class GetBlockState extends Callback {

	public GetBlockState() {
		module = "getBlockState";
		setLocalVariableList("block,properties");
	}

}

