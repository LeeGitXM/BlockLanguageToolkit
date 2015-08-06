/**
 *   (c) 2014-2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.block.proxy;

/**
 * Define the module that sets a value for a python block. 
 */
public class AcceptValue extends Callback {

	public AcceptValue() {
		module = "acceptValue";
		setLocalVariableList("block,port,value,quality,time");
	}
}

