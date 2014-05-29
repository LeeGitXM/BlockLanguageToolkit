/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that sets a value for a python block. 
 */
public class SetValue extends Callback {

	public SetValue() {
		module = "setValue";
		localVariableList="block,port,value,quality";
	}

}

