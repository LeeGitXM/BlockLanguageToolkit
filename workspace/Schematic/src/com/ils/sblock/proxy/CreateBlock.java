/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.sblock.proxy;

/**
 * Define the python module that handles block creation. 
 */
public class CreateBlock extends Callback {

	public CreateBlock() {
		module = "createBlockInstance";
		setLocalVariableList("className,parent,id,result");
	}
}

