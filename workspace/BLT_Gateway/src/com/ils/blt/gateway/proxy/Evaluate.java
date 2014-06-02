/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the python module that handles block creation. 
 */
public class Evaluate extends Callback {

	public Evaluate() {
		module = "evaluate";
		setLocalVariableList("block");
	}
}

