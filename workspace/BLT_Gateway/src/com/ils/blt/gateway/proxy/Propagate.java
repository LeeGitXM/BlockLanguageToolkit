/**
 *   (c) 2017  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the python module that handles block creation. 
 */
public class Propagate extends Callback {

	public Propagate() {
		module = "propagate";
		setLocalVariableList("block");
	}
}

