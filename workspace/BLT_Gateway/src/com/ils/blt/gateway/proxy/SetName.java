/**
 *   (c) 2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the python module that handles block creation. 
 */
public class SetName extends Callback {

	public SetName() {
		module = "setName";
		setLocalVariableList("block,name");
	}
}