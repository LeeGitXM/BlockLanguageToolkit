/**
 *   (c) 2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.sblock.proxy;

/**
 * Define the module that sets a property in a specified block. 
 */
public class SetBlockProperty extends Callback {

	public SetBlockProperty() {
		module = "setBlockProperty";
		setLocalVariableList("block,property");
	}
}

