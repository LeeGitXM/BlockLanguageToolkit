/**
 *   (c) 2015  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.gateway.proxy;

/**
 * Define the module that returns the property list for a block. 
 */
public class GetBlockAnchors extends Callback {

	public GetBlockAnchors() {
		module = "getBlockAnchors";
		setLocalVariableList("block,anchors");
	}

}

