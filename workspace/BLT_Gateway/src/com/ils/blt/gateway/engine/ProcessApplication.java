/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.UUID;

import com.ils.blt.common.serializable.SerializableApplication;

/**
 * An application is a specialized process node.
 */
public class ProcessApplication extends ProcessNode {
	
	/**
	 * Constructor: Create an application node from the NavTree structure of an diagram.
	 *
	 * @param name of the node
	 * @param parent UUID of the parent of this node.
	 * @param me UUID of this node 
	 */
	public ProcessApplication(String name,UUID parent,UUID self) { 
		super(name,parent,self);
	}
	
	/**
	 * Constructor: Create a Gateway object that encapsulates attributes of an Application.
	 * @param app the serialized object that represents the application.
	 * @param parent 
	 */
	public ProcessApplication(SerializableApplication app,UUID parent) { 
		super(app.getName(),parent,app.getId());
	}
	
}
