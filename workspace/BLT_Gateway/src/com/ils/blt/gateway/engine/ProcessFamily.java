/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.UUID;

import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.serializable.SerializableFamily;

/**
* A family is a specialized process node.
*/
public class ProcessFamily extends ProcessNode {
	private UUID id;
	private String description = "";
	private int priority = 0;
	private ActiveState state = ActiveState.ACTIVE;
	/**
	 * Constructor: Create a family node from the NavTree structure of an diagram.
	 *
	 * @param name of the node
	 * @param parent UUID of the parent of this node.
	 * @param me UUID of this node 
	 */
	public ProcessFamily(String name,UUID parent,UUID self) { 
		super(name,parent,self);
	}
	
	/**
	 * Constructor: Create a Gateway object that encapsulates attributes of an Application.
	 * @param fam the serialized object that represents the family.
	 * @param parent 
	 */
	public ProcessFamily(SerializableFamily fam,UUID parent) { 
		super(fam.getName(),parent,fam.getId());
	}
	
	public String getDescription() {return description;}
	public UUID getId() {return id;}
	public int getPriority() {return priority;}
	public ActiveState getState() {return state;}
	
	public void setDescription(String description) {this.description = description;}
	public void setId(UUID id) {this.id = id;}
	public void setPriority(int priority) {this.priority = priority;}
	public void setState(ActiveState state) {this.state = state;}
}

