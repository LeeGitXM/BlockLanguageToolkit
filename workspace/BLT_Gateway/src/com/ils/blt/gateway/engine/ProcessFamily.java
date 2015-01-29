/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
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
	private ActiveState state = ActiveState.ACTIVE;
	private String addHook = "";
	private String cloneHook = "";
	private String deleteHook = "";

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
		setAddHook(fam.getAddHook());
		setCloneHook(fam.getCloneHook());
		setDeleteHook(fam.getDeleteHook());
	}
	
	public UUID getId() {return id;}
	public ActiveState getState() {return state;}
	
	public void setId(UUID id) {this.id = id;}
	public void setState(ActiveState state) {this.state = state;}
	
	public String getAddHook() {return addHook;}
	public String getCloneHook() {return cloneHook;}
	public String getDeleteHook() {return deleteHook;}

	public void setAddHook(String hook) {this.addHook = hook;}
	public void setCloneHook(String hook) {this.cloneHook = hook;}
	public void setDeleteHook(String hook) {this.deleteHook = hook;}
}

