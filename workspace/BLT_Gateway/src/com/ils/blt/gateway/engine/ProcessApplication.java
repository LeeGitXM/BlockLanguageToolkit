/**
 *   (c) 2014-2015  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.UUID;

import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.RampMethod;
import com.ils.blt.common.serializable.SerializableApplication;

/**
 * An application is a specialized process node.
 */
public class ProcessApplication extends ProcessNode {
	private UUID id;
	private ActiveState state = ActiveState.ACTIVE;
	private String addHook = "";
	private String cloneHook = "";
	private String deleteHook = "";
	
	/**
	 * Constructor: Create an application node from the NavTree structure of an diagram.
	 *
	 * @param name of the node
	 * @param parent UUID of the parent of this node.
	 * @param me UUID of this node 
	 */
	public ProcessApplication(String name,UUID parent,UUID self) { 
		super(name,parent,self);
		id = UUID.randomUUID();
	}
	
	/**
	 * Constructor: Create a Gateway object that encapsulates attributes of an Application.
	 * @param app the serialized object that represents the application.
	 * @param parent 
	 */
	public ProcessApplication(SerializableApplication app,UUID parent) { 
		super(app.getName(),parent,app.getId());
		setState(app.getState());
		setAddHook(app.getAddHook());
		setCloneHook(app.getCloneHook());
		setDeleteHook(app.getDeleteHook());
	}
	
	public ActiveState getState() {return state;}
	public UUID getId() {return id;}
	public void setId(UUID id) {this.id = id;}
	public void setState(ActiveState s) { this.state = s; }

	
	public String getAddHook() {return addHook;}
	public String getCloneHook() {return cloneHook;}
	public String getDeleteHook() {return deleteHook;}

	public void setAddHook(String hook) {this.addHook = hook;}
	public void setCloneHook(String hook) {this.cloneHook = hook;}
	public void setDeleteHook(String hook) {this.deleteHook = hook;}

}
