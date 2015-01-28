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
	private String console = "";
	private String description = "";
	private int highestPriorityProblem = 0;
	private boolean includeInMenus = false;
	private String messageQueue = "";
	private RampMethod rampMethod = RampMethod.NONE;
	private String unit = "";
	private ActiveState state = ActiveState.ACTIVE;
	private String addHook = "";
	private String deleteHook = "";
	private String updateHook = "";
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
		setAddHook(app.getAddHook());
		setDeleteHook(app.getDeleteHook());
		setUpdateHook(app.getUpdateHook());
	}
	
	public String getConsole() {return console;}
	public String getDescription() {return description;}
	public int getHighestPriorityProblem() {return highestPriorityProblem;}
	public String getMessageQueue() {return messageQueue;}
	public RampMethod getRampMethod() {return rampMethod;}
	public ActiveState getState() {return state;}
	public String getUnit() {return unit;}
	public UUID getId() {return id;}
	
	public void setConsole(String console) {this.console = console;}
	public void setDescription(String description) {this.description = description;}
	public void setHighestPriorityProblem(int highestPriorityProblem) {this.highestPriorityProblem = highestPriorityProblem;}
	public void setId(UUID id) {this.id = id;}
	public void setMessageQueue(String messageQueue) {this.messageQueue = messageQueue;}
	public void setRampMethod(RampMethod rampMethod) {this.rampMethod = rampMethod;}
	public void setState(ActiveState state) {this.state = state;}
	public void setUnit(String unit) {this.unit = unit;}
	
	public String getAddHook() {return addHook;}
	public String getDeleteHook() {return deleteHook;}
	public String getUpdateHook() {return updateHook;}
	public void setAddHook(String hook) {this.addHook = hook;}
	public void setDeleteHook(String hook) {this.deleteHook = hook;}
	public void setUpdateHook(String hook) {this.updateHook = hook;}
}
