package com.ils.blt.common.serializable;

import java.util.UUID;

import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.RampMethod;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class SerializableApplication {
	private SerializableFamily[] families;
	private SerializableFolder[] folders;

	private UUID id;
	private String name;
	private String console;
	private String description = "";
	private int highestPriorityProblem = 0;
	private boolean includeInMenu = false;
	private String messageQueue = "";
	private RampMethod rampMethod = RampMethod.NONE;
	private String unit = "";
	private ActiveState state = ActiveState.ACTIVE;
	
	public SerializableApplication() {	
		families = new SerializableFamily[0];
		name="UNSET";
		id = UUID.randomUUID();
	}
	
	public String getConsole() {return console;}
	public String getDescription() {return description;}
	public SerializableFamily[] getFamilies() { return families; }
	public SerializableFolder[] getFolders() {return folders;}
	public int getHighestPriorityProblem() {return highestPriorityProblem;}
	public UUID getId() {return id;}
	public String getMessageQueue() {return messageQueue;}
	public String getName() { return name; }
	public RampMethod getRampMethod() {return rampMethod;}
	public ActiveState getState() {return state;}
	public String getUnit() {return unit;}
	public boolean isIncludeInMenu() {return includeInMenu;}
	
	public void setConsole(String console) {this.console = console;}
	public void setDescription(String description) {this.description = description;}
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setHighestPriorityProblem(int highestPriorityProblem) {this.highestPriorityProblem = highestPriorityProblem;}
	public void setId(UUID id) {this.id = id;}
	public void setIncludeInMenu(boolean includeInMenu) {this.includeInMenu = includeInMenu;}
	public void setMessageQueue(String messageQueue) {this.messageQueue = messageQueue;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setRampMethod(RampMethod rampMethod) {this.rampMethod = rampMethod;}
	public void setState(ActiveState state) {this.state = state;}
	public void setUnit(String unit) {this.unit = unit;}
}
