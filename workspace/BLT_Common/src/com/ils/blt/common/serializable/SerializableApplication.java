package com.ils.blt.common.serializable;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.RampMethod;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior. The annotation is
 * to make this backward-compatible to before callbacks were introduced.
 */
@JsonIgnoreProperties
public class SerializableApplication {
	private SerializableFamily[] families;
	private SerializableFolder[] folders;

	private UUID id;
	private String name;
	private String console;
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
	
	public SerializableApplication() {	
		families = new SerializableFamily[0];
		name="UNSET";
		id = UUID.randomUUID();
	}
	
	public void addFamily(SerializableFamily sfam) {
		SerializableFamily[] extended = new SerializableFamily[families.length+1];
	    extended[families.length] = sfam;
	    System.arraycopy(families, 0, extended, 0, families.length);
	    families = extended;
	}
	public void addFolder(SerializableFolder sfold) {
		SerializableFolder[] extended = new SerializableFolder[folders.length+1];
	    extended[folders.length] = sfold;
	    System.arraycopy(folders, 0, extended, 0, folders.length);
	    folders = extended;
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
	public boolean isIncludeInMenus() {return includeInMenus;}
	
	public void setConsole(String console) {this.console = console;}
	public void setDescription(String description) {this.description = description;}
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setHighestPriorityProblem(int highestPriorityProblem) {this.highestPriorityProblem = highestPriorityProblem;}
	public void setId(UUID id) {this.id = id;}
	public void setIncludeInMenus(boolean includeInMenu) {this.includeInMenus = includeInMenu;}
	public void setMessageQueue(String messageQueue) {this.messageQueue = messageQueue;}
	public void setName(String nam) { if(nam!=null) name=nam; }
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
