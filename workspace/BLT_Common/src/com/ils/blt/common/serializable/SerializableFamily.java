package com.ils.blt.common.serializable;

import java.util.UUID;

import com.ils.blt.common.block.ActiveState;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO object should have no behavior.
 */
public class SerializableFamily {
	private SerializableDiagram[] diagrams;
	private SerializableFolder[] folders;
	private UUID id;
	private String name;
	private String description = "";
	private int priority = 0;
	private ActiveState state = ActiveState.ACTIVE;
	
	public SerializableFamily() {	
		diagrams = new SerializableDiagram[0];
		folders = new SerializableFolder[0];
		name="UNSET";
		id = UUID.randomUUID();
	}

	public String getDescription() {return description;}
	public SerializableDiagram[] getDiagrams() { return diagrams; }
	public SerializableFolder[] getFolders() {return folders;}
	public UUID getId() {return id;}
	public String getName() { return name; }
	public int getPriority() {return priority;}
	public ActiveState getState() {return state;}
	
	public void setDescription(String description) {this.description = description;}
	public void setDiagrams(SerializableDiagram[] list) { diagrams=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setId(UUID id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setPriority(int priority) {this.priority = priority;}
	public void setState(ActiveState state) {this.state = state;}
	
}