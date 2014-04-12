package com.ils.blt.common.serializable;

import java.util.UUID;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class SerializableFamily {
	private SerializableDiagram[] diagrams;
	private SerializableFolder[] folders;
	private UUID id;
	private String name;
	private double priority = 0.0;
	public SerializableFamily() {	
		diagrams = new SerializableDiagram[0];
		folders = new SerializableFolder[0];
		name="UNSET";
		id = UUID.randomUUID();
	}

	public SerializableDiagram[] getDiagrams() { return diagrams; }
	public SerializableFolder[] getFolders() {return folders;}
	public UUID getId() {return id;}
	public String getName() { return name; }
	public double getPriority() {return priority;}
	public void setDiagrams(SerializableDiagram[] list) { diagrams=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setId(UUID id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setPriority(double priority) {this.priority = priority;}
}
