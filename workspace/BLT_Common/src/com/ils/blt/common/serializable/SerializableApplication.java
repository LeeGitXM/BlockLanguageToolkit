package com.ils.blt.common.serializable;

import java.util.UUID;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class SerializableApplication {
	private SerializableFamily[] families;
	private SerializableFolder[] folders;
	private double highestPriorityProblem = 0.0;

	private UUID id;
	private String name;
	public SerializableApplication() {	
		families = new SerializableFamily[0];
		name="UNSET";
		id = UUID.randomUUID();
	}
	
	public SerializableFamily[] getFamilies() { return families; }
	public SerializableFolder[] getFolders() {return folders;}
	public double getHighestPriorityProblem() {return highestPriorityProblem;}
	public UUID getId() {return id;}
	public String getName() { return name; }
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setHighestPriorityProblem(double highestPriorityProblem) {this.highestPriorityProblem = highestPriorityProblem;}
	public void setId(UUID id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
}
