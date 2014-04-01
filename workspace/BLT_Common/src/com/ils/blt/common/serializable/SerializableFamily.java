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
	private String name;
	private UUID id;

	public SerializableFamily() {	
		diagrams = new SerializableDiagram[0];
		name="UNSET";
	}
	
	public SerializableDiagram[] getDiagrams() { return diagrams; }
	public String getName() { return name; }

	public void setDiagrams(SerializableDiagram[] list) { diagrams=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
	public UUID getId() {return id;}
	public void setId(UUID id) {this.id = id;}
}
