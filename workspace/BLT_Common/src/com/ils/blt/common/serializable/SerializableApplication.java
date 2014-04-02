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
	private String name;
	private UUID id;
	


	public SerializableApplication() {	
		families = new SerializableFamily[0];
		name="UNSET";
		id = UUID.randomUUID();
	}
	
	public SerializableFamily[] getFamilies() { return families; }
	public String getName() { return name; }

	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
	public UUID getId() {return id;}
	public void setId(UUID id) {this.id = id;}
}
