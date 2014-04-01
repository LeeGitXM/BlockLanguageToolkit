package com.ils.blt.common.serializable;

import java.util.UUID;



/**
 * This class is a stand-in for an Ignition folder resource.
 * Its purpose is to represent a node in the Application
 * hierarchy for export/import purposes.
 */
public class SerializableFolder {
	private UUID parentId;
	private String name;
	private UUID id;
	


	public SerializableFolder() {	
		name="UNSET";
	}
	
	public String getName() { return name; }
	public void setName(String nam) { if(nam!=null) name=nam; }
	public UUID getId() {return id;}
	public void setId(UUID id) {this.id = id;}
	public UUID getParentId() {return parentId;}
	public void setParentId(UUID id) {this.parentId = id;}
}
