package com.ils.blt.common.serializable;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.block.ActiveState;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior. The annotation is
 * to make this backward-compatible to before callbacks were introduced.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SerializableApplication {
	private SerializableFamily[] families;
	private SerializableFolder[] folders;

	private UUID id;
	private String name;
	private ActiveState state = ActiveState.ACTIVE;
	
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
	
	public SerializableFamily[] getFamilies() { return families; }
	public SerializableFolder[] getFolders() {return folders;}
	public UUID getId() {return id;}
	public String getName() { return name; }
	public ActiveState getState() {return state;}
	
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setId(UUID id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setState(ActiveState state) {this.state = state;}
}
