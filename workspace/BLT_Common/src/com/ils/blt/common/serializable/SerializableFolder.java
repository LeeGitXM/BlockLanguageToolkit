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
	private SerializableDiagram[] diagrams;
	private SerializableFamily[] families;
	private SerializableFolder[] folders;


	public SerializableFolder() {	
		name="UNSET";
	}
	public void addDiagram(SerializableDiagram sdiag) {
		SerializableDiagram[] extended = new SerializableDiagram[diagrams.length+1];
	    extended[diagrams.length] = sdiag;
	    System.arraycopy(diagrams, 0, extended, 0, diagrams.length);
	    diagrams = extended;
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
	
	public SerializableDiagram[] getDiagrams() { return diagrams; }
	public SerializableFamily[] getFamilies() { return families; }
	public SerializableFolder[] getFolders() {return folders;}
	public String getName() { return name; }
	public void setDiagrams(SerializableDiagram[] list) { diagrams=list; }
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public UUID getId() {return id;}
	public void setId(UUID id) {this.id = id;}
	public UUID getParentId() {return parentId;}
	public void setParentId(UUID id) {this.parentId = id;}
}
