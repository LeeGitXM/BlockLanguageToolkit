package com.ils.blt.common.serializable;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;



/**
 * This class is a stand-in for an Ignition folder resource.
 * Its purpose is to represent a node in the Application
 * hierarchy for export/import purposes.
 */
public class SerializableFolder extends SerializableNode {
	private SerializableDiagram[] diagrams;
	private SerializableFolder[] folders;


	public SerializableFolder() {	
		name="UNSET";
	}
	
	public ResourceType getResourceType() {
		return BLTProperties.FOLDER_RESOURCE_TYPE;
	}
	
	public void addDiagram(SerializableDiagram sdiag) {
		SerializableDiagram[] extended = new SerializableDiagram[diagrams.length+1];
	    extended[diagrams.length] = sdiag;
	    System.arraycopy(diagrams, 0, extended, 0, diagrams.length);
	    diagrams = extended;
	}

	public void addFolder(SerializableFolder sfold) {
		SerializableFolder[] extended = new SerializableFolder[folders.length+1];
	    extended[folders.length] = sfold;
	    System.arraycopy(folders, 0, extended, 0, folders.length);
	    folders = extended;
	}
	
	public SerializableDiagram[] getDiagrams() { return diagrams; }
	public SerializableFolder[] getFolders() {return folders;}
	public void setDiagrams(SerializableDiagram[] list) { diagrams=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}

}
