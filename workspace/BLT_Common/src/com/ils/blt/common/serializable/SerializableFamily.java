package com.ils.blt.common.serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.BLTProperties;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;

/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO object should have no behavior.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SerializableFamily extends SerializableNode {
	private SerializableDiagram[] diagrams;
	private SerializableFolder[] folders;
	private GeneralPurposeDataContainer auxiliaryData;
	
	public SerializableFamily() {	
		diagrams = new SerializableDiagram[0];
		folders = new SerializableFolder[0];
		auxiliaryData = new GeneralPurposeDataContainer();
	}
	public ResourceType getResourceType() {
		return BLTProperties.FAMILY_RESOURCE_TYPE;
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

	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public SerializableDiagram[] getDiagrams() { return diagrams; }
	public SerializableFolder[] getFolders() {return folders;}
	
	public void setAuxiliaryData(GeneralPurposeDataContainer data) {if(data!=null) this.auxiliaryData = data;}
	public void setDiagrams(SerializableDiagram[] list) { diagrams=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}	
}