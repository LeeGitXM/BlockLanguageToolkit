package com.ils.blt.common.serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.BLTProperties;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;

/**
 * Implement a plain-old-java-object representing a model application
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior. The annotation is
 * to make this backward-compatible to before callbacks were introduced.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SerializableApplication extends SerializableNode {
	private SerializableFamily[] families;
	private SerializableFolder[] folders;
	private GeneralPurposeDataContainer auxiliaryData;

	public SerializableApplication() {	
		families = new SerializableFamily[0];
		name="UNSET";
		path = null;
		auxiliaryData = new GeneralPurposeDataContainer();
	}
	public ResourceType getResourceType() {
		return BLTProperties.APPLICATION_RESOURCE_TYPE;
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
	
	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public SerializableFamily[] getFamilies() { return families; }
	public SerializableFolder[] getFolders() {return folders;}
	
	public void setAuxiliaryData(GeneralPurposeDataContainer data) {if(data!=null) this.auxiliaryData = data;}
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
}
