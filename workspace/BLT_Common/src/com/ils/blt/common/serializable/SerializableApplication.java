package com.ils.blt.common.serializable;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.DiagramState;
import com.ils.common.GeneralPurposeDataContainer;



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
	private GeneralPurposeDataContainer auxiliaryData;

	private UUID id;
	private String name;
	private DiagramState state = DiagramState.ACTIVE;
	public SerializableApplication() {	
		families = new SerializableFamily[0];
		name="UNSET";
		id = UUID.randomUUID();
		auxiliaryData = new GeneralPurposeDataContainer();
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
	public UUID getId() {return id;}
	public String getName() { return name; }
	public DiagramState getState() {return state;}
	
	public void setAuxiliaryData(GeneralPurposeDataContainer data) {if(data!=null) this.auxiliaryData = data;}
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setId(UUID id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setState(DiagramState state) {this.state = state;}
}
