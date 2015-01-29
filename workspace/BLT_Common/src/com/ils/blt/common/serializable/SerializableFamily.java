package com.ils.blt.common.serializable;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.block.ActiveState;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO object should have no behavior.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SerializableFamily {
	private SerializableDiagram[] diagrams;
	private SerializableFolder[] folders;
	private UUID id;
	private String name;
	private ActiveState state = ActiveState.ACTIVE;
	private String addHook = "";
	private String cloneHook = "";
	private String deleteHook = "";
	private String getAuxDataHook = "";
	private String setAuxDataHook = "";
	private String updateHook = "";
	
	public SerializableFamily() {	
		diagrams = new SerializableDiagram[0];
		folders = new SerializableFolder[0];
		name="UNSET";
		id = UUID.randomUUID();
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
	public UUID getId() {return id;}
	public String getName() { return name; }
	public ActiveState getState() {return state;}
	
	public void setDiagrams(SerializableDiagram[] list) { diagrams=list; }
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public void setId(UUID id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setState(ActiveState state) {this.state = state;}
	
	public String getAddHook() {return addHook;}
	public String getCloneHook() {return cloneHook;}
	public String getDeleteHook() {return deleteHook;}
	public String getGetAuxDataHook() {return getAuxDataHook;}
	public String getSetAuxDataHook() {return setAuxDataHook;}
	public String getUpdateHook() {return updateHook;}
	public void setAddHook(String hook) {this.addHook = hook;}
	public void setCloneHook(String hook) {this.cloneHook = hook;}
	public void setDeleteHook(String hook) {this.deleteHook = hook;}
	public void setGetAuxDataHook(String hook) {this.getAuxDataHook = hook;}
	public void setSetAuxDataHook(String hook) {this.setAuxDataHook = hook;}
	public void setUpdateHook(String hook) {this.updateHook = hook;}
}