package com.ils.blt.common.serializable;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.block.ActiveState;
import com.ils.blt.common.block.RampMethod;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior. The annotation is
 * to make this backward-compatible to before callbacks were introduced.
 */
@JsonIgnoreProperties
public class SerializableApplication {
	private SerializableFamily[] families;
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
