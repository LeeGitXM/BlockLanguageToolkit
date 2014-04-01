package com.ils.blt.common.serializable;

import java.util.UUID;



/**
 * This class represents the entire folder hierarchy of an
 * Application. This is designed for export/import. The
 * ProjectResource version of this creates individual resources
 * for each node of this tree.
 */
public class SerializableApplicationTree {
	private SerializableApplication application;
	private SerializableFamily[] families;
	private SerializableFolder[] folders;
	private SerializableDiagram[] diagrams;
	


	public SerializableApplicationTree() {	
		families = new SerializableFamily[0];
		folders = new SerializableFolder[0];
		diagrams = new SerializableDiagram[0];
	}
	
	public SerializableFamily[] getFamilies() { return families; }
	public void setFamilies(SerializableFamily[] list) { families=list; }
	public SerializableApplication getApplication() {return application;}
	public void setApplication(SerializableApplication application) {this.application = application;}
	public SerializableFolder[] getFolders() {return folders;}
	public void setFolders(SerializableFolder[] folders) {this.folders = folders;}
	public SerializableDiagram[] getDiagrams() {return diagrams;}
	public void setDiagrams(SerializableDiagram[] diagrams) {this.diagrams = diagrams;}

}
