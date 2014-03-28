package com.ils.blt.common.serializable;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class SerializableDiagram {
	private SerializableBlock[] blocks;
	private SerializableConnection[] connections;
	private String name;
	private String treePath;  // A unique name from the Designer
	


	public SerializableDiagram() {	
		blocks = new SerializableBlock[0];
		connections= new SerializableConnection[0];
		name="UNSET";
	}
	
	public SerializableBlock[] getBlocks() { return blocks; }
	public SerializableConnection[] getConnections() { return connections; }
	public String getName() { return name; }
	public String getTreePath() {return treePath;}

	public void setBlocks(SerializableBlock[] list) { blocks=list; }
	public void setConnections(SerializableConnection[] list) { connections=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setTreePath(String treePath) {this.treePath = treePath;}
}
