package com.ils.blt.common.serializable;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via the Ignition XML serializer.
 * 
 * The POJO objects should have no behavior.
 */
public class SerializableDiagram {
	private SerializableBlock[] blocks;
	private SerializableConnection[] connections;
	private String name;
	
	public SerializableDiagram() {	
		blocks = new SerializableBlock[0];
		connections= new SerializableConnection[0];
		name="UNSET";
	}
	
	public SerializableBlock[] getBlocks() { return blocks; }
	public SerializableConnection[] getConnections() { return connections; }
	public String getName() { return name; }

	public void setBlocks(SerializableBlock[] list) { blocks=list; }
	public void setConnections(SerializableConnection[] list) { connections=list; }
	public void setName(String nam) { name=nam; }
}
