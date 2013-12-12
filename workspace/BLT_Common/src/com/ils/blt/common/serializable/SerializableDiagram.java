package com.ils.blt.common.serializable;

import java.util.ArrayList;
import java.util.List;

/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via the Ignition XML serializer.
 * 
 * The POJO objects should have no behavior.
 */
public class SerializableDiagram {
	private List<SerializableBlock> blocks;
	private List<SerializableConnection> connections;
	private String name;
	
	public SerializableDiagram() {	
		blocks = new ArrayList<SerializableBlock>();
		connections= new ArrayList<SerializableConnection>();
		name="UNSET";
	}
	
	public List<SerializableBlock> getBlocks() { return blocks; }
	public List<SerializableConnection> getConnections() { return connections; }
	public String getName() { return name; }

	public void setBlocks(List<SerializableBlock> list) { blocks=list; }
	public void setConnections(List<SerializableConnection> list) { connections=list; }
	public void setName(String nam) { name=nam; }

}
