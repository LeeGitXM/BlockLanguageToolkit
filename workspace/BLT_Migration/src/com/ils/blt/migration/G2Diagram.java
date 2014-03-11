package com.ils.blt.migration;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a XML or JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class G2Diagram {
	private G2Block[] blocks;
	private SerializableConnection[] connections;
	private String name;
	
	public G2Diagram() {	
		blocks = new G2Block[0];
		connections= new SerializableConnection[0];
		name="UNSET";
	}
	
	public G2Block[] getBlocks() { return blocks; }
	public SerializableConnection[] getConnections() { return connections; }
	public String getName() { return name; }

	public void setBlocks(G2Block[] list) { blocks=list; }
	public void setConnections(SerializableConnection[] list) { connections=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
}
