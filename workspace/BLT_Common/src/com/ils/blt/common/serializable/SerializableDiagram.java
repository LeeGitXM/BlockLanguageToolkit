package com.ils.blt.common.serializable;

import java.util.UUID;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class SerializableDiagram {
	private SerializableBlock[] blocks;
	private SerializableConnection[] connections;
	private UUID id = null;
	private String name = "UNSET";
	private UUID encapsulationBlockID = null;   // Applies only to diagrams that are sub-workspaces of an encapsulation
	private long resourceId = -1;
	private DiagramState state = DiagramState.ACTIVE;
	private boolean dirty = false;

	public SerializableDiagram() {	
		blocks = new SerializableBlock[0];
		connections= new SerializableConnection[0];
		name = "UNSET";
		id = UUID.randomUUID();
	}

	public SerializableBlock[] getBlocks() { return blocks; }
	public SerializableConnection[] getConnections() { return connections; }
	public UUID getEncapsulationBlockId() {return encapsulationBlockID;}
	public UUID getId() {return id;}
	public String getName() { return name; }
	public long getResourceId() {return resourceId;}
	public DiagramState getState() {return state;}
	public boolean isDirty() {return dirty;}
	
	public void setBlocks(SerializableBlock[] list) { blocks=list; }
	public void setConnections(SerializableConnection[] list) { connections=list; }
	public void setDirty(boolean dirty) {this.dirty = dirty;}
	public void setEncapsulationBlockId(UUID parentId) {this.encapsulationBlockID = parentId;}
	public void setId(UUID id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setResourceId(long resourceId) {this.resourceId = resourceId;}
	public void setState(DiagramState state) {this.state = state;}
	
	/**
	 * This is a linear search. We are assured (not guaranteed) that the
	 * blocks will have unique names.
	 * @return the block in the diagram with the specified name, else null
	 */
	public SerializableBlock getNamedBlock(String nam) {
		for(SerializableBlock blk:getBlocks()) {
			if(blk.getName().equalsIgnoreCase(nam) ) return blk;
		}
		return null;
	}
	/**
	 * Add a connection to this diagram. Woe to any entity holding on to the old array.
	 */
	public void addConnection(SerializableConnection newConnection) {
		SerializableConnection[] newConnections = new SerializableConnection[connections.length+1];
		int index = 0;
		for(SerializableConnection cxn:connections) {
			newConnections[index] = cxn;
			index++;
		}
		newConnections[index] = newConnection;
		connections = newConnections;
	}
}
