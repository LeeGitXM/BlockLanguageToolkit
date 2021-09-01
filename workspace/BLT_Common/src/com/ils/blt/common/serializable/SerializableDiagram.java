package com.ils.blt.common.serializable;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.block.AttributeDisplay;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SerializableDiagram extends SerializableNode {
	private AttributeDisplay[] attributeDisplays;
	private SerializableBlock[] blocks;
	private SerializableConnection[] connections;
	private UUID encapsulationBlockID = null;   // Only for diagrams that are sub-workspaces of an encapsulation
	private String watermark = "";
	private boolean dirty = false;

	public SerializableDiagram() {
		attributeDisplays = new AttributeDisplay[0];
		blocks = new SerializableBlock[0];
		connections= new SerializableConnection[0];
	}
	
	public ResourceType getResourceType() {
		return BLTProperties.DIAGRAM_RESOURCE_TYPE;
	}
	
	public AttributeDisplay[] getAttributeDisplays() { return attributeDisplays; }
	public SerializableBlock[] getBlocks() { return blocks; }
	public SerializableConnection[] getConnections() { return connections; }
	public UUID getEncapsulationBlockId() {return encapsulationBlockID;}
	public String getWatermark() {return watermark;}
	public boolean isDirty() {return dirty;}
	
	public void setAttributeDisplays(AttributeDisplay[] list) { attributeDisplays=list; }
	public void setBlocks(SerializableBlock[] list) { blocks=list; }
	public void setConnections(SerializableConnection[] list) { connections=list; }
	public void setDirty(boolean dirty) {this.dirty = dirty;}
	public void setEncapsulationBlockId(UUID parentId) {this.encapsulationBlockID = parentId;}
	public void setWatermark(String mark) { this.watermark = mark; }
	
	/**
	 * This is a linear search. We are assured (not guaranteed) that the
	 * blocks will have unique names.
	 * @param nam block name
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
	 * @param newConnection the connection to add
	 */
	public void addConnection(SerializableConnection newConnection) {
		if( newConnection==null ) throw new IllegalArgumentException("Attempt to add a null connection to "+getName());
		SerializableConnection[] newConnections = new SerializableConnection[connections.length+1];
		int index = 0;
		for(SerializableConnection cxn:connections) {
			newConnections[index] = cxn;
			index++;
		}
		newConnections[index] = newConnection;
		connections = newConnections;
	}
	
	/**
	 * Remove a block from the block list. The caller is responsible for calling this
	 * method with a block that is currently in the diagram. If no match is found
	 * expect an IndexOutOfRangeException.
	 * @param block the serializable block
	 */
	public void removeBlock(SerializableBlock block) {
		int index = 0;
		SerializableBlock[] newBlocks = new SerializableBlock[blocks.length-1];
		for(SerializableBlock blk:blocks) {
			if( blk==null ) throw new NullPointerException("Null block found while searching for block to remove in diagram "+getName());
			if( blk.getId().equals(block.getId())) continue;
			newBlocks[index]=blk;
			index++;
		}
		this.blocks = newBlocks;
	}
	/**
	 * Remove a connection from the block list. The caller is responsible for calling this
	 * method with a connection that is currently in the diagram. If no match is found
	 * expect an IndexOutOfRangeException.
	 * @param connection the serializable connection
	 */
	public void removeConnection(SerializableConnection connection) {
		int index = 0;
		SerializableConnection[] newConnections = new SerializableConnection[connections.length-1];
		for(SerializableConnection cxn:connections) {
			if( cxn==null ) throw new NullPointerException("Null connection found while searching for connection to remove in diagram "+getName());
			if( cxn.getBeginAnchor() == null ) continue;  // Dangling
			if( cxn.getEndAnchor() == null )   continue;  // Dangling
			if( cxn.getBeginBlock().equals(connection.getBeginBlock())                   &&
				cxn.getBeginAnchor().getId().equals(connection.getBeginAnchor().getId()) &&
				cxn.getEndBlock().equals(connection.getEndBlock())                       &&
				cxn.getEndAnchor().getId().equals(connection.getEndAnchor().getId())	) continue;
			newConnections[index]=cxn;
			index++;
		}
		this.connections = newConnections;
	}
}
