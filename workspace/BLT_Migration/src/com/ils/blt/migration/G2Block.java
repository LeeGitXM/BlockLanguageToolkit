package com.ils.blt.migration;

import java.util.UUID;

import com.ils.block.common.BlockProperty;
import com.ils.block.common.BlockState;
import com.ils.block.common.BlockStyle;
import com.ils.blt.common.serializable.SerializableAnchor;

/**
 * Implement a plain-old-java-object representing a G2 process block.
 * This is designed for serialization via a JSON serializer.
 * 
 * Use arrays instead of Java-generics lists to make this serializable.
 */
public class G2Block {
	private int x = 0;
	private int y = 0;
	private UUID id = null;
	private String className = null;
	private String label;
	private G2Property[] properties = null;
	private G2Connection[] connections = null;
	
	public G2Block() {
		this.connections = new G2Connection[0];
	}
	
	public String getClassName() {return className;}
	public void setClassName(String className) {this.className = className;}
	public UUID getId() { return id; }
	public void setId(UUID uuid) { id = uuid; }
	public String getLabel() { return label; }
	public void setLabel(String label) { this.label = label; }
	public int getX() { return x; }
	public int getY() { return y; }
	public void setX(int xx) { this.x=xx; }
	public void setY(int yy) { this.y=yy; }
	public G2Connection[] getConnections() { return connections; }
	public void setConnections(G2Connection[] array) {
		connections = array;
	}

	public G2Property[] getProperties() { return properties; }
	public void setProperties(G2Property[] array) { this.properties = array; }

}
