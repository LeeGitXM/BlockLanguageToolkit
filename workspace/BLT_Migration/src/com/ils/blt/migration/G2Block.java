package com.ils.blt.migration;


/**
 * Implement a plain-old-java-object representing a G2 process block.
 * This is designed for serialization via a JSON serializer.
 * 
 * Use arrays instead of Java-generics lists to make this serializable.
 */
public class G2Block {
	private String className = null;
	private String comments;
	private G2Anchor[] connections = null;
	private String uuid = null;
	private String name;
	private G2Property[] properties = null;
	private int x = 0;
	private int y = 0;
	
	public G2Block() {
		this.connections = new G2Anchor[0];
	}
	public String getClassName() {return className;}
	
	public String getComments() {return comments;}
	
	public G2Anchor[] getConnections() { return connections; }
	public String getUuid() { return uuid; }
	public String getName() { return name; }
	public G2Property[] getProperties() { return properties; }
	public int getX() { return x; }
	public int getY() { return y; }
	public void setClassName(String className) {this.className = className;}
	public void setComments(String comments) {this.comments = comments;}
	public void setConnections(G2Anchor[] array) {
		connections = array;
	}
	public void setUuid(String id) { uuid = id; }
	public void setName(String label) { this.name = label; }
	public void setProperties(G2Property[] array) { this.properties = array; }

	public void setX(int xx) { this.x=xx; }
	public void setY(int yy) { this.y=yy; }

}
