package com.ils.blt.migration;



/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class G2Diagram {
	private G2Block[] blocks;
	private String className;
	private String comments;
	private G2Connection[] connections;
	private String name;
	private G2Property[] properties;
	private String uuid;
	private int x;
	private int y;
	
	public G2Diagram() {	
		blocks = new G2Block[0];
		connections = new G2Connection[0];
		properties = new G2Property[0];
		name="UNSET";
	}
	
	public G2Block[] getBlocks() { return blocks; }
	public String getClassName() {return className;}
	public String getComments() {return comments;}
	public G2Connection[] getConnections() {return connections;}
	public String getName() { return name; }
	public G2Property[] getProperties() {return properties;}
	public String getUuid() {return uuid;}	
	public int getX() {return x;}
	public int getY() {return y;}
	public void setBlocks(G2Block[] list) { blocks=list; }
	public void setClassName(String className) {this.className = className;}
	public void setComments(String comments) {this.comments = comments;}
	public void setConnections(G2Connection[] connections) {this.connections = connections;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setProperties(G2Property[] properties) {this.properties = properties;}
	public void setUuid(String uuid) {this.uuid = uuid;}

	public void setX(int x) {this.x = x;}
	public void setY(int y) {this.y = y;}
}
