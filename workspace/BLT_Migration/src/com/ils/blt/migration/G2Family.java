package com.ils.blt.migration;



/**
 * Implement a plain-old-java-object representing a G2 family of diagrams
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class G2Family {
	private String className; 
	private String comments;
	private G2Folder[] folders;
	private String name;
	private G2Diagram[] problems;
	private G2Property[] properties = null;
	private String uuid;
	private int x;
	private int y;
	public G2Family() {	
		problems = new G2Diagram[0];
		folders = new G2Folder[0];
		name="UNSET";
		comments = "";
	}
	public String getClassName() {return className;}
	public String getComments() {return comments;}
	public G2Folder[] getFolders() {return folders;}
	public String getName() { return name; }
	public G2Diagram[] getProblems() { return problems; }
	public G2Property[] getProperties() { return properties; }
	public String getUuid() {return uuid;}
	public int getX() {return x;}
	public int getY() {return y;}
	public void setClassName(String className) {this.className = className;}
	public void setComments(String comments) {this.comments = comments;}
	public void setFolders(G2Folder[] folders) {this.folders = folders;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setProblems(G2Diagram[] list) { problems=list; }
	public void setProperties(G2Property[] array) { this.properties = array; }
	public void setUuid(String uuid) {this.uuid = uuid;}
	public void setX(int x) {this.x = x;}
	public void setY(int y) {this.y = y;}
}
