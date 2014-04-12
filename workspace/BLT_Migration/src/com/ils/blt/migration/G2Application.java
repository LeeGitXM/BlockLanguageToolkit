package com.ils.blt.migration;



/**
 * Implement a plain-old-java-object representing a G2 Application
 * that is serializable via a JSON serializer.
 * 
 * This POJO objects should have no behavior.
 */
public class G2Application {
	private String className;
	private String comments;
	private G2Family[] families;
	private G2Folder[] folders;
	private String name;
	private G2Property[] properties = null;
	private String uuid;
	private int x;
	private int y;
	
	
	

	public G2Application() {	
		families = new G2Family[0];
		folders = new G2Folder[0];
		name="UNSET";
	}
	
	public String getClassName() {return className;}
	public String getComments() {return comments;}
	public G2Family[] getFamilies() { return families; }	
	public String getName() { return name; }
	public G2Property[] getProperties() { return properties; }
	public String getUuid() {return uuid;}
	public int getX() {return x;}
	public int getY() {return y;}
	public void setClassName(String className) {this.className = className;}
	public void setComments(String comments) {this.comments = comments;}
	public void setFamilies(G2Family[] list) { families=list; }
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setProperties(G2Property[] array) { this.properties = array; }
	public void setUuid(String uuid) {this.uuid = uuid;}
	public void setX(int x) {this.x = x;}
	public void setY(int y) {this.y = y;}
	public G2Folder[] getFolders() {return folders;}
	public void setFolders(G2Folder[] folders) {this.folders = folders;}
}
