package com.ils.blt.common.serializable;

import java.io.Serializable;


/**
 * Use this class to describe the resources known to
 * the Gateway block controller. This class allows the
 * display of engine resources in Designer or Client scope.
 * 
 * This is used for both project resources and blocks.
 */
public class SerializableResourceDescriptor implements Serializable {
	private static final long serialVersionUID = 5498197358912286066L;
	private String name;
	private String id;
	private String className;
	private String path;
	private long projectId;
	private long resourceId;
	private String type;
	
	public SerializableResourceDescriptor() {	
		name="UNSET";
		id = "";
		className = "";
		path = "";
		projectId = -1;
		resourceId = -1;
		type = "";
	}
	
	public String getClassName() {return className;}
	public String getId() {return id;}
	public String getName() { return name; }
	public String getPath() { return path; }
	public long getProjectId() {return projectId;}
	public long getResourceId() {return resourceId;}
	public String getType() {return type;}
	
	public void setClassName(String className) {this.className = className;}
	public void setId(String id) {this.id = id;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setPath(String p) { if(p!=null) path=p; }
	public void setProjectId(long projectId) {this.projectId = projectId;}
	public void setResourceId(long resourceId) {this.resourceId = resourceId;}
	public void setType(String type) {this.type = type;}

}
