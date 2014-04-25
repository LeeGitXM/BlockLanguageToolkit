package com.ils.blt.common.serializable;




/**
 * Use this class to describe the resources known to
 * the Gateway block controller. This is a debugging aid to
 * display engine resource in the Designer.
 */
public class SerializableResourceDescriptor {
	private String name;
	private long projectId;
	private long resourceId;
	private String type;
	
	public SerializableResourceDescriptor() {	
		name="UNSET";
	}
	
	public String getName() { return name; }
	public long getProjectId() {return projectId;}
	public long getResourceId() {return resourceId;}
	public String getType() {return type;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setProjectId(long projectId) {this.projectId = projectId;}
	public void setResourceId(long resourceId) {this.resourceId = resourceId;}
	public void setType(String type) {this.type = type;}

}
