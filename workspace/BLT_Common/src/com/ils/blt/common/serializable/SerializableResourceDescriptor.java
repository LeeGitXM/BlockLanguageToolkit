package com.ils.blt.common.serializable;

import java.io.Serializable;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.project.Project;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;


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
	private String className;
	private boolean folder;
	private String path;
	private String projectName;
	private final ResourceType rtype;
	
	public SerializableResourceDescriptor() {	
		name="UNSET";
		className = "";
		folder = false;
		path = "";
		projectName = Project.GLOBAL_PROJECT_NAME;
		rtype = BLTProperties.DIAGRAM_RESOURCE_TYPE;;
	}
	
	public String getClassName() {return className;}
	public String getName() { return name; }
	public String getPath() { return path; }
	public String getProjectName() {return projectName;}
	public boolean isFolder() { return folder; }
	/**
	 * Create a project resource id from components of the descriptor
	 * @return a resource id
	 */
	public ProjectResourceId getResourceId() {
		ProjectResourceId resourceId = new ProjectResourceId(projectName,rtype,path);
		return resourceId;}
	public ResourceType getType() {return rtype;}
	
	public void setClassName(String className) {this.className = className;}
	public void setIsFolder(boolean flag) { this.setIsFolder(this.folder = flag); }
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setPath(String p) { if(p!=null) path=p; }
	public void setProjectName(String name) {this.projectName = name;}

}
