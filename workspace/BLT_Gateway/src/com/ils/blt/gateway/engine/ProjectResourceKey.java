package com.ils.blt.gateway.engine;

import java.io.Serializable;

import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;


//====================================== ProjectResourceKey =================================
/**
 * Class for keyed storage by projectId, resourceId. The two-parameter comparisons are
 * necessary because the resourceId by itself is not unique.
 */
public class ProjectResourceKey implements Serializable {
	private static final long serialVersionUID = 6331391411649992726L;
	private String projectName;
	private ProjectResourceId resourceId;
	
	/**
	 * Constructor: No arg version required for serialization.
	 */
	public ProjectResourceKey() {
	}
	
	public ProjectResourceKey(String name,ProjectResourceId resid) {
		this.projectName = name;
		this.resourceId = resid;
	}
	public String getProjectName() { return projectName; }
	public ProjectResourceId getResourceId() { return resourceId; }
	public void setProjectName(String nam) { this.projectName=nam; }
	public void setResourceId(ProjectResourceId id) { this.resourceId=id; }

	// So that class may be used as a map key
	// Same projectId and resourceId is sufficient to prove equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof ProjectResourceKey) {
			ProjectResourceKey that = (ProjectResourceKey)arg;
			if( this.getProjectName().equals(that.getProjectName()) &&
					(this.getResourceId()==that.getResourceId())   ) {
				result = true;
			}
		}
		return result;
	}
	@Override
	public int hashCode() {
		return (int)(this.projectName.hashCode()*100000+this.resourceId.hashCode());
	}
}
