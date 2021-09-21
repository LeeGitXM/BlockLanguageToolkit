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
	private ProjectResourceId resourceId;
	
	/**
	 * Constructor: No arg version required for serialization.
	 */
	public ProjectResourceKey() {
	}
	
	public ProjectResourceKey(ProjectResourceId resid) {
		this.resourceId = resid;
	}

	public String getProjectName() { return resourceId.getProjectName(); }
	
	public ProjectResourceId getResourceId() { return resourceId; }
	public void setResourceId(ProjectResourceId id) { this.resourceId=id; }
	// So that class may be used as a map key
	// Same resourceId proves equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof ProjectResourceKey) {
			ProjectResourceKey that = (ProjectResourceKey)arg;
			if( this.getResourceId().equals(that.getResourceId())  ) {
				result = true;
			}
		}
		return result;
	}
	@Override
	public int hashCode() {
		return 7*resourceId.getResourcePath().hashCode()+37*resourceId.getProjectName().hashCode();
	}
}
