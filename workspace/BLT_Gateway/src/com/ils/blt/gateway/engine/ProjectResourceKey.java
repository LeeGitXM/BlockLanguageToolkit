package com.ils.blt.gateway.engine;

import java.io.Serializable;


//====================================== ProjectResourceKey =================================
/**
 * Class for keyed storage by projectId, resourceId. The two-parameter comparisons are
 * necessary because the resourceId by itself is not unique.
 */
public class ProjectResourceKey implements Serializable {
	private static final long serialVersionUID = 6331391411649992726L;
	private long projectId;
	private long resourceId;
	
	/**
	 * Constructor: No arg version required for serialization.
	 */
	public ProjectResourceKey() {
	}
	
	public ProjectResourceKey(long projid,long resid) {
		this.projectId = projid;
		this.resourceId = resid;
	}
	public long getProjectId() { return projectId; }
	public long getResourceId() { return resourceId; }
	public void setProjectId(long id) { this.projectId=id; }
	public void setResourceId(long id) { this.resourceId=id; }

	// So that class may be used as a map key
	// Same projectId and resourceId is sufficient to prove equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof ProjectResourceKey) {
			ProjectResourceKey that = (ProjectResourceKey)arg;
			if( (this.getProjectId()==that.getProjectId()) &&
					(this.getResourceId()==that.getResourceId())   ) {
				result = true;
			}
		}
		return result;
	}
	@Override
	public int hashCode() {
		return (int)(this.projectId*100000+this.resourceId);
	}
}
