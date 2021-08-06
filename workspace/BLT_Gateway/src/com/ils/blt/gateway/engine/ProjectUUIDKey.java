package com.ils.blt.gateway.engine;

import java.io.Serializable;
import java.util.UUID;


//====================================== ProjectUUIDKey =================================
/**
 * Class for keyed storage by project name, UUID.
 */
public class ProjectUUIDKey implements Serializable {
	private static final long serialVersionUID = 6221391411649992726L;
	private String projectName;
	private UUID uuid;
	
	/**
	 * Constructor: No arg version required for serialization.
	 */
	public ProjectUUIDKey(String projnam,UUID id) {
		this.projectName = projnam;
		this.uuid = id;
	}
	public String getProjectName() { return projectName; }
	public UUID getUUID() { return uuid; }
	public void setProjectName(String nam) { this.projectName=nam; }
	public void setUUD(UUID id) { this.uuid=id; }

	// So that class may be used as a map key
	// Same projectId and resourceId is sufficient to prove equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof ProjectUUIDKey) {
			ProjectUUIDKey that = (ProjectUUIDKey)arg;
			if( (this.getProjectName()==that.getProjectName()) &&
					(this.getUUID().equals(that.getUUID()))   ) {
				result = true;
			}
		}
		return result;
	}
	@Override
	public int hashCode() {
		return (int)(this.projectName.hashCode()*100000+uuid.hashCode());
	}
}
