package com.ils.blt.gateway.engine;

import java.io.Serializable;
import java.util.UUID;


//====================================== ProjectUUIDKey =================================
/**
 * Class for keyed storage by projectId, UUID.
 */
public class ProjectUUIDKey implements Serializable {
	private static final long serialVersionUID = 6221391411649992726L;
	private long projectId;
	private UUID uuid;
	
	/**
	 * Constructor: No arg version required for serialization.
	 */
	public ProjectUUIDKey(long projid,UUID id) {
		this.projectId = projid;
		this.uuid = id;
	}
	public long getProjectId() { return projectId; }
	public UUID getUUID() { return uuid; }
	public void setProjectId(long id) { this.projectId=id; }
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
		return (int)(this.projectId*100000+uuid.hashCode());
	}
}
