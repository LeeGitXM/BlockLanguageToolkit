package com.ils.blt.common.serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.DiagramState;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;

/**
 * A SerialiableNode is the bass class for a collection of project
 * resources that are passed between gateway and designer contexts.
 * The base class handles issues of the resource path.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public abstract class SerializableNode {
	protected StringPath path;
	protected String name;
	protected DiagramState state = DiagramState.ACTIVE;
	
	public SerializableNode() {	
		path = new StringPath(new String[0]);
		name = "UNSET";
	}
	
	public abstract ResourceType getResourceType();
	
	public ResourcePath getResourcePath() {
		return new ResourcePath(getResourceType(),path);
	}
	public String getName() { return name; }
	public StringPath getParentPath() { return this.path.getParentPath(); }
	public StringPath getPath() { return this.path; }
	public DiagramState getState() {return state;}
	
	public void setParentPath(StringPath parent) {
		path = StringPath.extend(parent, name);
	}
	public void setPath(StringPath p) {this.path = p;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setState(DiagramState state) {this.state = state;}
}
