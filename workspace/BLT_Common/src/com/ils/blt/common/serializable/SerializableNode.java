package com.ils.blt.common.serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.ils.blt.common.DiagramState;;

/**
 * A SerialiableNode is the base class for a collection of project
 * resources that are passed between gateway and designer contexts.
 * The base class handles issues of the resource path.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public abstract class SerializableNode {
	protected String path;
	protected String name;
	protected DiagramState state = DiagramState.ACTIVE;
	
	public SerializableNode() {	
		path = "";
		name = "UNSET";
	}
	
	public abstract boolean isFolder();

	public String getName() { return name; }
	public String getPath() { return this.path; }
	public DiagramState getState() {return state;}
	
	public void setPath(String p) {this.path = p;}
	public void setName(String nam) { if(nam!=null) name=nam; }
	public void setState(DiagramState ds) { if( ds!=null ) this.state = ds;}
}
