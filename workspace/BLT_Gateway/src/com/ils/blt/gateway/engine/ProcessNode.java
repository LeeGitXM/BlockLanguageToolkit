/**
 *   (c) 2014-2022  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.gateway.ControllerRequestHandler;
import com.inductiveautomation.ignition.common.StringPath;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * A process node is a container of type: folder, or diagram.
 * We require the hierarchy in the gateway for purposes of routing conclusions
 * to the proper destination.
 */
public class ProcessNode implements Serializable {
	private static final long serialVersionUID = 6280701183405134254L;
	private final String CLSS = "ProcessNode";
	private final static boolean DEBUG = true;
	protected String name;
	protected final ProjectResourceId resourceId;   // Resource set when serialized.
	protected final Map<ResourcePath,ProcessNode> children;   // Key by resource path
	protected final LoggerEx log;

	/**
	 * Constructor: 
	 * @param projectName project for this resource
	 * @param nam name of the node
	 * @param parentPath string path to the parent node
	 */
	public ProcessNode(String projectName,String nam,String parentPath) { 
		this.name = nam;
		StringPath parent = StringPath.parse(parentPath);
		String path = StringPath.extend(parent, nam).toString();
		ControllerRequestHandler requestHandler = ControllerRequestHandler.getInstance();
		this.resourceId = requestHandler.createResourceId(projectName, path);
		this.children = new HashMap<>();
		this.log = LogUtil.getLogger(getClass().getPackageName());
		if(DEBUG) log.infof("%s.constructor(): %s - %s", CLSS, nam, parentPath);
	}
	
	/**
	 * Constructor: 
	 * @param resid resourceId of this node.
	 * @param nam of the node
	 */
	public ProcessNode(ProjectResourceId resid,String nam) { 
		this.resourceId = resid;
		this.name = nam;
		this.children = new HashMap<>();
		this.log = LogUtil.getLogger(getClass().getPackageName());
		if(DEBUG) log.infof("%s.constructor(): %s", CLSS, nam);
	}

	public void addChild(ProcessNode child)    { 
		children.put(child.getResourceId().getResourcePath(),child);
		if(DEBUG) log.infof("%s.addChild() Path: <%s>, Parent: <%s>, child: %s", CLSS, child.getResourceId().getResourcePath(), name, child.getName());
	}

	// So that class is comparable
	// Same resourceId is sufficient to prove equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof ProcessNode) {
			ProcessNode that = (ProcessNode)arg;
			if( this.getResourceId().equals(that.getResourceId()) ) {
				result = true;
			}
		}
		return result;
	}

	public ProcessNode getChildForName(String nam) {
		ProcessNode child = null;
		for(ProcessNode node:children.values()) {
			if(node.getName().equalsIgnoreCase(name)) {
				child = node;
				break;
			}
		}
		return child;
	}
	public Collection<ProcessNode> getChildren() { 
		
		// List the nodes
		if( DEBUG ) log.infof("%s getting the children of %s", CLSS, name);
		for(ProcessNode node:children.values()) {
			if( DEBUG ) log.infof("%s: found %s - %s", CLSS, node.getName(), node.getPath());
		}
		
		return children.values(); 
		}
	
	public String getName() {return name;}
	public ProjectResourceId getResourceId() { return this.resourceId; }
	public String getProjectName() {
		String project = "";
		if( resourceId!=null)project = resourceId.getProjectName();
		return project;
	}
	
	/**
	 * Create a list of descendants by recursing through the children. Add
	 * self to the list last. This results in a list ordered bottom up.
	 */
	public void collectDescendants(List<ProcessNode> descendants) { 
		for(ProcessNode child:getChildren()) {
			child.collectDescendants(descendants);
		}
		descendants.add(this);
	}

	/**
	 * Create a string path from resourceId
	 */
	public String getPath_ORIGINAL() {return resourceId.getFolderPath();}
	public String getPath() {
		String path = "";
		if( resourceId!=null)path = resourceId.getFolderPath();
		return path;
		}

	@Override
	public int hashCode() {
		return this.getResourceId().hashCode();
	}	
	public void removeChild(ProcessNode child) { children.remove(child);} 
	public void setName(String nam) { this.name = nam; }

	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
		descriptor.setName(getName());
		descriptor.setPath(getPath());
		descriptor.setProjectName(resourceId.getProjectName());
		descriptor.setIsFolder(true);
		return descriptor;
	}
}
