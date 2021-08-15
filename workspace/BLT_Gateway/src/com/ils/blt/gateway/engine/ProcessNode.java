/**
d *   (c) 2014-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.GeneralPurposeDataContainer;
import com.ils.common.log.ILSLogger;
import com.ils.common.log.LogMaker;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;

/**
 * A process node is a folder of type: application, family, folder, or diagram.
 * We require the hierarchy in the gateway for purposes of routing conclusions
 * to the proper destination.
 */
public class ProcessNode implements Serializable {
	private static final long serialVersionUID = 6280701183405134254L;
	private final static String PATH_SEPARATOR = ":";
	private final Map<ProjectResourceId,ProcessNode> children;   // Key by resourceId
	protected final ILSLogger log;
	private String name;
	protected ResourcePath parent;
	protected ProjectResourceId resourceId;   // Resource set when serialized.
	private final String CLSS = "ProcessNode";
	private GeneralPurposeDataContainer auxiliaryData;
	
	/**
	 * Constructor: 
	 * @param nam of the node
	 * @param parent UUID of the parent of this node.
	 * @param me UUID of this node 
	 */
	public ProcessNode(String nam, ResourcePath parent, ProjectResourceId me) { 
		this.resourceId = me;
		this.parent = parent;
		this.name = nam;
		this.auxiliaryData = new GeneralPurposeDataContainer();
		this.children = new HashMap<>();
		this.log = LogMaker.getLogger(this);
	}

	public void addChild(ProcessNode child)    { 
		children.put(child.getResourceId(),child);
		log.debugf("%s.addChild: %s[%s]",CLSS,getName(),child.getName());
	}

	// So that class is comparable
	// Same self (UUID) is sufficient to prove equality
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

	public ProcessNode getChildForName(String nam) { return children.get(nam); }
	public Collection<ProcessNode> getChildren() { return children.values(); }
	public String getName() {return name;}
	/**
	 * @return the resource path of the parent of this node.
	 *         The parent of the root node is null.
	 */
	public ResourcePath getParent() { return this.parent; }
	public ProjectResourceId getResourceId() { return this.resourceId; }
	public String getProjectName() {return resourceId.getProjectName();}
	public void setParent(ResourcePath p) { this.parent = p; }
	
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
	 * Create a string path from the nade name and parent path
	 */
	public String getPath() {
		String path = String.format("%s/%s", parent.getPath().toString(),name);
		return path;
	}
	/**
	 * Traverse the parentage prepending names to produce a tree path.
	 * The full path does NOT include the project name.
	 * Use nodesByUUID to help converting our UUIDs to objects
	 * @return
	 */
	public String getTreePath(Map<ResourcePath,ProcessNode> nodesByPath) {
		StringBuffer buf = new StringBuffer(name);
		ProcessNode parentNode = nodesByPath.get(getParent());
		while(parentNode!=null) {
			buf.insert(0,PATH_SEPARATOR);
			buf.insert(0,parentNode.getName());
			if( parentNode.getParent()==null) break;
			parentNode = nodesByPath.get(parentNode.getParent());
		}
		buf.insert(0,PATH_SEPARATOR);  // Leading colon
		return buf.toString();
	}
	@Override
	public int hashCode() {
		return this.getResourceId().hashCode();
	}	
	public void removeChild(ProcessNode child) { children.remove(child.getResourceId());} 
	public void setName(String nam) { this.name = nam; }
	public void setResourceId(ProjectResourceId resid) {this.resourceId = resid;}
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
		descriptor.setName(getName());
		descriptor.setPath(resourceId.getResourcePath().getPath().toString());
		descriptor.setProjectName(resourceId.getProjectName());
		descriptor.setType(BLTProperties.FOLDER_RESOURCE_TYPE);
		return descriptor;
	}
	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public void setAuxiliaryData(GeneralPurposeDataContainer auxiliaryData) {this.auxiliaryData = auxiliaryData;}
}
