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
import com.ils.common.GeneralPurposeDataContainer;
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
	private final Map<ResourcePath,ProcessNode> children;   // Key by resourceId
	protected final LoggerEx log;
	private String name;
	protected final ControllerRequestHandler requestHandler;
	protected ProjectResourceId resourceId;   // Resource set when serialized.
	private final String CLSS = "ProcessNode";
	private GeneralPurposeDataContainer auxiliaryData;
	
	/**
	 * Constructor: 
	 * @param nam of the node
	 * @param parent UUID of the parent of this node.
	 * @param projectName project for this resource
	 * @param path string path to this resource
	 * @param type resource type as a string 
	 */
	public ProcessNode(ResourcePath parent,String projectName,String nam,String type) { 
		requestHandler = ControllerRequestHandler.getInstance();
		this.name = nam;
		String path = StringPath.extend(parent.getPath(), nam).toString();
		this.resourceId = requestHandler.createResourceId(projectName, path,type);
		this.auxiliaryData = new GeneralPurposeDataContainer();
		this.children = new HashMap<>();
		this.log = LogUtil.getLogger(getClass().getPackageName());
	}
	
	/**
	 * Constructor: 
	 * @param me resourceId of this node.
	 * @param nam of the node
	 */
	public ProcessNode(ProjectResourceId me,String nam) { 
		requestHandler = ControllerRequestHandler.getInstance();
		this.resourceId = me;
		this.name = nam;
		this.auxiliaryData = new GeneralPurposeDataContainer();
		this.children = new HashMap<>();
		this.log = LogUtil.getLogger(getClass().getPackageName());
	}

	public void addChild(ProcessNode child)    { 
		children.put(child.getResourceId().getResourcePath(),child);
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
	public Collection<ProcessNode> getChildren() { return children.values(); }
	public String getName() {return name;}
	public ProjectResourceId getResourceId() { return this.resourceId; }
	public String getProjectName() {return resourceId.getProjectName();}
	
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
	public String getPath() {return resourceId.getFolderPath();}

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
		descriptor.setPath(getPath());
		descriptor.setProjectName(resourceId.getProjectName());
		descriptor.setIsFolder(true);
		return descriptor;
	}
	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public void setAuxiliaryData(GeneralPurposeDataContainer auxiliaryData) {this.auxiliaryData = auxiliaryData;}
}
