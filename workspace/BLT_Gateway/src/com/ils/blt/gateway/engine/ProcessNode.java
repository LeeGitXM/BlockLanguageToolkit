/**
 *   (c) 2014  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.common.GeneralPurposeDataContainer;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * A process node is a folder of type: application, family, folder, or diagram.
 * We require the hierarchy in the gateway for purposes of routing conclusions
 * to the proper destination.
 */
public class ProcessNode implements Serializable {
	private static final long serialVersionUID = 6280701183405134254L;
	private final static String PATH_SEPARATOR = ":";
	private final Map<Long,ProcessNode> children;   // Key by resourceId
	protected final LoggerEx log;
	private String name;
	protected UUID parent;
	protected long projectId = -1;
	protected long resourceId = -1;   // Resource set when serialized.
	protected final UUID self;
	private final String TAG = "ProcessNode";
	private GeneralPurposeDataContainer auxiliaryData;
	
	/**
	 * Constructor: 
	 * @param nam of the node
	 * @param parent UUID of the parent of this node.
	 * @param me UUID of this node 
	 */
	public ProcessNode(String nam, UUID parent, UUID me) { 
		this.self = me;
		this.parent = parent;
		this.name = nam;
		this.auxiliaryData = null;
		this.children = new HashMap<Long,ProcessNode>();
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	public void addChild(ProcessNode child)    { 
		children.put(new Long(child.getResourceId()),child);
		log.debugf("%s.addChild: %s[%s]",TAG,getName(),child.getName());
	}

	// So that class is comparable
	// Same self (UUID) is sufficient to prove equality
	@Override
	public boolean equals(Object arg) {
		boolean result = false;
		if( arg instanceof ProcessNode) {
			ProcessNode that = (ProcessNode)arg;
			if( this.getSelf().equals(that.getSelf()) ) {
				result = true;
			}
		}
		return result;
	}

	public ProcessNode getChildForName(String nam) { return children.get(nam); }
	public Collection<ProcessNode> getChildren() { return children.values(); }
	public String getName() {return name;}
	/**
	 * @return the UUID of the parent of this node.
	 *         The parent of the root node is null.
	 */
	public UUID getParent() { return this.parent; }
	public long getResourceId() { return this.resourceId; }
	public long getProjectId() {return projectId;}
	public void setParent(UUID p) { this.parent = p; }
	public void setProjectId(long projectId) {this.projectId = projectId;}
	
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
	 * @return the UUID of this node
	 */
	public UUID getSelf() { return this.self; }

	/**
	 * Traverse the parentage prepending names to produce a tree path.
	 * The full path does NOT include the project name.
	 * Use nodesByUUID to help converting our UUIDs to objects
	 * @return
	 */
	public String getTreePath(Map<UUID,ProcessNode> nodesByUUID) {
		StringBuffer buf = new StringBuffer(name);
		ProcessNode parentNode = nodesByUUID.get(getParent());
		while(parentNode!=null) {
			buf.insert(0,PATH_SEPARATOR);
			buf.insert(0,parentNode.getName());
			if( parentNode.getParent()==null) break;
			parentNode = nodesByUUID.get(parentNode.getParent());
		}
		buf.insert(0,PATH_SEPARATOR);  // Leading colon
		return buf.toString();
	}
	@Override
	public int hashCode() {
		return this.getSelf().hashCode();
	}	
	public void removeChild(ProcessNode child) { children.remove(new Long(child.getResourceId()));} 
	public void setName(String nam) { this.name = nam; }
	public void setResourceId(long resourceId) {this.resourceId = resourceId;}
	public SerializableResourceDescriptor toResourceDescriptor() {
		SerializableResourceDescriptor descriptor = new SerializableResourceDescriptor();
		descriptor.setName(getName());
		descriptor.setId(self.toString());
		descriptor.setProjectId(projectId);
		descriptor.setResourceId(resourceId);
		descriptor.setType(BLTProperties.FOLDER_RESOURCE_TYPE);
		return descriptor;
	}
	public GeneralPurposeDataContainer getAuxiliaryData() {return auxiliaryData;}
	public void setAuxiliaryData(GeneralPurposeDataContainer auxiliaryData) {this.auxiliaryData = auxiliaryData;}
}
