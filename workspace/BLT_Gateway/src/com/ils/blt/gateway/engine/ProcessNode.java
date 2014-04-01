/**
 *   (c) 2012-2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * A process node is a folder of type: application, family, folder, or diagram.
 * We require the hierarchy in the gateway for purposes of routing conclusions
 * to the proper destination.
 */
public class ProcessNode {
	protected final LoggerEx log;
	private final UUID self;
	private final UUID parent;
	private final Map<String,ProcessNode> children;
	private String name;
	
	/**
	 * Constructor: 
	 * @param name of the node
	 * @param parent UUID of the parent of this node.
	 * @param me UUID of this node 
	 */
	public ProcessNode(String nam, UUID parent, UUID me) { 
		this.self = me;
		this.parent = parent;
		this.name = nam;
		this.children = new HashMap<String,ProcessNode>();
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	public void addChild(ProcessNode child)    { children.put(child.getName(),child); }
	public void removeChild(ProcessNode child) { children.remove(child.getName()); }
	public Collection<ProcessNode> getChildren() { return children.values(); }
	public ProcessNode getChildForName(String name) { return children.get(name); }
	public String getName() {return name;}
	public void setName(String nam) { this.name = nam; }

	/**
	 * @return the UUID of this node
	 */
	public UUID getSelf() { return this.self; }
	/**
	 * @return the UUID of the parent of this node.
	 *         The parent of the root node is null.
	 */
	public UUID getParent() { return this.parent; }	
	
	/**
	 * Traverse the parentage prepending names to produce a tree path.
	 * The full path does NOT include the project name.
	 * nodesByUUID we need help coverting our UUIDs to objects
	 * @return
	 */
	public String getTreePath(Map<UUID,ProcessNode> nodesByUUID) {
		StringBuffer buf = new StringBuffer(name);
		ProcessNode parentNode = nodesByUUID.get(getParent());
		while(parentNode!=null) {
			buf.insert(0,":");
			buf.insert(0,parentNode.getName());
			if( parentNode.getParent()==null) break;
			parentNode = nodesByUUID.get(parentNode.getParent());
		}
		buf.insert(0,":");  // Leading colon
		return buf.toString();
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
	@Override
	public int hashCode() {
		return this.getSelf().hashCode();
	}
	
}
