/**
 *   (c) 2014-2022  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.List;

/**
 * A project node is a child of the root node. Its children are ProcessNodes representing
 * project resources - diagrams and folders.
 * A project node is identified by the project name. Its parent is the root node.
 */
public class ProjectNode extends ProcessNode {
	private static final long serialVersionUID = 6280701183405134254L;

	/**
	 * Constructor: Set the name of the node to the project.
	 * @param projName the project
	 */
	public ProjectNode(String name) { 
		super(null,name);
	}
	/**
	 * Create a flat list of nodes of all sorts known to belong to the project.
	 * The list does not include the ProjectNode
	 * @param project
	 * @return the list of folder and diagram nodes in the project
	 */
	public List<ProcessNode> allNodes() {
		List<ProcessNode> nodes = new ArrayList<ProcessNode>();
		addChildrenToList(nodes,this);
		return nodes;
	}
	
	/**
	 * Recursively add children in the tree to the list.
	 * @param nodes
	 * @param node
	 */
	private void addChildrenToList(List<ProcessNode> nodes,ProcessNode parent) {
		nodes.add(parent);
		for(ProcessNode node:parent.children.values()) {
			addChildrenToList(nodes,node);
		}
	}
}
