/**
 *   (c) 2014-2022  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.List;

import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;

/**
 * A project node is a child of the root node. Its children are ProcessNodes representing
 * project resources - diagrams and folders.
 * A project node is identified by the project name. Its parent is the root node.
 */
public class ProjectNode extends ProcessNode {
	private static final long serialVersionUID = 6280701183405134254L;
	private final LoggerEx log;
	private final static boolean DEBUG = false;
	private static String CLSS = "ProjectNode";

	/**
	 * Constructor: Set the name of the node to the project.
	 * @param projName the project
	 */
	public ProjectNode(String name) { 
		super(null,name);
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}
	/**
	 * Create a flat list of nodes of all sorts known to belong to the project.
	 * The list does not include the ProjectNode
	 * @param project
	 * @return the list of folder and diagram nodes in the project
	 */
	public List<ProcessNode> allNodes() {
		if( DEBUG ) log.infof("In %s.allNodes()", CLSS);
		List<ProcessNode> nodes = new ArrayList<ProcessNode>();
		addChildrenToList(nodes,this);
		return nodes;
	}
	
	/**
	 * Recursively add children in the tree to the list.
	 * @param nodes
	 * @param node
	 */
	private void addChildrenToList_BAD(List<ProcessNode> nodes, ProcessNode parent) {
		if( DEBUG ) log.infof("In %s.addChildrenToList(), adding: %s - %s", CLSS, parent.getName(), parent.getPath());
		nodes.add(parent);
		
		List<ProcessNode> nodesToDelete = new ArrayList<>();
		parent.collectDescendants(nodesToDelete);  // "head" is in the list
		for(ProcessNode node:nodesToDelete ) {
			if( DEBUG ) log.infof("...checking for children of %s - %s", node.getName(), node.getPath());
			addChildrenToList(nodes,node);
		}
	}
	
	private void addChildrenToList(List<ProcessNode> nodes, ProcessNode parent) {
		if( DEBUG ) log.infof("In %s.addChildrenToList(), adding: %s - %s", CLSS, parent.getName(), parent.getPath());
		nodes.add(parent);
		for(ProcessNode node:parent.children.values()) {
			if( DEBUG ) log.infof("...checking for children of %s - %s", node.getName(), node.getPath());
			addChildrenToList(nodes,node);
		}
	}
}
