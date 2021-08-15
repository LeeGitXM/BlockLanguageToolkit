/**
 *   (c) 2014-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;

import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;

/**
 * A project node is a construction solely for the use of the status panel
 * browser. It is a way to create a full tree from the root node.
 * A project node is identified by the project name. Its parent is the root node.
 */
public class ProjectNode extends ProcessNode {
	private static final long serialVersionUID = 6280701183405134254L;
	private final RootNode root;
	
	/**
	 * Constructor: 
	 * @param rootNode the root node
	 * @param me UUID of this node 
	 * @param projName the project
	 */
	public ProjectNode(RootNode rootNode, ProjectResourceId me) { 
		super(me.getProjectName(),rootNode.getResourceId().getResourcePath(),me);
		this.root = rootNode;
	}

	public void addChild(ProjectNode child)    { 
		throw new UnsupportedOperationException();
	}

	public Collection<ProcessNode> getChildren() { 
		return root.allNodesForProject(this.resourceId.getProjectName()); 
	}


	public void setProjectName(String name) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Normally the tree path does not include the project, so is not appropriate here.
	 * @return
	 */
	@Override
	public String getTreePath(Map<ResourcePath,ProcessNode> nodesByPath) {
		throw new UnsupportedOperationException();
	} 
	@Override
	public void removeChild(ProcessNode child) { 
		throw new UnsupportedOperationException();
	} 
}
