/**
 *   (c) 2012-2021  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * This is the node at the top of the hierarchy. There is only one of these.
 * Its children are logically the applications. The node also keeps track of 
 * the projects and adds project name to the tree path when called for.
 * 
 * Keep track of children by project.
 */
public class RootNode extends ProcessNode {
	private static final long serialVersionUID = 4266822873285521574L;
	private static String CLSS = "RootNode";
	protected final GatewayContext context;   // Use to get project name
	// The child key is resourceId (which is immutable)
	private final Map <String,Map<ProjectResourceId,ProcessNode>>childrenByProjectName;

	/**
	 * Constructor: 
	 * @param ctx Gateway context 
	 */
	public RootNode(GatewayContext ctx,ProjectResourceId id) { 
		super(BLTProperties.ROOT_FOLDER_NAME,ResourcePath.createModuleRoot(BLTProperties.MODULE_ID),id);
		this.context = ctx;
		this.childrenByProjectName = new HashMap<>();
	}
	
	public void addChild(ProcessNode child,String childProjectName) {
		log.debugf("%s.addChild: %s[%s]",CLSS,getName(),child.getName());
		String key = childProjectName;
		
		
		Map<ProjectResourceId,ProcessNode>map = childrenByProjectName.get(key);
		if( map==null ) {
			map = new HashMap<>();
			childrenByProjectName.put(key, map);
		}
		map.put(child.getResourceId(),child);
	}
	
	/**
	 * This method should not be called ..
	 */
	@Override
	public void addChild(ProcessNode child) {
		log.errorf("%s.addChild: ERROR use addChild(child,projectId) for a RootNode",CLSS);
	}
	
	
	
	public Collection<String> allProjects() {
		return childrenByProjectName.keySet();
	}
	/**
	 * Create a flat list of nodes of all sorts known to belong to the project.
	 * @param queryProjectId
	 * @return the list of application, family, folder and diagram nodes in the project
	 */
	public List<ProcessNode> allNodesForProject(String queryName) {
		List<ProcessNode> nodes = new ArrayList<ProcessNode>();
		Map<ProjectResourceId,ProcessNode> map = childrenByProjectName.get(queryName);
		if( map!=null) {
			Collection<ProcessNode> children = map.values();
			if( children!=null) {
				for(ProcessNode child:children) {
					addNodeToList(child,nodes);
				}
			}
		}
		else {
			log.debugf("%s.allNodesForProject: No nodes found for project %d", CLSS,queryName);
		}
		return nodes;
	}
	
	private void addNodeToList(ProcessNode root,List<ProcessNode>list) {
		for( ProcessNode child:root.getChildren() ) {
			addNodeToList(child,list);
		}
		list.add(root);
	}
	/**
	 * Remove the children of a project. 
	 */
	public void removeChildFromProjectRoot(String childProjectName,ProcessNode node) {
		Map<ProjectResourceId,ProcessNode> map = childrenByProjectName.get(childProjectName);
		if( map!=null) map.remove(node.getResourceId());	
	}
	
	/**
	 * Remove all traces of a project. 
	 * NOTE: The project name to Id mapping remains.
	 */
	public void removeProject(String projectToRemove) {
		childrenByProjectName.remove(projectToRemove);	
	}
}
