/**
 *   (c) 2012-2013  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.BLTProperties;
import com.inductiveautomation.ignition.common.project.ProjectVersion;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * This is the node at the top of the hierarchy. There is only one of these.
 * Its children are logically the applications. The node also keeps track of 
 * the projects and adds project name to the tree path when called for.
 * 
 * Keep track of children by project.
 */
public class RootNode extends ProcessNode {
	private static String TAG = "RootNode";
	private final GatewayContext context;   // Use to get project name
	// The child key is resourceId (which is immutable)
	private final Map <Long,Map<Long,ProcessNode>>childrenByProjectId;
	private final Map<String,Long> projectIdByName;

	
	/**
	 * Constructor: 
	 * @param ctx Gateway context 
	 */
	public RootNode(GatewayContext ctx) { 
		super("root",null,BLTProperties.ROOT_FOLDER_UUID);
		this.context = ctx;
		this.projectIdByName = new HashMap<String,Long>();
		this.childrenByProjectId = new HashMap<Long,Map<Long,ProcessNode>>();
	}
	
	public void addChild(ProcessNode child,long childProjectId) {
		log.infof("%s.addChild: %s[%s]",TAG,getName(),child.getName());
		Long key = new Long(childProjectId);
		String projectName = context.getProjectManager().getProjectName(childProjectId, ProjectVersion.Published);
		if( projectName==null ) {
			log.warnf("%s.addChild: No name for projectId %d. No child added.",TAG,childProjectId);
			return;
		}
		if( projectIdByName.get(projectName) == null ) {
			projectIdByName.put(projectName,key);
		}
		
		Map<Long,ProcessNode>map = childrenByProjectId.get(key);
		if( map==null ) {
			map = new HashMap<Long,ProcessNode>();
			childrenByProjectId.put(key, map);
		}
		map.put(new Long(child.getResourceId()),child);
	}
	
	/**
	 * This method should not be called ..
	 */
	@Override
	public void addChild(ProcessNode child) {
		log.errorf("%s.addChild: ERROR use addChild(child,projectId) for a RootNode",TAG);
	}
	
	
	
	public Collection<Long> allProjects() {
		return projectIdByName.values();
	}
	/**
	 * Create a flat list of nodes of all sorts known to belong to the project.
	 * @param queryProjectId
	 * @return the list of application, family, folder and diagram nodes in the project
	 */
	public List<ProcessNode> allNodesForProject(Long queryProjectId) {
		List<ProcessNode> nodes = new ArrayList<ProcessNode>();
		Map<Long,ProcessNode> map = childrenByProjectId.get(queryProjectId);
		if( map!=null) {
			Collection<ProcessNode> children = map.values();
			if( children!=null) {
				for(ProcessNode child:children) {
					addNodeToList(child,nodes);
				}
			}
		}
		else {
			log.warnf("%s.allNodesForProject: No nodes found for project %d", TAG,queryProjectId.longValue());
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
	public void removeChildFromProjectRoot(Long childProjectId,ProcessNode node) {
		Map<Long,ProcessNode> map = childrenByProjectId.get(childProjectId);
		if( map!=null) map.remove(new Long(node.getResourceId()));	
	}
	
	/**
	 * Remove all traces of a project. 
	 * NOTE: The project name to Id mapping remains.
	 */
	public void removeProject(Long projectToRemove) {
		childrenByProjectId.remove(projectToRemove);	
	}
}
