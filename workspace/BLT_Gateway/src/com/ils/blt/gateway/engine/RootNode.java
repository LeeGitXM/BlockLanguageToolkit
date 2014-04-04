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
	private final Map <Long,Map<String,ProcessNode>>childrenByProjectId;
	private final Map<String,Long> projectIdByName;

	
	/**
	 * Constructor: 
	 * @param ctx Gateway context 
	 */
	public RootNode(GatewayContext ctx) { 
		super("root",null,BLTProperties.ROOT_FOLDER_UUID);
		this.context = ctx;
		this.projectIdByName = new HashMap<String,Long>();
		this.childrenByProjectId = new HashMap<Long,Map<String,ProcessNode>>();
	}
	
	public void addChild(ProcessNode child,long projectId) {
		log.infof("%s.addChild: %s[%s]",TAG,getName(),child.getName());
		Long key = new Long(projectId);
		String projectName = context.getProjectManager().getProjectName(projectId, ProjectVersion.Published);
		if( projectName==null ) {
			log.warnf("%s.addChild: No name for projectId %d. No child added.",TAG,projectId);
			return;
		}
		if( projectIdByName.get(projectName) == null ) {
			projectIdByName.put(projectName,key);
			childrenByProjectId.put(key, new HashMap<String,ProcessNode>());
		}
		
		Map<String,ProcessNode>map = childrenByProjectId.get(key);
		map.put(child.getName(),child);
	}
	
	/**
	 * This method should not be called ..
	 */
	@Override
	public void addChild(ProcessNode child) {
		log.errorf("%s.addChild: ERROR use addChild(child,projectId) for a RootNode",TAG);
	}
	
	/**
	 * The segment delimiters in a tree path are ":".
	 * @param projectName
	 * @param treePath
	 * @return the node that corresponds to the specified tree path
	 */
	public ProcessNode findNode(String projectName,String treePath) {
		ProcessNode result = null;
		Long projectId = projectIdByName.get(projectName);
		if( projectId!=null  ) {
			// Ignore any leading colon
			if( treePath.startsWith(":") ) treePath = treePath.substring(1);
			// The root map is slightly different than the rest.
			// We do the first segment, then recurse
			Map<String,ProcessNode> map = childrenByProjectId.get(projectId);
			if( map!=null)  {
				ProcessNode child = null;
				ProcessNode parent = null;
				String path = null;
				int index = treePath.indexOf(":");
				if( index>0 ) {
					path = treePath.substring(0,index);
					if( treePath.length()>index+1 ) treePath = treePath.substring(index+1);  // Skip the ":"
					else treePath = "";
					child = map.get(path);
				}
				else {  // No colon signifies the last segment
					path = treePath;
					treePath = "";
					child = map.get(path);
				}

				while(child!=null && treePath.length()>0) {
					index = treePath.indexOf(":");
					if( index>0 ) {
						path = treePath.substring(0,index);
						if( treePath.length()>index+1 ) treePath = treePath.substring(index+1);  // Skip the ":"
						else treePath = "";
						child = child.getChildForName(path);
					}
					else {  // No colon signifies the last segment
						path = treePath;
						treePath = "";
						child = child.getChildForName(path);
					}
				}
				result = child;
			}
			else {
				log.warnf("%s.findNode: No nodes have been saved for project %s", TAG,projectName);
			}
		}
		else {
			log.warnf("%s.findNode: No nodes found for project %s", TAG,projectName);
		}
		
		return result;
	}
	/**
	 * Create a flat list of nodes of all sorts known to belong to the project.
	 * @param projectId
	 * @return the list of application, family, folder and diagram nodes in the project
	 */
	public List<ProcessNode> allNodesForProject(Long projectId) {
		List<ProcessNode> nodes = new ArrayList<ProcessNode>();
		Map<String,ProcessNode> map = childrenByProjectId.get(projectId);
		if( map!=null) {
			Collection<ProcessNode> children = map.values();
			if( children!=null) {
				for(ProcessNode child:children) {
					addNodeToList(child,nodes);
				}
			}
		}
		else {
			log.warnf("%s.allNodesForProject: No nodes found for project %d", TAG,projectId.longValue());
		}
		return nodes;
	}
	
	private void addNodeToList(ProcessNode root,List<ProcessNode>list) {
		for( ProcessNode child:root.getChildren() ) {
			addNodeToList(child,list);
		}
		list.add(root);
	}
	
	public void removeProject(Long projectId) {
		childrenByProjectId.remove(projectId);
	}

}
