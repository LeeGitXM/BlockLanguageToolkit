/**
 *   (c) 2012-2022  ILS Automation. All rights reserved. 
 */
package com.ils.blt.gateway.engine;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.script.ScriptNotificationManager;
import com.ils.blt.gateway.ControllerRequestHandler;
import com.ils.common.JavaToPython;
import com.ils.common.PythonToJava;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.gateway.model.GatewayContext;

/**
 * This is the node at the top of the hierarchy. There is only one of these, a singleton.
 * Its children are ProjectNodes. The getter for a project node will create a new node if it doesn't exist.
 * 
 * Keep track of children by project.
 */
public class RootNode extends ProcessNode {
	private static final long serialVersionUID = 4266822873285521574L;
	private static String CLSS = "RootNode";
	private static RootNode instance = null;
	private Map<String,ProjectNode> projects;
	private final static boolean DEBUG = false;
	private final LoggerEx log;
	
	/**
	 * The handler, make this private per Singleton pattern. 
	 */
	private RootNode() {
		super(null, BLTProperties.ROOT_FOLDER_NAME);
		projects = new HashMap<>();
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
	}

	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static RootNode getInstance() {
		if (instance == null) {
			synchronized (ScriptNotificationManager.class) {
				instance = new RootNode();
			}
		}
		return instance;
	}

	
	/**
	 * When we add a child to the root, we actually add it to the appropriate
	 * child ProjectNode.
	 */
	public void addChild(ProcessNode child) {
		String project = child.getProjectName();
		ProjectNode projNode = (ProjectNode)projects.get(project);
		if( projNode==null) {
			projNode = new ProjectNode(project);
			projects.put(project, projNode);
		}
		projNode.addChild(child);
	}
	
	public Collection<String> allProjects() {
		return projects.keySet();
	}
	/**
	 * Create a flat list of nodes of all sorts known to belong to the project.
	 * @param project
	 * @return the list of folder and diagram nodes in the project
	 */
	public List<ProcessNode> allNodesForProject(String project) {
		if( DEBUG ) log.infof("In %s.allNodesForProject() for project %s", CLSS, project);
		List<ProcessNode> nodes = new ArrayList<ProcessNode>();
		ProjectNode projNode = (ProjectNode)projects.get(project);
		if( projNode!=null) {
			nodes = projNode.allNodes();
		}
		return nodes;
	}
	/**
	 * The child is not really a child of the root node,
	 * but rather the child of the appropriate project node. 
	 */
	public void removeChild(ProcessNode child) {
		String project = child.getProjectName();
		ProjectNode projNode = (ProjectNode)projects.get(project);
		if( projNode==null) {
			projNode.removeChild(child);
		}
	}
	
	/**
	 * Remove all traces of a project in the ProcessNode tree. 
	 */
	public void removeProject(String projectToRemove) {
		projects.remove(projectToRemove);	
	}
}
