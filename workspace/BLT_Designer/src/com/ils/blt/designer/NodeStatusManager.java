/**
 *   (c) 2013-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.designer.navtree.NavTreeFolder;
import com.ils.blt.designer.workspace.ProcessDiagramView;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.project.resource.ResourceType;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 *  This class retains a list of all nav tree nodes associated with the
 *  toolkit. This allows us to re-use the nodes in the nav tree. We retain
 *  a map of these nodes indexed by resource number.
 *  It is a central reporting point for the current status of each node.
 *  This class is implemented as a singleton.
 *  
 *  The diagrams themselves track saved state. "dirtiness" refers to
 *  inconsistencies between a diagram in the designer and in the gateway. 
 *  When we set the state, it is immediately pushed to the gateway, so it does
 *  not cause the diagram to become dirty, just "unsaved". We do not change the state
 *  on disk until a project save.
 *  
 *  Note that a name change does not make a node dirty, just "unsaved".
 *  
 *  "Alerting" is completely independent of dirtiness. It refers to
 *  the state something inside a diagram. A node is alerting if any of its
 *  children are alerting.
 *  
 *  The resourceId is known to both the view code and the nav tree.
 */
public class NodeStatusManager   {
	private static String CLSS = "NodeStatusManager";
	private static final boolean DEBUG = true;
	private static NodeStatusManager instance = null;
	private final LoggerEx log;
	private final ApplicationRequestHandler handler;
	private final Map<String,StatusEntry> statusByPath;
	

	/**
	 * The manager, make this private per Singleton pattern ...
	 */
	private NodeStatusManager() {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.handler = new ApplicationRequestHandler();
		statusByPath = new HashMap<>();
	}
	
	/**
	 * Static method to create and/or fetch the single instance.
	 */
	public static NodeStatusManager getInstance() {
		if( instance==null) {
			synchronized(NodeStatusManager.class) {
				instance = new NodeStatusManager();
			}
		}
		return instance;
	}

	/**	
	 * Synchronize StatusEntries to current state of the resource. The resource name
	 * is set to the pending name and the resource removed from the "unsaved" list.
     */
	public void clearChangeMarkers(ProjectResourceId resourceId) {
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			se.clearChanges();
			if(DEBUG) log.infof("%s.commit: %s -----------------------------------------",CLSS,resourceId.getFolderPath());
		}
	}
	
	/**
	 * Define status for the root of the resource tree. 
	 * WARNING: The root node has no associated project resources.
	 * @param node of resource tree
	 */
	public void createRootResourceStatus(NavTreeFolder rootNode) {
		log.tracef("%s.newRootResource",CLSS);
		ProjectResourceId resourceId = rootNode.getResourceId();
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se == null ) {
			se = new StatusEntry(rootNode);  
			statusByPath.put(resourceId.getFolderPath(),se);
		}
	}
	
	/**
	 * @return the count of nodes with changes
	 */
	public int getModificationCount() {
		int count = 0;
		for(StatusEntry se:statusByPath.values()) {
			if(se.isModified()) {
				count++;
			}
		}
		return count;
	}
	/**
	 * Define status for a new nav tree node. Initially we get the node state from the gateway.
	 * @param node
	 * @param resourceId
	 */
	public StatusEntry createResourceStatus(AbstractResourceNavTreeNode node,ProjectResourceId resourceId) {
		if(node.getProjectResource()==null) throw new IllegalArgumentException("No project resource");
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se == null ) {
			se = new StatusEntry(node);
			se.setAlerting(handler.isAlerting(resourceId));
			statusByPath.put(resourceId.getFolderPath(),se);
		}
		log.debugf("%s.createResourceStatus: %s (%s)",CLSS,node.getName(),resourceId.getFolderPath());
		return se;
	}
	/**
	 * Delete a status entry and all its children. Prepare the real associated nodes for deletion as well.
	 * @param resourceId
	 */
	public void removeResource(ProjectResourceId resourceId ) {
		log.debugf("%s.removeResource(%s)",CLSS,resourceId.getResourcePath().getPath().toString());
		List<String> pathsToDelete = nodeDescendants(resourceId);
		for(String rp:pathsToDelete) {
			StatusEntry se = statusByPath.get(rp);
			statusByPath.remove(rp);
		}
	}
	/**
	 * Determine whether or not to display an alert badge. The node 
	 * is alerting if any of its children are alerting.
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public boolean getAlertState(ProjectResourceId resourceId) {
		boolean result = searchForAlert(resourceId.getResourcePath());
		return result;
	}
	
	/**
	 * Get the node. Null if not found. This is called in onClose() for a diagram workspace.
	 * @param resourceId
	 * @return the AbstractResourceNavTreeNode associated with the specified resourceId.
	 */
	public AbstractResourceNavTreeNode getNode(ProjectResourceId resourceId) {
		log.debugf("%s.findNode(%s)",CLSS,resourceId.getResourcePath().getPath().toString());
		AbstractResourceNavTreeNode node = null;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) node=se.getNode();
		return node;
	}
	
	/**
	 * Return a list of resource paths for descendants of the node representing the specified resourceId.
	 * The original node is contained in the list. 
	 * @param root
	 * @return
	 */
	private List<String> nodeDescendants(ProjectResourceId resourceId) {
		List<String> children = new ArrayList<>();
		String rootPath = resourceId.getFolderPath();
		children.add(resourceId.getFolderPath());
		for(String path: statusByPath.keySet()) {
			if( path.startsWith(rootPath)) children.add(path);
		}
		return children;
	}
	
	// If the current node or any of its children are alerting, then return true.
	private boolean searchForAlert(ResourcePath path) {
		boolean isAlerting = false;
		if( path!=null ) {
			String rootPath = path.getFolderPath();
			for(String rp: statusByPath.keySet()) {
				if( path.getFolderPath().startsWith(rootPath)) {
					StatusEntry se = statusByPath.get(rp);
					if( se.alerting ) {
						isAlerting = true;
						break;
					}
				}
			}
		}
		return isAlerting;
	}
	/**	
	 * When it is time to save the resource, get the intended name.
	 * A null implies no change.
     */
	public String getPendingName(ProjectResourceId resourceId) {
		String pendingName = null;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			pendingName = se.getPendingName();
			if(DEBUG) log.infof("%s.getPendingName: %s (%s)",CLSS,resourceId.getFolderPath(),(pendingName==null?"null":pendingName));
		}
		return pendingName;
	}
	/**	
	 * An edit has changed the node name in the nav tree. Record the new name for when the node is saved.
	 * This requires us to replicate the status entry under the new name. 
     */
	public void setPendingName(ProjectResourceId resourceId,String newName) {
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			se.setPendingName(newName);
			if(DEBUG) log.infof("%s.setPendingName: %s (%s)",CLSS,resourceId.getFolderPath(),(newName==null?"null":newName));
			String path = resourceId.getFolderPath();
			int index = path.lastIndexOf("/");
			path = path.substring(index+1);
			path = path + newName;
			ProjectResourceId rid = new ProjectResourceId(resourceId.getProjectName(),resourceId.getResourceType(),path); 
			StatusEntry clone = createResourceStatus(se.getNode(),rid);
			clone.setPendingName(newName);
		}
	}
	/**	
	 * Get the changed state, if any
     */
	public DiagramState getPendingState(ProjectResourceId resourceId) {
		DiagramState pendingState = null;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			pendingState = se.getPendingState();
			if(DEBUG) log.infof("%s.getPendingState: %s (%s)",CLSS,resourceId.getFolderPath(),(pendingState==null?"null":pendingState.name()));
		}
		return pendingState;
	}
	/**	
	 * An edit has changed the node name in the nav tree. The node is unsaved if the name
	 * differs from the resource name.
     */
	public void setPendingState(ProjectResourceId resourceId,DiagramState newState) {
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			se.setPendingState(newState);
			if(DEBUG) log.infof("%s.setPendingState: %s (%s)",CLSS,resourceId.getFolderPath(),(newState==null?"null":newState.name()));
		}
	}
		
	/**	
	 * When it is time to save the resource, get the intended view
     */
	public ProcessDiagramView getPendingView(ProjectResourceId resourceId) {
		ProcessDiagramView pendingView = null;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			pendingView = se.getPendingView();
			if(DEBUG) log.infof("%s.getPendingView: %s (%s)",CLSS,resourceId.getFolderPath(),(pendingView==null?"null":pendingView.getName()));
		}
		return pendingView;
	}
	/**	
	 * An edit has changed the node name in the nav tree. The node is unsaved if the name
	 * differs from the resource name.
     */
	public void setPendingView(ProjectResourceId resourceId,ProcessDiagramView newView) {
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			se.setPendingView(newView);
			if(DEBUG) log.infof("%s.setPendingView: %s (%s)",CLSS,resourceId.getFolderPath(),(newView==null?"null":newView.getName()));
		}
	}

	
	/**
	 * @return the dirty state of the indicated node. The node is dirty if it has
	 *     a pendingView, a pendingState or pendingName. If not found, we assume it is
	 *     new and, therefore, modified..
	 */
	public boolean isModified(ProjectResourceId resourceId) {
		boolean modified = true;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			if( se.getPendingName()==null && se.getPendingState()==null && se.getPendingView()==null) {
				modified = false;
			}
		}
		log.infof("%s.isModified: %s (%s)",CLSS,resourceId.getFolderPath(),(modified?"modified":"clean"));
		return modified;
	}

	/**
	 * Hold status information for a node in the nav tree.
	 * The pending is simply the last name change made on the node.
	 * In most cases this is simply the current name
	 */
	private class StatusEntry {
		private boolean alerting = false;
		private String pendingName;
		private DiagramState pendingState;
		private ProcessDiagramView pendingView;
		private final AbstractResourceNavTreeNode node;
		/**
		 * Constructor:
		 * @param antn
		 * @param s
		 */
		public StatusEntry(AbstractResourceNavTreeNode antn)  {
			this.node = antn;
			// Set pending vaules to null, meaning that there are no
			// changes yet
			this.pendingName = null;
			this.pendingState = null;
			this.pendingView = null;
		}
		public String getPendingName() { return this.pendingName; }
		public void setPendingName(String name) { this.pendingName = name; }
		public DiagramState getPendingState() { return this.pendingState; }
		public void setPendingState(DiagramState state) { this.pendingState = state; }
		public ProcessDiagramView getPendingView() { return this.pendingView; }
		public void setPendingView(ProcessDiagramView view) { this.pendingView = view; }
		public boolean isAlerting() { return alerting; }
		public void setAlerting(boolean flag) { alerting = flag; }
		public AbstractResourceNavTreeNode getNode() { return node; }
		
		public void clearChanges() {
			this.pendingName = null;
			this.pendingState = null;
			this.pendingView = null;
		}

		public boolean isModified() {
			boolean result = false;
			if( pendingName!=null || pendingState!=null || pendingView!=null ) {
				result = true;
			}
			return result;
		}

		@Override
		public String toString() {
			ProjectResourceId resourceId = node.getResourceId();
			String dump = String.format("%s(%s)",pendingName,resourceId.getFolderPath());
			return dump;
		}

		// Determine equality based soley on the node.
		@Override
		public int hashCode() { 
			return node.hashCode()+42;
		}

		//Compare on node only
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			else if (obj == null)
				return false;
			else if (getClass() != obj.getClass())
				return false;
			
			StatusEntry other = (StatusEntry) obj;
			if(this.node != other.getNode())
				return false;
			return true;
		}
	}
	
}
