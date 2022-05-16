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
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.navtree.NavTreeFolder;
import com.ils.blt.designer.navtree.NavTreeNodeInterface;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
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
 *  The nodes themselves track saved state. "dirtiness" refers to
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
public class NodeStatusManager implements NotificationChangeListener   {
	private static String CLSS = "NodeStatusManager";
	private static NodeStatusManager instance = null;
	private final LoggerEx log;
	private final ApplicationRequestHandler handler;
	private final NotificationHandler notificationHandler;
	private final List<String> unsavedNodes;    // String version of resource path
	private final Map<String,StatusEntry> statusByPath;
	

	/**
	 * The manager, make this private per Singleton pattern ...
	 */
	private NodeStatusManager() {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.handler = new ApplicationRequestHandler();
		this.notificationHandler = NotificationHandler.getInstance();
		statusByPath = new HashMap<>();
		unsavedNodes = new ArrayList<>();
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
	 * "unsavedNodes" are those nodes where the gateway version
	 * does not match the version saved on disk.
	 * These should be rendered as "italic"
	 */
	public void addToUnsavedList(ResourcePath path) {
		unsavedNodes.add(path.getFolderPath());
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
			se = new StatusEntry(rootNode,DiagramState.ACTIVE);  
			statusByPath.put(resourceId.getFolderPath(),se);
		}
	}
	/**
	 * Define status for a new nav tree node. Initially we get the node state from the gateway
	 * @param node
	 * @param resourceId
	 */
	public void createResourceStatus(AbstractResourceNavTreeNode node,ProjectResourceId resourceId) {
		if(node.getProjectResource()==null) throw new IllegalArgumentException("No project resource");
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se == null ) {
			DiagramState s = handler.getDiagramState(resourceId);
			se = new StatusEntry(node,s);
			se.setAlerting(handler.isAlerting(resourceId));
			notificationHandler.addNotificationChangeListener(NotificationKey.keyForAlert(resourceId), CLSS, this);
			statusByPath.put(resourceId.getFolderPath(),se);
		}
		log.debugf("%s.createResourceStatus: %s (%s) %s",CLSS,node.getName(),resourceId.getFolderPath(),
				                                           (se.getPendingState()==null?"":se.getPendingState().name()));
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
			se.prepareToBeDeleted();
			statusByPath.remove(rp);
			unsavedNodes.remove(rp);
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
	 * If the diagram is dirty, the state is the designer-scope unsaved state
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public DiagramState getPendingState(ProjectResourceId resourceId) {
		DiagramState result = DiagramState.UNSET;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			result = se.getPendingState();
		}
		log.tracef("%s.getPendingState: %s(%s) = %s",CLSS,(se==null?"null":resourceId.getResourcePath().getName()),resourceId.getResourcePath().getPath().toString(),result.name());
		return result;
	}
	
	/**
	 * Get the node. Null if not found. This is called in onClose() for a diagram workspace.
	 * @param resourceId
	 * @return the AbstractResourceNavTreeNode associated with the specified resourceId.
	 */
	public AbstractResourceNavTreeNode findNode(ProjectResourceId resourceId) {
		log.debugf("%s.findNode(%s)",CLSS,resourceId.getResourcePath().getPath().toString());
		AbstractResourceNavTreeNode node = null;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) node=se.getNode();
		return node;
	}
	
	public void reportDirtyState(ProjectResourceId resourceId) {
		boolean dirty = getDirtyState(resourceId);
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null && se.getNode() instanceof NavTreeNodeInterface) {
			((NavTreeNodeInterface)se.getNode()).updateUI(dirty);
		}
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
	 * When it is time to save the resource, get the intended name
     */
	public String getPendingName(ProjectResourceId resourceId) {
		String pendingName = null;
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			pendingName = se.getPendingName();
		}
		return pendingName;
	}
	/**	
	 * An edit has changed the node name in the nav tree. The node is unsaved if the name
	 * differs from the resource name.
     */
	public void setPendingName(ProjectResourceId resourceId,String newName) {
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			se.setPendingName(newName);
			if( se.getNode() instanceof NavTreeNodeInterface) {
				((NavTreeNodeInterface)se.getNode()).updateUI(se.isDirty());
			}
		}
	}
	/**	
	 * Synchronize StatusEntry to current state of resource. The resource name
	 * is set to the pending name and the state set to the state of the gateway.
     */
	public void commit(ProjectResourceId resourceId) {
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			unsavedNodes.remove(resourceId.getResourcePath().getFolderPath());
			se.commit();
			if( se.getNode() instanceof NavTreeNodeInterface) {
				((NavTreeNodeInterface)se.getNode()).updateUI(false);
			}
			
		}
	}
	
	/**
	 * @return the dirty state of the indicated node
	 */
	public boolean getDirtyState(ProjectResourceId resourceId) {
		boolean dirty = true;
		if( !unsavedNodes.contains(resourceId.getResourcePath().getFolderPath())) {
			StatusEntry se = statusByPath.get(resourceId.getFolderPath());
			if( se!=null ) {
				dirty = se.isDirty();
			}
		}
		log.infof("%s.getDirtyState: %s = %s",CLSS,resourceId.getFolderPath(),(dirty?"dirty":"clean"));
		return dirty;
	}
	/**
	 * Called after a save from the main menu. Update the status
	 * of the nav-tree nodes. All nodes have been saved.
	 */
	public void markAllSaved() {
		log.infof("%s.markAllSaved()",CLSS);
		unsavedNodes.clear();
		for(String key:statusByPath.keySet()) {
			StatusEntry se = statusByPath.get(key);
			se.commit();
			if( se.getNode() instanceof NavTreeNodeInterface) {
				((NavTreeNodeInterface)se.getNode()).updateUI(se.isDirty());
			}
		}
	}
	/**	
	 * A state change. If the state differs from the gateway, then the node is set to dirty.
     */
	public void setPendingState(ProjectResourceId resourceId,DiagramState ds) {
		StatusEntry se = statusByPath.get(resourceId.getFolderPath());
		if( se!=null ) {
			se.setPendingState(ds);
			if( se.getNode() instanceof NavTreeNodeInterface) {
				((NavTreeNodeInterface)se.getNode()).updateUI(se.isDirty());
			}
		}
	}

	/**
	 * Hold status information for a node in the nav tree.
	 */
	private class StatusEntry {
		private boolean alerting = false;
		private DiagramState pendingState;
		private String pendingName;
		private final AbstractResourceNavTreeNode node;
		/**
		 * Constructor:
		 * @param antn
		 * @param s
		 */
		public StatusEntry(AbstractResourceNavTreeNode antn,DiagramState s)  {
			this.pendingState = s;
			this.node = antn;
			//Set the pending name to the current
			this.pendingName = node.getName();
		}
		public String getPendingName() { return this.pendingName; }
		public boolean isAlerting() { return alerting; }
		public void setAlerting(boolean flag) { alerting = flag; }
		public AbstractResourceNavTreeNode getNode() { return node; }
		public void setPendingName(String name) { this.pendingName = name; }
		public void setPendingState(DiagramState st) { this.pendingState = st; } 
		public DiagramState getPendingState() { return pendingState; }

		public void prepareToBeDeleted() {
			if( node instanceof NavTreeNodeInterface && node.getName()!=BLTProperties.ROOT_FOLDER_NAME) {
				((NavTreeNodeInterface)this.node).prepareForDeletion();
			}
		}
		/**
		 * The node is dirty if there is either a name or state mismatch.
		 * For the name change, we just compare the pending change with the current.
		 * For the state comparison, check local name against the gateway version.
		 * We do not check the state of a newly created node here.
		 */
		public boolean isDirty() {
			boolean dirty = false;
			ProjectResourceId resourceId = node.getResourceId();
			if( resourceId.getResourcePath().getFolderPath().isEmpty()) {
				dirty = false;
			}
			else if( !resourceId.getResourcePath().getName().equals(pendingName)) {
				dirty = true;
			}
			else  {
				DiagramState gwstate = handler.getDiagramState(resourceId);
				if( !pendingState.equals(gwstate)) {
					dirty = true;
				}
			}
			return dirty;
		}
		// Mark node as being in-sync with the gateway. The save has occurred
		// so we read everything from the gateway.
		public void commit() {
			ProjectResourceId resourceId = node.getResourceId();
			pendingState = handler.getDiagramState(resourceId);
			pendingName = resourceId.getResourcePath().getName();
		}
		@Override
		public String toString() {
			ProjectResourceId resourceId = node.getResourceId();
			String dump = String.format("%s(%s) %s",pendingName,resourceId.getFolderPath(),pendingState.name());
			return dump;
		}
	}

// ================================ Notification Change Listener =========================================
@Override
public void diagramStateChange(String path, String state) {
	StatusEntry se = statusByPath.get(path);
	se.setAlerting(state.equalsIgnoreCase("true"));
	AbstractResourceNavTreeNode node = se.getNode();
	node.reload();
}

@Override
public void bindingChange(String pname,String binding) {}
@Override
public void nameChange(String name) {}
@Override
public void propertyChange(String pname,Object value) {}
@Override
public void valueChange(QualifiedValue value) {}
@Override
public void watermarkChange(String newWatermark) {}
	
}
