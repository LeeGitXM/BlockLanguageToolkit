/**
 *   (c) 2013-2022  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.common.serializable.SerializableResourceDescriptor;
import com.ils.blt.designer.navtree.DiagramTreeNode;
import com.ils.blt.designer.navtree.NavTreeFolder;
import com.ils.blt.designer.navtree.NavTreeNodeInterface;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
import com.inductiveautomation.ignition.common.project.resource.ResourcePath;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractNavTreeNode;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 *  This class retains a list of all nav tree nodes associated with the
 *  toolkit. This allows us to re-use the nodes in the nav tree. We retain
 *  a map of these nodes indexed by resource number.
 *  It is a central reporting point for the current status of each node.
 *  
 *  The nodes themselves have no dirty state. "dirtiness" refers to
 *  a change in state for the node.  We keep track of the state 
 *  (DISABLED,ISOLATION,ACTIVE) of child diagrams. When we set the state,
 *  it is in designer scope only. We do not change the state in the
 *  gateway until a project save.
 *  
 *  "Alerting" is completely independent of dirtiness. It refers to
 *  the state something inside a diagram. A node is alerting if any of its
 *  children are alerting.
 *  
 *  The resourceId is known to both the view code and the nav tree.
 */
public class NodeStatusManager implements NotificationChangeListener   {
	private static String CLSS = "NodeStatusManager";
	private final LoggerEx log;
	public final DesignerContext context;
	private final ApplicationRequestHandler handler;
	private final NotificationHandler notificationHandler;
	private final Map<ResourcePath,StatusEntry> statusByResourcePath;
	

	/**
	 * The manager. There should be only one - owned by the hook instance
	 */
	public NodeStatusManager(DesignerContext ctx,ApplicationRequestHandler h) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = ctx;
		this.handler = h;
		this.notificationHandler = NotificationHandler.getInstance();
		statusByResourcePath = new HashMap<>();
	}
	

	
	/**
	 * Define status for the root of the resource tree. 
	 * WARNING: The root node has no associated project resources.
	 * @param node of resource tree
	 */
	public void createRootResourceStatus(NavTreeFolder rootNode) {
		log.tracef("%s.newRootResource",CLSS);
		ProjectResourceId resourceId = rootNode.getResourceId();
		StatusEntry se = statusByResourcePath.get(resourceId.getResourcePath());
		if( se == null ) {
			se = new StatusEntry(rootNode,DiagramState.ACTIVE);  
			statusByResourcePath.put(resourceId.getResourcePath(),se);
		}
	}
	/**
	 * Define status for a new nav tree node. Initially we get the node state from the gateway
	 * @param node
	 * @param resourceId
	 */
	public void createResourceStatus(AbstractResourceNavTreeNode node,ProjectResourceId resourceId) {
		if(node.getProjectResource()==null) throw new IllegalArgumentException("No project resource");
		StatusEntry se = statusByResourcePath.get(resourceId.getResourcePath());
		if( se == null ) {
			DiagramState s = handler.getDiagramState(resourceId);
			se = new StatusEntry(node,s);
			se.setAlerting(handler.isAlerting(resourceId));
			notificationHandler.addNotificationChangeListener(NotificationKey.keyForAlert(resourceId), CLSS, this);
			statusByResourcePath.put(resourceId.getResourcePath(),se);
		}
		log.debugf("%s.createResourceStatus: %s (%s) %s",CLSS,node.getName(),resourceId.getFolderPath(),
				                                           (se.getState()==null?"":se.getState().name()));
	}
	/**
	 * Delete a status entry and all its children. Prepare the real associated nodes for deletion as well.
	 * @param resourceId
	 */
	public void removeResource(ProjectResourceId resourceId ) {
		log.debugf("%s.removeResource(%s)",CLSS,resourceId.getResourcePath().getPath().toString());
		List<ResourcePath> pathsToDelete = nodeDescendants(resourceId);
		for(ResourcePath rp:pathsToDelete) {
			StatusEntry se = statusByResourcePath.get(rp);
			se.prepareToBeDeleted();
			statusByResourcePath.remove(se);
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
	public DiagramState getResourceState(ProjectResourceId resourceId) {
		DiagramState result = DiagramState.UNSET;
		StatusEntry se = statusByResourcePath.get(resourceId.getResourcePath());
		if( se!=null ) {
			result = se.getState();
		}
		log.tracef("%s.getResourceState: %s(%s) = %s",CLSS,(se==null?"null":se.getName()),resourceId.getResourcePath().getPath().toString(),result.name());
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
		StatusEntry se = statusByResourcePath.get(resourceId.getResourcePath());
		if( se!=null ) node=se.getNode();
		return node;
	}
	
	/**
	 * Return a list of resource paths for descendants of the node representing the specified resourceId.
	 * The original node is contained in the list. 
	 * @param root
	 * @return
	 */
	private List<ResourcePath> nodeDescendants(ProjectResourceId resourceId) {
		List<ResourcePath> children = new ArrayList<>();
		String rootPath = resourceId.getFolderPath();
		children.add(resourceId.getResourcePath());
		for(ResourcePath path: statusByResourcePath.keySet()) {
			if( path.getFolderPath().startsWith(rootPath)) children.add(path);
		}
		return children;
	}
	
	// If the current node or any of its children are alerting, then return true.
	private boolean searchForAlert(ResourcePath path) {
		boolean isAlerting = false;
		if( path!=null ) {
			String rootPath = path.getFolderPath();
			for(ResourcePath rp: statusByResourcePath.keySet()) {
				if( path.getFolderPath().startsWith(rootPath)) {
					StatusEntry se = statusByResourcePath.get(rp);
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
	 * An edit has changed the node name in the nav tree. The node is dirty if the name
	 * differs from the resource name.
     */
	public void nameChange(ProjectResourceId resourceId) {
		StatusEntry se = statusByResourcePath.get(resourceId.getResourcePath());
		if( se!=null ) {
			se.reportDirtyState();
		}
	}
	/**	
	 * Synchronize StatusEntry to current state of resource.
     */
	public void markResourceClean(ProjectResourceId resourceId) {
		StatusEntry se = statusByResourcePath.get(resourceId.getResourcePath());
		if( se!=null ) {
			se.setClean();
			se.reportDirtyState();
		}
	}
	
	/**
	 * Called after a save from the main menu. Update the status
	 * of the nav-tree nodes.
	 */
	public void markAllClean() {
		log.debugf("%s.markAllClean()",CLSS);
		for(ResourcePath key:statusByResourcePath.keySet()) {
			StatusEntry se = statusByResourcePath.get(key);
			se.setClean();
			se.reportDirtyState();
		}
	}
	/**	
	 * A state change. If the state differs from the gateway, then the node is set to dirty.
     */
	public void setResourceState(ProjectResourceId resourceId,DiagramState ds) {
		StatusEntry se = statusByResourcePath.get(resourceId.getResourcePath());
		if( se!=null ) {
			se.setState(ds);
			se.reportDirtyState();
		}
	}

	/**
	 * Hold status information for a node in the nav tree.
	 */
	private class StatusEntry {
		private boolean alerting = false;
		private DiagramState state;
		private final AbstractResourceNavTreeNode node;
		/**
		 * Constructor:
		 * @param antn
		 * @param s
		 */
		public StatusEntry(AbstractResourceNavTreeNode antn,DiagramState s)  {
			this.state = s;
			this.node = antn;
		}
		
		public boolean isAlerting() { return alerting; }
		public void setAlerting(boolean flag) { alerting = flag; }
		public String getName() { return (node==null?"":node.getName()); }
		public AbstractResourceNavTreeNode getNode() { return node; }
		public void setState(DiagramState st) { this.state = st; }
		//public AbstractNavTreeNode getParent() { return (node==null?null:node.getParent()); } 
		public DiagramState getState() { return state; }

		public void prepareToBeDeleted() {
			if( node instanceof NavTreeNodeInterface && node.getName()!=BLTProperties.ROOT_FOLDER_NAME) {
				((NavTreeNodeInterface)this.node).prepareForDeletion();
			}
		}
		/**
		 * The node is dirty if there is either a name or state mismatch.
		 */
		public void reportDirtyState() {
			boolean isDirty = false;
			ProjectResourceId resourceId = node.getResourceId();
			DiagramState gwstate = handler.getDiagramState(resourceId);
			SerializableResourceDescriptor desc = handler.getDiagram(resourceId);
			if( !node.getName().equals(desc.getName())) {
				isDirty = true;
			}
			else if(!getState().equals(gwstate) ) {
				isDirty = true;
			}
			if( node instanceof NavTreeNodeInterface) {
				((NavTreeNodeInterface)this.node).updateUI(isDirty);
			}
		}
		// Mark node as being in-sync with the gateway
		public void setClean() {
			ProjectResourceId resourceId = node.getResourceId();
			state = handler.getDiagramState(resourceId);
			SerializableResourceDescriptor desc = handler.getDiagram(resourceId);
			if( node instanceof NavTreeNodeInterface && node.getName()!=BLTProperties.ROOT_FOLDER_NAME) {
				((NavTreeFolder)this.node).setName(desc.getName());
			}
			else if( node instanceof DiagramTreeNode && node.getName()!=BLTProperties.ROOT_FOLDER_NAME) {
				((DiagramTreeNode)this.node).setName(desc.getName());
			}
		}
		@Override
		public String toString() {
			ProjectResourceId resourceId = node.getResourceId();
			String dump = String.format("%s(%s) %s",getName(),resourceId.getFolderPath(),state.name());
			return dump;
		}
	}

// ================================ Notification Change Listener =========================================
@Override
public void diagramStateChange(String path, String state) {
	StatusEntry se = statusByResourcePath.get(path);
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
