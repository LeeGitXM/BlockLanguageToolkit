/**
 *   (c) 2013-2021  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.navtree.GeneralPurposeTreeNode;
import com.ils.blt.designer.navtree.NavTreeNodeInterface;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.resource.ProjectResourceId;
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
 *  the presence of unsaved diagrams and is managed by the project.
 *  
 *  We do keep track of the state (DISABLED,ISOLATION,ACTIVE). This
 *  refers to the state of child diagrams. When we set the state,
 *  it is set only for the current node, independent of child state.
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
	private final String projectName;
	private final Map<ProjectResourceId,Set<ProjectResourceId>>  childrenByResourceId;
	private final Map<ProjectResourceId,StatusEntry> statusByResourceId;
	

	/**
	 * The handler. There should be only one - owned by the hook instance
	 */
	public NodeStatusManager(DesignerContext ctx,ApplicationRequestHandler h) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = ctx;
		this.handler = h;
		this.notificationHandler = NotificationHandler.getInstance();
		this.projectName = context.getProject().getName();
		childrenByResourceId = new HashMap<>();
		statusByResourceId = new HashMap<>();
	}
	
	/**
	 * Define status for a new resource. The default should work for newly discovered resources.
	 * If this is re-called with the same resource, ignore.
	 * @param resourceId
	 */
	public void createResourceStatus(AbstractResourceNavTreeNode node,ProjectResourceId parentResourceId,ProjectResourceId resourceId) {
		if(node.getProjectResource()==null) throw new IllegalArgumentException("No project resource");
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se == null ) {
			DiagramState s = handler.getDiagramState(resourceId);
			se = new StatusEntry(node,s);
			se.setAlerting(handler.isAlerting(resourceId));
			notificationHandler.addNotificationChangeListener(NotificationKey.keyForAlert(resourceId), CLSS, this);
			statusByResourceId.put(resourceId,se);
		}
		// We had a "provisional" entry 
		else if( se.getNode()==null ) {
			se.setNode(node);
		}
		Set<ProjectResourceId> set = childrenByResourceId.get(parentResourceId);
		if( set == null ) {
			set = new HashSet<>();
			childrenByResourceId.put(parentResourceId,set);
		}
		set.add(parentResourceId);
		log.debugf("%s.createResourceStatus: %s (%d:%d) %s",CLSS,(node==null?"":node.getName()),parentResourceId,resourceId,
				                                           (se.getState()==null?"":se.getState().name()));
	}
	
	/**
	 * Define status for the root of the resource tree. 
	 * WARNING: The root node has no associated project resources.
	 * @param node of resource tree
	 */
	public void createRootResourceStatus(GeneralPurposeTreeNode rootNode) {
		log.tracef("%s.newRootResource",CLSS);
		ProjectResourceId resourceId = rootNode.getResourceId();
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se == null ) {
			DiagramState s = handler.getDiagramState(resourceId);  
			statusByResourceId.put(resourceId,se);
		}
		if( childrenByResourceId.get(resourceId) == null ) {
			Set<ProjectResourceId> set = new HashSet<>();
			set.add(resourceId);
			childrenByResourceId.put(resourceId,set);
		}
	}	
	/**
	 * Delete a resource. Prepare the real node for deletion as well.
	 * @param resourceId
	 */
	public void deleteResource(ProjectResourceId key ) {
		log.debugf("%s.deleteResource(%s)",CLSS,key.getResourcePath().getPath().toString());
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) {
			AbstractNavTreeNode antn = se.getParent();
			if( antn instanceof AbstractResourceNavTreeNode ) {
				ProjectResourceId parent = ((AbstractResourceNavTreeNode)antn).getResourceId();
				Set<ProjectResourceId> children = childrenByResourceId.get(parent);
				if( children!=null) {
					children.remove(key);
				}
				children = childrenByResourceId.get(key);
				recursivelyDeleteChildren(children);
				se.prepareToBeDeleted();
			}

		}
		statusByResourceId.remove(key);
	}
	/**
	 * Determine whether or not to display an alert badge. The node 
	 * is alerting if any of its children are alerting.
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public boolean getAlertState(ProjectResourceId resourceId) {
		boolean result = recursivelySearchForAlert(resourceId);
		return result;
	}
	
	/**
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public DiagramState getResourceState(ProjectResourceId resourceId) {
		DiagramState result = DiagramState.UNSET;
		StatusEntry se = statusByResourceId.get(resourceId);
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
	public AbstractResourceNavTreeNode findNode(ProjectResourceId key) {
		log.debugf("%s.findNode(%s)",CLSS,key.getResourcePath().getPath().toString());
		AbstractResourceNavTreeNode node = null;
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) node=se.getNode();
		return node;
	}
	
//	/**
//	 * Improved version of findNode.  Some newly created nodes don't seem to make it into here
//	 * @param resourceId
//	 * @return the AbstractResourceNavTreeNode associated with the specified resourceId.
//	 */
//	public AbstractResourceNavTreeNode findNodeSlower(long resourceId) {
//		log.debugf("%s.findNodeSlower(%d)",TAG,resourceId);
//		Long key = new Long(resourceId);
//		AbstractResourceNavTreeNode node = null;
//		StatusEntry se = childrenByResourceId.get(key);
//		if( se!=null ) node=se.getNode();
//		return node;
//	}
//	
	private void recursivelyDeleteChildren(Set<ProjectResourceId> children) {
		if( children==null ) return;
		for(ProjectResourceId child:children) {
			Set<ProjectResourceId> grandchildren = childrenByResourceId.get(child);
			childrenByResourceId.remove(child);
			recursivelyDeleteChildren(grandchildren);
		}
	}
	private boolean recursivelySearchForAlert(ProjectResourceId node) {
		if( node==null ) return false;
		StatusEntry se = statusByResourceId.get(node);
		if( se==null) return false;
		//log.tracef("%s.recursivelySearchForAlert: %s(%d) = %s",TAG,(se==null?"null":se.getName()),resourceId,(result?"TRUE":"FALSE"));
		if( se.isAlerting()) return true;
	
		Set<ProjectResourceId> children = childrenByResourceId.get(node);
		if( children!=null) {
			for(ProjectResourceId child:children) {
				boolean result = recursivelySearchForAlert(child);
				if( result ) return true;
			}
		}
		return false;
	}
	/**	
	 * A state change, is of necessity, accompanied by a save. Clear the dirty count.
	 * We explicitly synchronize with the gateway, but cache the result.
     */
	public void setResourceState(ProjectResourceId resourceId,DiagramState bs,boolean informGateway) {
		if( informGateway ) handler.setDiagramState(resourceId, bs.name());
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			se.setState(bs);
		}
		else {
			se = new StatusEntry(bs);
			statusByResourceId.put(resourceId,se);
		}
		log.tracef("%s.setResourceState: %s(%d) = %s",CLSS,se.getName(),resourceId,bs.name());
	}
	/**
	 * Called after a save from the main menu. Update the status
	 * of the nav-tree nodes.
	 */
	public void updateAll() {
		log.debugf("%s.updateAll()",CLSS);
		for(ProjectResourceId key:statusByResourceId.keySet()) {
			StatusEntry se = statusByResourceId.get(key);
			if( se!=null ) {
				se.setClean();
//				context.getProject().clearAllFlags();  // EREIAM JH - is this premature?
				se.reportDirtyState();
			}
		}
	}

	/**
	 * Get the parent's resourceId. Null if not found.
	 * @param presid
	 * @return the resourceId associated with the parent of the one specified
	 */
	public ProjectResourceId parentResourceId(ProjectResourceId resid) {
		log.debugf("%s.findNode(%d)",CLSS,resid.getResourcePath().getPath().toString());
		ProjectResourceId result = null;
		StatusEntry se = statusByResourceId.get(resid);
		if( se!=null ) { 
			AbstractNavTreeNode antn = se.getParent();
			if( antn instanceof AbstractResourceNavTreeNode) {
				result = ((AbstractResourceNavTreeNode)antn).getResourceId();
			}
		}
		return result;
	}

	/**
	 * For a diagram, the dirty child count represents dirty blocks.
	 * For folders, the children are other folders, including applications
	 * and families.
	 */
	private class StatusEntry {
		private boolean alerting = false;
		private boolean dirty = false;
		private int dirtyChildren = 0;
		private DiagramState state;
		private AbstractResourceNavTreeNode node;
		/**
		 * "Provisional" constructor. We've just created a node (probably
		 * via Application import). We have the resource state, but not 
		 * NavTreeNode.
		 */
		public StatusEntry(DiagramState s)  {
			this.state = s;
			this.node = null;
		}
		
		/**
		 * Constructor.
		 * @param antn
		 * @param s
		 */
		public StatusEntry(AbstractResourceNavTreeNode antn,DiagramState s)  {
			setNode(antn);
			this.state = s;
			this.node = antn;
		}
		
		public boolean isAlerting() { return alerting; }
		public void setAlerting(boolean flag) { alerting = flag; }
		public String getName() { return (node==null?"":node.getName()); }
		public AbstractResourceNavTreeNode getNode() { return node; }
		public void setNode(AbstractResourceNavTreeNode antn) { this.node = antn; }
		public AbstractNavTreeNode getParent() { return (node==null?null:node.getParent()); } 
		public DiagramState getState() { return state; }
		// Note: isDirty refers to the node of interest alone, excluding children
		public boolean isDirty() {return node.isChanged();} 

		public void prepareToBeDeleted() {
			if( node instanceof NavTreeNodeInterface && node.getName()!=BLTProperties.ROOT_FOLDER_NAME) {
				((NavTreeNodeInterface)this.node).prepareForDeletion();
			}
		}
		public void reportDirtyState() {
			if( node instanceof NavTreeNodeInterface) {
				((NavTreeNodeInterface)this.node).updateUI(isDirty());
			}
		}
		public void setClean() {
			if( node instanceof GeneralPurposeTreeNode) {
				((GeneralPurposeTreeNode)node).setDirty(false);
			}
		};
		public void setState(DiagramState s) { this.state = s; }
		@Override
		public String toString() {
			String dump = String.format("%s(%s, parent: %s, %s, %d dirty children",getName(),state.name(),getParent().getName(),
					(dirty?"dirty":"clean"),dirtyChildren);
			return dump;
		}
	}

// ================================ Notification Change Listener =========================================
@Override
public void diagramStateChange(String path, String state) {
	StatusEntry se = statusByResourceId.get(path);
	se.setAlerting(state.equalsIgnoreCase("true"));
	se.getNode().reload();
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
