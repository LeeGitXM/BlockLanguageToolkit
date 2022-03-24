/**
 *   (c) 2013-2022  ILS Automation. All rights reserved.
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
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.model.DesignerContext;
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
	private final Long projectId;
	private final Map<Long,Set<Long>>  childrenByResourceId;
	private final Map<Long,StatusEntry> statusByResourceId;
	

	/**
	 * The manager. There should be only one - owned by the hook instance
	 */
	public NodeStatusManager(DesignerContext ctx,ApplicationRequestHandler h) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.context = ctx;
		this.handler = h;
		this.notificationHandler = NotificationHandler.getInstance();
		this.projectId = new Long(context.getProject().getId());
		childrenByResourceId = new HashMap<>();
		statusByResourceId = new HashMap<>();
	}
	
	/**
	 * Define status for a new resource. The default should work for newly discovered resources.
	 * If this is re-called with the same resource, ignore. Initially we get the node state from the
	 * gateway
	 * @param resourceId
	 */
	public void createResourceStatus(AbstractResourceNavTreeNode node,long parentResourceId,long resourceId) {
		if(node.getProjectResource()==null) throw new IllegalArgumentException("No project resource");
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(key);
		if( se == null ) {
			DiagramState s = handler.getDiagramState(projectId, key);
			se = new StatusEntry(node,parentResourceId,s);
			se.setAlerting(handler.isAlerting(projectId, key));
			notificationHandler.addNotificationChangeListener(NotificationKey.keyForAlert(resourceId), CLSS, this);
			statusByResourceId.put(key,se);
		}
		// We had a "provisional" entry 
		else if( se.getNode()==null ) {
			se.setNode(node);
			se.setParent(parentResourceId);
		}
		Long parentKey = new Long(parentResourceId);
		Set<Long> set = childrenByResourceId.get(parentKey);
		if( set == null ) {
			set = new HashSet<>();
			childrenByResourceId.put(parentKey,set);
		}
		set.add(key);
		log.debugf("%s.createResourceStatus: %s (%d:%d) %s",CLSS,(node==null?"":node.getName()),parentResourceId,resourceId,
				                                           (se.getState()==null?"":se.getState().name()));
	}
	
	/**
	 * Define status for the root of the resource tree. 
	 * WARNING: The root node has no associated project resources.
	 * @param node of resource tree
	 */
	public void createRootResourceStatus(AbstractResourceNavTreeNode node) {
		log.tracef("%s.newRootResource",CLSS);
		Long key = new Long(BLTProperties.ROOT_RESOURCE_ID);
		Long parentKey = new Long(BLTProperties.ROOT_PARENT_ID); 
		if( statusByResourceId.get(key) == null ) {
			DiagramState s = handler.getDiagramState(projectId, key);  
			statusByResourceId.put(key,new StatusEntry(node,parentKey,s));
		}
		if( childrenByResourceId.get(key) == null ) {
			Set<Long> set = new HashSet<>();
			set.add(key);
			childrenByResourceId.put(parentKey,set);
		}
	}	
	/**
	 * Delete a resource. Prepare the real node for deletion as well.
	 * @param resourceId
	 */
	public void deleteResource(long resourceId ) {
		log.debugf("%s.deleteResource(%d)",CLSS,resourceId);
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			Long parent = new Long(se.parentId);
			Set<Long> children = childrenByResourceId.get(parent);
			if( children!=null) {
				children.remove(key);
			}
			children = childrenByResourceId.get(key);
			recursivelyDeleteChildren(children);
			se.prepareToBeDeleted();
		}
		statusByResourceId.remove(key);
	}
	/**
	 * Determine whether or not to display an alert badge. The node 
	 * is alerting if any of its children are alerting.
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public boolean getAlertState(long resourceId) {
		boolean result = recursivelySearchForAlert(new Long(resourceId));
		return result;
	}
	
	/**
	 * If the diagram is dirty, the state is the designer-scope unsaved state
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public DiagramState getResourceState(long resourceId) {
		DiagramState result = DiagramState.UNSET;
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			result = se.getState();
		}
		log.tracef("%s.getResourceState: %s(%d) = %s",CLSS,(se==null?"null":se.getName()),resourceId,result.name());
		return result;
	}
	
	/**
	 * Get the node. Null if not found. This is called in onClose() for a diagram workspace.
	 * @param resourceId
	 * @return the AbstractResourceNavTreeNode associated with the specified resourceId.
	 */
	public AbstractResourceNavTreeNode findNode(long resourceId) {
		log.debugf("%s.findNode(%d)",CLSS,resourceId);
		Long key = new Long(resourceId);
		AbstractResourceNavTreeNode node = null;
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) node=se.getNode();
		return node;
	}
	
	private void recursivelyDeleteChildren(Set<Long> children) {
		if( children==null ) return;
		for(Long child:children) {
			Set<Long> grandchildren = childrenByResourceId.get(child);
			childrenByResourceId.remove(child);
			recursivelyDeleteChildren(grandchildren);
		}
	}
	private boolean recursivelySearchForAlert(Long node) {
		if( node==null ) return false;
		StatusEntry se = statusByResourceId.get(node);
		if( se==null) return false;
		//log.tracef("%s.recursivelySearchForAlert: %s(%d) = %s",TAG,(se==null?"null":se.getName()),resourceId,(result?"TRUE":"FALSE"));
		if( se.isAlerting()) return true;
	
		Set<Long> children = childrenByResourceId.get(node);
		if( children!=null) {
			for(Long child:children) {
				boolean result = recursivelySearchForAlert(child);
				if( result ) return true;
			}
		}
		return false;
	}
	/**	
	 * A state change. If the state differs from the gateway, then the node is set to dirty.
     */
	public void setResourceState(long resourceId,DiagramState bs) {
		
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			se.setState(bs);
		}
		else {
			se = new StatusEntry(bs);
			statusByResourceId.put(resourceId,se);
		}
		Long key = new Long(resourceId);
		DiagramState gwstate = handler.getDiagramState(projectId,key);
		se.dirty = !se.getState().equals(gwstate);
		se.getNode().setItalic(se.dirty);
		log.tracef("%s.setResourceState: %s(%d) = %s",CLSS,se.getName(),resourceId,bs.name());
	}
	/**
	 * Called after a save from the main menu. Update the status
	 * of the nav-tree nodes.
	 */
	public void updateAll() {
		log.debugf("%s.updateAll()",CLSS);
		for(Long key:statusByResourceId.keySet()) {
			StatusEntry se = statusByResourceId.get(key);
			if( se!=null ) {
				se.setClean();
//				context.getProject().clearAllFlags();  // EREIAM JH - is this premature?
				se.reportDirtyState();
			}
		}
	}

	/**
	 * Get the parent's resourceId. Zero if not found.
	 * @param resourceId
	 * @return the resourceId associated with the parent of the one specified
	 */
	public long parentResourceId(long resourceId) {
		log.debugf("%s.findNode(%d)",CLSS,resourceId);
		Long key = new Long(resourceId);
		long result = 0;
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) result = se.getParent();
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
		private long parentId;           // A resourceId
		private long resourceId;
		private DiagramState state;
		private AbstractResourceNavTreeNode node;
		/**
		 * "Provisional" constructor. We've just created a node (probably
		 * via Application import). We have the resource state, but not 
		 * NavTreeNode.
		 */
		public StatusEntry(DiagramState s)  {
			this.parentId = -1;
			this.state = s;
			this.node = null;
		}
		
		/**
		 * Constructor.
		 * @param antn
		 * @param parent
		 * @param s
		 */
		public StatusEntry(AbstractResourceNavTreeNode antn, long parent, DiagramState s)  {
			setNode(antn);
			this.parentId = parent;
			this.state = s;
			this.node = antn;
		}
		
		public boolean isAlerting() { return alerting; }
		public void setAlerting(boolean flag) { alerting = flag; }
		public String getName() { return (node==null?"":node.getName()); }
		public AbstractResourceNavTreeNode getNode() { return node; }
		public void setNode(AbstractResourceNavTreeNode antn) {
			ProjectResource pr = antn.getProjectResource();
			long resid = BLTProperties.ROOT_RESOURCE_ID;
			if(pr!=null) resid = pr.getResourceId();
			this.resourceId = resid;
		}
		public long getParent() { return parentId; }
		public void setParent(long pid) { this.parentId=pid; }
		public DiagramState getState() { return state; }
		public boolean isDirty() {return context.getProject().isResourceDirty(resourceId);}

		public void prepareToBeDeleted() {
			if( node instanceof NavTreeNodeInterface && resourceId!=BLTProperties.ROOT_RESOURCE_ID) {
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
			String dump = String.format("%s(%d) %s, parent: %d, %s",getName(),resourceId,state.name(),parentId,
					(dirty?"dirty":"clean"),dirty);
			return dump;
		}
	}

// ================================ Notification Change Listener =========================================
@Override
public void diagramStateChange(long resId, String state) {
	StatusEntry se = statusByResourceId.get(new Long(resId));
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
