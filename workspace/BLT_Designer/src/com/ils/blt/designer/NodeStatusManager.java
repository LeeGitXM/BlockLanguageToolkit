/**
 *   (c) 2013-2016  ILS Automation. All rights reserved.
 */
package com.ils.blt.designer;

import java.util.HashMap;
import java.util.Map;

import com.ils.blt.common.ApplicationRequestHandler;
import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.DiagramState;
import com.ils.blt.common.notification.NotificationChangeListener;
import com.ils.blt.common.notification.NotificationKey;
import com.ils.blt.designer.navtree.NavTreeNodeInterface;
import com.inductiveautomation.ignition.common.model.values.QualifiedValue;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 *  This class retains a list of all nav tree nodes associated with the
 *  toolkit. This allows us to re-use the nodes in the nav tree. We retain
 *  a map of these nodes indexed by resource number.
 *  It is a central reporting point for the current status of each node.
 *  
 *  The nodes themselves have no dirty state. "dirtiness" refers to
 *  the presence of unsaved diagrams among their descendants.
 *  
 *  The resourceId is known to both the view code and the nav tree.
 */
public class NodeStatusManager implements NotificationChangeListener   {
	private static String TAG = "NodeStatusManager";
	private final LoggerEx log;
	private final ApplicationRequestHandler handler;
	private final NotificationHandler notificationHandler;
	private final Long projectId;
	private final Map<Long,StatusEntry> statusByResourceId;
	

	/**
	 * The handler. There should be only one - owned by the hook instance
	 */
	public NodeStatusManager(ApplicationRequestHandler h,long projId) {
		this.log = LogUtil.getLogger(getClass().getPackage().getName());
		this.handler = h;
		this.notificationHandler = NotificationHandler.getInstance();
		this.projectId = new Long(projId);
		statusByResourceId = new HashMap<>();
	}
		
	/**
	 * We're being saved from the main menu. Everyone is clean.
	 */
	public void cleanAll() {
		log.debugf("%s.cleanAll()",TAG);
		for(Long key:statusByResourceId.keySet()) {
			StatusEntry se = statusByResourceId.get(key);
			if( se!=null ) {
				se.clearDirtyChildCount();
				se.reportDirtyState();
			}
		}
	}
	
	/**
	 * If we're saving recursively downward, then we ignore
	 * dirty children. --- or perhaps we've saved a diagram.
	 * @param resourceId
	 */
	public void clearDirtyChildCount(long resourceId) {
		log.debugf("%s.clearDirtyChildCount(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) {
			int oldCount = se.getDirtyChildCount();
			se.clearDirtyChildCount();
			se.reportDirtyState();
			if( oldCount>0 ) {
				//Newly clean
				decrementDirtyNodeCount(se.parentId);
			}
		}
	}
	
	/**
	 * Define status for the root of the resource tree. 
	 * WARNING: The root node has no associated project resources.
	 * @param resourceId
	 */
	public void createRootResourceStatus(AbstractResourceNavTreeNode node) {
		log.tracef("%s.newRootResource",TAG);
		Long key = new Long(BLTProperties.ROOT_RESOURCE_ID);
		if( statusByResourceId.get(key) == null ) {
			Long parentKey = new Long(BLTProperties.ROOT_PARENT_ID);
			DiagramState s = handler.getDiagramState(projectId, key);  
			statusByResourceId.put(key,new StatusEntry(node,parentKey,s));
		}
	}
	
	/**
	 * Define status for a new resource. The default should work for newly discovered resources.
	 * If this is re-called with the same resource, ignore.
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
			notificationHandler.addNotificationChangeListener(NotificationKey.keyForAlert(resourceId), TAG, this);
			statusByResourceId.put(key,se);
		}
		// We had a "provisional" entry 
		else if( se.getNode()==null ) {
			se.setNode(node);
			se.setParent(parentResourceId);
		}
		log.debugf("%s.createResourceStatus: %s (%d:%d) %s",TAG,(node==null?"":node.getName()),parentResourceId,resourceId,
				                                           (se.getState()==null?"":se.getState().name()));
	}
	/**
	 * For a parent, note that a dirty child has been saved 
	 * (or deleted).
	 * @param resourceId
	 */
	public void decrementDirtyNodeCount(long resourceId) {
		log.debugf("%s.decrementDirtyNodeCount(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) {
			se.decrementDirtyChildCount();
			if( se.getDirtyChildCount()==0) {
				//Newly clean
				se.reportDirtyState();
				decrementDirtyNodeCount(se.parentId);
			}
		}
		else if( resourceId!=0 ) {
			log.warnf("%s.decrementDirtyNodeCount(%d) - status entry does not exist",TAG,resourceId);
		}
	}
	
	/**
	 * Delete a resource. Prepare the real node for deletion as well.
	 * @param resourceId
	 */
	public void deleteResource(long resourceId ) {
		log.debugf("%s.deleteResource(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) se.prepareToBeDeleted();
		statusByResourceId.remove(key);
	}
	
	/**
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public DiagramState getResourceState(long resourceId) {
		DiagramState result = DiagramState.UNSET;
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			result = se.getState();
		}
		log.tracef("%s.getResourceState: %s(%d) = %s",TAG,(se==null?"null":se.getName()),resourceId,result.name());
		return result;
	}
	
	/**
	 * @param resourceId
	 * @return a cached diagram state.
	 */
	public boolean getAlertState(long resourceId) {
		boolean result = false;
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			result = se.isAlerting();
		}
		log.tracef("%s.getAlertState: %s(%d) = %s",TAG,(se==null?"null":se.getName()),resourceId,(result?"TRUE":"FALSE"));
		return result;
	}
	/**
	 * Get the node. Null if not found.
	 * @param resourceId
	 * @return the AbstractResourceNavTreeNode associated with the specified resourceId.
	 */
	public AbstractResourceNavTreeNode findNode(long resourceId) {
		log.debugf("%s.findNode(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		AbstractResourceNavTreeNode node = null;
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) node=se.getNode();
		return node;
	}
	
	
	/**
	 * For any node indicate that the number of dirty children has increased.
	 * For a diagram, this means that either blocks are dirty, or a property
	 * change has been made. 
	 * 
	 * If this changes the state of the diagram, propagate up the tree.
	 * @param resourceId
	 */
	public void incrementDirtyNodeCount(long resourceId) {
		log.debugf("%s.incrementDirtyBlockCount(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) {
			se.incrementDirtyChildCount();
			if( se.getDirtyChildCount()==1 )  {
				// Newly dirty
				se.reportDirtyState();
				incrementDirtyNodeCount(se.parentId);
			}
		}
		else if( resourceId!=0 ) {
			log.warnf("%s.incrementDirtyBlockCount(%d) - status entry does not exist",TAG,resourceId);
		}
	}
	
	public boolean isResourceDirty(long resourceId) {
		boolean result = true;        // If we haven't seen it yet, it's dirty
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) result = se.isDirty();
		else log.debugf("%s.isResourceDirty(%d) - resource UNDEFINED",TAG,resourceId);
		return result;
	}
	
	public boolean isResourceDirtyOrHasDirtyChidren(long resourceId) {
		boolean result = true;        // If we haven't seen it yet, it's dirty
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) result = (se.isDirty() || se.getDirtyChildCount()>0);
		else log.debugf("%s.isResourceDirty(%d) - resource UNDEFINED",TAG,resourceId);
		return result;
	}

	/**	
	 * A state change, is of necessity, accompanied by a save. Clear the dirty count.
	 * We explicitly synchronize with the gateway, but cache the result.
	 */
	public void setResourceState(long resourceId,DiagramState bs) {
		handler.setDiagramState(projectId, new Long(resourceId), bs.name());
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			se.setState(bs);
			clearDirtyChildCount(resourceId);
		}
		else {
			se = new StatusEntry(bs);
			statusByResourceId.put(resourceId,se);
		}
		log.tracef("%s.setResourceState: %s(%d) = %s",TAG,se.getName(),resourceId,bs.name());
	}

	/**
	 * Get the parent's resourceId. Zero if not found.
	 * @param resourceId
	 * @return the resourceId associated with the parent of the one specified
	 */
	public long parentResourceId(long resourceId) {
		log.debugf("%s.findNode(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		long result = 0;
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) result = se.getParent();
		return result;
	}

//	
//	/**
//	 * Do a recursive search of the node's children looking for any that should
//	 * be deleted. This is painful - we'restructured to search up, not down.
//	 * 
//	 * @param resourceId
//	 * @return
//	 */
//	public void getPendingDeletesUnderNode(long resourceId,List<Long> list ) {
//		//log.tracef("%s.getPendingDeletesUnderNode(%d)",TAG,resourceId);
//		Long key = new Long(resourceId);
//		List<Long> children =  childrenByResourceId.get(key);
//		if( children!=null ) {
//			for( Long child:children ) {
//				getPendingDeletesUnderNode(child.longValue(),list ); 
//			}
//		}
//		StatusEntry se = statusByResourceId.get(key);
//		if( se!=null && se.pendingDelete() ) {
//			log.debugf("%s.getPendingDeletesUnderNode(%d): found %d",TAG,resourceId,key.longValue());
//			list.add(key);
//		}
//	}
//	


//	
//	/**
//	 * We're being saved from the level of the initial node.
//	 * Set this node and all its descendants to clean. 
//	 */
//	public void clean(AbstractResourceNavTreeNode node) {
//		long resid = node.getProjectResource().getResourceId();
//		log.debugf("%s.clean: %d",TAG,resid);
//		setResourceDirty(resid,false);
//		@SuppressWarnings("unchecked")
//		Enumeration<AbstractResourceNavTreeNode> walker = node.children();
//		while( walker.hasMoreElements() ) {
//			AbstractResourceNavTreeNode child = walker.nextElement();
//			clean(child);
//		}
//	}
//	
//	

//	public boolean isPendingDelete(long resourceId) {
//		boolean result = true;        // If we haven't seen it yet, it's dirty
//		StatusEntry se = statusByResourceId.get(resourceId);
//		if( se!=null ) result = se.pendingDelete();
//		else log.debugf("%s.isPendingDelete(%d) - resource UNDEFINED",TAG,resourceId);
//		return result;
//	}
//	

//	/**
//	 * Mark the resource as deleted, but not saved as such.
//	 * @param resourceId
//	 */
//	public void markPendingDelete(long resourceId) {
//		log.infof("%s.markPendingDelete(%d)",TAG,resourceId);
//		StatusEntry se = statusByResourceId.get(resourceId);
//		if( se!=null ) {
//			se.setPendingDelete(true);
//		}
//	}
//	
//
//	
//	/**
//	 * Report a change in resource dirty state to the node.
//	 * @param resourceId
//	 * @param dirtFlag
//	 */
//	public void setResourceDirty(long resourceId,boolean dirtFlag) {
//		StatusEntry se = statusByResourceId.get(resourceId);
//		if( se!=null && se.isDirty()!= dirtFlag || (se.getDirtyChildCount()==0)==dirtFlag )) {
//			log.infof("%s.setResourceDirty: %s (%s)",TAG,(dirtFlag?"DIRTY":"CLEAN"),se.toString());
//			se.setDirty(dirtFlag);   // Also sets the node status directly
//			// Propagate the change up the tree - if this represents a change
//			if( dirtFlag  ) recursivelyIncrementDirtyCount(se.parentId);
//			else            recursivelyDecrementDirtyCount(se.parentId);
//		}
//	}
//	
//	private void recursivelyDecrementDirtyCount(long resId) {
//		log.debugf("%s.recursivelyDecrementDirtyCount(%d)",TAG,resId);
//		StatusEntry se = statusByResourceId.get(resId);
//		if( se!=null ) {
//			se.decrementDirtyChildCount();
//			recursivelyDecrementDirtyCount(se.parentId);
//		}
//	}
//	private void recursivelyIncrementDirtyCount(long resId) {
//		log.debugf("%s.recursivelyIncrementDirtyCount(%d)",TAG,resId);
//		StatusEntry se = statusByResourceId.get(resId);
//		if( se!=null ) {
//			se.incrementDirtyChildCount();
//			recursivelyIncrementDirtyCount(se.parentId);
//		}
//	}
//	

	/**
	 * For a diagram, the dirty child count represents dirty blocks.
	 * For folders, the children are other folders, including applications
	 * and families.
	 * 
	 * @author chuckc
	 */
	private class StatusEntry {
		private boolean alerting = false;
		private boolean dirty = false;
		private int dirtyChildren = 0;
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
		public void clearDirtyChildCount() {dirtyChildren=0;}
		public void decrementDirtyChildCount() {dirtyChildren-=1;}
		public int getDirtyChildCount() {return dirtyChildren;}
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
		public void incrementDirtyChildCount() {dirtyChildren+=1;}
		// Note: isDirty refers to the node of interest alone, excluding children
		public boolean isDirty() {return dirtyChildren>0;}

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
		public void setState(DiagramState s) { this.state = s; }
		@Override
		public String toString() {
			String dump = String.format("%s(%d) %s, parent: %d, %s, %d dirty children",getName(),resourceId,state.name(),parentId,
					(dirty?"dirty":"clean"),dirtyChildren);
			return dump;
		}
	}

// ================================ Notification Change Listener =========================================
@Override
public void diagramAlertChange(long resId, String state) {
	StatusEntry se = statusByResourceId.get(new Long(resId));
	se.setAlerting(state.equalsIgnoreCase("true"));
	se.getNode().select();    // Cause a re-paint
}

@Override
public void bindingChange(String binding) {}

@Override
public void valueChange(QualifiedValue value) {}

@Override
public void watermarkChange(String newWatermark) {}
	
}
