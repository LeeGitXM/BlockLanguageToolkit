/**
 *   (c) 2013-2014  ILS Automation. All rights reserved.
 *  
 */
package com.ils.blt.designer;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ils.blt.common.BLTProperties;
import com.ils.blt.common.serializable.DiagramState;
import com.ils.blt.designer.navtree.NavTreeNodeInterface;
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
 *  The resourceId is known both the the view code and the nav tree.
 */
public class NodeStatusManager  {
	private static String TAG = "NodeStatusManager";
	private final LoggerEx log;
	private final Map<Long,StatusEntry> statusByResourceId;
	

	/**
	 * The handler, make this private per Singleton pattern ...
	 */
	public NodeStatusManager() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
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
			statusByResourceId.put(key,new StatusEntry(node,parentKey));
		}
	}
	
	/**
	 * Define status for a new resource. The default should work for newly discovered resources.
	 * If this is re-called with the same resource, ignore.
	 * @param resourceId
	 */
	public void createResourceStatus(AbstractResourceNavTreeNode node,long parentResourceId,long resourceId) {
		log.infof("%s.newResource(%d:%d)",TAG,parentResourceId,resourceId);
		if(node.getProjectResource()==null) throw new IllegalArgumentException("No project resource");
		Long key = new Long(resourceId);
		if( statusByResourceId.get(key) == null ) {
			statusByResourceId.put(key,new StatusEntry(node,parentResourceId));
		}
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
		log.infof("%s.deleteResource(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) se.prepareToBeDeleted();
		statusByResourceId.remove(key);
	}
	
	public DiagramState getResourceState(long resourceId) {
		DiagramState result = DiagramState.ACTIVE;  
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) result = se.getState();
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
	public void setResourceState(long resourceId,DiagramState bs) {
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			se.setState(bs);
		}
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
		private boolean dirty = false;
		private int dirtyChildren = 0;
		private final long parentId;           // A resourceId
		private final long resourceId;
		private DiagramState state = DiagramState.ACTIVE;
		private final AbstractResourceNavTreeNode node;
		
		public StatusEntry(AbstractResourceNavTreeNode antn, long parent)  {
			ProjectResource pr = antn.getProjectResource();
			long resid = BLTProperties.ROOT_RESOURCE_ID;
			if(pr!=null) resid = pr.getResourceId();
			this.resourceId = resid;
			this.parentId = parent;
			this.node = antn;
		}
		
		public void clearDirtyChildCount() {dirtyChildren=0;}
		public void decrementDirtyChildCount() {dirtyChildren-=1;}
		public int getDirtyChildCount() {return dirtyChildren;}
		public AbstractResourceNavTreeNode getNode() { return node; }
		public long getParent() { return parentId; }
		public DiagramState getState() {return state;}
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

		public void setState(DiagramState state) {this.state = state;}
		@Override
		public String toString() {
			
			String dump = String.format("%s(%d) was %s, parent: %d, dirty children %d",node.getName(),resourceId,(dirty?"dirty":"clean"),parentId,dirtyChildren);
			return dump;
		}
	}
	
}
