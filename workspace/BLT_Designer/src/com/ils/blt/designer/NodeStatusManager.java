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
import com.ils.blt.designer.navtree.BLTNavTreeNode;
import com.inductiveautomation.ignition.common.project.ProjectResource;
import com.inductiveautomation.ignition.common.util.LogUtil;
import com.inductiveautomation.ignition.common.util.LoggerEx;
import com.inductiveautomation.ignition.designer.navtree.model.AbstractResourceNavTreeNode;


/**
 *  This class retains a list of all nav tree nodes associated with the
 *  toolkit. This allows us to re-use the nodes in the nav tree.
 *  
 *  We retains a map of these nodes indexed by resource number.
 *  It is a central reporting point for the current status of each node.
 *  
 *  The resourceId is known both the the view code and the nav tree.
 */
public class NodeStatusManager  {
	private static String TAG = "NodeStatusManager";
	private final LoggerEx log;
	private final Map<Long,StatusEntry> statusByResourceId;
	private final Map<Long,List<Long>> childrenByResourceId;
	

	/**
	 * The handler, make this private per Singleton pattern ...
	 */
	public NodeStatusManager() {
		log = LogUtil.getLogger(getClass().getPackage().getName());
		childrenByResourceId = new HashMap<>();
		statusByResourceId = new HashMap<>();
	}
	
	/**
	 * Define the root of the resource tree. 
	 * WARNING: The root node has no associated project resources.
	 * @param resourceId
	 */
	public void newRootResource(AbstractResourceNavTreeNode node) {
		log.tracef("%s.newRootResource",TAG);
		Long key = new Long(BLTProperties.ROOT_RESOURCE_ID);
		if( statusByResourceId.get(key) == null ) {
			Long parentKey = new Long(BLTProperties.ROOT_PARENT_ID);
			statusByResourceId.put(key,new StatusEntry(node,parentKey));
			List<Long>children = childrenByResourceId.get(parentKey);
			if( children==null) {
				children = new ArrayList<>();
				childrenByResourceId.put(parentKey, children);
			}
			children.add(key);
		}
	}
	
	/**
	 * Define a new resource. The default should work for newly discovered resources.
	 * If this is re-called with the same resource, ignore.
	 * @param resourceId
	 */
	public void newResource(AbstractResourceNavTreeNode node,long parentResourceId,long resourceId) {
		log.infof("%s.newResource(%d:%d)",TAG,parentResourceId,resourceId);
		if(node.getProjectResource()==null) throw new IllegalArgumentException("No project resource");
		Long key = new Long(resourceId);
		if( statusByResourceId.get(key) == null ) {
			statusByResourceId.put(key,new StatusEntry(node,parentResourceId));
			Long parentKey = new Long(parentResourceId);
			List<Long>children = childrenByResourceId.get(parentKey);
			if( children==null) {
				children = new ArrayList<>();
				childrenByResourceId.put(parentKey, children);
			}
			children.add(key);
		}
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
	 * Delete a resource. Remove it as a child of its parent.
	 * @param resourceId
	 */
	public void deleteResource(long resourceId ) {
		log.infof("%s.deleteResource(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null ) {
			((BLTNavTreeNode)se.getNode()).prepareForDeletion();
			Long parentKey = new Long(se.parentId);
			List<Long> siblings = childrenByResourceId.get(parentKey);
			if( siblings!=null) {
				siblings.remove(key);
			}
			else {
				log.warnf("%s.deleteResource(%d): No siblings found",TAG,resourceId);
			}
		}
		statusByResourceId.remove(key);
	}
	
	/**
	 * Do a recursive search of the node's children looking for any that should
	 * be deleted. This is painful - we'restructured to search up, not down.
	 * 
	 * @param resourceId
	 * @return
	 */
	public void getPendingDeletesUnderNode(long resourceId,List<Long> list ) {
		//log.tracef("%s.getPendingDeletesUnderNode(%d)",TAG,resourceId);
		Long key = new Long(resourceId);
		List<Long> children =  childrenByResourceId.get(key);
		if( children!=null ) {
			for( Long child:children ) {
				getPendingDeletesUnderNode(child.longValue(),list ); 
			}
		}
		StatusEntry se = statusByResourceId.get(key);
		if( se!=null && se.pendingDelete() ) {
			log.debugf("%s.getPendingDeletesUnderNode(%d): found %d",TAG,resourceId,key.longValue());
			list.add(key);
		}
	}
	
	public DiagramState getResourceState(long resourceId) {
		DiagramState result = DiagramState.ACTIVE;  
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) result = se.getState();
		return result;
	}
	/**
	 * We're being saved from the main menu. Everyone is clean
	 */
	public void cleanAll() {
		log.debugf("%s.cleanAll()",TAG);
		for(Long key:statusByResourceId.keySet()) {
			StatusEntry se = statusByResourceId.get(key);
			if( se!=null ) {
				se.setDirty(false);
			}
		}
	}
	
	/**
	 * We're being saved from the level of the subject node.
	 * Set this node and all its descendants to clean.
	 * 
	 */
	public void clean(AbstractResourceNavTreeNode node) {
		long resid = node.getProjectResource().getResourceId();
		log.debugf("%s.clean: %d",TAG,resid);
		setResourceDirty(resid,false);
		@SuppressWarnings("unchecked")
		Enumeration<AbstractResourceNavTreeNode> walker = node.children();
		while( walker.hasMoreElements() ) {
			AbstractResourceNavTreeNode child = walker.nextElement();
			clean(child);
		}
	}
	public boolean isPendingDelete(long resourceId) {
		boolean result = true;        // If we haven't seen it yet, it's dirty
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) result = se.pendingDelete();
		else log.debugf("%s.isPendingDelete(%d) - resource UNDEFINED",TAG,resourceId);
		return result;
	}
	
	public boolean isResourceDirty(long resourceId) {
		boolean result = true;        // If we haven't seen it yet, it's dirty
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) result = se.isDirty();
		else log.debugf("%s.isResourceDirty(%d) - resource UNDEFINED",TAG,resourceId);
		return result;
	}
	/**
	 * Mark the resource as deleted, but not saved as such.
	 * @param resourceId
	 */
	public void markPendingDelete(long resourceId) {
		log.infof("%s.markPendingDelete(%d)",TAG,resourceId);
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			se.setPendingDelete(true);
		}
	}
	/**
	 * Report a change in resource dirty state to the node.
	 * @param resourceId
	 * @param dirtFlag
	 */
	public void setResourceDirty(long resourceId,boolean dirtFlag) {
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null && se.isDirty()!= dirtFlag ) {
			log.infof("%s.setResourceDirty: %s (%s)",TAG,(dirtFlag?"DIRTY":"CLEAN"),se.toString());
			se.setDirty(dirtFlag);    // Also sets the node status directly
		}
	}
	
	public void setResourceState(long resourceId,DiagramState bs) {
		StatusEntry se = statusByResourceId.get(resourceId);
		if( se!=null ) {
			se.setState(bs);
		}
	}
	
	/**
	 * For a diagram, the dirty child count represents dirty blocks.
	 * For folders, the children are other folders, including applications
	 * and families.
	 * 
	 * @author chuckc
	 */
	private class StatusEntry {
		private boolean dirty = false;
		private boolean deletePending = false;
		private final long parentId; 
		private DiagramState state = DiagramState.ACTIVE;
		private final AbstractResourceNavTreeNode node;
		
		public StatusEntry(AbstractResourceNavTreeNode antn, long parent)  {
			this.parentId = parent;
			this.node = antn;
		}
		public AbstractResourceNavTreeNode getNode() { return node; }
		public DiagramState getState() {return state;}
		public boolean isDirty() {return dirty;}
		public boolean pendingDelete() { return deletePending; }
		public void setDirty(boolean dirty) {
			this.dirty = dirty;
			if( node instanceof BLTNavTreeNode) {
				((BLTNavTreeNode)this.node).setDirty(dirty);
			}
		}
		public void setPendingDelete(boolean flag) { deletePending = flag; }
		public void setState(DiagramState state) {this.state = state;}
		@Override
		public String toString() {
			ProjectResource pr = node.getProjectResource();
			long resid = BLTProperties.ROOT_RESOURCE_ID;
			if(pr!=null) resid = pr.getResourceId();
			String dump = String.format("%s(%d) was %s, parent: %d",node.getName(),resid,(dirty?"dirty":"clean"),parentId);
			return dump;
		}
	}
	
}
